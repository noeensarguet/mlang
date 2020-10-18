(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir

let m_value_prelude : string = "#include \"m_value.h\""

let none_value = "m_undefined"

let generate_comp_op (op : Mast.comp_op) : string =
  match op with
  | Mast.Gt -> "m_gt"
  | Mast.Gte -> "m_gte"
  | Mast.Lt -> "m_lt"
  | Mast.Lte -> "m_lte"
  | Mast.Eq -> "m_eq"
  | Mast.Neq -> "m_neq"

let generate_binop (op : Mast.binop) : string =
  match op with
  | Mast.And -> "m_and"
  | Mast.Or -> "m_or"
  | Mast.Add -> "m_add"
  | Mast.Sub -> "m_sub"
  | Mast.Mul -> "m_mul"
  | Mast.Div -> "m_div"

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "m_not" | Mast.Minus -> "m_neg"

let generate_variable fmt (var : Variable.t) : unit =
  let v = match var.alias with Some v -> v ^ "_alias" | None -> Pos.unmark var.Variable.name in
  let v = String.lowercase_ascii v in
  let v =
    if
      same_execution_number var.Variable.execution_number
        (Mast_to_mvg.dummy_exec_number (Pos.get_position var.Variable.name))
    then v
    else
      Format.asprintf "%s_%d_%d" v var.Variable.execution_number.Mir.rule_number
        var.Variable.execution_number.Mir.seq_number
  in
  if '0' <= v.[0] && v.[0] <= '9' then Format.fprintf fmt "var_%s" v else Format.fprintf fmt "%s" v

let generate_name (v : Variable.t) : string =
  "v_" ^ match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let rec generate_c_expr (e : expression Pos.marked) :
    string * (LocalVariable.t * expression Pos.marked) list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "%s(%s, %s)" (generate_comp_op (Pos.unmark op)) se1 se2, s1 @ s2)
  | Binop (op, e1, e2) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "%s(%s, %s)" (generate_binop (Pos.unmark op)) se1 se2, s1 @ s2)
  | Unop (op, e) ->
      let se, s = generate_c_expr e in
      (Format.asprintf "%s(%s)" (generate_unop op) se, s)
  | Index (var, e) ->
      let se, s = generate_c_expr e in
      let size = Option.get (Pos.unmark var).Mir.Variable.is_table in
      (Format.asprintf "m_array_index(%a, %s, %d)" generate_variable (Pos.unmark var) se size, s)
  | Conditional (e1, e2, e3) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      let se3, s3 = generate_c_expr e3 in
      (Format.asprintf "m_cond(%s, %s, %s)" se1 se2 se3, s1 @ s2 @ s3)
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_present(%s)" se, s)
  | FunctionCall (NullFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_null(%s)" se, s)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_round(%s)" se, s)
  | FunctionCall (InfFunc, [ arg ]) ->
      let se, s = generate_c_expr arg in
      (Format.asprintf "m_floor(%s)" se, s)
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "m_max(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "m_min(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall (Multimax, [ e1; e2 ]) ->
      let se1, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "m_multimax(%s, %s)" se1 se2, s1 @ s2)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (Format.asprintf "m_literal(%s)" (string_of_float f), [])
  | Literal Undefined -> (Format.asprintf "%s" none_value, [])
  | Var var -> (Format.asprintf "%a" generate_variable var, [])
  | LocalVar lvar -> (Format.asprintf "v_%d" lvar.LocalVariable.id, [])
  | GenericTableIndex -> (Format.asprintf "generic_index", [])
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let _, s1 = generate_c_expr e1 in
      let se2, s2 = generate_c_expr e2 in
      (Format.asprintf "%s" se2, s1 @ ((lvar, e1) :: s2))

let format_local_vars_defs fmt (defs : (LocalVariable.t * expression Pos.marked) list) =
  List.iter
    (fun (lvar, e) ->
      let se, _ = generate_c_expr e in
      Format.fprintf fmt "v_%d = %s;@\n" lvar.LocalVariable.id se)
    defs

let generate_var_def var data (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      let se, defs = generate_c_expr e in
      Format.fprintf oc "%a%a = %s;@\n" format_local_vars_defs defs generate_variable var se
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          IndexMap.iter (fun i v ->
              let sv, defs = generate_c_expr v in
              Format.fprintf fmt "%a%a[%d] = %s;@\n" format_local_vars_defs defs generate_variable
                var i sv))
        es
  | TableVar (_size, IndexGeneric e) ->
      (* Format.asprintf "for (int generic_index=0; generic_index < %d; generic_index++) {@\n\ @[<h
         4> %a = %a;@]@\n\ }@\n" size generate_variable var generate_c_expr e *)
      Errors.raise_spanned_error "generic index table definitions not supported in C the backend"
        (Pos.get_position e)
  | InputVar -> assert false

let generate_var_cond (cond : condition_data) (oc : Format.formatter) =
  let scond, defs = generate_c_expr cond.cond_expr in
  let percent = Re.Pcre.regexp "%" in
  Format.fprintf oc
    "%acond = %s;@\n\
     if (m_is_defined_true(cond)) {@\n\
    \    printf(\"Error triggered: %a\\n\");@\n\
    \    return m_empty_output();@\n\
     }@\n"
    format_local_vars_defs defs scond
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt err ->
         let error_descr = Pos.unmark err.Error.descr in
         let error_descr = Re.Pcre.substitute ~rex:percent ~subst:(fun _ -> "%%") error_descr in
         Format.fprintf fmt "%s: %s" (Pos.unmark err.Error.name) error_descr))
    cond.cond_errors

let fresh_cond_counter = ref 0

let rec generate_stmt program oc stmt =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def var vdata oc
  | SConditional (cond, tt, ff) ->
      let pos = Pos.get_position stmt in
      let fname =
        String.map (fun c -> if c = '.' then '_' else c) (Filename.basename (Pos.get_file pos))
      in
      let cond_name =
        Format.asprintf "cond_%s_%d_%d_%d_%d_%d" fname (Pos.get_start_line pos)
          (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
          !fresh_cond_counter
      in
      fresh_cond_counter := !fresh_cond_counter + 1;
      let scond, defs = generate_c_expr (Pos.same_pos_as cond stmt) in
      Format.fprintf oc
        "%am_value %s = %s;@\n\
         if (m_is_defined_true(%s)) {@\n\
         @[<h 4>    %a@]@\n\
         };@\n\
         if (m_is_defined_false(%s)) {@\n\
         @[<h 4>    %a@]@\n\n\
         };@\n"
        format_local_vars_defs defs cond_name scond cond_name (generate_stmts program) tt cond_name
        (generate_stmts program) ff
  | SVerif v -> generate_var_cond v oc

and generate_stmts (program : Bir.program) oc stmts =
  Format.pp_print_list (generate_stmt program) oc stmts

let generate_main_function_signature_and_var_decls (p : Bir.program) (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "m_output m_extracted(m_input input) {@\n@[<h 4>    @\n";
  Format.fprintf oc "// First we extract the input variables from the dictionnary:@\n%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %a = input.%s;" generate_variable var (generate_name var)))
    input_vars;
  let assigned_variables = Bir.get_assigned_variables p in
  Format.fprintf oc "%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, ()) ->
         match var.Mir.Variable.is_table with
         | None -> Format.fprintf fmt "m_value %a;" generate_variable var
         | Some size -> Format.fprintf fmt "m_value %a[%d];" generate_variable var size))
    (Mir.VariableMap.bindings
       (Mir.VariableMap.filter (fun var _ -> not (List.mem var input_vars)) assigned_variables));
  Format.fprintf oc "%a@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, ()) -> Format.fprintf fmt "m_value v_%d;" var.Mir.LocalVariable.id))
    (Mir.LocalVariableMap.bindings (Bir.get_local_variables p));
  Format.fprintf oc "m_value cond;@\n@\n"

let generate_return oc (function_spec : Bir_interface.bir_function) =
  let returned_variables = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "return (struct m_output){@\n@[<h 4>    %a@]@\n};@\n@]@\n}"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt ".%s = %a," (generate_name var) generate_variable var))
    returned_variables

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc "// %s\n\n" Prelude.message;
  Format.fprintf oc "#include <stdio.h>\n";
  Format.fprintf oc "%s\n\n" m_value_prelude

let generate_input_type (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let input_vars = List.map fst (VariableMap.bindings function_spec.func_variable_inputs) in
  Format.fprintf oc "typedef struct m_input {@[<h 4>    %a@]@\n} m_input;@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var) (Pos.unmark var.Variable.descr)))
    input_vars;
  Format.fprintf oc
    "m_input m_empty_input() {@\n\
     @[<h 4>    return (struct m_input){@\n\
     @[<h 4>    %a@]@\n\
     };@]@\n\
     };@\n\
     @\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt ".%s = m_undefined," (generate_name var)))
    input_vars

let generate_output_type (oc : Format.formatter) (function_spec : Bir_interface.bir_function) =
  let output_vars = List.map fst (VariableMap.bindings function_spec.func_outputs) in
  Format.fprintf oc "typedef struct m_output {@\n@[<h 4>    %a@]@\n} m_output;@\n@\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var ->
         Format.fprintf fmt "m_value %s; // %s" (generate_name var) (Pos.unmark var.Variable.descr)))
    output_vars;
  Format.fprintf oc
    "m_output m_empty_output() {@\n\
     @[<h 4>    return (struct m_output){@\n\
     @[<h 4>    %a@]@\n\
     };@]@\n\
     };@\n\
     @\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt var -> Format.fprintf fmt ".%s = m_undefined," (generate_name var)))
    output_vars

let generate_c_program (program : Bir.program) (function_spec : Bir_interface.bir_function)
    (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a" generate_header () generate_input_type function_spec
    generate_output_type function_spec
    (generate_main_function_signature_and_var_decls program)
    function_spec (generate_stmts program) program.statements generate_return function_spec;
  close_out _oc
