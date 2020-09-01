(* Copyright (C) 2019 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr> Raphël Monat
   <raphael.monat@lip6.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mir
open Lexing
open Mlexer

type mvg_function = {
  func_variable_inputs : unit VariableMap.t;
  func_constant_inputs : expression Pos.marked VariableMap.t;
  func_outputs : unit VariableMap.t;
  func_conds : condition_data VariableMap.t;
}

let fit_function (p : program) (f : mvg_function) : program =
  {
    p with
    program_vars =
      VariableMap.mapi
        (fun var var_data ->
          if VariableMap.mem var f.func_variable_inputs then
            {
              var_data with
              var_io = Input;
              var_definition =
                ( match var_data.var_definition with
                | InputVar | SimpleVar _ -> InputVar
                | TableVar _ ->
                    Errors.raise_spanned_error
                      (Format.asprintf
                         "Defining a variable input for a table variable %s is not supported"
                         (Pos.unmark var.Variable.name))
                      (Pos.get_position var.Variable.name) );
            }
          else if VariableMap.mem var f.func_constant_inputs then
            {
              var_data with
              var_io = Regular;
              var_definition =
                ( match var_data.var_definition with
                | SimpleVar _ -> SimpleVar (VariableMap.find var f.func_constant_inputs)
                | InputVar -> SimpleVar (VariableMap.find var f.func_constant_inputs)
                | TableVar _ ->
                    Errors.raise_spanned_error
                      (Format.asprintf
                         "Defining a constant input for a table variable %s is not supported"
                         (Pos.unmark var.Variable.name))
                      (Pos.get_position var.Variable.name) );
            }
          else if VariableMap.mem var f.func_outputs then
            {
              var_data with
              var_io = Output;
              var_definition =
                ( match var_data.var_definition with
                | SimpleVar old_e -> SimpleVar old_e
                | InputVar ->
                    Errors.raise_spanned_error
                      (Format.asprintf
                         "Defining an output for a input variable %s who is not defined is not \
                          supported"
                         (Pos.unmark var.Variable.name))
                      (Pos.get_position var.Variable.name)
                | TableVar _ ->
                    Errors.raise_spanned_error
                      (Format.asprintf "Defining an output for a table variable %s is not supported"
                         (Pos.unmark var.Variable.name))
                      (Pos.get_position var.Variable.name) );
            }
          else
            {
              var_data with
              var_io = Regular;
              var_definition =
                ( match var_data.var_definition with
                | InputVar -> SimpleVar (Pos.same_pos_as (Literal Undefined) var.Variable.name)
                | SimpleVar old -> SimpleVar old
                | TableVar (size, old) -> TableVar (size, old) );
            })
        p.program_vars;
    program_conds = VariableMap.union (fun _ _ _ -> assert false) p.program_conds f.func_conds;
  }

let reset_all_outputs (p : program) : program =
  {
    p with
    program_vars =
      VariableMap.mapi
        (fun var var_data ->
          match var_data.var_io with
          | Input ->
              {
                var_data with
                var_io = Input;
                var_definition =
                  ( match var_data.var_definition with
                  | InputVar | SimpleVar _ -> var_data.var_definition
                  | TableVar _ ->
                      Errors.raise_spanned_error
                        (Format.asprintf
                           "Defining a\n\
                           \             variable input for a table variable %s is not supported"
                           (Pos.unmark var.Variable.name))
                        (Pos.get_position var.Variable.name) );
              }
          | _ ->
              {
                var_data with
                var_io = Regular;
                var_definition =
                  ( match var_data.var_definition with
                  | InputVar -> assert false
                  | SimpleVar old -> SimpleVar old
                  | TableVar (size, old) -> TableVar (size, old) );
              })
        p.program_vars;
  }

let fit_function_to_combined_program (p : Bir.program) (conds : condition_data VariableMap.t) :
    Bir.program =
  (* because evaluate_program redefines everything each time, we have to make sure that the
     redefinitions of our constant inputs are removed from the main list of statements *)
  let new_stmts =
    List.filter_map
      (fun stmt ->
        match Pos.unmark stmt with
        | Bir.SAssign (var, var_data) -> (
            let new_var_data =
              {
                var_data with
                var_io = Regular;
                var_definition =
                  ( match var_data.var_definition with
                  | InputVar -> SimpleVar (Pos.same_pos_as (Literal Undefined) var.Variable.name)
                  | SimpleVar old -> SimpleVar old
                  | TableVar (size, old) -> TableVar (size, old) );
              }
            in
            match new_var_data.var_definition with
            | InputVar -> None
            | _ -> Some (Pos.same_pos_as (Bir.SAssign (var, new_var_data)) stmt) )
        | _ -> Some stmt)
      p.Bir.statements
  in
  let conditions_stmts =
    VariableMap.fold
      (fun _ cond stmts -> (Bir.SVerif cond, Pos.get_position cond.cond_expr) :: stmts)
      conds []
  in
  { p with Bir.statements = new_stmts @ conditions_stmts }

let var_set_from_variable_name_list (p : program) (names : string Pos.marked list) :
    unit VariableMap.t =
  List.fold_left
    (fun acc alias ->
      let name =
        try find_var_name_by_alias p alias with Errors.StructuredError _ -> Pos.unmark alias
      in
      let var =
        try Mast_to_mvg.list_max_execution_number (Pos.VarNameToID.find name p.program_idmap)
        with Not_found ->
          Errors.raise_spanned_error
            (Format.asprintf "unknown variable %s" name)
            (Pos.get_position alias)
      in
      VariableMap.add var () acc)
    VariableMap.empty names

let check_const_expression_is_really_const (e : expression Pos.marked) : unit =
  match Pos.unmark e with
  | Literal _ -> ()
  | _ ->
      Errors.raise_spanned_error
        "Constant input defined in function specification file is not a constant expression"
        (Pos.get_position e)

let const_var_set_from_list (p : program)
    (names : (string Pos.marked * Mast.expression Pos.marked) list) :
    Mir.expression Pos.marked VariableMap.t =
  List.fold_left
    (fun acc ((name, e) : string Pos.marked * Mast.expression Pos.marked) ->
      let var =
        try
          List.hd
            (List.sort
               (fun v1 v2 ->
                 compare v1.Mir.Variable.execution_number v2.Mir.Variable.execution_number)
               (Pos.VarNameToID.find (Pos.unmark name) p.program_idmap))
        with Not_found -> (
          try
            let name = find_var_name_by_alias p name in
            List.hd
              (List.sort
                 (fun v1 v2 ->
                   compare v1.Mir.Variable.execution_number v2.Mir.Variable.execution_number)
                 (Pos.VarNameToID.find name p.program_idmap))
          with Errors.StructuredError _ ->
            Errors.raise_spanned_error
              (Format.asprintf "unknown variable %s" (Pos.unmark name))
              (Pos.get_position e) )
      in
      let new_e =
        Mast_to_mvg.translate_expression
          {
            table_definition = false;
            idmap = p.program_idmap;
            lc = None;
            int_const_values = VariableMap.empty;
            exec_number = Mast_to_mvg.dummy_exec_number Pos.no_pos;
            current_lvalue = Pos.unmark name;
          }
          e
      in
      check_const_expression_is_really_const new_e;
      VariableMap.add var new_e acc)
    VariableMap.empty names

let translate_cond idmap (conds : Mast.expression Pos.marked list) : condition_data VariableMap.t =
  let check_boolean (mexpr : Mast.expression Pos.marked) =
    match Pos.unmark mexpr with
    | Binop (((And | Or), _), _, _) -> true
    | Comparison (_, _, _) -> true
    | Unop (Not, _) -> true
    | TestInSet _ -> true
    (* TODO: check Literal Variable ? *)
    | _ -> false
  in
  let mk_neg (mexpr : Mast.expression Pos.marked) =
    Pos.same_pos_as (Mast.Unop (Mast.Not, mexpr)) mexpr
  in
  let test_error =
    Mir.Error.new_error ("-1", Pos.no_pos) ("Condition error in tests", Pos.no_pos) Mast.Anomaly
  in
  let verif_conds =
    List.fold_left
      (fun acc cond ->
        if not (check_boolean cond) then
          Errors.raise_spanned_error "condition should have type bool" (Pos.get_position cond)
        else
          Pos.same_pos_as
            { Mast.verif_cond_expr = mk_neg cond; verif_cond_errors = [ ("-1", Pos.no_pos) ] }
            cond
          :: acc)
      [] conds
  in
  let program =
    Mast.Verification
      {
        verif_name = [ ("000", Pos.no_pos) ];
        verif_applications = [];
        verif_conditions = verif_conds;
      }
  in
  Mast_to_mvg.get_conds [ test_error ] idmap [ [ (program, Pos.no_pos) ] ] None

let read_function_from_spec (p : program) : mvg_function =
  let spec_file =
    match !Cli.function_spec with
    | None ->
        Errors.raise_error "function specification file is not specified using --function_spec"
    | Some f -> f
  in
  let input = open_in spec_file in
  let filebuf = Lexing.from_channel input in
  Cli.debug_print "Parsing %s" spec_file;
  let filebuf = { filebuf with lex_curr_p = { filebuf.lex_curr_p with pos_fname = spec_file } } in
  try
    let func_spec = Mparser.function_spec token filebuf in
    close_in input;
    {
      func_variable_inputs = var_set_from_variable_name_list p func_spec.Mast.spec_inputs;
      func_constant_inputs = const_var_set_from_list p func_spec.Mast.spec_consts;
      func_outputs = var_set_from_variable_name_list p func_spec.Mast.spec_outputs;
      func_conds = translate_cond p.program_idmap func_spec.Mast.spec_conditions;
    }
  with
  | Errors.StructuredError e ->
      close_in input;
      raise (Errors.StructuredError e)
  | Mparser.Error ->
      Cli.error_print "Lexer error\n";
      close_in input;
      exit 1

type full_program = {
  dep_graph : Mir_dependency_graph.G.t;
  execution_order : Mir_dependency_graph.execution_order;
  program : Mir.program;
}

let to_full_program (program : program) : full_program =
  let dep_graph = Mir_dependency_graph.create_dependency_graph program in
  { program; dep_graph; execution_order = Mir_dependency_graph.get_execution_order dep_graph }

let make_function_from_program (_program : full_program) :
    literal VariableMap.t -> Bir_interpreter.ctx =
 fun _ -> assert false

(* TODO: reimplement *)

let read_inputs_from_stdin (f : mvg_function) : literal VariableMap.t =
  Cli.result_print "Enter the input values of the program, followed by a semicolon:";
  VariableMap.mapi
    (fun var _ ->
      Format.printf "%s (%s) = "
        (match var.Variable.alias with Some s -> s | None -> Pos.unmark var.Variable.name)
        (Pos.unmark var.Variable.descr);
      let value = read_line () in
      try
        let value_ast = Mparser.literal_input token (Lexing.from_string value) in
        match value_ast with
        | Mast.Float f -> Mir.Float f
        | Mast.Variable _ -> Errors.raise_error "input must be a numeric constant"
      with Mparser.Error ->
        Cli.error_print "Lexer error in input!";
        exit 1)
    f.func_variable_inputs

let print_output (f : mvg_function) (results : Bir_interpreter.ctx) : unit =
  VariableMap.iter
    (fun var value ->
      if VariableMap.mem var f.func_outputs then
        Cli.result_print "%a" Bir_interpreter.format_var_literal_with_var (var, value))
    results.ctx_vars
