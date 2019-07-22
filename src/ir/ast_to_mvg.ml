(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(** {!module: Ast } to {!module: Mvg } translation of M programs. *)

(** {1 Translation context } *)

(** {2 Variable declarations }*)

(** Program input/output *)
type io_status =
  | Input
  | Output
  | Constant
  | Regular (** Computed from other variables but not output *)

(** Intermediate container for variable declaration info *)
type var_decl_data = {
  var_decl_typ: Ast.value_typ option;
  var_decl_is_table: int option;
  var_decl_descr: string option;
  var_decl_io: io_status;
  var_pos: Ast.position;
}

(** {2 Loop translation context } *)

(**
   The M language has a strange way of doing loops. We translate them by unrolling; but for that
   we need a context to hold the loop parameters, which consists of a mapping from characters to
   integers or other characters.
*)

(** Map whose keys are loop parameters *)
module ParamsMap = Map.Make(Char)

(** The values of the map can be either strings of integers *)
type loop_param_value =
  | VarName of Ast.variable_name
  | RangeInt of int

let format_loop_param_value (v: loop_param_value) : string = match v with
  | VarName v -> v
  | RangeInt i -> string_of_int i

(**
   This is the context when iterating a loop : for each loop parameter, we have access to the
   current value of this loop parameter in this iteration.
*)
type loop_context = loop_param_value ParamsMap.t


(** Loops can have multiple loop parameters *)
type loop_domain = loop_param_value list ParamsMap.t

let format_loop_context (ld: loop_context) : string = ParamsMap.fold (fun param value acc ->
    acc ^ "; " ^ (Printf.sprintf "%c=" param)  ^
    format_loop_param_value value
  ) ld ""

let format_loop_domain (ld: loop_domain) : string = ParamsMap.fold (fun param values acc ->
    acc ^ "; " ^ (Printf.sprintf "%c=" param)  ^
    (String.concat "," (List.map (fun value -> format_loop_param_value value) values))
  ) ld ""

(**
   From a loop domain of varying loop parameters, builds by cartesian product the list of all
   iterations that the loop will take, each time assigining a different combination of values to the
   loop parameters.
*)
let rec iterate_all_combinations (ld: loop_domain) : loop_context list =
  try let param, values = ParamsMap.choose ld in
    match values with
    | [] -> []
    | hd::[] ->
      let new_ld = ParamsMap.remove param ld in
      let all_contexts = iterate_all_combinations new_ld in
      if List.length all_contexts = 0 then
        [ParamsMap.singleton param hd]
      else
        (List.map (fun c -> ParamsMap.add param hd c) all_contexts)
    | hd::tl ->
      let new_ld = ParamsMap.add param tl ld in
      let all_contexts_minus_hd_val_for_param = iterate_all_combinations new_ld in
      let new_ld = ParamsMap.add param [hd] ld in
      let all_context_with_hd_val_for_param = iterate_all_combinations new_ld in
      all_context_with_hd_val_for_param@all_contexts_minus_hd_val_for_param
  with
  | Not_found -> []

(** Helper to make a list of integers from a range *)
let rec make_range_list (i1: int) (i2: int) : loop_param_value list =
  if i1 > i2 then [] else
    let tl = make_range_list (i1 + 1) i2 in
    (RangeInt i1)::tl

(** {2 General translation context } *)

(** This context will be passed along during the translation *)
type translating_context = {
  table_definition: bool; (** [true] if translating an expression susceptible to contain a generic table index *)
  idmap : Mvg.idmap; (** Current string-to-{!type: Mvg.Variable.t} mapping *)
  lc: loop_context option; (** Current loop translation context *)
  int_const_values: int Mvg.VariableMap.t; (** Mapping from constant variables to their value *)
  exec_number: Mvg.execution_number; (** Number of the rule of verification condition being translated *)
}

(** Dummy execution number used for variable declarations *)
let dummy_exec_number (pos: Ast.position) : Mvg.execution_number =
  { Mvg.rule_number = -1; Mvg.seq_number = 0; pos }

(**
   When entering a loop, you are provided with a new loop context that you have to integrate
   to the general context with this function. The [position] argument is used to print an error message
   in case of the same loop parameters used in nested loops.
*)
let merge_loop_ctx (ctx: translating_context) (new_lc : loop_context) (pos:Ast.position) : translating_context =
  match ctx.lc with
  | None -> { ctx with lc = Some new_lc }
  | Some old_lc ->
    let merged_lc = ParamsMap.merge (fun param old_val new_val ->
        match (old_val, new_val) with
        | (Some _ , Some _) ->
          raise (Errors.TypeError
                   (Errors.LoopParam
                      (Printf.sprintf "Same loop parameter %c used in two nested loop contexts, %s"
                         param (Format_ast.format_position pos))))
        | (Some v, None) | (None, Some v) -> Some v
        | (None, None) -> assert false (* should not happen *)
      ) old_lc new_lc
    in
    { ctx with lc = Some merged_lc }

(** Helper to compute the max SSA candidate in a list *)
let rec list_max_execution_number (l: Mvg.Variable.t list) : Mvg.Variable.t =
  match l with
  | [] -> raise Not_found
  | [v] -> v
  | v::rest -> let max_rest = list_max_execution_number rest in
    match Mvg.(v.Mvg.Variable.execution_number ^^ max_rest.Mvg.Variable.execution_number) with
    | Mvg.Left -> v
    | Mvg.Right -> max_rest

(**
   Given a list of candidates for an SSA variable query, returns the correct one: the maximum in the
   same rule or if no candidates in the same rule, the maximum in other rules.
*)
let find_var_among_candidates
    (exec_number: Mvg.execution_number)
    (l: Mvg.Variable.t list)
  : Mvg.Variable.t =
  let same_rule =
    List.filter
      (fun var -> var.Mvg.Variable.execution_number.Mvg.rule_number = exec_number.Mvg.rule_number)
      l
  in
  if List.length same_rule = 0 then
    list_max_execution_number l
  else list_max_execution_number same_rule

(**
   Queries a [type: Mvg.variable.t] from an [type:idmap] mapping, the name of a variable and the rule number
   from which the variable is requested. Returns the variable with the same name and highest rule number
   that is below the current rule number from where this variable is requested
*)
let get_var_from_name
    (d:Mvg.Variable.t list Mvg.VarNameToID.t)
    (name:Ast.variable_name Ast.marked)
    (exec_number: Mvg.execution_number)
  : Mvg.Variable.t =
  try
    let same_name = Mvg.VarNameToID.find (Ast.unmark name) d in
    find_var_among_candidates exec_number
      (List.filter (fun var -> Mvg.(var.Mvg.Variable.execution_number <| exec_number)) same_name)
  with
  | Not_found ->
    raise (Errors.TypeError
             (Errors.Variable
                (Printf.sprintf "variable %s used %s, has not been declared"
                   (Ast.unmark name)
                   (Format_ast.format_position (Ast.get_position name))
                )))

(** Same but also take into account variables defined in the same execution unit *)
let get_var_from_name_lax
    (d:Mvg.Variable.t list Mvg.VarNameToID.t)
    (name:Ast.variable_name Ast.marked)
    (exec_number: Mvg.execution_number)
  : Mvg.Variable.t =
  try
    let same_name = Mvg.VarNameToID.find (Ast.unmark name) d in
    find_var_among_candidates exec_number
      (List.filter (fun var -> Mvg.(
           var.Mvg.Variable.execution_number <| exec_number ||
           var.Mvg.Variable.execution_number <=> exec_number
         )) same_name)
  with
  | Not_found ->
    raise (Errors.TypeError
             (Errors.Variable
                (Printf.sprintf "variable %s used %s, has not been declared"
                   (Ast.unmark name)
                   (Format_ast.format_position (Ast.get_position name))
                )))



(**{1 Translation }*)

(**{2 Loops }*)

(**
   The M language added a new feature in its 2017 edition : you can specify loop variable ranges bounds with
   constant variables. Because we need the actual value of the bounds to unroll everything, this function
   queries the const value in the context if needed.
*)
let var_or_int_value (ctx: translating_context) (l : Ast.literal Ast.marked) : int = match Ast.unmark l with
  | Ast.Int i -> i
  | Ast.Variable v ->
    (* We look up the value of the variable, which has to be const *)
    begin try Mvg.VariableMap.find
                (get_var_from_name ctx.idmap (Ast.same_pos_as (Ast.get_variable_name v) l) ctx.exec_number)
                ctx.int_const_values
      with
      | Not_found ->
        raise (Errors.TypeError
                 (Errors.Variable
                    (Printf.sprintf "variable %s used %s, is not an integer constant and cannot be used here"
                       (Ast.get_variable_name v)
                       (Format_ast.format_position (Ast.get_position l))
                    )))
    end
  | Ast.Float _ -> assert false (* should not happen *)

(**
   This function is the workhorse of loop unrolling : it takes a loop prefix containing the set of
   variables over which to iterate, and fabricates a combinator. This combinator takes auser-provided
   way of translating the loop body generically over the values of the iterated variables, and produce
   a list corresponding of the unrolled loop bodies expressions containing the iterated values.

   In OCaml terms, if you want [translate_loop_variables lvs f ctx], then you should define [f] by
   [let f = fun lc i ctx -> ...]  and use {!val: merge_loop_ctx} inside [...] before translating the loop
   body. [lc] is the loop context, [i] the loop sequence index and [ctx] the translation context.
*)
let translate_loop_variables (lvs: Ast.loop_variables Ast.marked) (ctx: translating_context) :
  ((loop_context -> int -> 'a ) -> 'a list) =
  match Ast.unmark lvs with
  | Ast.ValueSets lvs -> (fun translator ->
      let varying_domain = List.fold_left (fun domain (param, values) ->
          let values = List.flatten (List.map (fun value -> match value with
              | Ast.VarParam v -> [VarName (Ast.unmark v)]
              | Ast.IntervalLoop (i1,i2) -> make_range_list (var_or_int_value ctx i1) (var_or_int_value ctx i2)
            ) values) in
          ParamsMap.add (Ast.unmark param) values domain
        ) ParamsMap.empty lvs in
      let (_, t_list) =
        (List.fold_left
           (fun (i, t_list) lc ->
              let new_t = translator lc i in
              (i+1, new_t::t_list))
           (0, [])
           (iterate_all_combinations varying_domain))
      in
      (List.rev t_list)
    )
  | Ast.Ranges lvs -> (fun translator ->
      let varying_domain = List.fold_left (fun domain (param, values) ->
          let values = List.map (fun value -> match value with
              | Ast.VarParam v -> [VarName (Ast.unmark v)]
              | Ast.IntervalLoop (i1, i2) -> make_range_list (var_or_int_value ctx i1) (var_or_int_value ctx i2)
            ) values in
          ParamsMap.add (Ast.unmark param) (List.flatten values) domain
        ) ParamsMap.empty lvs in
      let (_, t_list) =
        (List.fold_left
           (fun (i, t_list) lc ->
              let new_t = translator lc i in
              (i+1, new_t::t_list))
           (0, [])
           (iterate_all_combinations varying_domain))
      in
      (List.rev t_list)
    )

(**{2 Variables }*)

(**
   Variables are tricky to translate; indeed, we have unrolled all the loops, and generic variables
   depend on the loop parameters. We have to interrogate the loop context for the current values of
   the loop parameter and then replace *inside the string* the loop parameter by its value to produce
   the new variable.
*)

(**
   A variable whose name is [X] should be translated as the generic table index expression.
   However sometimes there is a variable called [X] (yes...) so if there is no loop in the context
   we return the variable [X] for ["X"].
*)
let get_var_or_x
    (d:Mvg.Variable.t list Mvg.VarNameToID.t)
    (exec_number: Mvg.execution_number)
    (name:Ast.variable_name Ast.marked)
    (in_table: bool)
    (lax: bool)
  : Mvg.expression =
  if Ast.unmark name = "X" && in_table then
    Mvg.GenericTableIndex
  else if lax then
    Mvg.Var (get_var_from_name_lax d name exec_number)
  else
    Mvg.Var (get_var_from_name d name exec_number)


(**
   The M language has a weird and very annoying "feature", which is that you can define the following
   loop:
   {v
sum(i=05,06,07:Xi)
   v}
   In this example, what is [Xi] supposed to become ? [X05] ? [X5] ? The answer in the actual M codebase is:
   it depends. Indeed, sometimes [X05] is defines, sometimes it is [X5], and sometimes the [i=05,...] will
   have trailing zeroes, and sometimes not. So we have to try all combinations of trailing zeroes and find
   one where everything is correctly defined.

   It is unclear why this behavior is accepted by the M language. Maybe it has to do with the way a
   string to integer function works inside the official interpreter...
*)

(** To try everything, we have to cover all cases concerning trailing zeroes *)
type zero_padding =
  | ZPNone
  | ZPAdd
  | ZPRemove

let format_zero_padding (zp: zero_padding) : string = match zp with
  | ZPNone -> "no zero padding"
  | ZPAdd -> "add zero padding"
  | ZPRemove -> "remove zero padding"

(**{2 Preliminary passes }*)

(**
   Gets constant variables declaration data and values. Done in a separate pass because constant
   variables can be used in loop ranges bounds.
*)
let get_constants
    (p: Ast.program)
  : (var_decl_data Mvg.VariableMap.t * Mvg.idmap * int Mvg.VariableMap.t) =
  let (vars, idmap, int_const_list) =
    List.fold_left (fun (vars, (idmap : Mvg.idmap), int_const_list) source_file ->
        List.fold_left (fun (vars, (idmap : Mvg.idmap) , int_const_list) source_file_item ->
            match Ast.unmark source_file_item with
            | Ast.VariableDecl var_decl ->
              begin match var_decl with
                | Ast.ConstVar (marked_name, cval) ->
                  begin try
                      let old_var = List.hd (Mvg.VarNameToID.find (Ast.unmark marked_name) idmap) in
                      Cli.var_info_print
                        (Printf.sprintf "Dropping declaration of constant variable %s %s because variable was previously defined %s"
                           (Ast.unmark old_var.Mvg.Variable.name)
                           (Format_ast.format_position (Ast.get_position marked_name))
                           (Format_ast.format_position (Ast.get_position old_var.Mvg.Variable.name)));
                      (vars, idmap, int_const_list)
                    with
                    | Not_found ->
                      let new_var  =
                        Mvg.Variable.new_var
                          marked_name
                          None
                          (Ast.same_pos_as "constant" marked_name)
                          (dummy_exec_number (Ast.get_position marked_name))
                      in
                      let new_var_data = {
                        var_decl_typ = None;
                        var_decl_is_table = None;
                        var_decl_descr = None;
                        var_decl_io = Constant;
                        var_pos = Ast.get_position source_file_item;
                      } in
                      let new_vars = Mvg.VariableMap.add new_var new_var_data vars in
                      let new_idmap = Mvg.VarNameToID.add (Ast.unmark marked_name) [new_var] idmap in
                      let new_int_const_list = match Ast.unmark cval with
                        | Ast.Int i -> (new_var, i)::int_const_list
                        | Ast.Float f -> (new_var, int_of_float f)::int_const_list
                        | _ -> int_const_list
                      in
                      (new_vars, new_idmap, new_int_const_list)
                  end
                | _ -> (vars, idmap, int_const_list)
              end
            | _ -> (vars, idmap, int_const_list)
          ) (vars, idmap, int_const_list) source_file
      ) (Mvg.VariableMap.empty, Mvg.VarNameToID.empty, []) p in
  let int_const_vals : int Mvg.VariableMap.t = List.fold_left (fun out (var, i)  ->
      Mvg.VariableMap.add var i out
    ) Mvg.VariableMap.empty int_const_list
  in
  (vars, idmap, int_const_vals)

let belongs_to_app (r: Ast.application Ast.marked list) (application: string option) : bool = match application with
  | None -> true
  | Some application ->
    List.exists (fun app -> Ast.unmark app = application) r

(**
   Retrieves variable declaration data. Done in a separate pass because wen don't want to deal
   with sorting the dependencies between files or inside files.
*)
let get_variables_decl
    (p: Ast.program)
    (vars: var_decl_data Mvg.VariableMap.t)
    (idmap: Mvg.idmap)
  : (var_decl_data Mvg.VariableMap.t * Mvg.Error.t list * Mvg.idmap ) =
  let (vars, idmap, errors, out_list) =
    List.fold_left (fun (vars, (idmap: Mvg.idmap), errors, out_list) source_file ->
        List.fold_left (fun (vars, (idmap: Mvg.idmap), errors, out_list) source_file_item ->
            match Ast.unmark source_file_item with
            | Ast.VariableDecl var_decl ->
              begin match var_decl with
                | Ast.ComputedVar cvar ->
                  let cvar = Ast.unmark cvar in
                  (* First we check if the variable has not been declared a first time *)
                  begin try
                      let old_var = List.hd (Mvg.VarNameToID.find (Ast.unmark cvar.Ast.comp_name) idmap) in
                      Cli.var_info_print
                        (Printf.sprintf "Dropping declaration of %s %s because variable was previously defined %s"
                           (Ast.unmark old_var.Mvg.Variable.name)
                           (Format_ast.format_position (Ast.get_position cvar.Ast.comp_name))
                           (Format_ast.format_position (Ast.get_position old_var.Mvg.Variable.name)));
                      (vars, idmap, errors, out_list)
                    with
                    | Not_found ->
                      let new_var =
                        Mvg.Variable.new_var
                          cvar.Ast.comp_name
                          None
                          cvar.Ast.comp_description
                          (dummy_exec_number (Ast.get_position cvar.Ast.comp_name))
                      in
                      let new_var_data = {
                        var_decl_typ = Ast.unmark_option cvar.Ast.comp_typ;
                        var_decl_is_table = Ast.unmark_option cvar.Ast.comp_table;
                        var_decl_descr = Some (Ast.unmark cvar.Ast.comp_description);
                        var_decl_io = Regular;
                        var_pos = Ast.get_position source_file_item;
                      }
                      in
                      let new_vars = Mvg.VariableMap.add new_var new_var_data vars in
                      let new_idmap = Mvg.VarNameToID.add (Ast.unmark cvar.Ast.comp_name) [new_var] idmap in
                      let new_out_list = if List.exists (fun x -> match Ast.unmark x with
                          | Ast.GivenBack -> true
                          | Ast.Base -> false
                        ) cvar.Ast.comp_subtyp then
                          cvar.Ast.comp_name::out_list
                        else out_list
                      in
                      (new_vars, new_idmap, errors, new_out_list)
                  end
                | Ast.InputVar ivar ->
                  let ivar = Ast.unmark ivar in
                  begin try
                      let old_var = List.hd (Mvg.VarNameToID.find (Ast.unmark ivar.Ast.input_name) idmap) in
                      Cli.var_info_print
                        (Printf.sprintf "Dropping declaration of %s %s because variable was previously defined %s"
                           (Ast.unmark old_var.Mvg.Variable.name)
                           (Format_ast.format_position (Ast.get_position ivar.Ast.input_name))
                           (Format_ast.format_position (Ast.get_position old_var.Mvg.Variable.name)));
                      (vars, idmap, errors, out_list)
                    with
                    | Not_found ->
                      let new_var =
                        Mvg.Variable.new_var
                          ivar.Ast.input_name
                          (Some (Ast.unmark ivar.Ast.input_alias))
                          ivar.Ast.input_description
                          (dummy_exec_number (Ast.get_position ivar.Ast.input_name))
                          (* Input variables also have a low order *)
                      in
                      let new_var_data = {
                        var_decl_typ = begin match Ast.unmark_option ivar.Ast.input_typ with
                          | Some x -> Some x
                          | None -> begin match Ast.unmark ivar.Ast.input_subtyp with
                              | Ast.Income -> Some Ast.Real
                              | _ -> None
                            end
                        end;
                        var_decl_is_table = None;
                        var_decl_descr = Some (Ast.unmark ivar.Ast.input_description);
                        var_decl_io = Input;
                        var_pos = Ast.get_position source_file_item;
                      } in
                      let new_vars = Mvg.VariableMap.add new_var new_var_data vars in
                      let new_idmap = Mvg.VarNameToID.add (Ast.unmark ivar.Ast.input_name) [new_var] idmap in
                      (new_vars, new_idmap, errors, out_list)
                  end
                | Ast.ConstVar (_, _) -> (vars, idmap, errors, out_list) (* already treated before *)
              end
            | Ast.Output out_name ->
              (vars, idmap, errors, out_name::out_list)
            | Ast.Error err ->
              let err = Mvg.Error.new_error err.Ast.error_name
                  (Ast.same_pos_as (String.concat ":" (List.map (fun s -> Ast.unmark s) err.Ast.error_descr)) err.Ast.error_name) in
              (vars, idmap, err::errors, out_list)
            | _ -> (vars, idmap, errors, out_list)
          ) (vars, idmap, errors, out_list) source_file
      ) (vars, (idmap : Mvg.idmap), [], []) p in
  let vars : var_decl_data Mvg.VariableMap.t =
    List.fold_left (fun vars out_name ->
        try
          let out_var = get_var_from_name_lax
              idmap
              out_name
              (dummy_exec_number (Ast.get_position out_name))
          in
          let data = Mvg.VariableMap.find out_var vars in
          Mvg.VariableMap.add out_var { data with var_decl_io = Output } vars
        with
        | Not_found -> assert false (* should not happen *)
      ) vars out_list
  in
  (vars, errors, idmap)

(**{2 SSA construction }*)

(**
   Call it with [translate_variable idmap exec_number table_definition lc var lax]. SSA is all about
   assigning the correct variable assignment instance when a variable is used somewhere.
   That is why [translate_variable] needs the [execution_number]. [table_definition] is needed because
   the M language has a special ["X"] variable (generic table index) that has to be distinguished
   from a variable simply named ["X"]. [lc] is the loop context and the thing that complicates this
   function the most: variables used inside loops have loop parameters that have to be instantiated
   to give a normal variable name. [var] is the main argument that you want to translate. [lax] is
   a special argument for SSA construction that, if sets to [true], allows [translate_variable] to
   return a variable assigned exactly at [exec_number] (otherwise it always return a variable) in
   another rule or before in the same rule.
*)
let rec translate_variable
    (idmap: Mvg.idmap)
    (exec_number: Mvg.execution_number)
    (table_definition : bool)
    (lc: loop_context option)
    (var: Ast.variable Ast.marked)
    (lax: bool)
  : Mvg.expression Ast.marked =
  match Ast.unmark var with
  | Ast.Normal name ->
    Ast.same_pos_as (get_var_or_x idmap exec_number (Ast.same_pos_as name var) table_definition lax) var
  | Ast.Generic gen_name ->
    if List.length gen_name.Ast.parameters == 0 then
      translate_variable idmap exec_number table_definition lc (Ast.same_pos_as (Ast.Normal gen_name.Ast.base) var) lax
    else match lc with
      | None ->
        raise (Errors.TypeError
                 (Errors.LoopParam "variable contains loop parameters but is not used inside a loop context"))
      | Some _ -> instantiate_generic_variables_parameters idmap exec_number table_definition lc gen_name (Ast.get_position var) lax

(** The following function deal with the "trying all cases" pragma *)
and instantiate_generic_variables_parameters
    (idmap: Mvg.idmap)
    (exec_number: Mvg.execution_number)
    (table_definition : bool)
    (lc: loop_context option)
    (gen_name:Ast.variable_generic_name)
    (pos: Ast.position)
    (lax: bool)
  : Mvg.expression Ast.marked =
  instantiate_generic_variables_parameters_aux idmap exec_number table_definition lc gen_name.Ast.base ZPNone pos lax

and instantiate_generic_variables_parameters_aux
    (idmap: Mvg.idmap)
    (exec_number: Mvg.execution_number)
    (table_definition : bool)
    (lc: loop_context option)
    (var_name:string)
    (pad_zero: zero_padding)
    (pos: Ast.position)
    (lax: bool)
  : Mvg.expression Ast.marked
  =
  try match ParamsMap.choose_opt (
      match lc with
      | None ->
        raise (Errors.TypeError
                 (Errors.LoopParam "variable contains loop parameters but is not used inside a loop context"))
      | Some lc -> lc
    ) with
  | None ->
    translate_variable idmap exec_number table_definition lc (Ast.Normal var_name, pos) lax
  | Some (param, value) ->
        (*
          The [pad_zero] parameter is here because RangeInt variables are sometimes defined with a
          trailing 0 before the index. So the algorithm first tries to find a definition without the
          trailing 0, and if it fails it tries with a trailing 0.
      *)
    let new_var_name = match value with
      | VarName value ->
            (*
              Sometimes the DGFiP code inserts a extra 0 to the value they want to replace when it is
              already there... So we need to remove it here.
            *)
        let value = match pad_zero with
          | ZPNone -> value
          | ZPRemove ->
            string_of_int (int_of_string value)
          | ZPAdd ->
            "0" ^ value
        in
        Re.Str.replace_first (Re.Str.regexp (Printf.sprintf "%c" param)) value var_name
      | RangeInt i ->
        let value = match pad_zero with
          | ZPNone -> string_of_int i
          | ZPRemove ->
            string_of_int i
          | ZPAdd ->
            "0" ^ string_of_int i
        in
        Re.Str.replace_first (Re.Str.regexp (Printf.sprintf "%c" param))
          value var_name
    in
    instantiate_generic_variables_parameters_aux
      idmap exec_number table_definition
      (Some (ParamsMap.remove param (match lc with
           | None ->
             raise (Errors.TypeError
                      (Errors.LoopParam "variable contains loop parameters but is not used inside a loop context"))
           | Some lc -> lc))) new_var_name pad_zero pos lax
  with
  | err when (match lc with
      | None ->
        raise (Errors.TypeError
                 (Errors.LoopParam "variable contains loop parameters but is not used inside a loop context"))
      | Some lc ->
        ParamsMap.cardinal lc > 0
    ) ->
    let new_pad_zero = match pad_zero with
      | ZPNone -> ZPRemove
      | ZPRemove -> ZPAdd
      | _ -> raise err
    in
    instantiate_generic_variables_parameters_aux  idmap exec_number table_definition lc var_name new_pad_zero pos lax

(** Linear pass that fills [idmap] with all the variable assignments along with their execution number. *)
let get_var_redefinitions
    (p: Ast.program)
    (idmap: Mvg.idmap)
    (int_const_vals: int Mvg.VariableMap.t)
    (application: string option)
  : Mvg.idmap =
  let idmap =
    List.fold_left (fun (idmap: Mvg.idmap) source_file ->
        List.fold_left (fun (idmap: Mvg.idmap) source_file_item ->
            match Ast.unmark source_file_item with
            | Ast.Rule r ->
              let rule_number = Ast.rule_number r.Ast.rule_name in
              if not (belongs_to_app r.Ast.rule_applications application) then
                idmap
              else
                fst (List.fold_left (fun (idmap, seq_number) formula ->
                    match Ast.unmark formula with
                    | Ast.SingleFormula f ->
                      let exec_number = {
                        Mvg.rule_number = rule_number;
                        Mvg.seq_number = seq_number;
                        Mvg.pos = Ast.get_position f.Ast.lvalue
                      } in
                      let lvar = match Ast.unmark begin
                          translate_variable
                            idmap
                            exec_number
                            ((Ast.unmark f.Ast.lvalue).Ast.index <> None)
                            None
                            (Ast.unmark f.Ast.lvalue).Ast.var
                            false
                        end with
                      | Mvg.Var var -> var
                      | _ -> assert false (* should not happen *)
                      in
                      let new_var =
                        Mvg.Variable.new_var
                          lvar.Mvg.Variable.name
                          None
                          lvar.Mvg.Variable.descr
                          exec_number
                      in
                      let new_idmap =
                        Mvg.VarNameToID.add
                          (Ast.unmark lvar.Mvg.Variable.name)
                          (new_var::(Mvg.VarNameToID.find (Ast.unmark lvar.Mvg.Variable.name) idmap))
                          idmap
                      in
                      (new_idmap, seq_number + 1)
                    | Ast.MultipleFormulaes (lvs, f) ->
                      let exec_number = {
                        Mvg.rule_number = rule_number;
                        Mvg.seq_number = seq_number;
                        Mvg.pos = Ast.get_position f.Ast.lvalue
                      } in
                      let ctx = {
                        idmap;
                        lc = None;
                        int_const_values = int_const_vals;
                        table_definition = false;
                        exec_number;
                      } in
                      let loop_context_provider = translate_loop_variables lvs ctx in
                      let translator = fun lc idx ->
                        let exec_number = { exec_number with Mvg.seq_number = seq_number + idx } in
                        let lvar = match Ast.unmark begin
                            translate_variable
                              idmap
                              exec_number
                              ((Ast.unmark f.Ast.lvalue).Ast.index <> None)
                              (Some lc)
                              (Ast.unmark f.Ast.lvalue).Ast.var
                              false
                          end with
                        | Mvg.Var var -> var
                        | _ -> assert false (* should not happen *)
                        in
                        let new_var =
                          Mvg.Variable.new_var
                            lvar.Mvg.Variable.name
                            None
                            lvar.Mvg.Variable.descr
                            exec_number
                        in
                        (Ast.unmark lvar.Mvg.Variable.name, new_var)
                      in
                      let new_var_defs = loop_context_provider translator in
                      List.fold_left (fun (idmap, seq_number) (var_name, var_def) ->
                          (Mvg.VarNameToID.add var_name (
                              var_def::(Mvg.VarNameToID.find var_name idmap)
                            ) idmap, seq_number + 1)
                        ) (idmap, seq_number) new_var_defs
                  ) (idmap, 0) r.Ast.rule_formulaes)
            | _ -> idmap
          ) idmap source_file
      ) (idmap : Mvg.idmap) p in
  idmap

(** {2 Translation of expressions }*)

let translate_table_index (ctx: translating_context) (i: Ast.table_index Ast.marked) : Mvg.expression Ast.marked =
  match Ast.unmark i with
  | Ast.LiteralIndex i' -> Ast.same_pos_as (Mvg.Literal (Mvg.Int i')) i
  | Ast.SymbolIndex v ->
    let var = translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc (Ast.same_pos_as v i) false in
    var


(** Only accepts functions in {!type: Mvg.func}*)
let translate_function_name (f_name : string Ast.marked) = match Ast.unmark f_name with
  | "somme" -> Mvg.SumFunc
  | "min" -> Mvg.MinFunc
  | "max" -> Mvg.MaxFunc
  | "abs" -> Mvg.AbsFunc
  | "positif" -> Mvg.GtzFunc
  | "positif_ou_nul" -> Mvg.GtezFunc
  | "null" -> Mvg.NullFunc
  | "arr" -> Mvg.ArrFunc
  | "inf" -> Mvg.InfFunc
  | "present" -> Mvg.PresentFunc
  | "multimax" -> Mvg.Multimax
  | "supzero" -> Mvg.Supzero
  | x -> raise (Errors.TypeError (
      Errors.Function (
        Printf.sprintf "unknown function %s %s" x (Format_ast.format_position (Ast.get_position f_name))
      )))


(** Main translation function for expressions *)
let rec translate_expression (ctx : translating_context) (f: Ast.expression Ast.marked) : Mvg.expression Ast.marked =
  Ast.same_pos_as
    (match Ast.unmark f with
     | Ast.TestInSet (positive, e, values) ->
       let new_e = translate_expression ctx e in
       let local_var = Mvg.LocalVariable.new_var () in
       let local_var_expr =  Mvg.LocalVar local_var in
       let or_chain = List.fold_left (fun or_chain set_value ->
           let equal_test = match set_value with
             | Ast.VarValue set_var ->  Mvg.Comparison (
                 Ast.same_pos_as Ast.Eq set_var,
                 Ast.same_pos_as local_var_expr e,
                 translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc set_var false
               )
             | Ast.IntValue i -> Mvg.Comparison (
                 Ast.same_pos_as Ast.Eq i,
                 Ast.same_pos_as local_var_expr e,
                 Ast.same_pos_as (Mvg.Literal (Mvg.Int (Ast.unmark i))) i
               )
             | Ast.Interval (bn,en) ->
               if Ast.unmark bn > Ast.unmark en then
                 raise (Errors.TypeError
                          (Errors.Numeric
                             (Printf.sprintf "wrong interval bounds %s"
                                (Format_ast.format_position (Ast.get_position bn)))))
               else
                 Mvg.Binop (
                   Ast.same_pos_as Ast.And bn,
                   Ast.same_pos_as (Mvg.Comparison (
                       Ast.same_pos_as Ast.Gte bn,
                       Ast.same_pos_as local_var_expr e,
                       Ast.same_pos_as (Mvg.Literal (Mvg.Int (Ast.unmark bn))) bn
                     )) bn,
                   Ast.same_pos_as (Mvg.Comparison (
                       Ast.same_pos_as Ast.Lte en,
                       Ast.same_pos_as local_var_expr e,
                       Ast.same_pos_as (Mvg.Literal (Mvg.Int (Ast.unmark en))) en
                     )) en
                 )
           in
           Ast.same_pos_as (Mvg.Binop (
               Ast.same_pos_as Ast.Or f,
               Ast.same_pos_as equal_test f,
               or_chain
             )) f
         ) (Ast.same_pos_as (Mvg.Literal (Mvg.Bool false)) f) values in
       let or_chain = if not positive then
           Ast.same_pos_as (Mvg.Unop (Ast.Not, or_chain)) or_chain
         else or_chain
       in
       Mvg.LocalLet (local_var, new_e, or_chain)
     | Ast.Comparison (op, e1, e2) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       Mvg.Comparison (op, new_e1, new_e2)
     | Ast.Binop (op, e1, e2) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       Mvg.Binop (op, new_e1, new_e2)
     | Ast.Unop (op, e) ->
       let new_e = translate_expression ctx e in
       Mvg.Unop (op, new_e)
     | Ast.Index (t, i) ->
       let t_var = translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc t false in
       let new_i = translate_table_index ctx i in
       Mvg.Index ((match Ast.unmark t_var with
           | Mvg.Var v -> Ast.same_pos_as v t_var
           | _ -> assert false (* should not happen *)), new_i)
     | Ast.Conditional (e1, e2, e3) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       let new_e3 = match e3 with
         | Some e3 -> translate_expression ctx e3
         | None -> Ast.same_pos_as (Mvg.Literal Mvg.Undefined) e2
         (* the absence of a else branch for a ternary operators can yield an undefined term *)
       in
       let cond_var = Mvg.LocalVariable.new_var () in
       (*
         We put every conditional behind a local var to make it easier for later
         when we infer the bitvec size of every expression for Z3
       *)
       Mvg.LocalLet(cond_var,
                    Ast.same_pos_as (Mvg.Conditional (new_e1, new_e2, new_e3)) f,
                    Ast.same_pos_as (Mvg.LocalVar cond_var) f
                   )
     | Ast.FunctionCall (f_name, args) ->
       let f_correct = translate_function_name f_name in
       let new_args = translate_func_args ctx args in
       Mvg.FunctionCall (f_correct, new_args)
     | Ast.Literal l -> begin match l with
         | Ast.Variable var ->
           let new_var = translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc (Ast.same_pos_as var f) false in
           Ast.unmark new_var
         | Ast.Int i -> Mvg.Literal (Mvg.Int i)
         | Ast.Float f -> Mvg.Literal (Mvg.Float f)
       end
     (* These loops correspond to "pour un i dans ...: ... so it's OR "*)
     | Ast.Loop (lvs, e) ->
       let loop_context_provider = translate_loop_variables lvs ctx in
       let translator = fun lc _ ->
         let new_ctx = merge_loop_ctx ctx lc (Ast.get_position lvs) in
         translate_expression new_ctx e
       in
       let loop_exprs = loop_context_provider translator in
       List.fold_left (fun acc loop_expr ->
           Mvg.Binop(
             Ast.same_pos_as Ast.Or e,
             Ast.same_pos_as acc e,
             loop_expr
           )
         ) (Mvg.Literal (Mvg.Bool false)) loop_exprs
    ) f

(** Mutually recursive with {!val: translate_expression} *)
and translate_func_args (ctx: translating_context) (args: Ast.func_args) : Mvg.expression Ast.marked list =
  match args with
  | Ast.ArgList args -> List.map (fun arg ->
      translate_expression ctx arg
    ) args
  | Ast.LoopList (lvs, e) ->
    let loop_context_provider = translate_loop_variables lvs ctx in
    let translator = fun lc _ ->
      let new_ctx = merge_loop_ctx ctx lc (Ast.get_position lvs) in
      translate_expression new_ctx e
    in
    loop_context_provider translator

(** {2 Translation of source file items }*)

(** Helper type to indicate the kind of variable assignment *)
type index_def =
  | NoIndex
  | SingleIndex of int
  | GenericIndex

(** Translates lvalues into the assigning variable as well as the type of assignment *)
let translate_lvalue
    (ctx: translating_context)
    (lval: Ast.lvalue Ast.marked)
  : translating_context * Mvg.Variable.t * index_def =
  let var = match
      Ast.unmark (translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc (Ast.unmark lval).Ast.var true)
    with
    | Mvg.Var (var: Mvg.Variable.t) -> var
    | _ -> assert false (* should not happen *)
  in
  match (Ast.unmark lval).Ast.index with
  | Some ti -> begin match Ast.unmark ti with
      | Ast.SymbolIndex (Ast.Normal "X") -> ({ ctx with table_definition = true }, var, GenericIndex)
      | Ast.LiteralIndex i -> (ctx, var, SingleIndex i)
      | Ast.SymbolIndex ((Ast.Normal _ ) as v) ->
        let i = var_or_int_value ctx (Ast.same_pos_as (Ast.Variable v) ti) in
        (ctx, var, SingleIndex i)
      | Ast.SymbolIndex ((Ast.Generic _ ) as v) ->
        let mvg_v = translate_variable ctx.idmap ctx.exec_number ctx.table_definition ctx.lc (Ast.same_pos_as v ti) false in
        let i = var_or_int_value ctx (Ast.same_pos_as (Ast.Variable (match Ast.unmark mvg_v with
            | Mvg.Var v -> Ast.Normal (Ast.unmark v.Mvg.Variable.name)
            | _ -> assert false (* should not happen*)
          )) ti) in
        (ctx, var, SingleIndex i)
    end
  | None -> (ctx, var, NoIndex)


(** Date types are not supported *)
let translate_value_typ (typ: Ast.value_typ Ast.marked option) : Mvg.typ option =
  match typ with
  | Some (Ast.Integer, _) -> Some Mvg.Integer
  | Some (Ast.Boolean, _) -> Some Mvg.Boolean
  | Some (Ast.Real, _) -> Some Mvg.Real
  | Some (_ , _) -> Some Mvg.Integer
  | None -> None

(** Main toplevel declaration translator that adds a variable definition to the MVG program *)
let add_var_def
    (var_data : Mvg.variable_data Mvg.VariableMap.t)
    (var_lvalue: Mvg.Variable.t)
    (var_expr : Mvg.expression Ast.marked)
    (def_kind : index_def)
    (var_decl_data: var_decl_data Mvg.VariableMap.t)
    (idmap: Mvg.idmap)
  : Mvg.variable_data Mvg.VariableMap.t =
  let var_at_declaration =
    List.find
      (fun var ->
         Mvg.(var.Mvg.Variable.execution_number <=>
              (dummy_exec_number (Ast.get_position var_expr))))
      (Mvg.VarNameToID.find (Ast.unmark var_lvalue.name) idmap)
  in
  let decl_data = try
      Mvg.VariableMap.find var_at_declaration var_decl_data
    with
    | Not_found -> assert false (* should not happen *)
  in
  let var_typ = translate_value_typ
      (match decl_data.var_decl_typ with
       | Some x -> Some (x, decl_data.var_pos)
       | None -> None)
  in
  Mvg.VariableMap.add var_lvalue (
    try
      let io = match decl_data.var_decl_io with
        | Input -> Mvg.Input
        | Constant | Regular -> Mvg.Regular
        | Output -> Mvg.Output
      in
      if def_kind = NoIndex then
        { Mvg.var_definition = Mvg.SimpleVar var_expr;
          Mvg.var_typ = var_typ;
          Mvg.var_io = io;
        }
      else
        match decl_data.var_decl_is_table with
        | Some size -> begin
            match def_kind with
            | NoIndex -> assert false (* should not happen*)
            | SingleIndex i ->
              {
                Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexTable (Mvg.IndexMap.singleton i var_expr));
                Mvg.var_typ = var_typ;
                Mvg.var_io = io;
              }
            | GenericIndex ->
              {
                Mvg.var_definition = Mvg.TableVar (size, Mvg.IndexGeneric var_expr);
                Mvg.var_typ = var_typ;
                Mvg.var_io = io;
              }
          end
        | None -> raise (Errors.TypeError (
            Errors.Variable (
              Printf.sprintf "variable %s is defined %s as a table but has been declared %s as a non-table"
                (Ast.unmark var_lvalue.Mvg.Variable.name)
                (Format_ast.format_position (Ast.get_position var_expr))
                (Format_ast.format_position (Mvg.VariableMap.find var_lvalue var_decl_data).var_pos)
            )
          ))
    with
    | Not_found -> assert false (* should not happen *)
        (*
          should not happen since we already looked into idmap to get the var value
          from its name
        *)
  ) var_data


(**
   Main translation pass that deal with regular variable definition; returns a map whose keys are
   the variables being defined (with the execution number corresponding to the place where it is
   defined) and whose values are the expressions corresponding to the definitions.
*)
let get_var_data
    (idmap: Mvg.idmap)
    (var_decl_data: var_decl_data Mvg.VariableMap.t)
    (int_const_vals: int Mvg.VariableMap.t)
    (p: Ast.program)
    (application: string option)
  : Mvg.variable_data Mvg.VariableMap.t =
  List.fold_left (fun var_data source_file ->
      Cli.debug_print (Printf.sprintf "Expanding definitions in %s" (Ast.get_position (List.hd source_file)).Ast.pos_filename);
      List.fold_left (fun var_data  source_file_item ->
          match Ast.unmark source_file_item with
          | Ast.Rule r ->
            let rule_number = Ast.rule_number r.Ast.rule_name in
            if not (belongs_to_app r.Ast.rule_applications application) then
              var_data
            else
              fst (List.fold_left (fun (var_data, seq_number) formula ->
                  match Ast.unmark formula with
                  | Ast.SingleFormula f ->
                    let ctx = {
                      idmap;
                      lc = None;
                      int_const_values = int_const_vals;
                      table_definition = false;
                      exec_number = {
                        Mvg.rule_number = rule_number;
                        Mvg.seq_number = seq_number;
                        Mvg.pos = Ast.get_position f.Ast.lvalue
                      }
                    } in
                    let (ctx, var_lvalue, def_kind) = translate_lvalue ctx f.Ast.lvalue in
                    let var_expr = translate_expression ctx f.Ast.formula in
                    (add_var_def var_data var_lvalue var_expr def_kind var_decl_data idmap, seq_number + 1)
                  | Ast.MultipleFormulaes (lvs, f) ->
                    let ctx = {
                      idmap;
                      lc = None;
                      int_const_values = int_const_vals;
                      table_definition = false;
                      exec_number = {
                        Mvg.rule_number = rule_number;
                        Mvg.seq_number = seq_number;
                        Mvg.pos = Ast.get_position f.Ast.lvalue
                      }
                    } in
                    let loop_context_provider = translate_loop_variables lvs ctx in
                    let translator = fun lc idx ->
                      let new_ctx =
                        { ctx with
                          lc = Some lc ;
                          exec_number = {
                            Mvg.rule_number = rule_number;
                            Mvg.seq_number = seq_number + idx;
                            Mvg.pos = Ast.get_position f.Ast.lvalue
                          }
                        }
                      in
                      let (new_ctx, var_lvalue, def_kind) = translate_lvalue new_ctx f.Ast.lvalue in
                      let var_expr = translate_expression new_ctx f.Ast.formula in
                      (var_lvalue, var_expr, def_kind)
                    in
                    let data_to_add = loop_context_provider translator in
                    List.fold_left
                      (fun (var_data, seq_number) (var_lvalue, var_expr, def_kind) ->
                         (add_var_def var_data var_lvalue var_expr def_kind var_decl_data idmap, seq_number + 1)
                      ) (var_data, seq_number) data_to_add
                ) (var_data, 0) r.Ast.rule_formulaes)
          | Ast.VariableDecl (Ast.ConstVar (name, lit)) ->
            let var = get_var_from_name_lax idmap name (dummy_exec_number (Ast.get_position name)) in
            (add_var_def var_data var (Ast.same_pos_as (Mvg.Literal (begin match Ast.unmark lit with
                 | Ast.Variable var ->
                   raise (Errors.TypeError (
                       Errors.Variable (
                         Printf.sprintf "const variable %s declared %s cannot be defined as another variable"
                           (Format_ast.format_variable var)
                           (Format_ast.format_position (Ast.get_position source_file_item))
                       )))
                 | Ast.Int i -> Mvg.Int i
                 | Ast.Float f -> Mvg.Float f
               end)) lit) NoIndex var_decl_data idmap)
          | Ast.VariableDecl (Ast.InputVar (var, pos)) ->
            let var = get_var_from_name_lax idmap var.Ast.input_name (dummy_exec_number pos) in
            let var_decl = try Mvg.VariableMap.find var var_decl_data with
              | Not_found -> assert false (* should not happen *)
            in
            let typ = translate_value_typ (match var_decl.var_decl_typ  with
                | Some x -> Some (x, Ast.get_position var.Mvg.Variable.name)
                | None -> None
              ) in
            (Mvg.VariableMap.add
               var
               {
                 Mvg.var_definition = Mvg.InputVar;
                 Mvg.var_typ = typ;
                 Mvg.var_io = Mvg.Input;
               }
               var_data)
          | _ -> var_data
        ) var_data source_file
    ) Mvg.VariableMap.empty (List.rev p)


(**
   At this point [var_data] contains the definition data for all the times a variable is defined.
   However the M language deals with undefined variable, so for each variable we have to insert
   a dummy definition corresponding to the declaration and whose value is [Undefined].
*)
let add_dummy_definition_for_variable_declaration
    (var_data: Mvg.variable_data Mvg.VariableMap.t)
    (var_decl_data: var_decl_data Mvg.VariableMap.t)
    (idmap: Mvg.idmap)
  : Mvg.variable_data Mvg.VariableMap.t =
  Mvg.VariableMap.fold (fun var decl (var_data : Mvg.variable_data Mvg.VariableMap.t) ->
      (* The variable has not been defined in a rule *)
      begin match decl.var_decl_io with
        | Output | Regular  ->
          if List.for_all (fun var' ->
              var'.Mvg.Variable.execution_number.Mvg.rule_number = -1
            ) (Mvg.VarNameToID.find (Ast.unmark var.Mvg.Variable.name) idmap)
          then
            Cli.var_info_print (
              Printf.sprintf "variable %s declared %s is never defined in the application"
                (Ast.unmark var.Mvg.Variable.name)
                (Format_ast.format_position (Ast.get_position var.Mvg.Variable.name))
            );
          (* This is the case where the variable is not defined. *)
          let io = match decl.var_decl_io with
            | Output -> Mvg.Output
            | Regular | Constant -> Mvg.Regular
            | Input -> assert false (* should not happen *)
          in
          (* We insert the Undef expr *)
          begin match decl.var_decl_is_table with
            | Some size -> Mvg.VariableMap.add var {
                Mvg.var_definition = Mvg.TableVar (
                    size,
                    Mvg.IndexGeneric (Ast.same_pos_as (Mvg.Literal Mvg.Undefined) var.Mvg.Variable.name);
                  );
                Mvg.var_typ = translate_value_typ
                    (match decl.var_decl_typ with
                     | None -> None
                     | Some typ -> Some (Ast.same_pos_as typ var.name));
                Mvg.var_io = io;
              } var_data
            | None -> Mvg.VariableMap.add var {
                Mvg.var_definition = Mvg.SimpleVar (Ast.same_pos_as (Mvg.Literal Mvg.Undefined) var.Mvg.Variable.name);
                Mvg.var_typ = translate_value_typ
                    (match decl.var_decl_typ with
                     | None -> None
                     | Some typ -> Some (Ast.same_pos_as typ var.name));
                Mvg.var_io = io;
              } var_data
          end
        | Input ->
          Mvg.VariableMap.add var {
            Mvg.var_definition = Mvg.InputVar;
            Mvg.var_typ = translate_value_typ
                (match decl.var_decl_typ with
                 | None -> None
                 | Some typ -> Some (Ast.same_pos_as typ var.name));
            Mvg.var_io = Mvg.Input;
          } var_data
        | Constant -> var_data (* the variable's definition has already been inserted in [var_data] *)
      end
    ) var_decl_data var_data

(**
   Returns a map whose keys are dummy variables and whose values are the verification conditions.
*)
let get_conds
    (error_decls: Mvg.Error.t list)
    (idmap: Mvg.idmap)
    (p: Ast.program)
    (application: Ast.application option)
  : Mvg.condition_data Mvg.VariableMap.t =
  List.fold_left (fun conds source_file ->
      List.fold_left (fun conds source_file_item ->
          match Ast.unmark source_file_item with
          | Ast.Verification verif when belongs_to_app verif.Ast.verif_applications application ->
            let rule_number = Ast.verification_number verif.verif_name in
            List.fold_left (fun conds verif_cond ->
                let e = translate_expression
                    {
                      idmap;
                      lc = None;
                      int_const_values = Mvg.VariableMap.empty ;
                      table_definition = false;
                      exec_number = {
                        Mvg.rule_number = rule_number;
                        Mvg.seq_number = 0;
                        Mvg.pos = Ast.get_position verif_cond
                      }
                    }
                    (Ast.unmark verif_cond).Ast.verif_cond_expr
                in
                let errs = List.map
                    (fun err_name ->
                       try
                         Some
                           (List.find
                              (fun e -> Ast.unmark e.Mvg.Error.name = Ast.unmark err_name)
                              error_decls)
                       with
                       | Not_found -> begin
                           Cli.var_info_print
                             (Printf.sprintf "undeclared error %s %s"
                                (Ast.unmark err_name)
                                (Format_ast.format_position (Ast.get_position err_name))
                             );
                           None
                         end
                    )
                    (Ast.unmark verif_cond).Ast.verif_cond_errors
                in
                let dummy_var =
                  Mvg.Variable.new_var
                    (Ast.same_pos_as (Printf.sprintf "Verification condition %d" (Mvg.Variable.fresh_id ())) e)
                    None
                    (Ast.same_pos_as (Format_ast.format_position (Ast.get_position e)) e)
                    {
                      Mvg.rule_number = rule_number;
                      Mvg.seq_number = 0;
                      Mvg.pos = Ast.get_position verif_cond
                    }
                in
                Mvg.VariableMap.add dummy_var {
                  Mvg.cond_expr = e;
                  Mvg.cond_errors =
                    List.map
                      (fun x -> match x with | Some x -> x | None -> assert false (* should not happen *))
                      (List.filter (fun x -> match x with Some _ -> true | None -> false) errs)
                } conds
              ) conds verif.Ast.verif_conditions
          | _ -> conds
        ) conds source_file
    ) Mvg.VariableMap.empty p

(**
   Main translation function from the M AST to the M Variable Graph. This function performs 6 linear
   passes on the input code:
    {ul
      {li
        [get_constants] gets the value of all constant variables, the values of which are needed
        to compute certain loop bounds;
      }
      {li [get_variables_decl] retrieves the declarations of all other variables and errors; }
      {li
        [get_var_redefinitions] incorporates into [idmap] all definitions inside rules
        along with their execution number;
      }
      {li
        [get_var_data] is the workhorse pass that translates all the expressions corresponding to the
        definitions;
      }
      {li
        [add_dummy_definition_for_variable_declaration] adds [Undefined] definitions placeholder for
        all variables declarations;
      }
      {li
        [get_errors_conds] parses the verification conditions definitions.
      }
    }
*)
let translate (p: Ast.program) (application : string option): Mvg.program =
  let (var_decl_data, idmap, int_const_vals) = get_constants p in
  let (var_decl_data, error_decls, idmap) = get_variables_decl p var_decl_data idmap in
  let idmap = get_var_redefinitions p idmap int_const_vals application in
  let var_data = get_var_data idmap var_decl_data int_const_vals p application in
  let var_data = add_dummy_definition_for_variable_declaration var_data var_decl_data idmap in
  let conds = get_conds error_decls idmap p application in
  { Mvg.program_vars = var_data; Mvg.program_conds = conds; Mvg.program_idmap = idmap}