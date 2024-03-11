(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Mir

let format_typ fmt (t : typ) =
  Format.pp_print_string fmt (match t with Real -> "real")

let format_func fmt (f : func) =
  Format.pp_print_string fmt
    (match f with
    | SumFunc -> "somme"
    | AbsFunc -> "abs"
    | MinFunc -> "min"
    | MaxFunc -> "max"
    | GtzFunc -> "positif"
    | GtezFunc -> "positif_ou_nul"
    | NullFunc -> "null"
    | ArrFunc -> "arr"
    | InfFunc -> "inf"
    | PresentFunc -> "present"
    | Multimax -> "multimax"
    | Supzero -> "supzero"
    | VerifNumber -> "numero_verif"
    | ComplNumber -> "numero_compl")

let format_literal fmt (l : literal) =
  Format.pp_print_string fmt
    (match l with Float f -> string_of_float f | Undefined -> "indéfini")

let rec format_expression fmt (e : expression) =
  match e with
  | Comparison ((op, _), (e1, _), (e2, _)) ->
      Format.fprintf fmt "(%a %a %a)" format_expression e1
        Format_mast.format_comp_op op format_expression e2
  | Binop ((op, _), (e1, _), (e2, _)) ->
      Format.fprintf fmt "(%a %a %a)" format_expression e1
        Format_mast.format_binop op format_expression e2
  | Unop (op, (e, _)) ->
      Format.fprintf fmt "%a %a" Format_mast.format_unop op format_expression e
  | Conditional ((e1, _), (e2, _), (e3, _)) ->
      Format.fprintf fmt "(si %a alors %a sinon %a)" format_expression e1
        format_expression e2 format_expression e3
  | FunctionCall (f, args) ->
      Format.fprintf fmt "%a(%a)" format_func f
        (Format_mast.pp_print_list_comma
           (Format_mast.pp_unmark format_expression))
        args
  | Literal lit -> format_literal fmt lit
  | Var var -> Format.fprintf fmt "%s" (Pos.unmark var.name)
  | LocalVar lvar -> Format.fprintf fmt "t%d" lvar.LocalVariable.id
  | LocalLet (lvar, (e1, _), (e2, _)) ->
      Format.fprintf fmt "soit t%d = (%a) dans %a" lvar.LocalVariable.id
        format_expression e1 format_expression e2
  | Index (var, i) ->
      Format.fprintf fmt "%s[%a]"
        (Pos.unmark (Pos.unmark var).name)
        format_expression (Pos.unmark i)
  | NbCategory cats ->
      Format.fprintf fmt "nb_categorie(%a)" (Mir.CatVarSet.pp ()) cats
  | Attribut (v, _, a) ->
      Format.fprintf fmt "attribut(%s, %s)" (Pos.unmark v) (Pos.unmark a)
  | Size var -> Format.fprintf fmt "taille(%s)" (Pos.unmark var.name)
  | NbAnomalies -> Format.fprintf fmt "nb_anomalies()"
  | NbDiscordances -> Format.fprintf fmt "nb_discordances()"
  | NbInformatives -> Format.fprintf fmt "nb_informatives()"
  | NbBloquantes -> Format.fprintf fmt "nb_bloquantes()"

let format_variable_def fmt (def : variable_def) =
  match def with
  | SimpleVar e -> Format.fprintf fmt "%a@\n" format_expression (Pos.unmark e)
  | InputVar -> Format.fprintf fmt "[User input]@\n"
  | TableVar (_, IndexGeneric (v, e)) ->
      Format.fprintf fmt "%s -> %a@\n" (Pos.unmark v.name) format_expression
        (Pos.unmark e)
  | TableVar (_, IndexTable defs) ->
      IndexMap.pp (Format_mast.pp_unmark format_expression) fmt defs

let format_variable_data fmt (def : variable_data) =
  Format.fprintf fmt "type %a:\n%a"
    (fun fmt () ->
      match def.var_typ with
      | None -> Format.fprintf fmt "unknown"
      | Some t -> format_typ fmt t)
    () format_variable_def def.var_definition

let format_variables fmt (p : variable_data VariableMap.t) =
  VariableMap.pp format_variable_data fmt p

let format_error fmt (e : Error.t) =
  Format.fprintf fmt "erreur %s (%s)" (Pos.unmark e.Error.name)
    (Error.err_descr_string e |> Pos.unmark)

let format_precondition fmt (precond : condition_data) =
  Format.fprintf fmt "Précondition : %a\nSinon %a%a" format_expression
    (Pos.unmark precond.cond_expr)
    format_error (fst precond.cond_error)
    (Format.pp_print_option (fun fmt (v : Variable.t) ->
         Format.fprintf fmt " (%s)" (Pos.unmark v.name)))
    (snd precond.cond_error)

let format_program_rules fmt (vars : Variable.t StrMap.t)
    (rules : rule_data RuleMap.t) =
  RuleMap.iter
    (fun _ { rule_vars; rule_number; _ } ->
      let var_defs =
        List.fold_left
          (fun var_defs instr ->
            match Pos.unmark instr with
            | Mir.Affectation (vid, def) ->
                let var = StrMap.find vid vars in
                VariableMap.add var def var_defs
            | _ -> assert false
            (* never used *))
          VariableMap.empty rule_vars
      in
      Format.fprintf fmt "Regle %d\n%a\n"
        (num_of_rule_or_verif_id (Pos.unmark rule_number))
        format_variables var_defs)
    rules

let format_variable fmt (v : Variable.t) =
  Format.fprintf fmt "%s: %s" (Pos.unmark v.name) (Pos.unmark v.descr)
