(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

include Graph.Persistent.Digraph.Abstract (struct
  type t = Com.Var.t * string option * Com.literal
end)

let pp_vertex fmt (v : vertex) =
  let var, vdef, vval = V.label v in
  Format.fprintf fmt "@[<v>%s = %a@,@,@]@[<hov>%a@]" (Pos.unmark var.name)
    Com.format_literal vval
    (Format.pp_print_option
       ~none:(fun fmt () ->
         let descr =
           match Com.Var.cat_var_loc var with
           | Some Com.CatVar.LocInput -> "input var"
           | _ ->
               "undefined at that point (probably not live in the current \
                domain)"
         in
         Format.fprintf fmt "%s" descr)
       (fun fmt s ->
         let units = String.split_on_char ' ' s in
         Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun fmt s -> Format.fprintf fmt "%s" s)
           fmt units))
    vdef

type ctx_dbg = vertex StrMap.t
(* StrMap because we're only keeping track of the last vertex seen with a given name for now *)

let empty_ctxd = StrMap.empty
