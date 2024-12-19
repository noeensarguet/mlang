(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

(* module TRYGRAPH : Graph.Sig.P with type V.t = Bir.variable *
   Bir_interpreter.var_literal *)

val get_variable_def : string list -> Com.Var.t -> string option

(* TODO add to a formatter rather than stdout *)
val output_dot_eval_program :
  string list ->
  Mir.program ->
  Com.literal Com.Var.Map.t ->
  Cli.value_sort ->
  Cli.round_ops ->
  unit ->
  unit
