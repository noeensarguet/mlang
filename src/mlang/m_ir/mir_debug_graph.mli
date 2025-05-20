(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

(* val get_variable_def : string list -> Com.Var.t -> string option *)

val output_dot_eval_program :
  Mir_interpreter.DBGGRAPH.t ->
  Mir_interpreter.ctx_dbg ->
  string ->
  unit ->
  unit
