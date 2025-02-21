(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

let get_variable_def (files : string list) (var : Com.Var.t) =
  let rec read_lines acc in_channel =
    try
      let line = input_line in_channel in
      read_lines (line :: acc) in_channel
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  let var_name = Pos.unmark var.name in
  let grep_command =
    (* e.g. grep -E "^VARIABLE ?=" ir-calcul/sources2022*/*.m | cut -d: -f1 *)
    "grep -E \"^" ^ var_name ^ " ?=\" "
    ^ List.fold_left (fun file_name acc -> file_name ^ " " ^ acc) "" files
    ^ " | cut -d: -f1"
  in
  let in_channel = Unix.open_process_in grep_command in
  let rec aux l =
    try
      match (l, Com.Var.cat var) with
      | [], Input _ -> None
      | [ t ], Computed _ -> Some t
      | t :: l, Computed _ ->
          if List.mem var_name [ "VARTMP1"; "VARTMP2" ] then
            Some "Variable def not supported for VARTMP* at the moment"
          else begin
            Format.eprintf "%s defined in %s@." var_name t;
            aux l
          end
      | _ -> assert false
    with _ -> None
  in
  let file_o = aux (read_lines [] in_channel) in
  Option.bind file_o (fun file ->
      if List.mem var_name [ "VARTMP1"; "VARTMP2" ] then
        Some "Variable def not supported for VARTMP* at the moment"
      else
        let awk_command = "awk \"/^" ^ var_name ^ " ?=/,/;/\" " ^ file in
        (* awk "/^VARIABLE ?=/,/;/" FILE *)
        let in_channel = Unix.open_process_in awk_command in
        Some
          (let def =
             List.fold_left
               (fun acc s -> acc ^ "\n" ^ String.trim s)
               "" (read_lines [] in_channel)
           in
           if String.length def > 16000 then String.sub def 0 16000 ^ "..."
           else def))

let dbg_to_out_graph (files : string list) (dbg : Mir_interpreter.TRYGRAPH.t) :
    Mir_interpreter.DBGGRAPH.t =
  let h = Hashtbl.create (Mir_interpreter.TRYGRAPH.nb_vertex dbg) in
  let outgraph =
    Mir_interpreter.TRYGRAPH.fold_vertex
      (fun v g ->
        let var, vv = Mir_interpreter.TRYGRAPH.V.label v in
        let vdef = get_variable_def files var in
        let newvar =
          Mir_interpreter.DBGGRAPH.V.create (Pos.unmark var.name, vdef, vv)
        in
        Hashtbl.add h v newvar;
        Mir_interpreter.DBGGRAPH.add_vertex g newvar)
      dbg Mir_interpreter.DBGGRAPH.empty
    |> Mir_interpreter.TRYGRAPH.fold_edges
         (fun v1 v2 g ->
           let newv1 = try Hashtbl.find h v1 with Not_found -> assert false in
           let newv2 = try Hashtbl.find h v2 with Not_found -> assert false in
           Mir_interpreter.DBGGRAPH.add_edge g newv1 newv2)
         dbg
  in
  outgraph

let rec subgraph_depth n (g : Mir_interpreter.TRYGRAPH.t)
    (v : Mir_interpreter.TRYGRAPH.vertex) =
  let open Mir_interpreter in
  let module O = Graph.Oper.P (TRYGRAPH) in
  match n with
  | 0 -> TRYGRAPH.add_vertex TRYGRAPH.empty v
  | n ->
      TRYGRAPH.fold_pred
        (fun v2 sg ->
          let comp = subgraph_depth (n - 1) g v2 in
          TRYGRAPH.add_edge (O.union comp sg) v v2)
        g v
        (TRYGRAPH.add_vertex TRYGRAPH.empty v)

let to_dot (fmt : Format.formatter) (g : Mir_interpreter.DBGGRAPH.t) : unit =
  let open Mir_interpreter in
  let module GPr = Graph.Graphviz.Dot (struct
    include DBGGRAPH

    let graph_attributes (_ : t) = []

    let default_vertex_attributes (_ : t) = []

    let var_value_to_int (vv : Com.literal) =
      match vv with Float f -> int_of_float f | Undefined -> 0

    let vertex_name (v : vertex) =
      let vhash = DBGGRAPH.V.hash v in
      Format.asprintf "%d" vhash

    let vertex_attributes (v : vertex) =
      let var, vdef, vval = DBGGRAPH.V.label v in
      [
        `Label
          (Format.asprintf "%s = %a@,%a" var Com.format_literal vval
             (Format.pp_print_option
                ~none:(fun fmt () -> Format.fprintf fmt "input var")
                (fun fmt s -> Format.fprintf fmt "%s" s))
             vdef);
        `Shape `Box;
      ]

    let get_subgraph (_ : vertex) = None

    let default_edge_attributes (_ : t) = []

    let edge_attributes (_ : edge) = []
  end) in
  GPr.fprint_graph fmt g

(* TODO add to a formatter rather than stdout *)
let output_dot_eval_program (files : string list) (p : Mir.program)
    (inputs : Com.literal Com.Var.Map.t) (sort : Cli.value_sort)
    (roundops : Cli.round_ops) : unit -> unit =
  let dbg, ctxd =
    Mir_interpreter.evaluate_program_dbg p inputs sort roundops ()
  in
  let v = StrMap.find "NAPTIR" ctxd.ctxd_tgv in
  let subdbg = subgraph_depth 3 dbg v in
  Format.printf "subdbg : %d vertices -- %d edges@."
    (Mir_interpreter.TRYGRAPH.nb_vertex subdbg)
    (Mir_interpreter.TRYGRAPH.nb_edges subdbg);
  let out_graph = dbg_to_out_graph files subdbg in
  Format.printf "out_graph : %d vertices -- %d edges@."
    (Mir_interpreter.DBGGRAPH.nb_vertex out_graph)
    (Mir_interpreter.DBGGRAPH.nb_edges out_graph);
  fun () -> to_dot Format.err_formatter out_graph
