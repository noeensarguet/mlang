(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

let get_variable_def (files : string list) (var : Mir.variable) vdata =
  let rec read_lines acc in_channel =
    try
      let line = input_line in_channel in
      read_lines (line :: acc) in_channel
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  let var_name = Pos.unmark var.Mir.name in
  let grep_command =
    (* e.g. grep -E "^VARIABLE ?=" ir-calcul/sources2022*/*.m | cut -d: -f1 *)
    "grep -E \"^" ^ var_name ^ " ?=\" "
    ^ List.fold_left (fun file_name acc -> file_name ^ " " ^ acc) "" files
    ^ " | cut -d: -f1"
  in
  let in_channel = Unix.open_process_in grep_command in
  let rec aux l =
    match l with
    | [] when vdata.Mir.var_io = Input -> None
    | [ t ] -> Some t
    | t :: l ->
        if List.mem var_name [ "VARTMP1"; "VARTMP2" ] then
          Some "Variable def not supported for VARTMP* at the moment"
        else begin
          Format.eprintf "%s defined in %s@." var_name t;
          aux l
        end
    | _ -> assert false
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

let dbg_to_out_graph (files : string list) (prog : Mir.program)
    (dbg : Bir_interpreter.TRYGRAPH.t) : Bir_interpreter.DBGGRAPH.t =
  let module EMAP =
    Graph.Gmap.Edge
      (Bir_interpreter.TRYGRAPH)
      (struct
        include Bir_interpreter.DBGGRAPH

        let empty () = empty
      end)
  in
  let outgraph =
    let h = Hashtbl.create (Bir_interpreter.TRYGRAPH.nb_vertex dbg) in
    Bir_interpreter.TRYGRAPH.fold_edges
      (fun v1 v2 g ->
        let newv1 =
          try Hashtbl.find h v1
          with Not_found ->
            let var1, vv1 = Bir_interpreter.TRYGRAPH.V.label v1 in
            let var1 = Bir.var_to_mir var1 in
            let _, vdata1 = Mir.find_var_definition prog var1 in
            let vdef1 = get_variable_def files var1 vdata1 in
            let newvar =
              Bir_interpreter.DBGGRAPH.V.create
                (Pos.unmark var1.Mir.name, vdef1, vv1)
            in
            Hashtbl.add h v1 newvar;
            newvar
        in
        let newv2 =
          try Hashtbl.find h v2
          with Not_found ->
            let var2, vv2 = Bir_interpreter.TRYGRAPH.V.label v2 in
            let var2 = Bir.var_to_mir var2 in
            let _, vdata2 = Mir.find_var_definition prog var2 in
            let vdef2 = get_variable_def files var2 vdata2 in
            let newvar =
              Bir_interpreter.DBGGRAPH.V.create
                (Pos.unmark var2.Mir.name, vdef2, vv2)
            in
            Hashtbl.add h v2 newvar;
            newvar
        in
        Bir_interpreter.DBGGRAPH.add_edge g newv1 newv2)
      dbg Bir_interpreter.DBGGRAPH.empty
  in
  outgraph

let rec subgraph_depth n (g : Bir_interpreter.TRYGRAPH.t)
    (v : Bir_interpreter.TRYGRAPH.vertex) =
  let open Bir_interpreter in
  let module O = Graph.Oper.P (TRYGRAPH) in
  match n with
  | 0 -> TRYGRAPH.add_vertex TRYGRAPH.empty v
  | n ->
      TRYGRAPH.fold_pred
        (fun v2 sg ->
          let comp = subgraph_depth (n - 1) g v2 in
          TRYGRAPH.add_edge (O.union comp sg) v v2)
        g v TRYGRAPH.empty

let to_dot (fmt : Format.formatter) (g : Bir_interpreter.DBGGRAPH.t) : unit =
  let open Bir_interpreter in
  let module GPr = Graph.Graphviz.Dot (struct
    include DBGGRAPH

    let graph_attributes (_ : t) = []

    let default_vertex_attributes (_ : t) = []

    let var_value_to_int (vv : var_literal) =
      match vv with
      | SimpleVar l -> begin
          match l with Float f -> int_of_float f | Undefined -> 0
        end
      | TableVar (_, es) -> begin
          match es.(0) with Float f -> int_of_float f | Undefined -> 0
        end

    let vertex_name (v : vertex) =
      let vhash = DBGGRAPH.V.hash v in
      Format.asprintf "%d" vhash

    let vertex_attributes (v : vertex) =
      let var, vdef, vval = DBGGRAPH.V.label v in
      [
        `Label
          (Format.asprintf "%s = %a@,%a" var format_var_literal vval
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
let output_dot_eval_program (files : string list)
    (f : Bir_interface.bir_function) (p : Bir.program)
    (inputs : Mir.literal Bir.VariableMap.t) (code_loc_start_value : int)
    (sort : Cli.value_sort) (roundops : Cli.round_ops) : unit -> unit =
  let dbg, ctxd =
    Bir_interpreter.evaluate_program_dbg f p inputs code_loc_start_value sort
      roundops ()
  in
  let v = StrMap.find "NAPTIR" ctxd.ctxd_vars in
  let subdbg = subgraph_depth 3 dbg v in
  Format.printf "subdbg : %d vertices -- %d edges@."
    (Bir_interpreter.TRYGRAPH.nb_vertex subdbg)
    (Bir_interpreter.TRYGRAPH.nb_edges subdbg);
  let out_graph = dbg_to_out_graph files p.Bir.mir_program subdbg in
  Format.printf "out_graph : %d vertices -- %d edges@."
    (Bir_interpreter.DBGGRAPH.nb_vertex out_graph)
    (Bir_interpreter.DBGGRAPH.nb_edges out_graph);
  fun () -> to_dot Format.err_formatter out_graph
