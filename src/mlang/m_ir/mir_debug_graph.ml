(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

let rec subgraph_depth n (g : Mir_interpreter.DBGGRAPH.t)
    (v : Mir_interpreter.DBGGRAPH.vertex) =
  let open Mir_interpreter in
  let module O = Graph.Oper.P (DBGGRAPH) in
  match n with
  | 0 -> DBGGRAPH.add_vertex DBGGRAPH.empty v
  | n ->
      DBGGRAPH.fold_succ
        (fun v2 sg ->
          let comp = subgraph_depth (n - 1) g v2 in
          DBGGRAPH.add_edge (O.union comp sg) v v2)
        g v
        (DBGGRAPH.add_vertex DBGGRAPH.empty v)

let to_dot (fmt : Format.formatter) (g : Mir_interpreter.DBGGRAPH.t) : unit =
  let open Mir_interpreter in
  let module GPr = Graph.Graphviz.Dot (struct
    include DBGGRAPH

    let graph_attributes (_ : t) = []

    let default_vertex_attributes (_ : t) = [ `Style `Filled ]

    let vertex_name (v : vertex) =
      let vhash = DBGGRAPH.V.hash v in
      Format.asprintf "%d" vhash

    let vertex_attributes (v : vertex) =
      [
        `Label (Format.asprintf "%a" Mir_interpreter.DBGGRAPH.pp_vertex v);
        `Shape `Box;
        `Style `Filled;
        (let var, _, _ = DBGGRAPH.V.label v in
         `Fillcolor
           (match Com.Var.cat_var_loc var with
           | Some Com.CatVar.LocInput -> 0xadd8e6
           | _ -> if Com.Var.is_given_back var then 0xffa500 else 0xffffff));
      ]

    let get_subgraph (_ : vertex) = None

    let default_edge_attributes (_ : t) = []

    let edge_attributes (_ : edge) = []
  end) in
  GPr.fprint_graph fmt g

let output_dot_eval_program (p : Mir.program)
    (inputs : Com.literal Com.Var.Map.t) (sort : Cli.value_sort)
    (roundops : Cli.round_ops) (file : string) : unit -> unit =
  let dbg, ctxd =
    Mir_interpreter.evaluate_program_dbg p inputs sort roundops ()
  in
  (* Mir_interpreter.DBGGRAPH.iter_vertex
     (fun v ->
       let var, vdef, _vval = Mir_interpreter.DBGGRAPH.V.label v in
       match vdef with
       | None when Com.Var.cat_var_loc var = Some Com.CatVar.LocInput ->
           Format.eprintf "weird stuff on %s@." (Pos.unmark var.name)
       | _ -> ())
     dbg; *)
  let v_annee = StrMap.find "V_ANCSDED" ctxd in
  let _, _, annee = Mir_interpreter.DBGGRAPH.V.label v_annee in
  let annee = match annee with Float f -> int_of_float f | Undefined -> 0 in
  let v = StrMap.find (if annee = 2051 then "VARC" else "NAPTIR") ctxd in
  let subdbg = if annee = 2051 then dbg else subgraph_depth 3 dbg v in
  Format.printf "subdbg : %d vertices -- %d edges@."
    (Mir_interpreter.DBGGRAPH.nb_vertex subdbg)
    (Mir_interpreter.DBGGRAPH.nb_edges subdbg);
  (* let out_graph =
       dbg_to_out_graph files (if annee = 2051 then dbg else subdbg)
     in
     Format.printf "out_graph : %d vertices -- %d edges@."
       (Mir_interpreter.DBGGRAPH.nb_vertex out_graph)
       (Mir_interpreter.DBGGRAPH.nb_edges out_graph); *)
  fun () ->
    let oc = open_out file in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@." to_dot subdbg;
    close_out oc
