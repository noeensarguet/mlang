(*This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>. *)

module G = Dbggraph_types

let rec subgraph_depth n (g : G.t) (v : G.vertex) =
  let module O = Graph.Oper.P (G) in
  match n with
  | 0 -> G.add_vertex G.empty v
  | n ->
      G.fold_succ
        (fun v2 sg ->
          let comp = subgraph_depth (n - 1) g v2 in
          G.add_edge (O.union comp sg) v v2)
        g v (G.add_vertex G.empty v)

let to_dot (fmt : Format.formatter) (g : G.t) : unit =
  let module GPr = Graph.Graphviz.Dot (struct
    include G

    let graph_attributes (_ : t) = []

    let default_vertex_attributes (_ : t) = [ `Style `Filled ]

    let vertex_name (v : vertex) =
      let vhash = G.V.hash v in
      Format.asprintf "%d" vhash

    let vertex_attributes (v : vertex) =
      [
        `Label (Format.asprintf "%a" G.pp_vertex v);
        `Shape `Box;
        `Style `Filled;
        (let (var, _), _, _ = G.V.label v in
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

let output_dot_eval_program (dbg : G.t) (ctxd : G.ctx_dbg) (file : string) :
    unit -> unit =
  (* G.iter_vertex
     (fun v ->
       let var, vdef, _vval = G.V.label v in
       match vdef with
       | None when Com.Var.cat_var_loc var = Some Com.CatVar.LocInput ->
           Cli.warning_print "weird stuff on %s@." (Pos.unmark var.name)
       | _ -> ())
     dbg; *)
  let v_annee = StrMap.find "V_ANCSDED" ctxd in
  let _, _, annee = G.V.label v_annee in
  let annee = match annee with Float f -> int_of_float f | Undefined -> 0 in
  let v = StrMap.find (if annee = 2051 then "VARC" else "TXMARJ") ctxd in
  let subdbg = if annee = 2051 then dbg else subgraph_depth 3 dbg v in
  Format.printf "subdbg : %d vertices -- %d edges@." (G.nb_vertex subdbg)
    (G.nb_edges subdbg);
  fun () ->
    let oc = open_out file in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@." to_dot subdbg;
    close_out oc
