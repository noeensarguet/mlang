(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

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

open Mvg

let all_zero_input (p: program) (typing_info: Typechecker.typ_info): literal VariableMap.t =
  VariableMap.mapi
    (fun var _ ->
       let typ, _ = VariableMap.find var typing_info.Typechecker.typ_info_var in
       match typ with
       | Boolean -> Bool false
       | Integer -> Int 0
       | Real -> Float 0.
    )
    (VariableMap.filter (fun var def -> def.var_io = Input) p)

let print_output (results: Interpreter.var_literal VariableMap.t) : unit =
  VariableMap.iter (fun var value ->
      Cli.result_print
        (match value with
         | Interpreter.SimpleVar value ->
           Printf.sprintf "%s (%s): %s"
             (Ast.unmark var.Variable.name)
             (Ast.unmark var.Variable.descr)
             (Format_mvg.format_literal value)
         | Interpreter.TableVar (_, values) ->
           Printf.sprintf "%s (%s):\n%s"
             (Ast.unmark var.Variable.name)
             (Ast.unmark var.Variable.descr)
             (String.concat "\n"
                (List.mapi
                   (fun idx value ->
                      Printf.sprintf "[%d]: %s"
                        idx
                        (Format_mvg.format_literal value)
                   ) (Array.to_list values)))
        )
    ) results
