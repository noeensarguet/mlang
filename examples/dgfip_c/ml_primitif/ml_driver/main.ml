open Common

let read_test filename =
  let test = Read_test.read_test filename in
  let tgv = M.TGV.alloc_tgv () in
  let res_prim, ctl_prim =
    let fold_prim (res_prim, ctl_prim) s =
      match s with
      | `EntreesPrimitif pl ->
          List.iter (fun (code, montant) -> M.TGV.set tgv code montant) pl;
          res_prim, ctl_prim
      | `ResultatsPrimitif pl ->
          let res_prim =
            let fold res (code, montant) = StrMap.add code montant res in
            List.fold_left fold res_prim pl
          in
          res_prim, ctl_prim
      | `ControlesPrimitif el ->
          let ctl_prim =
            let fold err e = StrSet.add e err in
            List.fold_left fold ctl_prim el
          in
          res_prim, ctl_prim
      | _ -> res_prim, ctl_prim
    in
    List.fold_left fold_prim (StrMap.empty, StrSet.empty) test
  in
  tgv, res_prim, ctl_prim

let check_result tgv err expected_tgv expected_err =
  let result = ref true in
  StrMap.iter (fun code montant ->
      (* Consider missing variables as 0.0 and dismiss NBPT (DGFiP does the same) *)
      (* NBPT variable doesn't bear any meaning due to DGFiP test generation method *)
      match code with
      | "NBPT" -> ()
      | _ ->
          let montant' = try M.TGV.get_def tgv code 0.0 with Not_found -> 0.0 in
          (* Comparaison des valeurs au centime près, conformément à la doctrine
             DGFiP actuellement en vigueur *)
          let comp =
            let m = Float.round (montant *. 100.) in
            let m' = Float.round (montant' *. 100.) in
            abs_float (m -. m') > 0.0
          in
          if comp then
            begin
              result := false;
              Printf.eprintf "KO | %s = %f au lieu de %f\n"
               code montant' montant
            end
    ) expected_tgv;
  let missing_errs = StrSet.diff expected_err err in
  let unexpected_errs = StrSet.diff err expected_err in
  if not (StrSet.is_empty missing_errs && StrSet.is_empty unexpected_errs) then (
    result := false;
    StrSet.iter (Printf.eprintf "KO | %s attendue non recue\n") missing_errs;
    StrSet.iter (Printf.eprintf "KO | %s recu en trop\n") unexpected_errs;
  );
  !result

let var_addr () =
  let vars = lazy
    (let vf = open_in "vars.txt" in
     let rec aux vars =
       try
         let line = input_line vf in
         match String.split_on_char ' ' line with
         | addr::var::_ -> aux ((int_of_string ("0x"^addr), var)::vars)
         | _ -> assert false
       with
       | End_of_file -> vars
     in
     aux [] |> List.rev)
  in
  Lazy.force vars

let compare_dump out outexp =
  let out = open_in_bin out in
  let outexp = open_in_bin outexp in
  let read64 ic =
    let buf = Bytes.create 8 in
    really_input ic buf 0 8;
    buf
  in
  let rec read_val_diffs vars diffs =
    match vars with
    | [] -> diffs
    | (addr, var)::vars ->
      assert (pos_in out = addr);
      let res_val = read64 out in
      let expe_val = read64 outexp in
      if Bytes.equal res_val expe_val then
        read_val_diffs vars diffs
      else
        read_val_diffs vars ((var, res_val, expe_val)::diffs)
  in
  let rec read_strings ic strs =
    try
      let str = read64 ic in
      let strs = StrSet.add (Bytes.to_string str) strs in
      read_strings ic strs
    with
    | End_of_file -> strs
  in
  let diffs = read_val_diffs (var_addr ()) [] in
  let raised_disco = read_strings out StrSet.empty in
  let expected_disco = read_strings outexp StrSet.empty in
  StrSet.iter (fun ano ->
      Printf.eprintf "Raised unexpected discordance %s\n" ano
    ) (StrSet.diff raised_disco expected_disco);
  StrSet.iter (fun ano ->
      Printf.eprintf "Expected discordance %s not raised\n" ano
    ) (StrSet.diff expected_disco raised_disco);
  let undef = Int64.of_string "0xefbeaddeefbeadde" in
  let hex2floatstr bytes =
    let i64 = Bytes.get_int64_le bytes 0 in
    if Int64.equal i64 undef then "undef"
    else string_of_float(Int64.float_of_bits i64)
  in
  List.iter (fun (var, res, exp) ->
      Printf.eprintf "%s: %s found, expected %s\n"
        var (hex2floatstr res) (hex2floatstr exp)
    )
    diffs;
  flush stderr;
  close_in out;
  close_in outexp

let run_test test_file annee_exec =
  Printf.printf "Testing %s...\n%!" test_file;
  let annee_calc = M.annee_calc () in
  let tgv, res_prim, ctl_prim = read_test test_file in
  let annee_revenu = M.TGV.get_int_def tgv "ANREV" annee_calc in
  if annee_revenu <> annee_calc then (
    Printf.eprintf
      "Attention, année calculette (%d) <> année revenu (%d)\n%!"
      annee_calc
      annee_revenu
  );
  M.TGV.set_int tgv "IND_TRAIT" 4 (* = primitif *);
  M.TGV.set_int tgv "ANCSDED" annee_exec;
  M.init_errs tgv;
  let _err = M.enchainement_primitif tgv in
  M.export_errs tgv;
  let err_set =
    let add res e = StrSet.add e res in
    List.fold_left add StrSet.empty (M.get_err_list tgv)
  in
  check_result tgv err_set res_prim ctl_prim

let main () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Syntaxe :\n%s fichier_test\n" Sys.argv.(0);
    exit 31
  );
  let args = List.tl (Array.to_list Sys.argv) in
  let annee_exec, test_files =
    match args with
    | "--annee" :: ann :: files ->
        let annee =
          try int_of_string ann with
          | _ ->
              Printf.eprintf "--annee accepte un entier comme argument (%s)\n" ann;
              exit 31
        in
        annee, files
    | "--annee" :: []->
        Printf.eprintf "argument manquant pour --annee\n";
        exit 31
    | files ->
        let annee = M.annee_calc () + 1 in
        annee, files
  in
  let rec loop = function
  | [] -> true
  | test_file :: files ->
      run_test test_file annee_exec && ((* Gc.minor ();*) loop files)
  in
  match loop test_files with
  | true -> exit 0
  | false -> exit 1

let _ =
  Printexc.record_backtrace true;
  try main ()
  with e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 30

