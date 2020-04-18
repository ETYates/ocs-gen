let testfile = ref ""

let run request =
  let info = String.split_on_char ' ' request in
  match info with
  | [lemma; parse] ->
    (match (String.length parse) with
    | 3 ->
      let (tense, person, number) = Parse.makeparse parse in
      let features = Morph.VerbTag {person; number; tense} in
      Conj.generate (Phon.process lemma) features
    | _ -> failwith "Ошибка!")
  | _ -> failwith "Ошибка!" 


let _ =
  let usage_msg =
    Printf.sprintf "Usage: %s <-t [Test File]>" Sys.argv.(0) in
  let speclist = [
    ("-t", Arg.Set_string testfile,
      "Set the testing flag");
  ] in

  Arg.parse speclist (fun _ -> ()) usage_msg;

  match !testfile = "" with
  | true -> Printf.printf "\n\tOCS-GEN v. dev 0.1. Yates and Palleiko\n\n";
    let instr = ref (read_line ()) in
    while (String.compare !instr "q") != 0
      do
        if (String.compare !instr "") != 0 then
          Printf.printf "[%s] -> %s\n" !instr (run !instr);
          instr := read_line ()
      done
  | false ->
    Test.doTests !testfile
