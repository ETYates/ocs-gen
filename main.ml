let makeparse parse =
  let tense =
    match parse.[0] with
    | 'p' -> Morph.Present
    | 'd' -> Morph.Imperative
    | 'i' -> Morph.Imperfect
    | 'a' -> Morph.Aorist
    | _ -> failwith "Bad input" in
  let person =
    match parse.[1] with
    | '1' -> Morph.First
    | '2' -> Morph.Second
    | '3' -> Morph.Third
    | _ -> failwith "Bad input" in
  let number =
    match parse.[2] with
    | 's' -> Morph.Singular
    | 'd' -> Morph.Dual
    | 'p' -> Morph.Plural
    | _ -> failwith "Bad input" in
  (tense, person, number)

let run request =
  let info = String.split_on_char ' ' request in
  match info with
  | [lemma; parse] ->
    (match (String.length parse) with
    | 3 ->
      let (tense, person, number) = makeparse parse in
      let features = Morph.VerbTag {person; number; tense} in
      Conj.generate (Phon.process lemma) features
    | _ -> failwith "Ошибка!")
  | _ -> failwith "Ошибка!"

let _ =
  let instr = ref (read_line ()) in
  while (String.compare !instr "q") != 0
    do
      if (String.compare !instr "") != 0 then
        Printf.printf "[%s] => %s\n" !instr (run !instr);
        instr := read_line ()
    done

