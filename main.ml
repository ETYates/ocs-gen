
let run request =
  let info = String.split_on_char ' ' request in
  match info with
  | [lemma; parse] -> 
    (match (String.length parse) with
    | 3 -> 
      let (tense, person, number) = makeparse parse in
      let features = Morph.VerbTag {person; number; tense} in
      Conj.generate lemma features 
    | _ -> failwith "Ошибка!")
  | _ -> failwith "Ошибка!"
and
  makeparse parse =
  let tense =
  match parse.[0] with
  | 'p' -> Morph.Present
  | 'd' -> Morph.Imperative
  | 'i' -> Morph.Imperfect
  | 'a' -> Morph.Aorist
  in
  let person =
  match parse.[1] with
  | '1' -> Morph.First
  | '2' -> Morph.First
  | '3' -> Morph.First
  in
  let number = 
  match parse.[2] with
  | 's' -> Morph.Singular
  | 'd' -> Morph.Dual
  | 'p' -> Morph.Plural
  in
  (tense, person, number)
 
let _ = let instr = read_line in
  run instr
