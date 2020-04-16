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

