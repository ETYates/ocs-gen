let stemverb lemma =
  match lemma with
  | "i"::"t"::stem      -> String.concat "" (List.rev stem)
  | "ъ"::"t"::"ǫ"::stem -> String.concat "" (List.rev stem)
  | _ -> failwith "Oopsie in stemverb"

let fetchVerb psn num tns =
  match (tns, psn, num) with
  | (Morph.Present,    Morph.First,  Morph.Singular) -> "0-ǫ"
  | (Morph.Present,    Morph.Second, Morph.Singular) -> "e/i-ši"
  | (Morph.Present,    Morph.Third,  Morph.Singular) -> "e/i-tъ"
  | (Morph.Present,    Morph.First,  Morph.Dual)     -> "e/i-vě"
  | (Morph.Present,    Morph.Second, Morph.Dual)     -> "e/i-ta"
  | (Morph.Present,    Morph.Third,  Morph.Dual)     -> "e/i-te"
  | (Morph.Present,    Morph.First,  Morph.Plural)   -> "e/i-mъ"
  | (Morph.Present,    Morph.Second, Morph.Plural)   -> "e/i-te"
  | (Morph.Present,    Morph.Third,  Morph.Plural)   -> "ǫ/ę-tъ"
  | (Morph.Imperative, Morph.Second, Morph.Singular) -> "ě/i-0"
  | (Morph.Imperative, Morph.Third,  Morph.Singular) -> "ě/i-0"
  | (Morph.Imperative, Morph.First,  Morph.Dual)     -> "ě/i-vě"
  | (Morph.Imperative, Morph.Second, Morph.Dual)     -> "ě/i-ta"
  | (Morph.Imperative, Morph.First,  Morph.Plural)   -> "ě/i-mě"
  | (Morph.Imperative, Morph.Second, Morph.Plural)   -> "ě/i-te"
  | (Morph.Imperfect,  Morph.First,  Morph.Singular) -> "ěax-ъ"
  | (Morph.Imperfect,  Morph.Second, Morph.Singular) -> "ěaš-e"
  | (Morph.Imperfect,  Morph.Third,  Morph.Singular) -> "ěaš-e"
  | (Morph.Imperfect,  Morph.First,  Morph.Dual)     -> "ěax-ově"
  | (Morph.Imperfect,  Morph.Second, Morph.Dual)     -> "ěaš-eta"
  | (Morph.Imperfect,  Morph.Third,  Morph.Dual)     -> "ěaš-ete"
  | (Morph.Imperfect,  Morph.First,  Morph.Plural)   -> "ěax-omъ"
  | (Morph.Imperfect,  Morph.Second, Morph.Plural)   -> "ěaš-ete"
  | (Morph.Imperfect,  Morph.Third,  Morph.Plural)   -> "ěax-ǫ"
  | (Morph.Aorist,     Morph.First,  Morph.Singular) -> "(o)-xъ"
  | (Morph.Aorist,     Morph.Second, Morph.Singular) -> "(e)-0"
  | (Morph.Aorist,     Morph.Third,  Morph.Singular) -> "(e)-0"
  | (Morph.Aorist,     Morph.First,  Morph.Dual)     -> "(o)-xově"
  | (Morph.Aorist,     Morph.Second, Morph.Dual)     -> "(o)-sta"
  | (Morph.Aorist,     Morph.Third,  Morph.Dual)     -> "(o)-ste"
  | (Morph.Aorist,     Morph.First,  Morph.Plural)   -> "(o)-xomъ"
  | (Morph.Aorist,     Morph.Second, Morph.Plural)   -> "(o)-ste"
  | (Morph.Aorist,     Morph.Third,  Morph.Plural)   -> "(o)-šę"
  | _ -> "Обишка: нет имменых."

let generate lemma (parse : Morph.parse) =
  match parse with
  | Morph.VerbTag {person; number; tense} ->
    let stemstr = stemverb lemma in
    let stem = Morph.classify stemstr in
    let ending = fetchVerb person number tense in
    Sandhi.sandhi stem ending
  | _ -> "Ошибка! Нет имменых."
