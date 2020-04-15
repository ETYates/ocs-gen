
let generate lemma (parse : Morph.parse) =
  match parse with
  | Morph.VerbTag {person; number; tense} -> 
    let stemstr = stemverb lemma in
    let stem = Morph.classify stemstr in
    let ending = fetchVerb person number tense in
    Sandhi.sandhi stem ending 
  | _ -> "Ошибка! Нет имменых."
and 
  stemverb lemma =
  match lemma with
  | stem::"t"::"i"      -> stem
  | stem::"ǫ"::"t"::"ъ" -> stem
and
  fetchVerb psn num tns =
  match (tns, psn, num) with
  | (Present, First, Singular) -> "0-ǫ"
  | (Present, Second, Singular) -> "e/i-ši"
  | (Present, Third, Singular) -> "e/i-tъ"
  | (Present, First, Dual) -> "e/i-vě"
  | (Present, Second, Dual) -> "e/i-ta"
  | (Present, Third, Dual) -> "e/i-te"
  | (Present, First, Plural) -> "e/i-mъ"
  | (Present, Second, Plural) -> "e/i-te"
  | (Present, Third, Plural) -> "ǫ/ę-tъ"
  | (Imperative, Second, Singular) -> "ě/i-0"
  | (Imperative, Third, Singular) -> "ě/i-0"
  | (Imperative, First, Dual) -> "ě/i-vě"
  | (Imperative, Second, Dual) -> "ě/i-ta"
  | (Imperative, First, Plural) -> "ě/i-mě"
  | (Imperative, Second, Plural) -> "ě/i-te"
  | (Imperfect, First, Singular) -> "ěax-ъ"
  | (Imperfect, Second, Singular) -> "ěaš-e"
  | (Imperfect, Third, Singular) -> "ěaš-e"
  | (Imperfect, First, Dual) -> "ěax-ově"
  | (Imperfect, Second, Dual) -> "ěaš-eta"
  | (Imperfect, Third, Dual) -> "ěaš-ete"
  | (Imperfect, First, Plural) -> "ěax-omъ"
  | (Imperfect, Second, Plural) -> "ěaš-ete"
  | (Imperfect, Third, Plural) -> "ěax-ǫ"
  | (Aorist, First, Singular) -> "(o)-xъ"
  | (Aorist, Second, Singular) -> "(e)-0"
  | (Aorist, Third, Singular) -> "(e)-0"
  | (Aorist, First, Dual) -> "(o)-xově"
  | (Aorist, Second, Dual) -> "(o)-sta"
  | (Aorist, Third, Dual) -> "(o)-ste"
  | (Aorist, Thirst, Plural) -> "(o)-xomъ"
  | (Aorist, Second, Plural) -> "(o)-ste"
  | (Aorist, Third, Plural) -> "(o)-šę"
  | _ -> "Обишка: нет имменых."
