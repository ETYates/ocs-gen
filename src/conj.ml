let verbTbl = Hashtbl.create 100

let stemverb lemma =
  match lemma with
  | "i"::"t"::stem      -> String.concat "" (List.rev stem)
  | _ -> let lem = String.concat "" (List.rev lemma) in
         let stemlen = ((String.length lem) - 5) in
         let st = String.sub lem 0 stemlen in
         st

let fetchVerb psn num tns =
  match Hashtbl.find_opt verbTbl (tns, psn, num) with
  | Some s -> s
  | None -> "Обишка: нет имменых."

let generate lemma (parse : Morph.parse) =
  match parse with
  | Morph.VerbTag {person; number; tense} ->
    let stemstr = stemverb lemma in
    let stem = Morph.classify stemstr in
    let ending = fetchVerb person number tense in
    Sandhi.sandhi stem ending
  | _ -> "Ошибка! Нет имменых."

let _ =
  let initVerbTbl =
    Hashtbl.add verbTbl (Morph.Present,    Morph.First,  Morph.Singular) "0-ǫ";
    Hashtbl.add verbTbl (Morph.Present,    Morph.Second, Morph.Singular) "e/i-ši";
    Hashtbl.add verbTbl (Morph.Present,    Morph.Third,  Morph.Singular) "e/i-tъ";
    Hashtbl.add verbTbl (Morph.Present,    Morph.First,  Morph.Dual)     "e/i-vě";
    Hashtbl.add verbTbl (Morph.Present,    Morph.Second, Morph.Dual)     "e/i-ta";
    Hashtbl.add verbTbl (Morph.Present,    Morph.Third,  Morph.Dual)     "e/i-te";
    Hashtbl.add verbTbl (Morph.Present,    Morph.First,  Morph.Plural)   "e/i-mъ";
    Hashtbl.add verbTbl (Morph.Present,    Morph.Second, Morph.Plural)   "e/i-te";
    Hashtbl.add verbTbl (Morph.Present,    Morph.Third,  Morph.Plural)   "ǫ/ę-tъ";
    Hashtbl.add verbTbl (Morph.Imperative, Morph.Second, Morph.Singular) "ě/i-0";
    Hashtbl.add verbTbl (Morph.Imperative, Morph.Third,  Morph.Singular) "ě/i-0";
    Hashtbl.add verbTbl (Morph.Imperative, Morph.First,  Morph.Dual)     "ě/i-vě";
    Hashtbl.add verbTbl (Morph.Imperative, Morph.Second, Morph.Dual)     "ě/i-ta";
    Hashtbl.add verbTbl (Morph.Imperative, Morph.First,  Morph.Plural)   "ě/i-mě";
    Hashtbl.add verbTbl (Morph.Imperative, Morph.Second, Morph.Plural)   "ě/i-te";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.First,  Morph.Singular) "ěax-ъ";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.Second, Morph.Singular) "ěaš-e";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.Third,  Morph.Singular) "ěaš-e";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.First,  Morph.Dual)     "ěax-ově";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.Second, Morph.Dual)     "ěaš-eta";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.Third,  Morph.Dual)     "ěaš-ete";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.First,  Morph.Plural)   "ěax-omъ";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.Second, Morph.Plural)   "ěaš-ete";
    Hashtbl.add verbTbl (Morph.Imperfect,  Morph.Third,  Morph.Plural)   "ěax-ǫ";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.First,  Morph.Singular) "(o)-xъ";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.Second, Morph.Singular) "(e)-0";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.Third,  Morph.Singular) "(e)-0";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.First,  Morph.Dual)     "(o)-xově";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.Second, Morph.Dual)     "(o)-sta";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.Third,  Morph.Dual)     "(o)-ste";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.First,  Morph.Plural)   "(o)-xomъ";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.Second, Morph.Plural)   "(o)-ste";
    Hashtbl.add verbTbl (Morph.Aorist,     Morph.Third,  Morph.Plural)   "(o)-šę" in
  initVerbTbl
