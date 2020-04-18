let classificationTbl = Hashtbl.create 60

type conjugation =
  | Consonantal
  | Vocalic

type declension =
  | Twofold
  | Pronominal
  | Compound

type klass =
  | I_verb of string
  | E_verb of string
  | Sha_verb of string
  | Ja_verb of string
  | Ova_verb of string
  | Ca_verb of string
  | No_verb of string
  | C_verb of string
  | Aj_verb of string
  | Ej_verb of string
  | J_verb of string

type stem =
  | Verb of klass
  | Nominal of declension

type case =
  | Nominative
  | Genitive
  | Dative
  | Accusative
  | Instrumental
  | Prepositional

type person =
  | First
  | Second
  | Third

type tense =
  | Present
  | Past
  | Imperative
  | Imperfect
  | Aorist

type number =
  | Singular
  | Dual
  | Plural

type gender =
  | Masculine
  | Feminine
  | Neuter

type parse =
  | VerbTag of { person : person
               ; number : number
               ; tense : tense
               }
  | NounTag of { case : case
               ; number : number
               ; gender : gender
               }

let klassToString k =
  match k with
  | I_verb s   -> s
  | E_verb s   -> s
  | Sha_verb s -> s
  | Ja_verb s  -> s
  | Ova_verb s -> s
  | Ca_verb s  -> s
  | No_verb s  -> s
  | C_verb s   -> s
  | Aj_verb s  -> s
  | Ej_verb s  -> s
  | J_verb s   -> s

let classify stem =
  match List.rev (Phon.explode stem) with
  | 'i'::_                         -> I_verb stem
  (*| 'ě'::_                       -> E_verb stem*)
  | '\x9b'::'\xc4'::_              -> E_verb stem
  (*| 'a'::'č'::_                  -> Sha_verb stem*)
  | 'a'::'\x8d'::'\xc4'::_         -> Sha_verb stem
  (*| 'a'::'ž'::_                  -> Sha_verb stem*)
  | 'a'::'\xbe'::'\xc5'::_         -> Sha_verb stem

  (*| 'a'::'š'::_                  -> Sha_verb stem*)
  | 'a'::'\xa1'::'\xc5'::_         -> Sha_verb stem

  (*| 'a'::'t'::'š'::_             -> Sha_verb stem*)
  | 'a'::'t'::'\xa1'::'\xc5'::_    -> Sha_verb stem

  (*| 'a'::'d'::'ž'::_ؤ            -> Sha_verb stem*)
  | 'a'::'d'::'\xbe'::'\xc5'::_    -> Sha_verb stem
  | 'a'::'j'::_                    -> Ja_verb stem
  | 'a'::'v'::_                    -> Ova_verb stem
  (*| 'ǫ'::'n'::_                  -> No_verb stem *)
  | '\xab'::'\xc7'::'n'::_         -> No_verb stem
  | 'a'::'k'::_                    -> Ca_verb stem
  | 'a'::'t'::_                    -> Ca_verb stem
  | 'a'::'z'::_                    -> Ca_verb stem
  | 'a'::'l'::_                    -> Ca_verb stem
  (*| 'a'::'ẑ'::_                  -> Ca_verb stem *)
  | 'a'::'\x91'::'\xba'::'\xe1'::_ -> Ca_verb stem
  | 'a'::'m'::_                    -> Ca_verb stem
  | 'a'::'x'::_                    -> Ca_verb stem
  | 'a'::'d'::_                    -> Ca_verb stem
  | 'a'::'b'::_                    -> Ca_verb stem
  | 'a'::'p'::_                    -> Ca_verb stem
  | 'a'::'c'::_                    -> Ca_verb stem
  | 'a'::'g'::_                    -> Ca_verb stem
  | 'a'::'s'::_                    -> Ca_verb stem
  | 'a'::'n'::_                    -> Ca_verb stem
  | 'j'::'a'::_                    -> Aj_verb stem
  (*| 'j'::'ě'::_                  -> Ej_verb stem *)
  | 'j'::'\x9b'::'\xc4'::_         -> Ej_verb stem
  | 'j'::_                         -> J_verb stem
  | 'r'::_                         -> C_verb stem
  | 't'::_                         -> C_verb stem
  | 'p'::_                         -> C_verb stem
  (*| 'š'::_                       -> C_verb stem*)
  | '\xa1'::'\xc5'::_              -> C_verb stem
  (*| 'ž'::_                       -> C_verb stem*)
  | '\xbe'::'\xc5'::_              -> Sha_verb stem
  | 's'::_                         -> C_verb stem
  | 'd'::_                         -> C_verb stem
  | 'f'::_                         -> C_verb stem
  | 'g'::_                         -> C_verb stem
  | 'h'::_                         -> C_verb stem
  | 'k'::_                         -> C_verb stem
  | 'l'::_                         -> C_verb stem
  (*| 'č'::_                       -> C_verb stem*)
  | '\x8d'::'\xc4'::_              -> C_verb stem
  | 'z'::_                         -> C_verb stem
  | 'x'::_                         -> C_verb stem
  | 'c'::_                         -> C_verb stem
  | 'v'::_                         -> C_verb stem
  | 'b'::_                         -> C_verb stem
  | 'n'::_                         -> C_verb stem
  | 'm'::_                         -> C_verb stem
  | _ -> failwith ("Invalid char string: " ^ stem)


let classifyTbl stem =
  let slice l idx =
    let rec loop acc i = function
      | hd :: tl ->
        if i = 0 then
          acc
        else
          loop (hd :: acc) (i - 1) tl
      | [] -> List.rev acc in
    loop [] idx l in

  let retClassification = function
    | `I_verb -> I_verb stem
    | `E_verb -> E_verb stem
    | `Sha_verb -> Sha_verb stem
    | `Ja_verb -> Ja_verb stem
    | `Ova_verb -> Ova_verb stem
    | `Ca_verb -> Ca_verb stem
    | `No_verb -> No_verb stem
    | `C_verb -> C_verb stem
    | `Aj_verb -> Aj_verb stem
    | `Ej_verb -> Ej_verb stem
    | `J_verb -> J_verb stem in

  Printf.printf "Stem = %s\n" stem;
  let _ = Str.regexp ".*?\\(\\([b-dgj-npstvxz]\\|č\\|žd?\\|ẑ\\)a\\)\\|\\(š\\(t?a\\)?\\)\\|\\(nǫ\\)\\|\\([b-df-lnprstvxz]\\)\\|\\(\\(a\\|ě\\)?j\\)$" in
  let r = Str.regexp "\\(\\([b-dgj-npstvxz]\\|č\\|žd?\\|ẑ\\)?a\\)\\|\\(š\\(t?a\\)?\\)\\|\\(nǫ\\)\\|\\([b-df-lnprstvxz]\\|ě\\|č\\)\\|\\(ž\\(d?a\\)?\\)\\|\\([aě]?j\\)$" in

  let idx = Str.search_backward r stem (String.length stem) in
  Printf.printf "Matches %s " stem;
  let ending = Str.string_after stem idx in
  Printf.printf "%s\n" ending;
  (*Printf.printf "matched: %s\n" s; *)

  let s = List.rev (Phon.explode stem) in
  let last4 = slice s 4 in
  let rec lookup ending =
    match ending with
    | _ :: tl ->
      begin
        match Hashtbl.find_opt classificationTbl ending with
        | Some t -> retClassification t
        | None -> lookup tl
      end
    | [] -> failwith ("Invalid char string: " ^ stem) in

  lookup last4


let _ =
  (* Single chars *)
  Hashtbl.add classificationTbl [                        'b';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'c';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'd';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'f';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'g';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'h';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'i';   ] `I_verb;
  Hashtbl.add classificationTbl [                        'k';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'l';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'n';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'p';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'r';   ] `C_verb;
  Hashtbl.add classificationTbl [                        's';   ] `C_verb;
  Hashtbl.add classificationTbl [                        't';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'v';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'x';   ] `C_verb;
  Hashtbl.add classificationTbl [                        'z';   ] `C_verb;
  Hashtbl.add classificationTbl [                '\xc4'; '\x9b';] `E_verb;

  Hashtbl.add classificationTbl [                '\xc4'; '\x8d';] `C_verb;
  Hashtbl.add classificationTbl [                '\xc5'; '\xbe';] `Sha_verb;

  (* š *)
  Hashtbl.add classificationTbl [                '\xc5'; '\xa1';] `C_verb;
  Hashtbl.add classificationTbl [        '\xc5'; '\xa1'; 'a';   ] `Sha_verb;
  Hashtbl.add classificationTbl ['\xc5'; '\xa1'; 't';    'a';   ] `Sha_verb;

  (* 'a' endings *)
  Hashtbl.add classificationTbl [                'b';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'c';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'd';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'g';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'j';    'a';   ] `Ja_verb;
  Hashtbl.add classificationTbl [                'k';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'l';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'm';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'n';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'p';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                's';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                't';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'v';    'a';   ] `Ova_verb;
  Hashtbl.add classificationTbl [                'x';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [                'z';    'a';   ] `Ca_verb;
  Hashtbl.add classificationTbl [        '\xc4'; '\x8d'; 'a';   ] `Sha_verb;
  Hashtbl.add classificationTbl [        '\xc5'; '\xbe'; 'a';   ] `Sha_verb;
  Hashtbl.add classificationTbl ['\xc5'; '\xbe'; 'd';    'a';   ] `Sha_verb;
  Hashtbl.add classificationTbl ['\xe1'; '\xba'; '\x91'; 'a';   ] `Ca_verb;

  (* nǫ *)
  Hashtbl.add classificationTbl [        'n';    '\xc7'; '\xab';] `No_verb;

  (* 'j' endings *)
  Hashtbl.add classificationTbl [                        'j';   ] `J_verb;
  Hashtbl.add classificationTbl [        '\xc4'; '\x9b'; 'j';   ] `Ej_verb;
  Hashtbl.add classificationTbl [                'a';    'j';   ] `Aj_verb;
