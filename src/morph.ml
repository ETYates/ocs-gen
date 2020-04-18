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

let slice l idx =
  let rec loop acc i = function
    | hd :: tl ->
      if i = 0 then
        acc
      else
        loop (hd :: acc) (i - 1) tl
    | [] -> List.rev acc in
  loop [] idx l


let classifyTbl stem =
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

  let s = List.rev (Phon.explode stem) in
  let last4 = slice s 4 in
  List.iter (fun c -> Printf.printf "%x " (Char.code c)) last4;
  Printf.printf "\n";
  match Hashtbl.find_opt classificationTbl last4 with
  | Some t -> retClassification t
  | None ->
    begin
      let last3 = slice s 3 in
      List.iter (fun c -> Printf.printf "%x " (Char.code c)) last3;
      Printf.printf "\n";
      match Hashtbl.find_opt classificationTbl last3 with
        | Some t -> retClassification t
        | None ->
          begin
            let last2 = slice s 2 in
            List.iter (fun c -> Printf.printf "%x " (Char.code c)) last2;
            Printf.printf "\n";
            match Hashtbl.find_opt classificationTbl last2 with
              | Some t -> retClassification t
              | None ->
                begin
                  let last1 = slice s 1 in
                  match Hashtbl.find_opt classificationTbl last1 with
                    | Some t -> retClassification t
                    | None -> failwith ("Invalid char string: " ^ stem)
                end

          end
    end

let _ =
  (*| 'a'; 'č'; *)
  Hashtbl.add classificationTbl ['a'; '\xc4'; '\x8d'] `Sha_verb;
  (*| 'a'; 'ž';  *)
  Hashtbl.add classificationTbl ['\xc5'; '\xbe'; 'a'] `Sha_verb;
  (*| 'a'; 'š'; *)                                  
  Hashtbl.add classificationTbl ['\xc5'; '\xa1'; 'a' ] `Sha_verb;
  (*| 'a'; 't'; 'š'; _             -> Sha_verb stem*)
  Hashtbl.add classificationTbl ['a'; 't'; '\xc5'; '\xa1'] `Sha_verb;
  (*| 'a'; 'd'; 'ž'; _ؤ            -> Sha_verb stem*)
  Hashtbl.add classificationTbl ['a'; 'd'; '\xc5'; '\xbe'] `Sha_verb;
  Hashtbl.add classificationTbl ['a'; 'j'] `Ja_verb;
  Hashtbl.add classificationTbl ['v';'a'; ] `Ova_verb;
  (*| 'ǫ'; 'n'; _                  -> No_verb stem *)
  Hashtbl.add classificationTbl ['\xc7'; '\xab'; 'n'] `No_verb;
  Hashtbl.add classificationTbl ['k'; 'a'; ] `Ca_verb;
  Hashtbl.add classificationTbl ['t'; 'a'; ] `Ca_verb;
  Hashtbl.add classificationTbl ['z'; 'a'; ] `Ca_verb;
  Hashtbl.add classificationTbl ['l'; 'a'; ] `Ca_verb;
  (*| 'a'; 'ẑ'; _                  -> Ca_verb stem *)
  Hashtbl.add classificationTbl ['\xe1';'\xba';'\x91';'a'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'm'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'x'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'd'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'b'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'p'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'c'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'g'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 's'] `Ca_verb;
  Hashtbl.add classificationTbl ['a'; 'n'] `Ca_verb;
  Hashtbl.add classificationTbl ['j'; 'a'] `Aj_verb;
  (*| 'j'; 'ě'; _                  -> Ej_verb stem *)
  Hashtbl.add classificationTbl ['j'; '\xc4'; '\x9b'] `Ej_verb;
  
  
  Hashtbl.add classificationTbl ['i'] `I_verb;
  (*| 'ě'; _                       -> E_verb stem*)
  (* Hashtbl.add classificationTbl ['\x9b'; '\xc4'] `E_verb; *)
  Hashtbl.add classificationTbl ['\xc4'; '\x9b'] `E_verb;


  Hashtbl.add classificationTbl ['j'] `J_verb;
  Hashtbl.add classificationTbl ['r'] `C_verb;
  Hashtbl.add classificationTbl ['t'] `C_verb;
  Hashtbl.add classificationTbl ['p'] `C_verb;
  (*| 'š'; *)
  Hashtbl.add classificationTbl ['\xc5'; '\xa1'] `C_verb;
  (*| 'ž'; *)                                  
  Hashtbl.add classificationTbl ['\xc5'; '\xbe'] `Sha_verb;
  Hashtbl.add classificationTbl ['s'] `C_verb;
  Hashtbl.add classificationTbl ['d'] `C_verb;
  Hashtbl.add classificationTbl ['f'] `C_verb;
  Hashtbl.add classificationTbl ['g'] `C_verb;
  Hashtbl.add classificationTbl ['h'] `C_verb;
  Hashtbl.add classificationTbl ['k'] `C_verb;
  Hashtbl.add classificationTbl ['l'] `C_verb;
  (*| 'č'; *)
  Hashtbl.add classificationTbl ['\xc4'; '\x8d'] `C_verb;
  Hashtbl.add classificationTbl ['z'] `C_verb;
  Hashtbl.add classificationTbl ['x'] `C_verb;
  Hashtbl.add classificationTbl ['c'] `C_verb;
  Hashtbl.add classificationTbl ['v'] `C_verb;
  Hashtbl.add classificationTbl ['b'] `C_verb;
  Hashtbl.add classificationTbl ['n'] `C_verb;
  Hashtbl.add classificationTbl ['m'] `C_verb;
