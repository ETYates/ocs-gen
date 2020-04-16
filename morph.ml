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
  |  '\xa1'::'\xc5'::_             -> C_verb stem
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
  | _ -> failwith "Invalid char string"
