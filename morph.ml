

type parse = 
  | VerbTag of { person : person
               ; number : number
               ; tense : tense
               }
  | NounTag of { case : case
               ; number : number
               ; gender : gender
               }
and declension =
  | Twofold
  | Pronominal
  | Compound
and conjugation =
  | Consonantal
  | Vocalic 
and stem =
  | Verb of klass
  | Nominal of declension
and klass =
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
and case =
  | Nominative
  | Genitive
  | Dative
  | Accusative
  | Instrumental
  | Prepositional
and person =
  | First
  | Second
  | Third
and tense =
  | Present 
  | Past
  | Imperative
  | Imperfect
and number =
  | Singular
  | Dual
  | Plural
and gender =
  | Masculine
  | Feminine
  | Neuter

let classify stem =
  let stemcs = Phon.explode stem in
  match stemcs with
  | cs::'i'             -> I_verb stem
  | cs::'ě'             -> E_verb stem
  | cs::'č'::'a'        -> Sha_verb stem
  | cs::'ž'::'a'        -> Sha_verb stem
  | cs::'š'::'a'        -> Sha_verb stem
  | cs::'š'::'t'::'a'   -> Sha_verb stem
  | cs::'ž'::'d'::'a'ؤ  -> Sha_verb stem
  | cs::'j'::'a'        -> Ja_verb stem
  | cs::'v'::'a'        -> Ova_verb stem
  | cs::'n'::'ǫ'        -> No_verb stem
  | cs::'k'::'a'        -> Ca_verb stem
  | cs::'t'::'a'        -> Ca_verb stem
  | cs::'z'::'a'        -> Ca_verb stem
  | cs::'l'::'a'        -> Ca_verb stem
  | cs::'ẑ'::'a'        -> Ca_verb stem
  | cs::'m'::'a'        -> Ca_verb stem
  | cs::'x'::'a'        -> Ca_verb stem
  | cs::'d'::'a'        -> Ca_verb stem
  | cs::'b'::'a'        -> Ca_verb stem
  | cs::'z'::'a'        -> Ca_verb stem
  | cs::'p'::'a'        -> Ca_verb stem
  | cs::'c'::'a'        -> Ca_verb stem
  | cs::'g'::'a'        -> Ca_verb stem
  | cs::'s'::'a'        -> Ca_verb stem
  | cs::'n'::'a'        -> Ca_verb stem
  | cs::'a'::'j'        -> Aj_verb stem
  | cs::'ě'::'j'        -> Ej_verb stem
  | cs::'j'             -> J_verb stem
  | cs::'r'             -> C_verb stem
  | cs::'t'             -> C_verb stem
  | cs::'p'             -> C_verb stem
  | cs::'š'             -> C_verb stem
  | cs::'ž'             -> C_verb stem
  | cs::'s'             -> C_verb stem
  | cs::'d'             -> C_verb stem
  | cs::'f'             -> C_verb stem
  | cs::'g'             -> C_verb stem
  | cs::'h'             -> C_verb stem
  | cs::'k'             -> C_verb stem
  | cs::'l'             -> C_verb stem
  | cs::'č'             -> C_verb stem
  | cs::'z'             -> C_verb stem
  | cs::'x'             -> C_verb stem
  | cs::'c'             -> C_verb stem
  | cs::'v'             -> C_verb stem
  | cs::'b'             -> C_verb stem
  | cs::'n'             -> C_verb stem
  | cs::'m'             -> C_verb stem
