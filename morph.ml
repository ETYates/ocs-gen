

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
  | I_verbb of string 
  | E_verbb of string
  | Sha_verbb of string
  | Ja_verbb of string
  | Ova_verbb of string
  | Ca_verbb of string
  | No_verbb of string
  | C_verbb of string
  | Aj_verbb of string
  | Ej_verbb of string
  | J_verbb of string
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
  | cs::'i'             -> I_verb
  | cs::'ě'             -> E_verb
  | cs::'č'::'a'        -> Sha_verb
  | cs::'ž'::'a'        -> Sha_verb
  | cs::'š'::'a'        -> Sha_verb
  | cs::'š'::'t'::'a'   -> Sha_verb
  | cs::'ž'::'d'::'a'ؤ  -> Sha_verb
  | cs::'j'::'a'        -> Ja_verb
  | cs::'v'::'a'        -> Ova_verb
  | cs::'n'::'ǫ'        -> No_verb
  | cs::'k'::'a'        -> Ca_verb
  | cs::'t'::'a'        -> Ca_verb
  | cs::'z'::'a'        -> Ca_verb
  | cs::'l'::'a'        -> Ca_verb
  | cs::'ẑ'::'a'        -> Ca_verb
  | cs::'m'::'a'        -> Ca_verb
  | cs::'x'::'a'        -> Ca_verb
  | cs::'d'::'a'        -> Ca_verb
  | cs::'b'::'a'        -> Ca_verb
  | cs::'z'::'a'        -> Ca_verb
  | cs::'p'::'a'        -> Ca_verb
  | cs::'c'::'a'        -> Ca_verb
  | cs::'g'::'a'        -> Ca_verb
  | cs::'s'::'a'        -> Ca_verb
  | cs::'n'::'a'        -> Ca_verb
  | cs::'a'::'j'        -> Aj_verb
  | cs::'ě'::'j'        -> Ej_verb
  | cs::'j'             -> J_verb
  | cs::'r'             -> C_verb
  | cs::'t'             -> C_verb
  | cs::'p'             -> C_verb
  | cs::'š'             -> C_verb
  | cs::'ž'             -> C_verb
  | cs::'s'             -> C_verb
  | cs::'d'             -> C_verb
  | cs::'f'             -> C_verb
  | cs::'g'             -> C_verb
  | cs::'h'             -> C_verb
  | cs::'k'             -> C_verb
  | cs::'l'             -> C_verb
  | cs::'č'             -> C_verb
  | cs::'z'             -> C_verb
  | cs::'x'             -> C_verb
  | cs::'c'             -> C_verb
  | cs::'v'             -> C_verb
  | cs::'b'             -> C_verb
  | cs::'n'             -> C_verb
  | cs::'m'             -> C_verb
