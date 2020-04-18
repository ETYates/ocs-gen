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

val klassToString : klass -> string

val classify : string -> klass
val classifyTbl : string -> klass
