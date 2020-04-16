type vowel = { high : bool
             ; back : bool
             ; tense : bool
             ; rounded : bool
             ; nasal : bool
             ; glyph : string
             }

type consonant = { obstruent : bool
                 ; sonorant  : bool
                 ; glide : bool
                 ; labial : bool
                 ; dental : bool
                 ; palatal : bool
                 ; velar : bool
                 ; voice : bool
                 ; glyph : string
                 }

type phoneme =
  | Vowel of vowel
  | Consonant of consonant


val implode : char list -> string
val explode : string -> char list
val process : string -> string list
val toString : phoneme list -> string
val phonemize : string -> phoneme
