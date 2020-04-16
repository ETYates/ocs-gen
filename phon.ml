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

let explode s =
  let n = String.length s in
  let rec repeat i =
    match i = n with
    | true -> []
    | false -> s.[i] :: repeat (i + 1) in
  repeat 0

let implode chars =
  let res = Bytes.create (List.length chars) in
  let rec repeat i chars =
    match chars with
    | [] -> res
    | char :: chars ->
      Bytes.set res i char;
      repeat (i + 1) chars in
  Bytes.to_string (repeat 0 chars)

let process s =
  let chars = explode s in
  let rec repeat accum chars =
    match chars with
    | 'n'::'j'::chars -> repeat ("nj" :: accum) chars
    | 'r'::'j'::chars -> repeat ("rj" :: accum) chars
    | 'l'::'j'::chars -> repeat ("lj" :: accum) chars
    | c::chars -> repeat ((String.make 1 c) :: accum) chars
    | [] -> accum in
  repeat [] chars

let toString ps =
  let phonize p =
    match p with
    | Vowel {glyph=g; _} -> g
    | Consonant {glyph=g; _} -> g in
  let strs = List.map phonize ps in
  String.concat "" strs

and phonemize c =
  match c with
  | "i" -> Vowel { back=false
                ; high=true
                ; tense=true
                ; rounded=false
                ; nasal=false
                ; glyph="i"
                }
  | "y" -> Vowel { back=true
                ; high=true
                ; tense=true
                ; rounded=false
                ; nasal=false
                ; glyph="y"
                }
  | "u" -> Vowel { back=true
                ; high=true
                ; tense=true
                ; rounded=true
                ; nasal=false
                ; glyph="u"
                }
  | "ĭ" -> Vowel { back=false
                ; high=true
                ; tense=false
                ; rounded=false
                ; nasal=false
                ; glyph="ĭ"
                }
  | "ŭ" -> Vowel { back=true
                ; high=true
                ; tense=false
                ; rounded=false
                ; nasal=false
                ; glyph="ŭ"
                }
  | "e" -> Vowel { back=false
                ; high=false
                ; tense=false
                ; rounded=false
                ; nasal=false
                ; glyph="e"
                }
  | "o" -> Vowel { back=true
                ; high=false
                ; tense=false
                ; rounded=true
                ; nasal=false
                ; glyph="o"
                }
  | "ě" -> Vowel { back=false
                ; high=false
                ; tense=true
                ; rounded=false
                ; nasal=false
                ; glyph="ě"
                }
  | "a" -> Vowel { back=true
                ; high=false
                ; tense=true
                ; rounded=false
                ; nasal=false
                ; glyph="a"
                }
  | "ę" -> Vowel { back=false
                ; high=false
                ; tense=true
                ; rounded=false
                ; nasal=true
                ; glyph="ę"
                }
  | "ǫ" -> Vowel { back=true
                ; high=true
                ; tense=true
                ; rounded=true
                ; nasal=true
                ; glyph="ǫ"
                }
  | "p" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=true
                    ; dental=false
                    ; palatal=false
                    ; velar=false
                    ; voice=false
                    ; glyph="p"
                    }
  | "b" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=true
                    ; dental=false
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="b"
                    }
  | "m" -> Consonant { obstruent=false
                    ; sonorant=true
                    ; glide=false
                    ; labial=true
                    ; dental=false
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="m"
                    }
  | "w" -> Consonant { obstruent=false
                    ; sonorant=false
                    ; glide=true
                    ; labial=true
                    ; dental=false
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="w"
                    }
  | "t" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=false
                    ; glyph="t"
                    }
  | "d" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="d"
                    }
  | "s" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=false
                    ; glyph="s"
                    }
  | "z" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="z"
                    }
  | "c" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=false
                    ; glyph="c"
                    }
  | "ẑ" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="ẑ"
                    }
  | "n" -> Consonant { obstruent=false
                    ; sonorant=true
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="n"
                    }
  | "l" -> Consonant { obstruent=false
                    ; sonorant=true
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="l"
                    }
  | "r" -> Consonant { obstruent=false
                    ; sonorant=true
                    ; glide=false
                    ; labial=false
                    ; dental=true
                    ; palatal=false
                    ; velar=false
                    ; voice=true
                    ; glyph="r"
                    }
  | "č" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=false
                    ; palatal=true
                    ; velar=false
                    ; voice=false
                    ; glyph="č"
                    }
  | "š" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=false
                    ; palatal=true
                    ; velar=false
                    ; voice=false
                    ; glyph="š"
                    }
  | "ž" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=false
                    ; palatal=true
                    ; velar=false
                    ; voice=true
                    ; glyph="ž"
                    }
  | "nj" -> Consonant { obstruent=false
                     ; sonorant=true
                     ; glide=false
                     ; labial=false
                     ; dental=false
                     ; palatal=true
                     ; velar=false
                     ; voice=true
                     ; glyph="nj"
                     }
  | "lj" -> Consonant { obstruent=false
                     ; sonorant=true
                     ; glide=false
                     ; labial=false
                     ; dental=false
                     ; palatal=true
                     ; velar=false
                     ; voice=true
                     ; glyph="lj"
                     }
  | "rj" -> Consonant { obstruent=false
                     ; sonorant=true
                     ; glide=false
                     ; labial=false
                     ; dental=false
                     ; palatal=true
                     ; velar=false
                     ; voice=true
                     ; glyph="rj"
                     }
  | "j" -> Consonant { obstruent=false
                     ; sonorant=false
                     ; glide=true
                     ; labial=false
                     ; dental=false
                     ; palatal=true
                     ; velar=false
                     ; voice=true
                     ; glyph="j"
                     }
  | "k" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=false
                    ; palatal=false
                    ; velar=true
                    ; voice=false
                    ; glyph="k"
                    }
  | "g" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=false
                    ; palatal=false
                    ; velar=true
                    ; voice=true
                    ; glyph="g"
                    }
  | "x" -> Consonant { obstruent=true
                    ; sonorant=false
                    ; glide=false
                    ; labial=false
                    ; dental=false
                    ; palatal=false
                    ; velar=true
                    ; voice=false
                    ; glyph="x"
                    }
  | _ -> failwith "Invalid input"
