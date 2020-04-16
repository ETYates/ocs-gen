type change = Rule of { inn : Phon.phoneme
                      ; out : Phon.phoneme
                      ; context : context
                      }

and context = Locus of { bfr : Phon.phoneme
                       ; aftr : Phon.phoneme
                       }

val sandhi : Morph.klass -> string -> string
