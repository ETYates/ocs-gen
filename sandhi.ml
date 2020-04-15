
type change = Rule of { inn : Phon.Phoneme
                      ; out : Phon.Phoneme
                      ; context : context 
                      }
and context = Locus of { bfr : Phon.Phoneme
                       ; aftr : Phon.Phoneme
                       }

let sandhi stem ending = 
  String.concat "+" ((Phon.toString stem)::(Phon.toString ending)) 
