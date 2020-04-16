type change = Rule of { inn : Phon.phoneme
                      ; out : Phon.phoneme
                      ; context : context 
                      }
and context = Locus of { bfr : Phon.phoneme
                       ; aftr : Phon.phoneme
                       }

let sandhi (stem : Morph.klass) ending =
  String.concat "+" ((Morph.klassToString stem)::(ending)::[]) 
