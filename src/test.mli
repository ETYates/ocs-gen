type elem = Test of { id : int 
                    ; lemma : string
                    ; parse : string
                    ; form : string
                    }

val doTests : string -> bool -> unit
