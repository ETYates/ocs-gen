
type unittest = test list

and test = { lemma : string
           ; parse : string
           ; form : string
           }

let runTest paradigms =
  let file = "lunt.txt" in
  let paradigms = open_in file in

