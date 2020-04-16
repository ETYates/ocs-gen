

type elem = Test of { lemma : string
                   ; parse : string
                   ; form : string
                   }


let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines


let makeTest parastring =
  let info = String.split_on_char ' ' parastring in
  match info with
  | [lemma; parse; form] -> Test {lemma; parse; form}
  | _ -> failwith "Incorrect test input."

let _ = 
  let file = "lunt.txt" in
  let paradigms = read_file file in
  let tests = List.map makeTest paradigms in
  let rec run ts = 
  (match ts with
  | [] -> []
  | (Test {lemma; parse; form}) :: ts ->
    let (tee, per, num) = Main.makeparse parse in
    let newlem = Phon.process lemma in
    let realparse = Morph.VerbTag {tense=tee; person=per; number=num} in
    let src = Conj.generate newlem realparse in
    (match (String.compare src form) with
      | 0 -> Printf.printf "Test %s passed" lemma; [1]::(run ts) 
      | _ -> Printf.printf "Test %s failed" lemma; [0]::(run ts))) in
  run tests
  
