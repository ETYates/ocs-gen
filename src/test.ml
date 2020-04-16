type elem = Test of { lemma : string
                    ; parse : string
                    ; form : string
                    }

(**
 * [readFile] returns type string list.
 * Read from the file of name `fn` line by line and return a list of strings
 *)
let readFile fn =
  let ic = open_in fn in
  let rec build_list l =
    match input_line ic with
    | line ->
      line :: l
      |> build_list
    | exception End_of_file ->
      close_in ic;
      List.rev l in
  build_list []


let makeTest parastring =
  Printf.printf "%s\n" parastring;
  let info = String.split_on_char ' ' parastring in
  match info with
  | [lemma; parse; form] -> Test {lemma; parse; form}
  | _ -> failwith "Incorrect test input."

let doTests file =
  let paradigms = readFile file in
  let tests = List.map makeTest paradigms in
  let rec run ts =
    (match ts with
    | [] -> ()
    | (Test {lemma; parse; form}) :: ts ->
      Printf.printf "Attempting test [%s %s]\n" lemma parse;
      let (tense, person, number) = Parse.makeparse parse in
      let newlem = Phon.process lemma in
      (* Prtinf.printf "%s %s %s\n" *)
      let realparse = Morph.VerbTag {tense; person; number} in
      let src = Conj.generate newlem realparse in
      begin
        match (String.compare src form) with
        | 0 -> Printf.printf "Test '%s %s' passed\n" lemma parse;
          run ts
           (* [1]::(run ts) *)
        | _ -> Printf.printf "Test '%s %s' failed\n" lemma parse;
          run ts
          (* [0]::(run ts)) *)
      end
    ) in
  run tests
