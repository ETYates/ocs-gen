type elem = Test of { id : int 
                    ; lemma : string
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


let makeTest id parastring =
  (* Printf.printf "%s\n" parastring; *)
  let info = String.split_on_char ' ' parastring in
  match info with
  | [lemma; parse; form] -> 
    let form = String.trim form in
    Test {id; lemma; parse; form }
  | _ -> failwith "Incorrect test input."

let log benchmark testID lemma parse form src passed =
  if not benchmark then
      Printf.printf "%03d | Run test '%s' '%s' '%s' -> [%s] %s\n"
        testID
        lemma
        parse
        form
        src
        passed

let doTests file benchmark =
  let paradigms = readFile file in
  let tests = List.mapi makeTest paradigms in
  let rec run ts =
    (match ts with
    | [] -> ()
    | (Test {id; lemma; parse; form}) :: ts ->
      let (tense, person, number) = Parse.makeparse parse in
      let newlem = Phon.process lemma in
      (* Prtinf.printf "%s %s %s\n" *)
      let realparse = Morph.VerbTag {tense; person; number} in
      let src = Conj.generate newlem realparse in
      begin
        match (String.compare src form) with
        | 0 ->
          log benchmark id lemma parse form src "passed";
          run ts
           (* [1]::(run ts) *)
        | _ ->
          log benchmark id lemma parse form src "failed";
          run ts
          (* [0]::(run ts)) *)
      end
    ) in
  run tests
