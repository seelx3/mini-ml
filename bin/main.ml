open Typing.Parse
open Typing.Infer
open Typing.Reduction
open Typing.Syntax

let () =
  if Array.length Sys.argv = 1 then (
    Printf.printf "        MiniML version 0.0.1";
    while true do
      Printf.printf "\n# ";
      try
        let line = read_line () in
        (* TODO: refactor this line *)
        let line = String.sub line 0 (String.length line - 2) in
        let command = parse line in
        let isTypable = typable command in
        match isTypable with
        | false -> print_endline "Error: TypeError\n"
        | true ->
          let result = normalize command in
          print_endline (string_of_prog result)
      with End_of_file -> exit 0
    done)
  else
    let arg1 = Sys.argv.(1) in
    match arg1 with
    | "--help" -> Printf.printf "Usage: ./typing [file]\n"
    (* TODO: exec file *)
    | _ -> Printf.printf "file: %s\n" arg1
