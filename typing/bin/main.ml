open Typing.Parse
open Typing.Infer
open Typing.Reduction
open Typing.Syntax

let () =
  let isTypable =
    if typable (parse "if (1 + 2) < 3 then 4 else 5") then "Typable"
    else "Not typable"
  in
  print_endline isTypable;
  let result = normalize (parse "if (1 + 2) < 3 then 4 else 5") in
  print_endline (string_of_prog result)
