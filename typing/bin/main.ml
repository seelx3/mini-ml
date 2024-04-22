let () =
  let isTypable =
    if Typing.Infer.typable (Typing.Parse.parse "if (1 + 2) < 3 then 4 else 5")
    then "Typable"
    else "Not typable"
  in
  print_endline isTypable;
  let result =
    Typing.Reduction.normalize
      (Typing.Parse.parse "if (1 + 2) < 3 then 4 else 5")
  in
  print_endline (Typing.Syntax.string_of_prog result)
