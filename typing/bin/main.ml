let () =
  if Typing.Infer.typable (Typing.Parse.parse "if (1 + 2) < 3 then 4 else 5") then print_endline "Typable"
  else print_endline "Not typable";
