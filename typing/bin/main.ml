type typ = TBool | TInt

exception Type_error

let infer (e : Typing.Syntax.exp) : typ =
  let rec infer' (e : Typing.Syntax.exp) : typ =
    match e with
    | Bool b -> TBool
    | Int n -> TInt
    | Add (p, q) -> (
        let tp = infer' p in
        let tq = infer' q in
        match (tp, tq) with TInt, TInt -> TInt | _ -> raise Type_error)
    | Lt (p, q) -> (
        let tp = infer' p in
        let tq = infer' q in
        match (tp, tq) with TInt, TInt -> TBool | _ -> raise Type_error)
    | If (p, q, r) -> (
        let tp = infer' p in
        let tq = infer' q in
        let tr = infer' r in
        match tp with
        | TBool -> (
            match (tq, tr) with a, b when a = b -> a | _ -> raise Type_error)
        | _ -> raise Type_error)
  in
  infer' e

let typable (e : Typing.Syntax.exp) : bool =
  try
    let _ = infer e in
    true
  with Type_error -> false

let parse str = Typing.Parser.main Typing.Lexer.token (Lexing.from_string str)

let () =
  if typable (If (Lt (Add (Int 1, Int 2), Int 3), Int 4, Int 5)) then
    print_endline "Typable"
  else print_endline "Not typable";
  if typable (parse "if (1 + 2) < 3 then 4 else 5") then print_endline "Typable"
  else print_endline "Not typable";
  if typable (parse "1 + false") then print_endline "Typable"
  else print_endline "Not typable";