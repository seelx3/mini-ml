type typ = TBool | TInt

exception Type_error

let infer (e : Syntax.exp) : typ =
  let rec infer' (e : Syntax.exp) : typ =
    match e with
    | Bool _b -> TBool
    | Int _n -> TInt
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

let typable (e : Syntax.exp) : bool =
  try
    let _ = infer e in
    true
  with Type_error -> false
