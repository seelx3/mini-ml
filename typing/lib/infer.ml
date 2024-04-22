type typ = TBool | TInt | TProduct of typ * typ | TUnit

exception Type_error

let infer (e : Syntax.prog) : typ =
  let rec infer' (e : Syntax.prog) : typ =
    match e with
    | Bool _b -> TBool
    | Int _n -> TInt
    | Unit -> TUnit
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
    | Product (p, q) -> (
      let tp = infer' p in
      let tq = infer' q in
      match (tp, tq) with
      | TInt, TInt -> TProduct (TInt, TInt)
      | TInt, TBool -> TProduct (TInt, TBool)
      | TBool, TInt -> TProduct (TBool, TInt)
      | TBool, TBool -> TProduct (TBool, TBool)
      | _ -> raise Type_error)
    | Fst p -> (
      let tp = infer' p in
      match tp with
      | TProduct (TInt, _) -> TInt
      | TProduct (TBool, _) -> TBool
      | _ -> raise Type_error)
    | Snd p -> (
      let tp = infer' p in
      match tp with
      | TProduct (_, TInt) -> TInt
      | TProduct (_, TBool) -> TBool
      | _ -> raise Type_error)
  in
  infer' e

let typable (e : Syntax.prog) : bool =
  try
    let _ = infer e in
    true
  with Type_error -> false
