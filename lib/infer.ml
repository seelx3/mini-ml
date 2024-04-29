type typ = TBool | TInt | TProduct of typ * typ | TUnit

exception Type_error

let infer (e : Syntax.prog) : typ =
  let rec infer' (e : Syntax.prog) (env : (string * typ) list) : typ =
    match (e, env) with
    | Bool _b, _ -> TBool
    | Int _n, _ -> TInt
    | Unit, _ -> TUnit
    | Var x, _ -> ( try List.assoc x env with Not_found -> raise Type_error)
    | Add (p, q), _ -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match (tp, tq) with TInt, TInt -> TInt | _ -> raise Type_error)
    | Lt (p, q), _ -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match (tp, tq) with TInt, TInt -> TBool | _ -> raise Type_error)
    | If (p, q, r), _ -> (
      let tp = infer' p env in
      let tq = infer' q env in
      let tr = infer' r env in
      match tp with
      | TBool -> (
        match (tq, tr) with a, b when a = b -> a | _ -> raise Type_error)
      | _ -> raise Type_error)
    | Product (p, q), _ -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match (tp, tq) with
      | TInt, TInt -> TProduct (TInt, TInt)
      | TInt, TBool -> TProduct (TInt, TBool)
      | TBool, TInt -> TProduct (TBool, TInt)
      | TBool, TBool -> TProduct (TBool, TBool)
      | _ -> raise Type_error)
    | Fst p, _ -> (
      let tp = infer' p env in
      match tp with
      | TProduct (TInt, _) -> TInt
      | TProduct (TBool, _) -> TBool
      | _ -> raise Type_error)
    | Snd p, _ -> (
      let tp = infer' p env in
      match tp with
      | TProduct (_, TInt) -> TInt
      | TProduct (_, TBool) -> TBool
      | _ -> raise Type_error)
    | Let (x, p, q), _ ->
      let tp = infer' p env in
      infer' q ((x, tp) :: env)
  in
  infer' e []

let typable (e : Syntax.prog) : bool =
  try
    let _ = infer e in
    true
  with Type_error -> false
