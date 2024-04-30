open Syntax

exception Type_error

let infer (e : prog) : typ =
  let rec infer' (e : prog) (env : (string * typ) list) : typ =
    match e with
    | Bool _b -> TBool
    | Int _n -> TInt
    | Unit -> TUnit
    | Var x -> ( try List.assoc x env with Not_found -> raise Type_error)
    | Add (p, q) -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match (tp, tq) with TInt, TInt -> TInt | _ -> raise Type_error)
    | Lt (p, q) -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match (tp, tq) with TInt, TInt -> TBool | _ -> raise Type_error)
    | If (p, q, r) -> (
      let tp = infer' p env in
      let tq = infer' q env in
      let tr = infer' r env in
      match tp with
      | TBool -> (
        match (tq, tr) with a, b when a = b -> a | _ -> raise Type_error)
      | _ -> raise Type_error)
    | Product (p, q) -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match (tp, tq) with
      | TInt, TInt -> TProduct (TInt, TInt)
      | TInt, TBool -> TProduct (TInt, TBool)
      | TBool, TInt -> TProduct (TBool, TInt)
      | TBool, TBool -> TProduct (TBool, TBool)
      | _ -> raise Type_error)
    | Fst p -> (
      let tp = infer' p env in
      match tp with
      | TProduct (TInt, _) -> TInt
      | TProduct (TBool, _) -> TBool
      | _ -> raise Type_error)
    | Snd p -> (
      let tp = infer' p env in
      match tp with
      | TProduct (_, TInt) -> TInt
      | TProduct (_, TBool) -> TBool
      | _ -> raise Type_error)
    | Let (x, p, q) ->
      let tp = infer' p env in
      infer' q ((x, tp) :: env)
    | Fun (_, tx, p) ->
      let tp = infer' p env in
      TArraow (tx, tp)
    | App (p, q) -> (
      let tp = infer' p env in
      let tq = infer' q env in
      match tp with
      | TArraow (tx, tr) -> if tx = tq then tr else raise Type_error
      | _ -> raise Type_error)
    | FunVal (_, _, _) -> raise Type_error
  in
  infer' e []

let typable (e : prog) : bool =
  try
    let _ = infer e in
    true
  with Type_error -> false
