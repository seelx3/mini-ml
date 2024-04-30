type typ = TBool | TInt | TProduct of typ * typ | TUnit | TArraow of typ * typ

type prog =
  | Bool of bool
  | Int of int
  | Unit
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog
  | Product of prog * prog
  | Fst of prog
  | Snd of prog
  | Var of string
  | Let of string * prog * prog
  | Fun of string * typ * prog
  | FunVal of string * prog * (string * prog) list
  | App of prog * prog

let rec string_of_prog = function
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Unit -> "()"
  | Product (e1, e2) -> "(" ^ string_of_prog e1 ^ ", " ^ string_of_prog e2 ^ ")"
  | _ -> failwith "Not implemented"
