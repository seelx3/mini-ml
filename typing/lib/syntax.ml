type exp =
  | Bool of bool
  | Int of int
  | Add of exp * exp
  | Lt of exp * exp
  | If of exp * exp * exp
