(*type func = Func string list * expr;; * Lispの関数。文字列のリストは仮引数 *)

exception Lisp_error of string;;

(*型の宣言
 * func型は関数 FuncはLispの関数、Builtinは組み込み関数 
 * varlist型は変数リスト
 * environment型は環境 新しいスコープに入る時に変数リストを積む *)
type expr = Int of int | Symbol of string | List of expr list | Fn of func
and func = Func of expr * (string list) * environment
              | Macro of expr * (string list) * environment
              | Builtin of (expr list -> environment -> expr)
and varlist = (string * expr) list
and environment = (varlist ref) list;;

(* expr型を文字列に変換 *)
let rec string_of_expr = function
    Int x -> string_of_int x
  | Symbol s -> s
  | Fn _ -> "#<Function>"
  | List l -> "(" ^ let rec p = (function
        x::xs -> string_of_expr x ^ " " ^ p xs
      | [] -> ")") in
        p l;;

(* 変数を取り出す *)
let rec getvar name env = match env with
    head::tail -> if List.mem_assoc name !head then List.assoc name !head
                  else getvar name tail
  | [] -> raise (Lisp_error (name ^ " is not defined"));;
(* 変数代入 *)
let setvar name var env = match env with
    head::tail -> head := (name, var)::!head
  | [] -> raise (Lisp_error "invalid env");;

(* Lisp int to Caml int *)
let lint2cint = function
    Int x -> x
  | _ -> raise (Lisp_error "not given Int");;

let rec eval expr env =
  match expr with
      Int x -> Int x
    | Symbol s -> getvar s env
    | Fn f -> Fn f
    | List l -> (match if l<>[] then (eval (List.hd l) env)::List.tl l
                      else []
                with
        [] -> List []
      | (Fn f)::args -> apply f args env
      | _ -> raise (Lisp_error "Invalid application"))
and apply fn args env = match fn with
    Func (ex, params, e) -> 
      if (List.length params) <> (List.length args) then
        raise (Lisp_error "Invalid argument")
      else
        eval ex ((ref (List.combine params
                                  (List.map (fun ex -> eval ex env) args)))::(e@env))
  | Macro (ex, params, e) ->
      if (List.length params) <> (List.length args) then
        raise (Lisp_error "Invalid argument")
      else eval (eval ex ((ref (List.combine params args))::(e@env))) env
  | Builtin f -> f args env
;;
