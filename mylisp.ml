open Evaluator

(* リストの最後の要素 *)
let rec last = function
    [] -> []
  | [_] as xs -> xs
  | x::xs -> last xs
;;
(* 最後以外 *)
let rec butlast = function
    [] -> []
  | x::[] -> []
  | x::xs -> x::(butlast xs)
;;

let copyenv = List.map (fun l -> ref !l);;

(* 組み込み関数の定義 *)
let lisp_plus args env =
  (List.fold_left (fun a b -> Int ((lint2cint a) + (lint2cint b)))
              (Int 0) (List.map (fun a -> eval a env) args))
;;
let lisp_minus args env = let arg = (List.map (fun a -> eval a env) args) in
  if arg == [] then raise (Lisp_error "requires at least 1 args")
  else (List.fold_left (fun a b -> Int ((lint2cint a) - (lint2cint b)))
        (List.hd arg) (List.tl arg))
;;
let lisp_mul args env =
  List.fold_left (fun a b -> Int ((lint2cint a) * (lint2cint b)))
              (Int 1) (List.map (fun a -> eval a env) args)
;;
let lisp_div args env = let arg = (List.map (fun a -> eval a env) args) in
  if arg == [] then raise (Lisp_error "requires at least 1 args")
  else (List.fold_left (fun a b -> Int ((lint2cint a) / (lint2cint b)))
        (List.hd arg) (List.tl arg))
;;
let lisp_print args env = match (List.map (fun a -> eval a env) args) with
          [x] -> print_string (string_of_expr x); print_newline(); List []
        | _ -> List []
;;
let lisp_list args env = List (List.map (fun a -> eval a env) args);;
let lisp_car args env = match (List.map (fun a -> eval a env) args) with
    [List l] -> List.hd l
  | _ -> raise (Lisp_error "Invalid argument")
;;
let lisp_cdr args env = match (List.map (fun a -> eval a env) args) with
    [List l] -> List (List.tl l)
  | _ -> raise (Lisp_error "Invalid argument")
;;
let lisp_def args env = (match args with
          [Symbol name; value] -> setvar name (eval value env) env; List []
        | _ -> raise (Lisp_error "malformed def"))
;;
let lisp_fun args env = match args with
    [List params; body] -> Fn (Func (body, (List.map (function
        Symbol s -> s
      | _ -> raise (Lisp_error "Invalid parameters")) params), copyenv ((ref [])::env)))
  | _ -> raise (Lisp_error "malformed fun")
;;
let lisp_mac args env = match args with
    [List params; body] -> Fn (Macro (body, (List.map (function
        Symbol s -> s
      | _ -> raise (Lisp_error "Invalid parameters")) params), copyenv ((ref [])::env)))
  | _ -> raise (Lisp_error "malformed mac")
;;
let lisp_begin args env = let newenv = (ref [])::env in
  List.iter (fun ex -> ignore (eval ex newenv)) (butlast args);
  match (last args) with
      [x] -> eval x newenv
    | _ -> List []
;;

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let vars = [ref [
      ("+", Fn (Builtin lisp_plus));
      ("-", Fn (Builtin lisp_minus));
      ("*", Fn (Builtin lisp_mul));
      ("/", Fn (Builtin lisp_div));
      ("print", Fn (Builtin lisp_print));
      ("list", Fn (Builtin lisp_list));
      ("car", Fn (Builtin lisp_car));
      ("cdr", Fn (Builtin lisp_cdr));
      ("begin", Fn (Builtin lisp_begin));
      ("def", Fn (Builtin lisp_def));
      ("fun", Fn (Builtin lisp_fun));
      ("mac", Fn (Builtin lisp_mac));
      ("foo", Int 10)]] in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      try
        print_string (string_of_expr (eval result vars)); print_newline(); flush stdout
      with Lisp_error msg -> print_string ("Error: "  ^ msg); print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0
