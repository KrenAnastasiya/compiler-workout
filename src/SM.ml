(* Simple expressions: syntax and semantics *)

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
             
(* The type for the expression. Note, in regular OCaml there is no "@type..." 
   notation, it came from GT. 
*)
@type expr =
  (* integer constant *) | Const of int
  (* variable         *) | Var   of string
  (* binary operator  *) | Binop of string * expr * expr with show


(* State: a partial map from variables to integer values. *)
type state = string -> int

(* Empty state: maps every variable into nothing. *)
let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

(* Update: non-destructively "modifies" the state s by binding the variable x 
   to value v and returns the new state.
*)
let update x v s = fun y -> if x = y then v else s y

(* An example of a non-trivial state: *)                                                   
let s = update "x" 1 @@ update "y" 2 @@ update "z" 3 @@ update "t" 4 empty


    let int_bool a = if a != 0 then true else false (*if 1->true, if 0->false*)
    let bool_int a = if a then 1 else 0 (*if true->1, if false->0*)

    let rec eval state expression = 
    match expression with
    | Var v -> state v 
    | Const c -> c 
    | Binop (operator, expression1, expression2) ->(*we have two operands and one operator*)
    let num1 = eval state expression1 in
    let num2 = eval state expression2 in
   
    match operator with
    |"+" -> (num1 + num2)
    |"-" -> (num1 - num2)
    |"*" -> (num1 * num2)
    |"/" -> (num1 / num2)
    |"%" -> (num1 mod num2)
    |"<"  -> bool_int (num1 < num2)
    |"<=" -> bool_int (num1 <= num2)
    |">"  -> bool_int (num1 > num2)
    |">=" -> bool_int (num1 >= num2)
    |"==" -> bool_int (num1 == num2)
    |"!=" -> bool_int (num1 != num2)
    |"!!" -> bool_int (int_bool num1 || int_bool num2)
    |"&&" -> bool_int (int_bool num1 && int_bool num2)
    | _ -> failwith "Not found this operator"
