(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator
          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    
    let eval _ = failwith "Not implemented yet"*)
		let binop operation op1 op2 =
			     let num2bool value = 
			       if value == 0 then false else true in
			     let bool2num value = 
			       if value then 1 else 0 in
			     match operation with
			     | "+"   -> op1 + op2
			     | "-"   -> op1 - op2
			     | "*"   -> op1 * op2
			     | "/"   -> op1 / op2
			     | "%"   -> op1 mod op2
			     | "&&"  -> bool2num (num2bool op1 && num2bool op2)
			     | "!!"  -> bool2num (num2bool op1 || num2bool op2)
			     | "<"   -> bool2num (op1 < op2)
			     | "<="  -> bool2num (op1 <= op2)
			     | ">"   -> bool2num (op1 > op2)
			     | ">="  -> bool2num (op1 >= op2)
			     | "=="  -> bool2num (op1 == op2)
			     | "!="  -> bool2num (op1 != op2)

		let rec eval state expr = 
   		match expr with
   		| Const const -> const
   		| Var variable -> state variable
   		| Binop (operation, op1, op2) ->
				binop operation (eval state op1) (eval state op2)

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator
          val eval : config -> t -> config
       Takes a configuration and a statement, and returns another configuration
    
    let eval _ = failwith "Not implemented yet"*)
	let rec eval conf state = 
			let (st, i, o) = conf in
			match state with
			| Read v ->
				(match i with
				|h::t -> (Expr.update v h st, i, o)
				|[] -> failwith "Input is empty!")
			| Write expr ->
				(st, i, o @ [(Expr.eval st expr)])
			| Assign (v, expr) ->
				let ex = Expr.eval st expr in
				(Expr.update v ex st, i, o)                                 
			| Seq (l, r) ->
				let tmp_config = eval conf l in
				eval tmp_config r
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator
     eval : int list -> t -> int list
   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
let _, _, o = Stmt.eval (Expr.empty, i, []) p in o