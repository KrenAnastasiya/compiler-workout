(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    let uvar x = failwith (Printf.sprintf "Undefined variable %s" x)
    (* Empty state *)
    let empty = { g = uvar; l = uvar; scope = [] }

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = 
      let update_gl_state x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then
        { s with l = update_gl_state x v s.l }
      else { s with g = update_gl_state x v s.g }
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = 
      if List.mem x s.scope then s.l x
      else s.g x

    (* Creates a new scope, based on a given state *)
    let enter st xs = { g = st.g; l = uvar; scope = xs }

    (* Drops a scope *)
    let leave st st' = { g = st'.g; l = st.l; scope = st.scope }

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let int_to_bool value = 
      if value == 0 then false else true
    let bool_to_int value = 
      if value then 1 else 0

    let binop operation left_operand right_operand = 
      match operation with
      | "+"   -> left_operand + right_operand
      | "-"   -> left_operand - right_operand
      | "*"   -> left_operand * right_operand
      | "/"   -> left_operand / right_operand
      | "%"   -> left_operand mod right_operand
      | "&&"  -> bool_to_int (int_to_bool left_operand && int_to_bool right_operand)
      | "!!"  -> bool_to_int (int_to_bool left_operand || int_to_bool right_operand)
      | "<" -> bool_to_int (left_operand < right_operand)
      | "<="  -> bool_to_int (left_operand <= right_operand)
      | ">" -> bool_to_int (left_operand > right_operand)
      | ">="  -> bool_to_int (left_operand >= right_operand)
      | "=="  -> bool_to_int (left_operand == right_operand)
      | "!="  -> bool_to_int (left_operand != right_operand)
      | _ -> failwith("Undefined operator!")

    let rec eval st expr = 
      match expr with
    | Const const -> const
    | Var variable -> State.eval st variable
    | Binop (operation, left, right) ->
      let left_operand = 
        eval st left in
      let right_operand = 
        eval st right in
      binop operation left_operand right_operand

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    
    let mapoperation operations = List.map (fun operation -> ostap($(operation)), (fun left right -> Binop (operation, left, right))) operations

    ostap (
      parse: 
        !(Ostap.Util.expr
          (fun x -> x)
          [|
            `Lefta, mapoperation ["!!"];
            `Lefta, mapoperation ["&&"];
            `Nona,  mapoperation ["=="; "!=";">="; ">"; "<="; "<"];
            `Lefta, mapoperation ["+"; "-"];
            `Lefta, mapoperation ["*"; "/"; "%"];
          |]
          primary
        );
      primary: variable:IDENT {Var variable} | const:DECIMAL {Const const} | -"(" parse -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)

    let rec eval env conf stmt =
      match conf, stmt with
      | (st, input, output) , Assign (variable, expr) -> 
        let value = 
          Expr.eval st expr in
        (State.update variable value st, input, output)
      | (st, z::input, output), Read variable ->
        (State.update variable z st, input, output)
      | (st, input, output), Write expr ->
        let value = 
          Expr.eval st expr in
        (st, input, output @ [value])
      | conf, Seq (left_stmt, right_stmt) ->
        eval env (eval env conf left_stmt) right_stmt
      | conf, Skip -> conf
      | (st, input, output), If (expr, th, el) ->
        let if_answer expr = if Expr.eval st expr == 0 then el else th in
        eval env (st, input, output) (if_answer expr)
      | (st, input, output), While (expr, while_stmt) ->
        if Expr.eval st expr == 0 then (st, input, output)
        else eval env (eval env (st, input, output) while_stmt) stmt
      | (st, input, output), Repeat (expr, repeat_stmt) ->
        let (st', input', output') = eval env (st, input, output) repeat_stmt in
        if Expr.eval st' expr == 0 then eval env (st', input', output') stmt
        else (st', input', output')
      | (st, input, output), Call (name, args) ->
        let args_f, locals_f, body = env#getdef name in
        let enter_st = State.enter st (args_f @ locals_f) in
        let association = List.combine args_f args in
        let st_args = List.fold_left 
                  (fun state (x,expr) -> State.update x (Expr.eval st expr) state)
                  enter_st
                  association in
        let st', input', output' = eval env (st_args, input, output) body in
        (State.leave st st'), input', output'

      | _, _ -> failwith("Undefined statement!")
                                
    (* Statement parser *)
    ostap (
     
      parse: 
      seq | stmt;
  
      stmt:
          read | write | assign | skip | repeat_stmt | while_stmt | if_stmt | for_stmt | call;

      assign: 
      variable:IDENT -":=" expr:!(Expr.parse) {Assign (variable, expr)};
    read: 
      %"read" -"(" variable:IDENT -")" {Read variable};
    write: 
      %"write" -"(" expr:!(Expr.parse) -")" {Write expr};
    skip: 
      %"skip" {Skip};
    if_stmt: 
      %"if" expr:!(Expr.parse) %"then"
      if_true:parse 
      if_false:else_stmt %"fi" {If (expr, if_true, if_false)};
    else_stmt:  
        %"else" if_false:parse {if_false} 
        | %"elif" expr:!(Expr.parse) %"then"
          if_true:parse 
          if_false:else_stmt {If (expr, if_true, if_false)}
        | "" {Skip};
    while_stmt:
      %"while" expr:!(Expr.parse) %"do"
      while_body:parse
      %"od" {While (expr, while_body)};
    repeat_stmt:
      %"repeat" 
      repeat_body:parse 
      %"until" expr:!(Expr.parse) {Repeat (expr, repeat_body)};
    for_stmt:
      %"for" init:parse -"," expr:!(Expr.parse) -"," loop:parse %"do"
      for_body:parse
      %"od" { Seq (init, While(expr, Seq (for_body, loop))) };
    call: x:IDENT "(" args:!(Util.list0)[Expr.parse] ")" {Call (x, args)};
    seq: 
      left_stmt:stmt -";" right_stmt:parse {Seq (left_stmt, right_stmt)}    
    ) 
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
        arg_type: IDENT;
        parse:
          %"fun" name:IDENT -"(" args: !(Util.list0 arg_type) -")" local_vars: (%"local" !(Util.list arg_type))?
          -"{" 
          body: !(Stmt.parse) 
          -"}" { (name, (args, (match local_vars with None -> [] | Some l -> l), body))}
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i = 
  let module Map_def = Map.Make (String) in
  let defMap = List.fold_left (fun dm ((name, _) as def) -> Map_def.add name def dm) Map_def.empty defs in
  let env = 
    object 
      method getdef name = 
        let _, second = (Map_def.find name defMap) in
        second
    end in
  let _, _, o = Stmt.eval env (State.empty, i, []) body in
  o
                                   
(* Top-level parser *)
let parse = 
  ostap (
      defs:!(Definition.parse)* main:!(Stmt.parse) { (defs, main) }
  )