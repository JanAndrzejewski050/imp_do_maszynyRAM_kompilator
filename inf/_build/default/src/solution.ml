open Ast

let rec find_on_stack (stack : var list) (x : var) (acc : int) : int = 
  match stack with
  | [] -> failwith "value is not on the stack"
  | s :: stack' -> if s = x then acc else find_on_stack stack' x (acc + 1)

let rec compile_var (vs : var list) : cmd list = 
  match vs with
  | [] -> []
  | _ :: vs' -> READ :: (PUSH :: compile_var vs')

let rec compile_aexp (e : aexp) (stack : var list) : cmd list= 
  match e with
  | Int n -> [CONST n]
  | Var x -> TOP :: [LOAD(find_on_stack stack x 0)]
  | Binop(op, e1, e2) -> compile_aexp e1 stack @ [PUSH] @ compile_aexp e2 ("none" :: stack) @ [PRIM op]
  | _ -> failwith "Not implementet Call"

let compile_bexp (e : bexp) (stack : var list) : cmd list =
  match e with
  | Bool b -> if b then [CONST 1] else [CONST 0]
  | Cmp (op, a_e1, a_e2) -> compile_aexp a_e1 stack @ [PUSH] @ compile_aexp a_e2 ("none" :: stack) @ [CMP op]
  | _ -> []

let rec compile (stmt : stmt) (stack : var list) : cmd list = 
  match stmt with
  | Block ss ->
    List.fold_left (fun acc s -> acc @ compile s stack) [] ss
  | Write aexp -> compile_aexp aexp stack @ [WRITE]
  | If (bexp, stmt1, stmt2) -> compile_bexp bexp stack @ [BRANCH (compile stmt1 stack, compile stmt2 stack)]
  | While (bexp, stmt0) -> [WHILE (compile_bexp bexp stack, compile stmt0 stack)]  
  | _ -> []

let compile_prog (p : prog) : vm_prog =
  let stack, _, stmt = p  in
  compile_var stack @ compile stmt (List.rev stack), []