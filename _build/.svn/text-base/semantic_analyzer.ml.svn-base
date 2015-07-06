
(* Author Ryan Jones and Kevin Henrick *)
(* The semantic analyzer takes in an AST and type checks each node of the tree,*)
(* and returns the correctly checked node to the semantically analyzed abstract*)
(* syntax tree (SAST). *)

open Ast
open Corelibrary

(* module StringMap = Map.Make(String) *)

(* Creating the symbol_table *)
type symbol_table = {
  parent : symbol_table option;
  functions : Ast.fdecl list; 
  variables : Ast.vdecl list;
}

let typeEq t1 t2 = match t1,t2 with
| AnytypeType, _ -> true
| _, AnytypeType -> true
| _, _ -> if (t1 = t2)
            then true
					else false

let string_of_datatype = function
      IntType -> "IntType"
    | DoubleType -> "DoubleType"
    | BoolType -> "BoolType"
    | StringType -> "StringType"
    | CardType -> "CardType"
    | ListType -> "ListType"
    | PlayerType -> "PlayerType"
    | AnytypeType -> "AnytypeType"

let string_of_expr = function
	| _ -> "(not implemented yet)"

let string_of_binop = function
	| Assign -> "Assign"
	| Plus -> "Plus"
	| Sub -> "Sub"
	| Mult -> "Mult"
	| Div -> "Div"
	| Mod -> "Mod"
	| Eq -> "Eq"
	| Neq -> "Neq"
	| Teq -> "Teq"
	| Tneq -> "Tneq"
	| Lt -> "Lt"
	| Leq -> "Leq"
	| Gt -> "Gt"
	| Geq -> "Geq"
	| And -> "And"
	| Or -> "Or"
	| Concat -> "Concat"
	| Addl -> "Addl"
	| Addr -> "Addr"
	| Dot -> "Dot"

(* DEFAULT fdt is Ast.IntType *)

(* Core functions *)


let f1 = {pdt = Ast.IntType; pname = "int"}	
let intToString = 		{ fdt = Ast.StringType; fname = "intToString"; 		formals = [f1]; fbody = []; }

let f1 = {pdt = Ast.DoubleType; pname = "double"}	
let doubleToString = 	{ fdt = Ast.StringType; fname = "doubleToString"; formals = [f1]; fbody = []; }

let f1 = {pdt = Ast.StringType; pname = "string"}
let stringToInt =			{ fdt = Ast.IntType;    fname = "stringToInt";		formals = [f1]; fbody = []; }

let f1 = {pdt = Ast.StringType; pname = "double"}
let stringToDouble =	{ fdt = Ast.DoubleType; fname = "stringToDouble";	formals = [f1]; fbody = []; }

let f1 = {pdt = Ast.CardType; pname = "valueCard"}
let value =						{ fdt = Ast.IntType;    fname = "value";					formals = [f1]; fbody = []; }

let scan =						{ fdt = Ast.StringType;    fname = "scan";				formals = [];   fbody = []; }

let f1 = {pdt = Ast.StringType; pname = "printString"}	
let print =						{ fdt = Ast.IntType;    fname = "print";					formals = [f1]; fbody = []; }

let f1 = {pdt = Ast.CardType; pname = "suitString"}
let suit =						{ fdt = Ast.StringType; fname = "suit";						formals = [f1]; fbody = []; }

let random =					{ fdt = Ast.ListType;   fname = "random";					formals = []; fbody = []; }

let f1 = {pdt = Ast.ListType; pname = "deck"}
let shuffle =					{ fdt = Ast.ListType;   fname = "shuffle";				formals = [f1]; fbody = []; }

let f1 = {pdt = Ast.PlayerType; pname = "player"}
let turn =						{ fdt = Ast.IntType;    fname = "turn";						formals = [f1]; fbody = []; }

let win =							{ fdt = Ast.IntType;    fname = "win";						formals = [];	fbody = []; }



(* Core variables *)
let standard = { vdt = Ast.ListType;    vname = "STANDARD";  value = Ast.Noexpr }
let nemo     = { vdt = Ast.PlayerType;  vname = "NEMO";      value = Ast.Noexpr }

(* Pre-defined player data fields *)
let name     = { vdt = Ast.StringType;  vname = "$PVARname";   value = Ast.Noexpr }
let turnID   = { vdt = Ast.IntType;     vname = "$PVARturnID"; value = Ast.Noexpr }

let global_scope = {
  parent = None;
	variables = [ standard; nemo; name; turnID];
	functions = [ intToString; doubleToString; 
	          stringToInt; stringToDouble; 
	          value;       suit;
						scan;        print;  
						random;      shuffle;
						turn;        win ];
}


(* Finding the variable in the scope by its name*)
let rec find_variable scope name =
  try
	  List.find (fun {vdt=_; vname=s; value=_} -> s = name) scope.variables
  with Not_found ->
		match scope.parent with
    | Some(parent) -> find_variable parent name
    | _ -> let pTrim st = ( if (String.length st) > 4 
		                          then (String.sub st 0 5)
													  else st                    )
					 in
			     if ( (pTrim name) <> "$PVAR")
		         then find_variable scope ("$PVAR"^name)
					 else
						 (* raise (Failure("Var not found in scope (TMP message)")) *)
						 let outName = if (String.length name) > 4
						                 then
															(String.sub name 5 (String.length name-5))
													 else name
						 in
             raise (Failure("Variable "^outName^" not found in scope"))
             (* find-string starts with PVAR but still not found, i.e. not a player var *)

(*
		let _ = (
			let rec getGlobalScope scope = match scope.parent with
		  | None -> scope
		  | Some(parent) -> (getGlobalScope parent)
			| 
	  let build_string tmpString nextString = tmpString^" \n"^nextString in
	  let func_names_string = List.fold_left build_string("") (List.map (fun {fdt=_; fname=n; formals=_; fbody=_} -> n ) (getGlobalScope scope).functions) in
    let num_funcs = List.length (getGlobalScope scope).functions in
	
	  let var_names_string = List.fold_left build_string("") (List.map (fun {vdt=_; vname=n; value=_} -> n ) (getGlobalScope scope).variables) in
    let num_vars = List.length (getGlobalScope scope).functions in
    raise(Failure("At end, vars found were "^((string_of_int num_vars)^var_names_string)
	               ^" \n\nand funcs found were "^((string_of_int num_funcs)^func_names_string) ))
	  ) in
*)


(* check if a function is present in global scope *)
let rec find_function scope name =
	let rec getGlobalScope scope = match scope.parent with
		| None -> scope
		| Some(parent) -> (getGlobalScope parent)
	in
	try
		List.find (fun {fdt=_; fname=s; formals=_; fbody=_} -> s = name) (getGlobalScope scope).functions
	with Not_found ->
		let build_string tmpString nextString = tmpString^" \n"^nextString in
		let func_names_string = List.fold_left build_string("") (List.map (fun {fdt=_; fname=n; formals=_; fbody=_} -> n ) (getGlobalScope scope).functions) in
		let num_funcs = List.length (getGlobalScope scope).functions in
		raise(Failure("Function "^name^" not found in global scope, funcs found were "^(string_of_int num_funcs)^func_names_string))



(* Used identifiers must be defined: use find_variable function OR find_func function *)
(* Identifier references must be variables or functions  *)
(* Function calls must refer to functions: use find_func function *)
(* Left side of assign binop must be existing variable of same type as right side. *)


let rec exprCheck scope = function

	(* pass literals right through, already validated in the scanner *)
	| Ast.Int i       -> Sast.Int(i),    Ast.IntType
	| Ast.Double d    -> Sast.Double(d), Ast.DoubleType
	| Ast.Bool b      -> Sast.Bool(b),   Ast.BoolType
	| Ast.String s    -> Sast.String(s), Ast.StringType
	| Ast.Card (i,c)  -> Sast.Card(i,c), Ast.CardType


  | Ast.CardExpr (e,c)  -> 
			let is_valid expr = match expr with
		    | Ast.Int i ->  if ( i>1 && i<15 ) then 
					                true (* value is valid *)
				                else 
													raise (Failure ("card integer value must be between 2 and 14 (inclusive), but found "^(string_of_int i)^".")	)	 
		    | Ast.Id  id -> let curVdecl = find_variable scope id in
				                if( typeEq curVdecl.vdt Ast.IntType)  (* id must refer to IntType in CardExpr*)
												  then true
												else raise (Failure ("Id in CardExpr must be type Int, found type " ^ string_of_datatype curVdecl.vdt))
				| Ast.Ele   ->  let eleType = (find_variable scope "ele").vdt (* lookup what ele currently refers to in local symbol table *)
				                in
												if ( typeEq eleType Ast.IntType)
												  then true
												else raise (Failure ("Ele must refer to an element of type Int, found type " ^ (string_of_datatype eleType)) )
		
												
				| _         ->  raise (Failure ("card expression is wrong type, found "^string_of_expr expr^" ."))
	    in 
			if ( is_valid e ) then 
        Sast.CardExpr( exprCheck scope e, c), Ast.CardType  (* SUCCESS RETURN VALUE *)
		  else raise (Failure ("card expression value or type is wrong"))
			
  | Ast.List (elements) -> Sast.List( List.map (exprCheck scope) elements ), Ast.ListType
				
  | Ast.Player (e1, e2) ->
		    let is_valid_player expr1 expr2 = match snd(exprCheck scope e1), snd(exprCheck scope e2) with
				| Ast.StringType, Ast.IntType -> true
				| _ , _ -> false
				in 
				if is_valid_player e1 e2
				  then Sast.Player(exprCheck scope e1 , exprCheck scope e2), Ast.PlayerType
	      else 
					raise(Failure("Invalid player name or turn id, e1 type: "^(string_of_datatype (snd(exprCheck scope e1)) )^" e2 type:"^(string_of_datatype (snd(exprCheck scope e2))) ))

  | Ast.Id(s) -> (* use find_variable and find_function *)
	  let newVdecl = find_variable scope s in
		Sast.Id(newVdecl.vname), newVdecl.vdt   (* look for it as a variable with find_variable *)

	| Ast.Ele -> Sast.Ele, (find_variable scope "ele").vdt
  | Ast.Your -> 
		  if( (find_variable scope "your").vname = "your") 
			  then Sast.Your, Ast.PlayerType
		  else
			  raise(Failure("Your referenced outside of turn block"))
				(* MIGHT NOT BE REACHED EVER *)
			
	| Ast.Binop (expr1, op, expr2) ->  (* taken from MicroC Type Checker*)
	      (
	      (* The types of operands for unary and binary operators must be consistent. *)
				let e1 = exprCheck scope expr1 in
				
				(* if only operator is dot, than add look for player.___ !!!!!!!!!!!!!*) 
				let e2 = exprCheck scope expr2 in
        let _, t1 = e1 (* Get the type of each child *) in
        let _, t2 = e2 in
				match t1, op, t2 with
				| (AnytypeType | BoolType), (And| Or), (AnytypeType | BoolType) ->  (* Bool *)
					Sast.Binop(e1, op, e2), Ast.BoolType
				
				| (AnytypeType | IntType), Mod, (AnytypeType | IntType) ->  (* Ints mod *)
					Sast.Binop(e1, op, e2), Ast.IntType
					
				| (AnytypeType | IntType | DoubleType), (Lt | Leq | Gt | Geq), (AnytypeType | IntType| DoubleType) ->  (* Int operations *)
					Sast.Binop(e1, op, e2), Ast.BoolType
					
				| (AnytypeType | IntType), (Plus | Sub | Mult | Div), (AnytypeType | IntType) ->  (* Ints or Floats (with above cases excluded) *)
					Sast.Binop(e1, op, e2), Ast.IntType   
					
				| (AnytypeType | IntType | DoubleType), (Plus | Sub | Mult | Div), (AnytypeType | IntType | DoubleType) ->  (* Ints or Floats *)
					Sast.Binop(e1, op, e2), Ast.DoubleType   
				
				| _, (Eq | Neq | Teq | Tneq), _  ->   (* Anything? *)
					Sast.Binop(e1, op, e2), Ast.BoolType
					
				| (StringType | AnytypeType), Concat, (StringType | AnytypeType) ->  (* Strings *)   (* DOUBLE CHECK ANYTYPE ALLOWED*)
				  Sast.Binop(e1, op, e2), Ast.StringType
					
				| (ListType|AnytypeType), Addl, (IntType | CardType | DoubleType | StringType | BoolType | ListType | PlayerType | AnytypeType) -> (* expr lists *)
				  Sast.Binop(e1, op, e2), Ast.ListType
					
				| (IntType | CardType | DoubleType | StringType | BoolType | ListType | PlayerType | AnytypeType), Addr, (ListType|AnytypeType) -> (* expr lists *)
				  Sast.Binop(e1, op, e2), Ast.ListType
							
				| PlayerType, Dot, t  -> if (typeEq t AnytypeType)
				                           then Sast.Binop(e1, op, e2), Ast.AnytypeType(* left expr is expr * expr or Id, right expr is Id *)
																 else raise(Failure("player dotted with wrong type (should never throw compile-time error?)"))
 
				| _, Assign, _ -> if (typeEq t1 t2)
				    then Sast.Binop(e1, op, e2), Ast.AnytypeType (* !?! DANGEROUS !?! --> FIX RETURN TYPE *)
					else raise(Failure("Operand types must match for Assign operator"))
					
				| tA, op, tB -> raise(Failure("Binop "^ (string_of_binop op) ^" has improper operands, found "^ (string_of_datatype tA) ^", "^ (string_of_datatype tB) ^ "\n"))
				)
		
  | Ast.Unopl (op, expr) ->
		  (
			let e1 = exprCheck scope expr in
      let _, t1 = e1 in (* Get the type of each child *)
			match op, t1 with 
		  | Not, BoolType -> Sast.Unopl(op, e1), Ast.BoolType (* Bool *)
		  | Reml, ListType -> Sast.Unopl(op, e1), Ast.AnytypeType  
      | _,_ -> raise(Failure("Invalid operand ("^string_of_datatype t1^") for left-associative unary operator"))
			)

  | Ast.Unopr (expr, op) ->
		  (
		  let e1 = exprCheck scope expr in
      let _, t1 = e1 in (* Get the type of each child *)
			match t1, op with 
		  | ListType, Remr -> Sast.Unopr(e1, op), Ast.ListType
		  | _ -> raise(Failure("Invalid operand ("^string_of_datatype t1^") for right-associative unary operator"))
			)

  | Ast.Call (string, exprList) ->
		(
		let funcFound = find_function scope string in
		let formalTypes = List.map (fun {pdt=s; pname=_} -> s) funcFound.formals in
		let checkedExprs = List.map (exprCheck scope) exprList in
		let checkedTypes = List.map snd(checkedExprs) in
		
		(* check each type from checked list against fdecl param types in scope's function list *)
		let rec typesMatch list1 list2 = match list1,list2 with
		| [], [] -> true
		| [], _ -> raise(Failure(" found parameters when expecting none "))
		| _ , [] -> raise(Failure(" found no parameters when expecting parameters"))
		| _ , _ -> 
			try
		  ( typeEq (List.hd(list1)) (List.hd(list2)) ) && typesMatch (List.tl(list1)) (List.tl(list2))
			with Failure("hd") ->
				raise(Failure(" trying List.hd on [] in exprCheck:Ast.Call"))
		in
		if (typesMatch formalTypes checkedTypes)
		  then Sast.Call (string , checkedExprs) , funcFound.fdt
	  else
			raise(Failure("Arguments for function: "^ string ^" do not match function signature"))
		)

  | Ast.Noexpr -> Sast.Noexpr, Ast.AnytypeType



(* adds player data fields to global scope so that player.x is visible in any turn block. *)
(* INS: curScope:symbol_table , prevScope:symbol_table , scopeStack:(symbol_table list) , vd:Vdecl  *)
(* OUTS: out:symbol_table*)

let empty_table =   (* empty table will be a flag to indicate the first function call *)
(*Some(symbol_table) or None  (i.e. symbol_table Option *)
	                 { parent = None;
									   functions = [];
										 variables = []; }
let rec addPvarGlobal curScope prevScope scopeStack vd = match curScope, prevScope, scopeStack with

| cur, prev, [] ->  (* empty table will be a flag to indicate the first function call *)
                    (* let _ = print_string " cur,prev,[] called in addPvarGlobal \n" in *)
									  let symNonOpt s = match s with
										  | Some(sym_tab) -> sym_tab
											| None -> ( let _ = print_string("hack should work around this (in addPvarGlobal)") in empty_table)
										in
										let prevIsEmpty prev = match prev with
										| Some(sym_tab) -> (sym_tab = empty_table)
										| _ -> false
										in
	                  (* if (prev = empty_table) (*ascending*) *)
										if (prevIsEmpty prev) (*ascending*)
                      then
												(* let _ = print_string("ascending (stack+) \n") in *)							
												let curParentNone cur = match cur with
												| Some(sym_tab) -> (sym_tab.parent = None)
												| _ -> false
												in 
												(* if ( cur.parent = None) (*started right in the global*) *)
												if ( curParentNone cur) (*started right in the global*)
                          then let newVd =
									  	    { vdt = vd.vdt;
									          vname = "$PVAR"^vd.vname;
											      value = vd.value; }
												  in
										  	  let newGlobal =    
	                        { parent = None;
									          functions = (symNonOpt cur).functions;     
										        variables = newVd::((symNonOpt cur).variables) } in
												    Some(newGlobal)
											  else (addPvarGlobal (symNonOpt cur).parent cur (cur::[]) vd)
										else (* cur.parent = prev , i.e. descending *)
										  (* let _ = print_string "final descend \n" in *)
										  cur  (* FINAL RETURN VALUE *)
											
| cur, prev, h::t ->
	                  let _ = print_string " cur,prev,h::t called in addPvarGlobal \n" in
	                  let symNonOpt s = match s with
										  | Some(sym_tab) -> sym_tab
											| None -> ( let _ = print_string("hack should work around this (in addPvarGlobal)\n") in empty_table)
										in
	                   if (symNonOpt prev).parent = cur (*ascending*) 
                       then
												 let _ = print_string( "ascending (stack+) \n") in
	                       if (symNonOpt cur).parent = None  then    (*middle state, prev is global scope*)
												   let newVd =
											       { vdt = vd.vdt;
											         vname = "$PVAR"^vd.vname;
												       value = vd.value; } in
                           let newGlobal =        
	                         { parent = None;
									           functions = (symNonOpt cur).functions;
										         variables = newVd::( (symNonOpt cur).variables); } in
									         let newCur = { parent = Some newGlobal;
									                        variables = (symNonOpt h).variables;
																		      functions = (symNonOpt h).functions; }
									         in addPvarGlobal (Some newCur) (Some newGlobal) t vd 
                         else (addPvarGlobal (symNonOpt cur).parent cur (cur::(h::t)) vd)   (*walking state*)
										 else
											 if (symNonOpt cur).parent = prev (*descending*)
												then
												let _ = print_string ("descending (stack-) \n") in
												addPvarGlobal h cur t vd
											 else
												let _ = print_string "shouldn't be reached \n" in None
	

(* The below function iteratively checks each statement in CGL by folding over an intermediate scope/statement list tuple. *)

let rec processStatement (scope,stmtList) stmt = 
	let newScope = {
	  parent =  Some scope;  (* set parent to newglobalscope parameter *)
	  functions = [];
	  variables = []; 
	} in
	match stmt with
  | Expr(e1)  -> scope, Sast.Expr( exprCheck scope e1 )::stmtList
  | Vdecl(vd) ->
		let exprType = snd(exprCheck scope vd.value) in 
		if (typeEq vd.vdt exprType)  (* TODO: Refactor and make sequential *)
      then if (try (find_variable scope vd.vname).vname = vd.vname with Failure(msg) -> false)  (* Failure is good because no duplicate declaration *)
				  then raise(Failure("Duplicate variable declaration for var:"^vd.vname))
			  else
				  let updScope = 
		      { parent = scope.parent;
	          functions = scope.functions;
			      variables = vd::(scope.variables) } in
					
					
					let updScopePvar = (
						let emptyScope = { parent = None;
									             functions = [];
										           variables = []; }
							(* serves as flag for addPvarGlobal *) in
						
						 if (try let _ = find_variable scope "IS_IN_PLAYER" in true with Failure(msg) -> false )  (* if current block is player block*)
					    then
							(* let _ = print_string("is IS_IN_PLAYER \n") in *)
							match ( addPvarGlobal (Some updScope) (Some emptyScope) [] vd ) with   (* add player variable to global scope as PVARname *)
							| Some(sym_tab) -> sym_tab
							| None -> let _ = print_string "should never be reached (in process:stmt)" in emptyScope
					  else 
							(* let _ = print_string("not IS_IN_PLAYER \n") in *)
							updScope	
						)				
					in
					
					
					
			    let retVdecl = fst( vd, exprType ) in
		      updScopePvar, Sast.Vdecl(retVdecl)::stmtList  (* !!!!!!! until updScopePvar WORKS *)
	  else raise(Failure("Assignment variable type:"^(string_of_datatype vd.vdt)^
		                   " does not match expression type:"^(string_of_datatype exprType)))

  | Block(stmts) -> scope, Sast.Block( snd( List.fold_left processStatement(newScope,[]) stmts ) )::stmtList

  | If(e, s1, s2) ->
		let ec = exprCheck scope e in
    let _, t1 = ec in
		if (typeEq t1 Ast.BoolType) then 
			let newS1, s1c = (processStatement(newScope, []) s1) in 
			let _, s2c = (processStatement (newScope, []) s2) in
			try
			newS1, Sast.If( ec, List.hd(s1c), List.hd(s2c) )::stmtList
			with Failure("hd") ->
				raise(Failure(" trying List.hd on [] in processStatement:If"))
		else raise(Failure("predicate of If must be boolean expression, found "^(string_of_datatype t1)))
		
	| While(e, s) ->
	  let ec = exprCheck scope e in
    let _, t1 = ec in
		if (typeEq t1 Ast.BoolType) then
		  let newS1, sc = (processStatement(newScope, []) s) in
			try
		  newS1, Sast.While( ec, List.hd(sc) )::stmtList
			with Failure("hd") ->
			  raise(Failure(" trying List.hd on [] in processStatement:While"))
		else raise(Failure("predicate of While must be boolean expression, found "^(string_of_datatype t1)))

  | Foreach(e, s) -> (* Includes ELE *)
	  let ec = exprCheck scope e in
    let _, t1 = ec in
		let ele = {
			vdt = Ast.AnytypeType; (* evaluate e and get type of first element, b/c e should be a list *)
			vname = "ele";
			value = Ast.Ele;  (* evaluate e and get value of first element, b/c e should be a list *)
		} in
		let eleScope =  (* eleScope extends newScope to include Ele *)
    {
	    parent = newScope.parent;  (* set parent to newglobalscope parameter *)
	    functions = newScope.functions;
	    variables = ele::newScope.variables; 
	  } in
		if (typeEq t1 Ast.ListType) then
		  let newS1, sc = (processStatement(eleScope, []) s) in
			try
		  newS1, Sast.Foreach( ec, List.hd(sc))::stmtList
		  with Failure("hd") ->
			  raise(Failure(" trying List.hd on [] in processStatement:Foreach"))
		else raise(Failure("predicate of Foreach must be ListType expression, found "^(string_of_datatype t1)))
		
  | Return(e) -> 
		let curReturnType = (find_variable scope "return").vdt  (* store return type of current function in scope *)
		in
		let ec = exprCheck scope e in
    let _, t1 = ec in
		if (typeEq t1 curReturnType )
		  then scope, Sast.Return(ec)::stmtList
		else raise(Failure("Return type of function body doesn't match return type of function signature, 
		  found"^(string_of_datatype t1)^", expected "^(string_of_datatype curReturnType)))



let processFdecl curScope fdecl =	match fdecl.fbody with
	| [] -> raise(Failure("Empty functions not allowed"))
  | x ->
		let retVdecl = {
		  vdt = fdecl.fdt;
			vname = "return";
			value = Ast.Noexpr;
		} in
		let formalToVdecl = function
			| frml -> { vdt = frml.pdt;
				          vname = frml.pname;
									value = Ast.Noexpr }
		in
	  let retScope = {
			parent = curScope.parent;
			functions = curScope.functions;
			variables = (List.map formalToVdecl (fdecl.formals) )@(retVdecl::curScope.variables);
    } in
		let checkedFdecl = 
		{
		  fdt = fdecl.fdt;
		  fname = fdecl.fname;
		  formals = fdecl.formals;
		  fbody = fst(x, (List.fold_left processStatement(retScope, []) fdecl.fbody ));   (* UGLY HACK (x) *)
		} in
		checkedFdecl


		

let processFdecls bname fdecls scope = match bname, fdecls with
| "SETUP", fdecl_list ->
	(
	  (* LOAD all funcs in list into symbol table (b/c all funcs should be able to "find" i.e. call each other) *)
	    let addFunc scope fdecl =  
				{ parent = scope.parent;
				  functions = fdecl::scope.functions;
				  variables = scope.variables }
			in
			let newGlobal = List.fold_left addFunc(scope) fdecls (* new scope containing all funcs in setup *)
		  in  
	    let subScope = {
			  parent =  Some newGlobal;  (* set parent to newglobalscope parameter *)
			  functions = [];
			  variables = []; 
		  }
		  in
		  (* MAP through each function and check *)
		  (List.map (processFdecl subScope) fdecl_list), newGlobal  (* newFdecls, newScope *)
   )
| _ , [] ->   [], scope  (* Non-"SETUP" bname should have empty fdecls list *)
| _ , _  ->   raise(Failure("Functions can only be declared in SETUP (at beginning)")) 




let processBdecl (scope, checkedBdecls) bdecl = 
		let funcsAndScope = processFdecls bdecl.bname bdecl.funcs scope in
    (* IN: bname, funcs list  --->>  OUT: func list, updated symbol table  *)
		let updScope = snd(funcsAndScope) in

		let yourVdecl = {vdt = Ast.PlayerType; vname = "your"; value = Ast.Noexpr; } in  (* dummy "value" *)
		let updScopeYour = {
		  parent = updScope.parent;
			functions = updScope.functions;
			variables = yourVdecl::updScope.variables;
		} in
		
		(* let _ = print_string ("\n\nbdecl.name is "^(bdecl.bname)^"\n") in *)
	  if bdecl.bname = "TURN"
		  then
		  let bbodyAndScope = List.fold_left processStatement(updScopeYour,[]) bdecl.bbody
		  in  (* IN: stmtList, scope, bbody   -->>   OUT: stmt list, updated symbol table *)
		  let returnBdecl = {
			bname = bdecl.bname;
			bid =   bdecl.bid;
			funcs = fst funcsAndScope;
			bbody = fst(bdecl.bbody, snd bbodyAndScope);   (* UGLY HACK (ast instead of sast) *)
		  }  in
		  fst(bbodyAndScope), returnBdecl::checkedBdecls  (* intermediate scope and bdecl list *)
			
			
		else if (bdecl.bname = "PLAYER")      (* HANDLE DIFFERENTLY FROM OTHERS *)
		  then
			let newV = {vdt = Ast.BoolType; vname="IS_IN_PLAYER"; value=Ast.Noexpr} in
			
			let flaggedUpdScope = {
				parent = updScope.parent;
				functions = updScope.functions;
				variables = ( newV::updScope.variables;) } in  (* flag is_in_player so that all player vdecls are added to global with a special string prefix *)
			
			let unFlag scopeF = let newVars = (List.filter (fun x -> (x.vname)<>"IS_IN_PLAYER") scopeF.variables) in
			                    { parent = scopeF.parent;
													  functions = scopeF.functions;
														variables = newVars } 
			in
		  
			let bbodyAndScope = List.fold_left processStatement(flaggedUpdScope,[]) bdecl.bbody
		  in  (* IN: stmtList, scope, bbody   -->>   OUT: stmt list, updated symbol table *)
			let rec isAllVdecls stmtList = match stmtList with
			| [] -> true
			| Ast.Vdecl(x)::tail -> isAllVdecls tail
			| _ -> false
			in
		  let returnBdecl = {
			bname = bdecl.bname;
			bid =   bdecl.bid;
			funcs = fst funcsAndScope;
			bbody = fst(bdecl.bbody, snd bbodyAndScope);   (* UGLY HACK (ast instead of sast) *)
		  }  in
			if isAllVdecls bdecl.bbody  (* PLAYER bbody must only have variable declarations *)
			  then (unFlag (fst(bbodyAndScope))), returnBdecl::checkedBdecls  (* intermediate scope and bdecl list *)
		  else raise(Failure("Player block must consist only of variable declarations"))
			
			
		else
		  let bbodyAndScope = List.fold_left processStatement(updScope,[]) bdecl.bbody
		  in  (* IN: stmtList, scope, bbody   -->>   OUT: stmt list, updated symbol table *)
		  let returnBdecl = {
			bname = bdecl.bname;
			bid =   bdecl.bid;
			funcs = fst funcsAndScope;
			bbody = fst(bdecl.bbody, snd bbodyAndScope);   (* UGLY HACK (ast instead of sast) *)
		  }  in
		  fst(bbodyAndScope), returnBdecl::checkedBdecls  (* intermediate scope and bdecl list *)
		




	
(* START valid next blocks for each block - - - - - - - - - - - -  - - - - - - - - - - -*)
(* remove numbers from end of TURN blocks *)
let turnTrim string =  
	if ( (String.length string > 3) && (String.sub string 0 4 = "TURN") )
	  then "TURN" 
	else string

let curValid state block = match ( turnTrim state, turnTrim block)
with ("START", "PLAYER") -> true
  | ("START", "SETUP") -> true
  | ("PLAYER", "SETUP") -> true
	| ("SETUP", "TURN") -> true
	| ("SETUP", "WIN") -> true
	| ("SETUP", "END") -> true
	| ("TURN", "TURN") -> true
	| ("TURN", "WIN") -> true
	| ("TURN", "END") -> true
	| ("WIN", "END") -> true
	| _ -> false

(* check that blocks are in a valid order *)
let rec allValid state blocks = match blocks with
		| [] -> let _ = print_string "Order check finished \n" in true
		| head::tail -> if curValid state head && allValid head tail
                   then true
	               else raise(Failure("Blocks not in correct order"))

let startState = "START"

let check_order block_list =
    let extract_name bdecl = bdecl.bname in
    let block_name_list = List.map extract_name block_list in
		allValid startState block_name_list
(* END valid next blocks for each block - - - - - - - - - - - -  - - - - - - - - - - -*)	




(* MAIN *)
(* Parameters:  Ast.Program, Symbol_Table(global)  *)
(* Output:     Sast.Program *)
let checkProgram program globalTable =
	let _ = check_order program in
	let endScope, _ = List.fold_left processBdecl(globalTable, []) program in
	
	(* endScope *)
	
	(*
	(* debug info below *)
	let rec getGlobalScope scope = match scope.parent with
		| None -> scope
		| Some(parent) -> (getGlobalScope parent)
	in
	let build_string tmpString nextString = tmpString^" \n"^nextString in
	let func_names_string = List.fold_left build_string("") (List.map (fun {fdt=_; fname=n; formals=_; fbody=_} -> n ) (getGlobalScope endScope).functions) in
  let num_funcs = List.length (getGlobalScope endScope).functions in
	
	let var_names_string = List.fold_left build_string("") (List.map (fun {vdt=_; vname=n; value=_} -> n ) (getGlobalScope endScope).variables) in
  let num_vars = List.length (getGlobalScope endScope).functions in
  let _ = print_string ("At end, funcs found were "^((string_of_int num_vars)^var_names_string)^" \n"
	               ^"and var found were "^((string_of_int num_funcs)^func_names_string) ) in
	*)
	
	endScope
	
	




		