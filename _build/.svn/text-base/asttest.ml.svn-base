type datatype = IntType | DoubleType | BoolType | StringType | CardType |
                ListType | PlayerType | AnytypeType

type binop = Plus | Sub | Mult | Div | Mod | Eq | Neq | Teq | Tneq |
             Lt | Leq | Gt | Geq | And | Or | Concat | Addl | Addr | Dot
type unopl = Not | Reml
type unopr = Remr

type expr =
      Int of int
    | Double of float
    | Bool of bool
    | String of string
    | Card of int * char
    | CardExpr of expr * char
    | List of expr list
    | Player of expr * expr
    | Id of string
    | Ele
    | Your
    | Assign of string * expr
    | Binop of expr * binop * expr
    | Unopl of unopl * expr
    | Unopr of expr * unopr
    | Call of string * expr list
    | Noexpr

type stmt =
      Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | Foreach of expr * stmt
    | While of expr * stmt

type formal =
{
    pdt : datatype;
    pname : string;
}

type vdecl =
{
    vdt : datatype;
    vname : string;
    value : expr;
}

type fdecl =
{
    fdt : datatype;
    fname : string;
    formals : formal list;
    fvars : vdecl list;
    fbody : stmt list;
}

type bdecl =
{
    bname : string;
    bid : int;
    funcs : fdecl list;
    bvars : vdecl list;
    bbody : stmt list;
}

type program = bdecl list

(*
let string_of_binop = function
      Plus -> "+"
	| Sub -> "-"
	| Mult -> "*"
	| Div -> "/"
	| Mod -> "%"
	| Eq -> "=="
	| Neq -> "!="
	| Teq -> "==="
	| Tneq -> "!=="
	| Lt -> "<"
	| Leq -> "<="
	| Gt -> ">"
	| Geq -> ">="
	| And -> "&&"
	| Or -> "||"
    | Concat -> "^"
	| Addl -> "<+"
    | Addr -> "+>"

let string_of_unopl = function
      Not -> "!"
    | Reml -> "<-"

let string_of_unopr = function
    Remr -> "->"

let rec string_of_expr = function
      Lit(str, dt) ->
        str
    | CardVar(e, str) ->
        str
    | ListVar(l) ->
        ""
    | PlayerVar(e1, e2) ->
        ""
    | Id(s) ->
        s
    | Assign(var, e) ->
		var ^ " = " ^ string_of_expr e
    | Binop(e1, o, e2) ->
		string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^
        string_of_expr e2
    | Unopl(o, e) ->
        string_of_unopl o ^ " " ^ string_of_expr e
    | Unopr(e, o) ->
        string_of_expr e ^ " " ^ string_of_unopr o
	| Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Noexpr -> ""

let rec string_of_stmt = function
      Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
    | Expr(expr) ->
        string_of_expr expr ^ ";\n";
    | Return(expr) ->
        "return " ^ string_of_expr expr ^ ";\n";
    | If(e, s, Block([])) ->
        "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
    | If(e, s1, s2) ->
        "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^
        "else\n" ^ string_of_stmt s2
    | Foreach(e, s) ->
        "foreach (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
    | While(e, s) ->
        "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl vdecl = match vdecl.vdt with
      IntType ->    "int "    ^ vdecl.vname ^ ";\n"
    | DoubleType -> "double " ^ vdecl.vname ^ ";\n"
    | BoolType ->   "bool "   ^ vdecl.vname ^ ";\n"
    | StringType -> "string " ^ vdecl.vname ^ ";\n"
    | CardType ->   "card "   ^ vdecl.vname ^ ";\n"
    | ListType ->   "list "   ^ vdecl.vname ^  ";\n"
    | PlayerType -> "player " ^ vdecl.vname ^ ";\n"
    | AnytypeType -> "BADBADBADBADBADBAD"

let string_of_formal formal = ""

let string_of_fdecl fdecl =
    fdecl.fname ^
    "(" ^ String.concat ", " (List.map string_of_formal fdecl.formals) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.fvars) ^
    String.concat "" (List.map string_of_stmt fdecl.fbody) ^
    "}\n"
*)
