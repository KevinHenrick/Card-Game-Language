(* Author: Mark Micchelli *)

type datatype = IntType | DoubleType | BoolType | StringType | CardType |
    ListType | PlayerType | AnytypeType

type binop = Assign | Plus | Sub | Mult | Div | Mod | Eq | Neq | Teq |
    Tneq | Lt | Leq | Gt | Geq | And | Or | Concat | Addl | Addr | Dot
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
    | Binop of expr * binop * expr
    | Unopl of unopl * expr
    | Unopr of expr * unopr
    | Call of string * expr list
    | Noexpr      


type vdecl =
{
    vdt : datatype;
    vname : string;
    value : expr;
}

type stmt =
      Block of stmt list
    | Expr of expr
    | Vdecl of vdecl
    | Return of expr
    | If of expr * stmt * stmt
    | Foreach of expr * stmt
    | While of expr * stmt

type formal =
{
    pdt : datatype;
    pname : string;
}

type fdecl =
{
    fdt : datatype;
    fname : string;
    formals : formal list;
    fbody : stmt list;
}

type bdecl =
{
    bname : string;
    bid : int;
    funcs : fdecl list;
    bbody : stmt list;
}

type program = bdecl list
