(* Author Ryan Jones and Kevin Henrick *)

open Ast

type expr_detail =
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
    | Binop of expr * Ast.binop * expr
    | Unopl of unopl * expr
    | Unopr of expr * Ast.unopr
    | Call of string * expr list
    | Noexpr      
  and expr = expr_detail * Ast.datatype



type stmt =
      Block of (stmt list) (* statement list and var list - only line changed here*)
    | Expr of expr
    | Vdecl of Ast.vdecl
    | Return of expr
    | If of expr * stmt * stmt
    | Foreach of expr * stmt
    | While of expr * stmt

type formal =
{
    pdt : Ast.datatype;
    pname : string;
}

type fdecl =
{
    fdt : Ast.datatype;
    fname : string;
    formals : Ast.formal list;
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
