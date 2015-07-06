%{
(* Author: Mark Micchelli *)
open Ast
%}

%token ASSIGN
%token PLUS SUB MULT DIV MOD
%token GEQ LEQ GT LT EQ NEQ TEQ TNEQ
%token AND OR NOT
%token CONCAT
%token ADDR ADDL REMR REML
%token DOT
%token SEMI COMMA LPAREN RPAREN LBRACE RBRACE
%token DOLLAR LBRACK RBRACK
%token INCLUDE
%token DEF ELSE ELE FOREACH IF PLAYER RETURN SETUP TURN WHILE WIN YOUR
%token <Ast.datatype> TYPE
%token <int> INT
%token <float> DOUBLE
%token <bool> BOOL
%token <string> STRING
%token <int * char> CARD
%token <char> SUIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%right ADDL
%left ADDR
%right REML
%left REMR
%left OR
%left AND
%left TEQ TNEQ
%left EQ NEQ
%left LT GT LEQ GEQ
%left CONCAT
%left PLUS SUB
%left MULT DIV MOD
%right NOT
%right DOT

%start program
%type <Ast.program> program

%%

/* program is a bdecl list */
program:
    bdecl_list { List.rev $1 }

bdecl_list:
      /* nothing */ { [] }
    | bdecl_list bdecl {$2 :: $1 }

bdecl:
      PLAYER LBRACE stmt_list RBRACE
        { { bname = "PLAYER"; bid = -1;
        funcs = []; bbody = List.rev $3; } }
    | SETUP LBRACE fdecl_list stmt_list RBRACE
        { { bname = "SETUP"; bid = -1; funcs = List.rev $3;
        bbody = List.rev $4; } }
    | TURN INT LBRACE fdecl_list stmt_list RBRACE
        { { bname = "TURN"; bid = $2; funcs = List.rev $4;
        bbody = List.rev $5; } }
    | WIN LBRACE fdecl_list stmt_list RBRACE
        { { bname = "WIN"; bid = -1; funcs = List.rev $3;
        bbody = List.rev $4; } }

fdecl_list:
      /* nothing */    { [] }
    | fdecl_list fdecl { $2 :: $1 }

fdecl:
    DEF TYPE ID LPAREN formal_opt RPAREN LBRACE stmt_list RBRACE
    { { fdt = $2;
	    fname = $3;
	    formals = $5;
	    fbody = List.rev $8; }
    }  

formal_opt:
      /* nothing */ { [] }
    | formal_list { List.rev $1 }

formal_list:
      formal { [$1] }
    | formal_list COMMA formal { ($3 :: $1) }

formal:
    TYPE ID { { pdt = $1; pname = $2; } }

vdecl:
    TYPE ID ASSIGN expr SEMI
    { { vdt = $1;
        vname = $2;
        value = $4; }
    }

stmt_list:
      /* nothing */  { [] }
    | stmt_list stmt { $2 :: $1 }

stmt:
      expr SEMI { Expr($1) }
    | RETURN expr SEMI { Return($2) }
    | vdecl { Vdecl($1) }
    | LBRACE stmt_list RBRACE { Block(List.rev $2) }
    | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
    | FOREACH LPAREN expr RPAREN stmt { Foreach($3, $5) }
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
      /* nothing */ { [] }
    | expr_list { List.rev $1 }

expr_list:
      expr { [$1] }
    | expr_list COMMA expr { $3 :: $1 }

expr:
      INT	           { Int($1) }
    | DOUBLE           { Double($1) }
    | BOOL             { Bool($1) }
    | STRING           { String($1) }
    | CARD             { Card(fst $1, snd $1) }
    | DOLLAR expr SUIT { CardExpr($2, $3) }
    | LBRACK expr_opt RBRACK { List($2) }
    | LT expr COMMA expr GT  { Player($2, $4) }
    | ID               { Id($1) }
    | ELE              { Ele }
    | YOUR             { Your }
    | expr ASSIGN expr { Binop($1, Assign, $3) }
    | expr PLUS   expr { Binop($1, Plus,   $3) }
    | expr SUB    expr { Binop($1, Sub,    $3) }
    | expr MULT   expr { Binop($1, Mult,   $3) }
    | expr DIV    expr { Binop($1, Div,    $3) }
    | expr MOD    expr { Binop($1, Mod,    $3) }
    | expr EQ     expr { Binop($1, Eq,     $3) }
    | expr NEQ    expr { Binop($1, Neq,    $3) }
    | expr TEQ    expr { Binop($1, Teq,    $3) }
    | expr TNEQ   expr { Binop($1, Tneq,   $3) }
    | expr LT     expr { Binop($1, Lt,     $3) }
    | expr LEQ    expr { Binop($1, Leq,    $3) }
    | expr GT     expr { Binop($1, Gt,     $3) }
    | expr GEQ    expr { Binop($1, Geq,    $3) }
    | expr AND    expr { Binop($1, And,    $3) }
    | expr OR     expr { Binop($1, Or,     $3) }
    | expr CONCAT expr { Binop($1, Concat, $3) }
    | expr ADDL   expr { Binop($1, Addl,   $3) }
    | expr ADDR   expr { Binop($1, Addr,   $3) }
    | expr DOT    expr { Binop($1, Dot,    $3) }
    | NOT  expr        { Unopl(Not,  $2) }
    | REML expr        { Unopl(Reml, $2) }
    | expr REMR        { Unopr($1, Remr) }
    | ID LPAREN expr_opt RPAREN { Call($1, $3) }
    | LPAREN expr RPAREN { $2 }
