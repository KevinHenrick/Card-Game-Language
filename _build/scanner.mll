{
(* Author: Mark Micchelli *)
open Parser

let card_of_string card =
    let int_of_value value =
         if value = 'A' then 14 else
         if value = 'K' then 13 else
         if value = 'Q' then 12 else
         if value = 'J' then 11 else
         if value = '*' then 0 else
             let valueStr = Char.escaped value in
         int_of_string valueStr in
    if String.length card = 3 then (int_of_value card.[1], card.[2])
    else (10, card.[3])
}

(* useful subsections of lits and ids *)
let letter = ['a'-'z' 'A'-'Z'] 
let digit = ['0'-'9']
let punc = ['~' '`' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '+' '='
            ',' '.' '?' '/' '<' '>' ':' ''' ';' '{' '}' '[' ']' '|' ' ']
let escape = ("\\n" | "\\t" | "\\\"" | "\\\\")
let value = ('2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | "10" |
             'J' | 'Q' | 'K' | 'A' | '*')
let suit = ['C' 'D' 'H' 'S' '*']
let cardExprPt1 = '$' '('
let cardExprPt2 = ')' suit

(* regular expressions for identifiers *)
let identifier = (letter) (letter | digit | '_')*

(* regular expressions for 6 of our 7 main data types;
 * lists are not included because they are recursive and
 * thus not definable by a regular expression *)
let intLit = ('-')? (digit)+
let doubleLit = ('-')? (digit)+ '.' (digit)*
let boolLit = ("true" | "false")
let stringLit = '"' (letter|digit|punc|escape)* '"'
let cardLit = '$' value suit

rule token = parse

['\n' '\r' '\t' ' '] { token lexbuf }
| "^M"     { token lexbuf }
| "/*"     { comment lexbuf }

(* Assignment Operator -- LRM 2.2.1 *)
| '='      { ASSIGN }

(* Arithmetic Operators -- LRM 2.2.2 *)
| '+'      { PLUS }
| '-'      { SUB }
| '*'      { MULT }
| '/'      { DIV }
| '%'      { MOD }

(* Relational Operators -- LRM 2.2.3
(GT and LT also used for player -- LRM 1.7 *)
| ">="     { GEQ }
| "<="     { LEQ }
| ">"      { GT }
| '<'      { LT }
| "=="     { EQ }
| "!="     { NEQ }
| "==="    { TEQ }
| "!=="    { TNEQ  }

(* Boolean Operators -- LRM 2.2.4 *)
| "!"      { NOT }
| "&&"     { AND }
| "||"     { OR  }

(* String Operator -- LRM 2.2.5 *)
| '^'      { CONCAT }

(* List Operators -- LRM 2.2.6 *)
| "+>"     { ADDR }
| "<+"     { ADDL }     
| "<-"     { REML }
| "->"     { REMR }

(* Punctuators -- LRM 2.3 *)
| ';'      { SEMI }
| '('      { LPAREN }           
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ','      { COMMA }

(* Card Stuff -- LRM 1.5 *)
| cardExprPt1 { DOLLAR }
| cardExprPt2 as lxm { SUIT(lxm.[1])  }

(* List Stuff -- LRM 1.6 *)
| '['      { LBRACK }
| ']'      { RBRACK }

(* Player Stuff -- LRM 1.7 *)
| '.'      { DOT }

(* Preprocessing -- LRM 2.5 *)
| "#include" { INCLUDE }

(* Keywords -- LRM 2.6 *)
| "anytype" { TYPE(Ast.AnytypeType) }
| "bool"    { TYPE(Ast.BoolType) }
| "card"    { TYPE(Ast.CardType) }
| "def"     { DEF }
| "double"  { TYPE(Ast.DoubleType) }
| "ele"     { ELE }
| "else"    { ELSE }
| "foreach" { FOREACH }
| "if"      { IF }
| "int"     { TYPE(Ast.IntType) }
| "list"    { TYPE(Ast.ListType) }
| "player"  { TYPE(Ast.PlayerType) }
| "PLAYER"  { PLAYER }
| "return"  { RETURN }
| "SETUP"   { SETUP }
| "string"  { TYPE(Ast.StringType) }
| "TURN"    { TURN }
| "while"   { WHILE }
| "WIN"     { WIN  }
| "your"    { YOUR }

(* Literals -- LRM 1 *)
| intLit    as lxm { INT(int_of_string lxm) }
| doubleLit as lxm { DOUBLE(float_of_string lxm) }
| boolLit   as lxm { BOOL(bool_of_string lxm) }
| stringLit as lxm { STRING(lxm) }
| cardLit   as lxm { CARD(card_of_string lxm) }

(* Identifiers -- LRM 2.1 *)
| identifier as lxm { ID(lxm) }

| eof { EOF }
| _ as char { raise (Failure("illegal character: " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
