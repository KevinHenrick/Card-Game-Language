(*
 * Author: Mark Micchelli
 * The generator takes in a CGL AST and converts it to a string of a Java
 * program. The two functions from here you'd want to call are
 * string_of_player_class and string_of_main_class.
 *)

open Ast
open Corelibrary

let string_of_datatype = function
      IntType -> "Integer"
    | DoubleType -> "Double"
    | BoolType -> "Boolean"
    | StringType -> "String"
    | CardType -> "Card"
    | ListType -> "CGLList"
    | PlayerType -> "Player"
    | AnytypeType -> "Object"

let string_of_binop = function
	  Assign -> "="
	| Plus -> "+"
    | Sub -> "-"
	| Mult -> "*"
	| Div -> "/"
	| Mod -> "%"
	| Eq -> "never reached"
	| Neq -> "never reached"
	| Teq -> "never reached"
	| Tneq -> "never reached"
	| Lt -> "<"
	| Leq -> "<="
	| Gt -> ">"
	| Geq -> ">="
	| And -> "&&"
	| Or -> "||"
	| Concat -> "+"
	| Addl -> "never reached"
	| Addr -> "never reached"
    | Dot -> "never reached"    

let rec string_of_expr = function
      Int(i) -> "new Integer(" ^ string_of_int i ^ ")"
    | Double(d) -> "new Double(" ^ string_of_float d ^ ")"
    | Bool(b) -> "new Boolean(" ^ string_of_bool b ^ ")"
    | String(s) -> "new String(" ^ s ^ ")"
    | Card(i, c) ->
        "new Card(" ^ string_of_int i ^ ", '" ^ Char.escaped c ^ "')"
    | CardExpr(e, c) ->
        "new Card(" ^ string_of_expr e ^ ", '" ^ Char.escaped c ^ "')"
    | List(el) ->
        "new CGLList(" ^ String.concat ", "
        (List.map string_of_expr el) ^ ")"
    | Player(e1, e2) ->
        "new Player(" ^ string_of_expr e1 ^ ", " ^
        string_of_expr e2 ^ ")"
    | Id(s) -> s
    | Ele -> "ele"
    | Your -> "your"
    | Binop(e1, o, e2) ->
	    let string_of_binop_expr e1 o e2 = match o with
          Eq -> string_of_expr e1 ^ ".equals(" ^ string_of_expr e2 ^ ")"
        | Neq -> "!" ^ string_of_expr e1 ^ ".equals(" ^ string_of_expr e2 ^ ")"
	    | Teq -> "teq(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
        | Tneq -> "!teq(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
            ^ ")"
        | Addl ->
            string_of_expr e1 ^ ".addLast(" ^ string_of_expr e2 ^ ")"
        | Addr ->
            string_of_expr e2 ^ ".addFirst(" ^ string_of_expr e1 ^ ")"
        | Dot -> string_of_expr e1 ^ "." ^ string_of_expr e2
        | _ -> string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^
               string_of_expr e2
	    in string_of_binop_expr e1 o e2
    | Unopl(o, e) ->
	    let string_of_unopl_expr o e = match o with
	      Not -> "!" ^ string_of_expr e
	    | Reml -> string_of_expr e ^ ".removeFirst()"
        in string_of_unopl_expr o e
    | Unopr(e, o) -> string_of_expr e ^ ".removeLast()"
	| Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Noexpr -> ""

let rec string_of_stmt = function
      Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
    | Expr(expr) ->
        string_of_expr expr ^ ";\n"
    | Vdecl(vdecl) ->
        string_of_datatype vdecl.vdt ^ " " ^ vdecl.vname ^ " = " ^
        string_of_expr vdecl.value ^ ";\n"
    | Return(expr) ->
        "return " ^ string_of_expr expr ^ ";\n";
    | If(e, s, Block([])) ->
        "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
    | If(e, s1, s2) ->
        "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^
        "else\n" ^ string_of_stmt s2
    | Foreach(e, s) ->
        "list = " ^ string_of_expr e ^ ".toArray();
        for (Object ele : list)
            " ^ string_of_stmt s ^ "
        "
        
    | While(e, s) ->
        "while (" ^ string_of_expr e ^ ") \n" ^ string_of_stmt s

(*
 * Appends each formal with a "$". There is no risk of variable name
 * overlap because "$" is a character that's allowed in Java identifiers,
 * but not CGL ones.
 *)
let string_of_formal formal =
     "Object " ^ formal.pname ^ "$$"

(*
 * Makes Java function declarations, but this requires a lot of care because
 * CGL is pass-by-value and Java is pass-by-reference. I get around this
 * by cloning each formal at the beginning of each function, and by
 * referring only to the cloned variables throughout the rest of the
 * function. Because each formal is appended with a "$", the cloned
 * variables can be referred to by their unaltered CGL names.
 *)
let string_of_fdecl fdecl =
    (* Clone all non-primitive formals into the original variables. *)
    let clone formal =
        let name = formal.pname in
        match formal.pdt with
          IntType ->
            "
            Integer " ^ name ^ "$ = (Integer) " ^ name ^ "$$;
            Integer " ^ name ^ " = new Integer(" ^ name ^ "$.intValue());
            "
        | DoubleType ->
            "
            Double " ^ name ^ "$ = (Double) " ^ name ^ "$$;
            Double " ^ name ^ " = new Double(" ^ name ^ "$.doubleValue());
            "
        | BoolType ->
            "
            Boolean " ^ name ^ "$ = (Boolean) " ^ name ^ "$$;
            Boolean " ^ name ^ " = new Boolean(" ^ name ^
            "$.booleanValue());
            "
        | StringType ->
            "
            String " ^ name ^ "$ = (String) " ^ name ^ "$$;
            String " ^ name ^ " = new String(" ^ name ^ "$);
            "
        | CardType ->
            "
            Card " ^ name ^ "$ = (Card) " ^ name ^ "$$;
            Card " ^ name ^ " = new Card(value(" ^ name ^ "$), suit(" ^
            name ^ "$));
            "
        | ListType ->
            "
            CGLList " ^ name ^ "$ = (CGLList) " ^ name ^ "$$;
            CGLList " ^ name ^ " = new CGLList();
            ListIterator iter$$$ = " ^ name ^ "$.listIterator(0);
            while (iter$$$.hasNext())
                " ^ name ^ ".addLast(iter$$$.next());
            "
        (* needs work -- how do you copy over other fields? *)
        | PlayerType ->
            "
            Player " ^ name ^ "$ = (Player) " ^ name ^ "$$;
            Player " ^ name ^ " = new Player(" ^ name ^ "$.name, " ^ name ^
            "$.turnID);
            "
        (* also needs work -- probably won't clone properly *)
        | AnytypeType -> "Object " ^ name ^ " = " ^ name ^ "$$;"
    in
    (* Finally, the Java code proper. *)
    "private static " ^ string_of_datatype fdecl.fdt ^ " " ^ fdecl.fname
    ^ "(" ^ String.concat ", " (List.map string_of_formal fdecl.formals) ^
    ")\n{\n" ^
    String.concat "" (List.map clone fdecl.formals) ^
    String.concat "" (List.map string_of_stmt fdecl.fbody) ^
    "}\n"

(* Creates the Player class *)
let string_of_player_class bdecl_list =
    let first_block = List.hd bdecl_list in
    (* Creates the variables at the bottom of the class *)
    let make_player_vars bdecl =
        if bdecl.bname = "PLAYER" then
            let string_of_player_var stmt = match stmt with
              Vdecl(vdecl) ->
                "public " ^ string_of_datatype vdecl.vdt ^ " " ^
                vdecl.vname ^ ";\n"
            | _ -> ""
            in
            String.concat "" (List.map string_of_player_var bdecl.bbody)
        else ""
    in
    (* Assigns values to those variables in the constructor *)
    let make_player_construct bdecl =
        if bdecl.bname = "PLAYER" then
            let string_of_player_assn stmt = match stmt with
              Vdecl(vdecl) ->
                  vdecl.vname ^ " = " ^ string_of_expr vdecl.value ^ ";\n"
            | _ -> ""
            in
            String.concat "\t" (List.map string_of_player_assn bdecl.bbody)
        else ""
    in
    "
    public class Player
    {
        public Player(String name, Integer turnID)
        {
            this.name = name;
            this.turnID = turnID;
            " ^ make_player_construct first_block ^ "
        }

        public boolean equals(Object other)
        {
            if (other instanceof Player)
            {
                Player that = (Player) other;
                boolean sameName = this.name.equals(that.name);
                boolean sameID = this.turnID.equals(that.turnID);
                return sameName && sameID;
            }
            return false;
        }

        public static Player NEMO = new Player(\"NEMO\", -1);
        public String name;
        public Integer turnID;
        " ^ make_player_vars first_block ^ "
    }
    "

(* Writes the turn() function of the core library *)
let string_of_turn_block_list block_list =
    if List.length block_list > 0 then
        let string_of_turn_block block =
            "
            if (your.turnID == " ^ string_of_int block.bid ^ ")
            {" ^
            String.concat "" (List.map string_of_stmt block.bbody) ^
            "}
            else " in
        "private static void turn(Player your)
        {
        " ^ String.concat "" (List.map string_of_turn_block block_list) ^
        "if (your.turnID < 0)
            win();
        System.exit(0);
        }
        "
    else ""

(* Writes the win() function of the core library *)
let string_of_win_block block =
    "private static void win()
    {
    " ^ String.concat "" (List.map string_of_stmt block.bbody) ^
    "
    System.exit(0);
    }
    "

(* The primary Java code generator function. *)
let make_main setup turn_list win =
    (* strips the vdecls in the main() block of their datatypes and access
     * level modifiers, because vdecls are global variables *)
    let string_of_setup_stmt stmt = match stmt with
          Vdecl(vdecl) ->
            vdecl.vname ^ " = " ^ string_of_expr vdecl.value ^ ";\n"
        | _ -> string_of_stmt stmt
    in
    (* makes the vdecls global variables *)
    let string_of_setup_vdecl stmt = match stmt with
          Vdecl(vdecl) ->
            "private static " ^ string_of_datatype vdecl.vdt ^ " " ^
            vdecl.vname ^ ";\n"
        | _ -> ""
    in

    (* converts the turn and win blocks into their corresponding strings *)
    let turn_block = string_of_turn_block_list turn_list in
    let win_block = match win.bname with
          "WIN" -> string_of_win_block win
        | _ -> "public static void win() {}" in
   
    (* creates the string of the setup block by invoking Java's main()
     * function; also accounts for other necessary parts of the program *)
    "
    import java.util.*; 
    
    public class Main {
    " ^ turn_block ^ "\n" ^ win_block ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl setup.funcs) ^
    Corelibrary.coreFunc ^ "\n" ^ 
    "
    public static void main(String[] args)
    {" ^
        Corelibrary.fillConst ^ "\n" ^
        String.concat "" (List.map string_of_setup_stmt setup.bbody) ^
    "}
    
    " ^
    Corelibrary.coreConst ^ "\n" ^
    String.concat "" (List.map string_of_setup_vdecl setup.bbody) ^ "\n" ^
    "private static Object[] list;
    }\n"

(* Figures out the PLAYER/SETUP/TURN/WIN structure of the CGL program, and
 * then calls make_main with the appropriate arguments
 *)
let string_of_main_class bdecl_list =
    let empty = { bname = ""; bid = 0; funcs = []; bbody = []; } in
    let a = Array.of_list bdecl_list in
    let length = Array.length a in
    let first = a.(0) in
    let last = a.(length - 1) in
    if first.bname = "PLAYER" then
        let setup = a.(1) in
        if last.bname = "WIN" then
            let turn_array = Array.sub a 2 (length - 3) in
            let turn_list = Array.to_list turn_array in
            make_main setup turn_list last
        else
            let turn_array = Array.sub a 2 (length - 2) in
            let turn_list = Array.to_list turn_array in
            make_main setup turn_list empty
    else
        let setup = a.(0) in
        if last.bname = "WIN" then
            let turn_array = Array.sub a 1 (length - 2) in
            let turn_list = Array.to_list turn_array in
            make_main setup turn_list last
        else 
            let turn_array = Array.sub a 1 (length - 1) in
            let turn_list = Array.to_list turn_array in
            make_main setup turn_list empty
