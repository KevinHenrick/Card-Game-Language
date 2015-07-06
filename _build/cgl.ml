(* Author: Ryan Jones and Mark Micchelli *)

open Parser
open Generator
open Javaclasses
open Semantic_analyzer

type action = Semantic | Java | Classes | Execute
exception InvalidOption of string
exception WrongNumOfArguments of string

let usage_string = "Usage: ./cgl [-s|-j|-c|-e] filename.cgl"
let _ =
    let num_args = Array.length Sys.argv in
    let get_action =
        if num_args == 3 then
            match Sys.argv.(1) with
                  "-s" -> Semantic
                | "-j" -> Java
                | "-c" -> Classes
                | "-e" -> Execute
                | _ -> raise (InvalidOption(usage_string))
        else raise (WrongNumOfArguments(usage_string)) in
    let in_channel = open_in Sys.argv.(2) in
    let lexbuf = Lexing.from_channel in_channel in
    let program = Parser.program Scanner.token lexbuf in
    let main_string = Generator.string_of_main_class program in
    let player_string = Generator.string_of_player_class program in
    let card_string = Javaclasses.string_of_card_class in
    let list_string = Javaclasses.string_of_list_class in
    let execute_action = match get_action with
      Semantic ->
    (
        try
        let _ = checkProgram program global_scope in
        print_string "passed semantic checking \n"
        with Failure(msg) ->
        print_string ("didn't pass semantic checking: \n" ^ msg ^ "\n")
    )
    | Java ->    
        let create_files =
            let main_java = open_out "Main.java" in
            let player_java = open_out "Player.java" in
            let card_java = open_out "Card.java" in
            let list_java = open_out "CGLList.java" in    
            output_string main_java main_string;
            output_string player_java player_string;
            output_string card_java card_string;
            output_string list_java list_string
        in
      create_files
    | Classes -> print_string "not yet implemented\n"
    | Execute -> print_string "not yet implemented\n"
    in execute_action
