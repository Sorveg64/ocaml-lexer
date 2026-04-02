(* Author: Keith Groves
 * Major: Computer Science
 * creation date: 3/23/2026
 * Due date: 3/30/2026
 * Course: CSC 310 010
 * Professor: Dr. Dylan Schwesinger
 * Assignment: Project 3
 * Filename: main.ml
 * Purpose: This program converts the source text of a 
 * program into a series of tokens and outputs 
 * them to the terminal. *)

open Project3.Lexer
open Project3.Token

(* Function Name: print_usage
 * Description: prints a statement to the console.
 * Parameters: none
 * Return value: none *)

let print_usage () : unit =
  print_string "USAGE: <filename>\n"

(* Function Name: string_of_list
 * Description: maps a function to every element in a list, 
 * placing each element on a new line 
 * Parameters: 'a -> string f: the function being applied - input
 *             'a list lst: the list being maped onto
 * Return value: string - the string form of the list, 
 * with each element on ist own line *)

let string_of_list (f : 'a -> string) (lst : 'a list) : string =
  String.concat "\n" (List.map f lst)

let () =
  if Array.length Sys.argv <> 2 then 
    print_usage ()
  else 
    try
      let fp = open_in Sys.argv.(1) in
      let all = In_channel.input_all fp in
      let toks = tokenize all 0 in
      print_endline (string_of_list string_of_token toks);
      flush stdout;
      close_in fp
    with _ ->
      print_endline "something went wrong"
