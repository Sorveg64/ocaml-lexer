(* Author: Keith Groves
 * Major: Computer Science
 * creation date: 3/23/2026
 * Due date: 3/30/2026
 * Course: CSC 310 010
 * Professor: Dr. Dylan Schwesinger
 * Assignment: Project 3
 * Filename: lexer.ml
 * Purpose: This program provides the implementation 
 * of a lexer for a standard C like language *)

open Token
open Str

let integer = regexp "-?[0-9]+"
let identifier = regexp "[a-zA-Z][a-zA-Z0-9]*"
let boolean = regexp "\\(true\\|false\\)"
let keyword = regexp "(\\if|\\else|\\while|\\int|\\for|\\bool|\\print|\\from|\\to)"
let whitespace = regexp "[ \n\t]+"
let semicolon = regexp ";"
let assign = regexp "=" 
let equal = regexp "==" 
let not = regexp "!" 
let notEqual = regexp "!="
let less = regexp "<" 
let lessEqual = regexp "<="
let greater = regexp ">" 
let greaterEqual = regexp ">="
let lparen = regexp "("
let rparen = regexp ")"
let lbrace = regexp "{"
let rbrace = regexp "}"
let orType = regexp "||"
let andType = regexp "&&"
let plus = regexp "+"
let minus = regexp "-"
let times = regexp "*"
let div = regexp "/"
let pow = regexp "\\^"
let regList = [integer; identifier; boolean; keyword; whitespace; semicolon; assign; equal; not; notEqual; less; lessEqual; greater; greaterEqual; lparen; rparen; lbrace; rbrace; orType; andType; plus; minus; times; div; pow]

(* Function Name: findString
 * Description: returns the last substring of the input string that was
 * matched to the regular expression.
 * Parameters: string input: string being matched - input
 *             regexp reg: the regular expression being matched against - input
 *             int index: the starting position of the input string - input
 * Return value: string - the last substring matched to the regexp *)

let findString (input : string) (reg : regexp) (index : int) : string =  
  if string_match reg input index then
    matched_string input
  else 
    "Not_found"

(* Function Name: findGreatest
 * Description: returns the string with the greatest length out of a
 * list of string.
 * Parameters: string list elems: the list of strings being evaluated - input
 * Return value: string - the string with the greatest length *)

let findGreatest (elems : string list) : string = 
  List.fold_left (fun acc x -> 
    if String.length x > String.length acc && x <> "Not_found" then
      x
    else 
      acc
  ) "" elems

(* Function Name: boolString
 * Description: returns a boolean expression that matches the inputed string.
 * Parameters: string input: the string being evaluated - input
 * Return value: boolean - the bool matching the string *)

let boolString (input : string) : bool = 
  if input = "true" then 
    true
  else 
    false

(* Function Name: findToken
 * Description: returns a token that matches a given string.
 * Parameters: string input: the string being converted into a token
 * Return value: token - token that matches the string *)

let findToken (input : string) : token = 
  if input = "int" then 
    Tok_Int_Type
  else if input = "else" then
    Tok_Else
  else if input = "while" then
    Tok_While
  else if input = "if" then 
    Tok_If
  else if input = "bool" then 
    Tok_Bool_Type
  else if input = "print" then
    Tok_Print
  else if input = "for" then 
    Tok_For
  else if input = "from" then
    Tok_From
  else if input = "to" then
    Tok_To
  else if input = "=" then 
    Tok_Assign
  else if input = "==" then
    Tok_Equal
  else if input = "!" then
    Tok_Not
  else if input = "!=" then 
    Tok_NotEqual
  else if input = "<" then
    Tok_Less
  else if input = "<=" then
    Tok_LessEqual
  else if input = ">" then
    Tok_Greater
  else if input = ">=" then
    Tok_GreaterEqual
  else if input = ";" then
    Tok_Semi
  else if input = "(" then
    Tok_LParen
  else if input = ")" then
    Tok_RParen
  else if input = "{" then
    Tok_LBrace
  else if input = "}" then
    Tok_RBrace
  else if input = "||" then
    Tok_Or
  else if input = "&&" then
    Tok_And
  else if input = "+" then
    Tok_Add
  else if input = "-" then
    Tok_Sub
  else if input = "*" then
    Tok_Mult
  else if input = "/" then
    Tok_Div
  else if input = "^" then
    Tok_Pow
  else if input = "true" || input = "false" then
    Tok_Bool (boolString input)
  else if string_match identifier input 0 then
    Tok_ID input
  else if string_match integer input 0 then
    Tok_Int (int_of_string input)
  else 
    raise (InvalidInputException "Invald token")
    
        
(* Function Name: tokenize
 * Description: processes a string into a list of tokens
 * Parameters: string input: the string being processed
 *             int index: the starting position of string
 * Return value: token list - a list of tokens *)

let rec tokenize (input : string) (indx : int) : token list =
  let matchList = List.map(fun x -> findString input x indx) regList in
  let currentString = findGreatest matchList in 
  if string_match whitespace input indx then
    tokenize input (indx + (String.length currentString))
  else if indx < String.length input then
    (findToken currentString)::(tokenize input (indx + (String.length currentString))) 
  else
    EOF::[]






