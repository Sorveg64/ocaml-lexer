(* Author: Keith Groves
 * Major: Computer Science
 * creation date: 3/23/2026
 * Due date: 3/30/2026
 * Course: CSC 310 010
 * Professor: Dr. Dylan Schwesinger
 * Assignment: Project 3
 * Filename: token.ml
 * Purpose: This program provides a token type 
 * and a string repressentation for every expression 
 * of a given language *)

exception InvalidInputException of string

type token =
  | Tok_For
  | Tok_From
  | Tok_To
  | Tok_While
  | Tok_Int_Type
  | Tok_Bool_Type
  | Tok_Sub
  | Tok_Semi
  | Tok_RParen
  | Tok_RBrace
  | Tok_Print
  | Tok_Pow
  | Tok_Add
  | Tok_Or
  | Tok_NotEqual
  | Tok_Not
  | Tok_Mult
  | Tok_LessEqual
  | Tok_Less
  | Tok_LParen
  | Tok_LBrace
  | Tok_Int of int
  | Tok_If
  | Tok_ID of string
  | Tok_GreaterEqual
  | Tok_Greater
  | Tok_Equal
  | Tok_Else
  | Tok_Div
  | Tok_Bool of bool
  | Tok_Assign
  | Tok_And
  | EOF

(* Function Name: string_of_token
 * Description: converts a token into a string 
 * Parameters: token t: the token being converted - input
 * Return value: string - the string form of the token *)

let string_of_token (t : token) : string =
  match t with
  | Tok_For -> "Tok_For"
  | Tok_From -> "Tok_From"
  | Tok_To -> "Tok_To"
  | Tok_While -> "Tok_While"
  | Tok_Int_Type -> "Tok_Int_Type"
  | Tok_Bool_Type -> "Tok_Bool_Type"
  | Tok_Sub -> "Tok_Sub"
  | Tok_Semi -> "Tok_Semi"
  | Tok_RParen -> "Tok_RParen"
  | Tok_RBrace -> "Tok_RBrace"
  | Tok_Print -> "Tok_Print"
  | Tok_Pow -> "Tok_Pow"
  | Tok_Add -> "Tok_Add"
  | Tok_Or -> "Tok_Or"
  | Tok_NotEqual -> "Tok_NotEqual"
  | Tok_Not -> "Tok_Not"
  | Tok_Mult -> "Tok_Mult"
  | Tok_LessEqual -> "Tok_LessEqual"
  | Tok_Less -> "Tok_Less"
  | Tok_LParen -> "Tok_LParen"
  | Tok_LBrace -> "Tok_LBrace"
  | Tok_Int i -> "Tok_Int(" ^ string_of_int i ^ ")"
  | Tok_If -> "Tok_If"
  | Tok_ID id -> "Tok_ID(\"" ^ id ^ "\")"
  | Tok_GreaterEqual -> "Tok_GreaterEqual"
  | Tok_Greater -> "Tok_Greater"
  | Tok_Equal -> "Tok_Equal"
  | Tok_Else -> "Tok_Else"
  | Tok_Div -> "Tok_Div"
  | Tok_Bool b -> "Tok_Bool(" ^ string_of_bool b ^ ")"
  | Tok_Assign -> "Tok_Assign"
  | Tok_And -> "Tok_And"
  | EOF -> "EOF"

