# lexer Implementation in OCaml
## Overview: 
A lexer for a C-like language that takes a program's source code and turns each element into a corresponding token

## Features
- Takes source code and converts it into a list of tokens.
- Supports keywords, operators, integers, identifiers, booleans, parentheses, and braces.
- Skips whitespace, tabs, and newlines during tokenization.
- Finds the longest substring matched to a given regular expression to prevent it from being assigned the wrong token.
- Uses regular expressions to match substrings to their appropriate tokens.

## How to run
- Run the program with a source file
- Ex: *dune exec bin/main.exe examples/assign1.txt*

## example run
- **input:**
```
int a = 123
```
- **output:**
```
  Tok_Int_Type
  Tok_ID ("a")
  Tok_Assign
  Tok_Int (123)
  EOF
```

## Acknowledgments
- token.ml, main.ml, and the project template were provided by Dylan Schwesinger
- lexer.ml and student.ml were written independently
