open OUnit2
open Project3.Lexer
open Project3.Token

let test_sanity _ = assert_equal 1 1

let test_int _ = 
    assert_equal [Tok_Int(32); EOF] (tokenize "32" 0) ~msg:"int (1)";
    assert_equal [Tok_Int(-233); EOF] (tokenize "-233" 0) ~msg:"int (2)"

let test_identifier _ = 
    assert_equal [Tok_ID("abab123"); EOF] (tokenize "abab123" 0) ~msg:"identifier (1)";
    assert_equal [Tok_Int(123); Tok_ID("abc"); EOF] (tokenize "123abc" 0) ~msg:"identifier (2)"


let test_bool _ = 
    assert_equal [Tok_Bool(true); EOF] (tokenize "true" 0) ~msg:"bool (1)";
    assert_equal [Tok_Bool(false); EOF] (tokenize "false" 0) ~msg:"bool (2)";
    assert_equal [Tok_ID("truebetter"); EOF] (tokenize "truebetter" 0) ~msg:"bool (3)"

let test_if _ = 
    assert_equal [Tok_If; EOF] (tokenize "if" 0) ~msg:"if (1)";
    assert_equal [Tok_If; Tok_ID("abld"); EOF] (tokenize "if abld" 0) ~msg:"if (2)"

let test_else _ = 
    assert_equal [Tok_Else; EOF] (tokenize "else" 0) ~msg:"else (1)";
    assert_equal [Tok_If; Tok_Else; EOF] (tokenize "if else" 0) ~msg:"else (2)" 

let test_while _ = 
    assert_equal [Tok_While; EOF] (tokenize "while" 0) ~msg:"while (1)";
    assert_equal [Tok_ID("whilesc"); Tok_While; EOF] (tokenize "whilesc while" 0) ~msg:"while (2)" 

let test_int_type _ = 
    assert_equal [Tok_Int_Type; EOF] (tokenize "int" 0) ~msg:"int_type (1)";
    assert_equal [Tok_ID("intt"); Tok_Int(123); EOF] (tokenize "intt 123" 0) ~msg:"int_type (2)" 


let test_for _ = 
    assert_equal [Tok_For; EOF] (tokenize "for" 0) ~msg:"for (1)";
    assert_equal [Tok_ID("forin"); Tok_For; EOF] (tokenize "forin for" 0) ~msg:"for (2)" 

let test_bool_type _ = 
    assert_equal [Tok_Bool_Type; EOF] (tokenize "bool" 0) ~msg:"bool_type (1)";
    assert_equal [Tok_Bool_Type; Tok_ID("boolint"); EOF] (tokenize "bool boolint" 0) ~msg:"bool_type (2)" 

let test_print _ = 
    assert_equal [Tok_Print; EOF] (tokenize "print" 0) ~msg:"print (1)";
    assert_equal [Tok_ID("printtype"); EOF] (tokenize "printtype" 0) ~msg:"print (2)" 

let test_to _ =
    assert_equal [Tok_To; EOF] (tokenize "to" 0) ~msg:"to (1)";
    assert_equal [Tok_ID("toprint"); EOF] (tokenize "toprint" 0) ~msg:"to (2)" 

let test_from _ =
    assert_equal [Tok_From; EOF] (tokenize "from" 0) ~msg:"from (1)";
    assert_equal [Tok_ID("fromfor"); EOF] (tokenize "fromfor" 0) ~msg:"from (2)" 

let test_semicolon _ =
    assert_equal [Tok_Semi; EOF] (tokenize ";" 0) ~msg:"semicolon (1)";
    assert_equal [Tok_For; Tok_Semi; EOF] (tokenize "for;" 0) ~msg:"semicolon (2)"

let test_assign _ = 
    assert_equal [Tok_Assign; EOF] (tokenize "=" 0) ~msg:"assign (1)";
    assert_equal [Tok_ID("x"); Tok_Assign; Tok_Int(123); EOF] (tokenize "x=123" 0) ~msg:"assign (2)"  

let test_equal _ = 
    assert_equal [Tok_Equal; EOF] (tokenize "==" 0) ~msg:"equal (1)";
    assert_equal [Tok_ID("x"); Tok_Equal; Tok_Int(123); EOF] (tokenize "x==123" 0) ~msg:"equal (2)"  

let test_not _ = 
    assert_equal [Tok_Not; EOF] (tokenize "!" 0) ~msg:"not (1)";
    assert_equal [Tok_Not; Tok_ID("x"); EOF] (tokenize "!x" 0) ~msg:"not (2)"  

let test_notEqual _ = 
    assert_equal [Tok_NotEqual; EOF] (tokenize "!=" 0) ~msg:"notEqual (1)";
    assert_equal [Tok_ID("x"); Tok_NotEqual; Tok_ID("y"); EOF] (tokenize "x != y" 0) ~msg:"notEqual (2)"  

let test_less _ = 
    assert_equal [Tok_Less; EOF] (tokenize "<" 0) ~msg:"less (1)";
    assert_equal [Tok_ID("x"); Tok_Less; Tok_ID("y"); EOF] (tokenize "x < y" 0) ~msg:"less (2)"  

let test_lessEqual _ = 
    assert_equal [Tok_LessEqual; EOF] (tokenize "<=" 0) ~msg:"lessEqual (1)";
    assert_equal [Tok_ID("x"); Tok_LessEqual; Tok_ID("y"); EOF] (tokenize "x <= y" 0) ~msg:"lessEqual (2)" 

let test_greater _ = 
    assert_equal [Tok_Greater; EOF] (tokenize ">" 0) ~msg:"greater (1)";
    assert_equal [Tok_ID("x"); Tok_Greater; Tok_ID("y"); EOF] (tokenize "x > y" 0) ~msg:"greater (2)"  

let test_greaterEqual _ = 
    assert_equal [Tok_GreaterEqual; EOF] (tokenize ">=" 0) ~msg:"greaterEqualEqual (1)";
    assert_equal [Tok_ID("x"); Tok_GreaterEqual; Tok_ID("y"); EOF] (tokenize "x >= y" 0) ~msg:"greaterEqualEqual (2)" 

let test_lparen _ = 
    assert_equal [Tok_LParen; EOF] (tokenize "(" 0) ~msg:"lparen (1)";
    assert_equal [Tok_LParen; Tok_ID("given"); EOF] (tokenize "(given" 0) ~msg:"lparen (2)" 

let test_rparen _ = 
    assert_equal [Tok_RParen; EOF] (tokenize ")" 0) ~msg:"rparen (1)";
    assert_equal [Tok_RParen; Tok_ID("given"); EOF] (tokenize ")given" 0) ~msg:"rparen (2)" 

let test_lbrace _ = 
    assert_equal [Tok_LBrace; EOF] (tokenize "{" 0) ~msg:"lbrace (1)";
    assert_equal [Tok_LBrace; Tok_ID("given"); EOF] (tokenize "{given" 0) ~msg:"lparen (2)" 

let test_rbrace _ = 
    assert_equal [Tok_RBrace; EOF] (tokenize "}" 0) ~msg:"rparen (1)";
    assert_equal [Tok_RBrace; Tok_ID("given"); EOF] (tokenize "}given" 0) ~msg:"rparen (2)" 

let test_orType _ =
    assert_equal [Tok_Or; EOF] (tokenize "||" 0) ~msg:"or (1)";
    assert_equal [Tok_Or; Tok_ID("brac"); EOF] (tokenize "||brac" 0) ~msg:"or (2)" 

let test_andType _ =
    assert_equal [Tok_And; EOF] (tokenize "&&" 0) ~msg:"and (1)";
    assert_equal [Tok_ID("x"); Tok_And; Tok_ID("y"); EOF] (tokenize "x&&y" 0) ~msg:"and (2)" 


let test_plus _ =
    assert_equal [Tok_Add; EOF] (tokenize "+" 0) ~msg:"add (1)";
    assert_equal [Tok_ID("x"); Tok_Add; Tok_ID("y"); EOF] (tokenize "x+y" 0) ~msg:"add (2)" 

let test_minus _ =
    assert_equal [Tok_Sub; EOF] (tokenize "-" 0) ~msg:"sub (1)";
    assert_equal [Tok_ID("x"); Tok_Sub; Tok_ID("y"); EOF] (tokenize "x - y" 0) ~msg:"sub (2)" 

let test_times _ =
    assert_equal [Tok_Mult; EOF] (tokenize "*" 0) ~msg:"mult (1)";
    assert_equal [Tok_ID("x"); Tok_Mult; Tok_ID("y"); EOF] (tokenize "x * y" 0) ~msg:"mult (2)" 

let test_div _ =
    assert_equal [Tok_Div; EOF] (tokenize "/" 0) ~msg:"div (1)";
    assert_equal [Tok_ID("x"); Tok_Div; Tok_ID("y"); EOF] (tokenize "x / y" 0) ~msg:"div (2)" 

let test_pow _ =
    assert_equal [Tok_Pow; EOF] (tokenize "^" 0) ~msg:"pow (1)";
    assert_equal [Tok_ID("x"); Tok_Pow; Tok_ID("y"); EOF] (tokenize "x ^ y" 0) ~msg:"pow (2)" 

let test_EOF _ =
    assert_equal [EOF] (tokenize "" 0) ~msg:"EOF (1)"

let suite = 
  "student" >::: [ 
    "int" >:: test_int;
    "identifier" >:: test_identifier;
    "bool" >:: test_bool;
    "if" >:: test_if;
    "else" >:: test_else;
    "while" >:: test_while;
    "int_type" >:: test_int_type;
    "for" >:: test_for;
    "bool_type" >:: test_bool_type;
    "print" >:: test_print;
    "to" >:: test_to;
    "from" >:: test_from;
    "semicolon" >:: test_semicolon;
    "assign" >:: test_assign;
    "equal" >:: test_equal;
    "not" >:: test_not;
    "notEqual" >:: test_notEqual;
    "less" >:: test_less;
    "lessEqual" >:: test_lessEqual;
    "greater" >:: test_greater;
    "greaterEqual" >:: test_greaterEqual;
    "lparen" >:: test_lparen;
    "rparen" >:: test_rparen;
    "lbrace" >:: test_lbrace;
    "rbrace" >:: test_rbrace;
    "orType" >:: test_orType;
    "andType" >:: test_andType;
    "plus" >:: test_plus;
    "minus" >:: test_minus;
    "times" >:: test_times;
    "div" >:: test_div;
    "pow" >:: test_pow;
    "EOF" >:: test_EOF;
    "sanity" >:: test_sanity 
  ]
let _ = run_test_tt_main suite
