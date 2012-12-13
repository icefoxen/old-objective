(* lexer.mll
   A lexer for Objective.  Ocamllex.  Good stuff.

   XXX: Write parsechr and parsestr!

   Simon Heath
   18/2/2004
*)

{

open Parser
open ErrorReport
exception Eof
exception Lexer_error

let inComment = ref 0;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  let c = (gs lb) in
  (*
  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb);
  *)
  chrNum := !chrNum + (String.length (Lexing.lexeme lb));;

(* Parses a binary string and returns an int *)
let b2i str =
   let res = ref 0.0
   and p = ref 0.0 in
   for i = String.length( str ) - 1 downto 0 do
      if (String.get str i) = '1' then
         res := !res +. (2.0 ** !p)
      else ();
      p := !p +. 1.
   done;
   Int64.of_float !res;;

let parseChr str =
  if str.[1] = '\\' then
    match str.[2] with
	'0' -> char_of_int (int_of_string (String.sub str 2 ((String.length str) - 3)))
      | 'n' -> '\n'
      | 'b' -> '\b'
      | 'r' -> '\r'
      | 't' -> '\t'
      | '\'' -> '\''
      | '\\' -> '\\'
      | _ -> (error ("Invalid char escape: " ^ str )); (raise Lexer_error)
  else
    str.[1]
;;

(* Grar.  Wiggy.  *)
let parseChrFromString str =
  if str.[0] = '\\' then
    match str.[1] with
	'0' -> char_of_int (int_of_string (String.sub str 2 ((String.length str) - 3)))
      | 'n' -> '\n'
      | 'b' -> '\b'
      | 'r' -> '\r'
      | 't' -> '\t'
      | '\'' -> '\''
      | '\\' -> '\\'
      | _ -> (error ("Invalid char escape: " ^ str )); (raise Lexer_error)
  else
    str.[0]
;;

let string_of_char c =
  String.make 1 c;;  

(* Massively inefficient, but hopefully it'll work.  *)
(* XXX: Not quite right with parseChr... *)
let parseStr str =
  let s = ref "" in
    for x = 0 to (String.length str) - 1 do
      if str.[x] = '\\' then
	s := !s ^ 
	(string_of_char 
	   (parseChrFromString 
	      (String.sub str x (String.length str - x))))
      else
         s := !s ^ string_of_char( str.[x] )
    done;
    !s
;;



let str2int x =
   Scanf.sscanf x "%i" (fun x -> x)
;;


}


let id = 
  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+|"0o"['0'-'7']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*

let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")


rule token = parse
   ' '                  { adv lexbuf; token lexbuf } (* Skip blanks *)
 | '\t'			{ print_endline "ERROR: Tabs are illegal!"; raise Lexer_error }
 | inum			{ adv lexbuf; INT( Int64.of_string (gs lexbuf) ) }
 | fnum			{ adv lexbuf; FLOAT( float_of_string (gs lexbuf) ) }
 | bnum			{ adv lexbuf; INT( b2i (gs lexbuf) ) }
 | chr                  { adv lexbuf; let n = parseChr (gs lexbuf) in
				CHAR( n ) }
 | "_INDENT"            { adv lexbuf; INDENT }
 | "_DEDENT"            { adv lexbuf; DEDENT }
 | '\n'                { nl (gs lexbuf); token lexbuf }
 | '\r'                 { token lexbuf }
 | ";"                  { adv lexbuf; ENDSTM }
 | "<-"                 { adv lexbuf; ASSIGN }
 | "<-+"                { adv lexbuf; ASSIGNADD }
 | "<--"                { adv lexbuf; ASSIGNSUB }
 | "<-*"                { adv lexbuf; ASSIGNMUL }
 | "<-/"                { adv lexbuf; ASSIGNDIV }
 | "<-%"                { adv lexbuf; ASSIGNMOD }
 | ">="                 { adv lexbuf; GTE }
 | "<="                 { adv lexbuf; LTE }
 | ">"                  { adv lexbuf; GT }
 | "<"                  { adv lexbuf; LT }
 | "="                  { adv lexbuf; EQ }
 | "/="                 { adv lexbuf; NEQ }
 | "=="                 { adv lexbuf; SEQ }
 | "/=="                { adv lexbuf; SNEQ }
 | "{|"			{ adv lexbuf; BLOCKSTART }
 | "|}"			{ adv lexbuf; BLOCKEND }
 | "("                  { adv lexbuf; LPAREN }
 | ")"                  { adv lexbuf; RPAREN }
 | "::"			{ adv lexbuf; PKGREF }
 | ":"                  { adv lexbuf; COLON }
 | "["                  { adv lexbuf; LBRACK }
 | "]"                  { adv lexbuf; RBRACK }
 | "{"                  { adv lexbuf; LBRACE }
 | "}"                  { adv lexbuf; RBRACE }
 | "."                  { adv lexbuf; PERIOD }
 | ","                  { adv lexbuf; COMMA }
 | "and"                { adv lexbuf; AND }
 | "or"                 { adv lexbuf; OR }
 | "not"                { adv lexbuf; NOT }
 | "xor"                { adv lexbuf; XOR }
 | "&"                  { adv lexbuf; BAND }
 | "|"                  { adv lexbuf; BOR }
 | "!"                  { adv lexbuf; BNOT } 
 | "^"			{ adv lexbuf; BXOR }
 | "~"                  { adv lexbuf; BCOMP }
 | "++"			{ adv lexbuf; CONCAT }
 | "<<"			{ adv lexbuf; SHL }
 | ">>"			{ adv lexbuf; SHR }
 | "+"                  { adv lexbuf; ADD }
 | "-"                  { adv lexbuf; SUB }
 | "*"                  { adv lexbuf; MUL }
 | "/"                  { adv lexbuf; DIV }
 | "\""                 { adv lexbuf; grabstr lexbuf }
 | "%"                  { adv lexbuf; MOD }
 | "_FILE"		{ adv lexbuf; FILEDEC }
 | "interface"		{ adv lexbuf; INTERFACE }
 | "var"		{ adv lexbuf; VAR }
 | "raise"              { adv lexbuf; RAISE }
 | "try"		{ adv lexbuf; TRY }
 | "with"               { adv lexbuf; WITH }
 | "finally"            { adv lexbuf; FINALLY }
 | "break"              { adv lexbuf; BREAK }
 | "return"             { adv lexbuf; RETURN }
 | "continue"           { adv lexbuf; CONTINUE }
 | "if"                 { adv lexbuf; IF }
 | "elif"               { adv lexbuf; ELIF }
 | "else"               { adv lexbuf; ELSE}
 | "for"                { adv lexbuf; FOR }
 | "foreach"		{ adv lexbuf; FOREACH }
 | "to"			{ adv lexbuf; TO }
 | "by"			{ adv lexbuf; BY }
 | "in"			{ adv lexbuf; IN }
 | "while"              { adv lexbuf; WHILE }
 | "pass"               { adv lexbuf; PASS }
 | "nil"                { adv lexbuf; NIL }
 | "use"                { adv lexbuf; USE }
 | "import"             { adv lexbuf; IMPORT }
 | "export"             { adv lexbuf; EXPORT }
 | "true"		{ adv lexbuf; INT( Int64.one ) }
 | "false"		{ adv lexbuf; INT( Int64.zero ) }
 | "class"		{ adv lexbuf; CLASS }
 | "interface"		{ adv lexbuf; INTERFACE }
 | "self"		{ adv lexbuf; SELF }
 | "super"		{ adv lexbuf; SUPER }
 | "global"		{ adv lexbuf; GLOBAL }
 | "const"		{ adv lexbuf; CONST } 
 | "$"			{ adv lexbuf; lcomment lexbuf }
 | "{-"			{ adv lexbuf; inComment := !inComment + 1; bcomment lexbuf }
 | id			{ adv lexbuf; ID( gs lexbuf ) }
 | eof			{ EOF }
 | _			{ adv lexbuf; 
                          error ("Invalid token: " ^ (gs lexbuf)); 
			  raise Lexer_error;
			  token lexbuf }


(* We have to do a few evil string-permutations to make it parse right... *)
 (* This bit grabs any sequence ending with ", not containing " but possibly
    containing \".  Wow, it actually works!  ^_^   silly comment stuff -> " *)

and grabstr = parse
   '"'			{ adv lexbuf; token lexbuf }
 | ("\\\""|[^'"'])*'"'	{ 
   			adv lexbuf; 
			STRING( parseStr 
			         (String.sub (gs lexbuf) 
			                     0 
				             ((String.length 
					        (gs lexbuf)) - 1) )) } 

and lcomment = parse
   '\n'			{ nl " "; token lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }

and bcomment = parse
   "{-"			{ adv lexbuf; inComment := !inComment + 1; 
                          bcomment lexbuf }
 | "-}"			{ adv lexbuf; inComment := !inComment - 1; 
                          if !inComment <= 0 then token lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl " "; bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 
