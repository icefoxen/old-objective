
let compileFile fn =
  ErrorReport.reset fn;
  try
    let lexbuf = Lexing.from_channel (open_in fn) in
    let parsetree = Parser.main Lexer.token lexbuf in
      Syntree.printParseTree parsetree; 
      Package.dumppkg parsetree;
      print_endline "Parsing succeeded!  Have a cookie!";

  with
      Sys_error a -> (Printf.eprintf "File does not exist: %s\n" a; 
		    exit 1)
    | Parsing.Parse_error -> 
	ErrorReport.error "Fatal parse error: Bad programmer, no cookie for you!"
;;

let usage () = 
  print_endline "Usage: testy filename.obv"
;;


let _ =
  
  let fn = Sys.argv.(1) in
    compileFile fn
  
;;

