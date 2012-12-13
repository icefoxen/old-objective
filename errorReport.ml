(* errorReport.ml
   General error reporting.  This module keeps track of where we are in
   the file, amoung other things.

   Simon Heath
   13/1/2004
*)


let lineNum = ref 1;;
let chrNum = ref 1;;
let fileName = ref "";;


(* Increments line count *)
let nl n =
   let n = String.length n in
   lineNum := !lineNum + n;
   chrNum := 0;;

(* Prints an error message *)
let error msg =
   Printf.eprintf "%s: " !fileName;
   Printf.eprintf "%d.%d: %s\n" !lineNum !chrNum msg;;

(* Resets the error-checker *)
let reset fname =
   fileName := fname;
   lineNum := 1;
   chrNum := 0;;
