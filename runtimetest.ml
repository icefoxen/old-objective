(* runtimetest.ml
   Tests out the Objective runtime
   A fib function in hard-coded Ocaml

   Simon Heath
   16/05/2004
*)

open Runtime;;

let classFib = {
  cname = "Fib";
  csuper = nilClass;
  cinstanceVarNum = 0;
  cmethods = Hashtbl.create 8;
}

let getInt n =
  match n with
      Int a -> a
    | _ -> 0
;;

let getBool n =
  match n with
      Bool a -> a
    | _ -> true
;;


let addMethod ins args =
  Int( (getInt args.(0)) + (getInt args.(1)) )
;;

let subMethod ins args =
  Int( (getInt args.(0)) - (getInt args.(1)) )
;;

let mulMethod ins args =
  Int( (getInt args.(0)) * (getInt args.(1)) )
;;

let gtMethod ins args =
  Bool( (getInt args.(0)) > (getInt args.(1)) )
;;

let ltMethod ins args =
  Bool( (getInt args.(0)) < (getInt args.(1)) )
;;

let zero = Int( 0 )
and one  = Int( 1 )
and two  = Int( 2 );;

(*
  - fib v:
    if v < 2:
      1
    else: 
      (self fib (v - 1)) + (self fib (v - 2))
*)


(* Profile this!
   Would skiplists be more efficent than hashtables???
*)
let fibMethod ins args =
  let v = args.(0) in
    if (getBool (sendMessage "lt" ins [|v; two|])) then
      one
    else
      (sendMessage "add" ins [|
	 (sendMessage "fib" ins [|(sendMessage "sub" ins [|v; one|])|]);
	 (sendMessage "fib" ins [|(sendMessage "sub" ins [|v; two|])|])
       |])
;;

let factMethod ins args =
  let v = args.(0) in
    if (getBool (sendMessage "lt" ins [|v; two|])) then
      one
    else
      (sendMessage "mul" ins [| v; 
				(sendMessage "fact" ins [|
				   (sendMessage "sub" ins [|v; one|])|])|])
;;



registerMethod "add" addMethod classFib;;
registerMethod "sub" subMethod classFib;;
registerMethod "mul" subMethod classFib;;
registerMethod "gt" gtMethod classFib;;
registerMethod "lt" ltMethod classFib;;
registerMethod "fib" fibMethod classFib;;
registerMethod "fact" fibMethod classFib;;

let fib = createInstance classFib;;
print_int (getInt (sendMessage "fib" fib [|Int( 30 )|]));;
print_newline ();;
