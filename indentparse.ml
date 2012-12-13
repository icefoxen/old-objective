(* indentparse.ml
   The indent-tokenizer for Objective.
   Re-coded verbatem from the Python prototype, which really shows in the
   horrible horrible things I do to lists here...
   It's almost certainly dreadfully inefficient compared to what it could be...
   But who cares?

   TODO: Rewrite so that you can use all spaces or all tabs,
   but not both mixed.  Right now it just screams and dies on tabs.

   XXX: Grar.

   Simon Heath
   10/05/2004
*)

open String;;

exception IndentException of string;;

let linecommentchar = '$';;

let indexoffirstchar x  =
  let idx = ref 0 in
  for i = 0 to (length x) - 1 do
    if x.[i] == ' ' then
      incr idx
    else if x.[i] == '\t' then
      raise (IndentException "Tabs are illegal!")
  done;
  !idx
;;

let mergestrings strlst  =
  List.fold_left (^) "" strlst
;;


let list_last n =
  let i = List.length n in
    List.nth n (i - 1)
;;

let list_append x y =
  x := y :: !x
;;


(* As we're using real linked-lists here instead of Python's vectors,
   we build and remove from the front instead of the end.
*)
let parseIndents( strlst ) =
  let indentslst = ref []
  and lineno = ref 0
  and indentno = ref 0
  and indentstack = ref [0] 
  and retlst = ref [] in
  let step1 str =
    let ic = indexoffirstchar( x ) in
      if x == '\n' or x[ic] == linecommentchar or ic + 1 == length x then (
	list_append indentslst (List.hd !indentslst);
	incr lineno;
      )
      else (
	list_append indentslst ic;
	incr lineno;
      )
