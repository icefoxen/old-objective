(* package.ml
   Contains code to read an abstract syntax tree of a bunch of files and
   dump it all to a package definition file.  ^_^

   This is basically the exact inverse of the parser (and a LOT easier
   to write); it dumps everything directly to Objective syntax, except
   for method bodies and global defaults.  
   This means we can use the same parser to re-read the package file, and 
   it'll dump it into a nice syntax tree automatically.
   Coool, ne?  ^_^
   Possibly slow, 'cause each time we compile a file we have to re-parse
   the package interface files, but we can cache them between
   file-compilations if we have to.

   XXX: Consts and globals have to have initial values to be valid to the
   parser!  For now they're just set to Nil.

   Simon Heath
   10/05/2004
*)

open Syntree;;
open Printf;;

(* Everything dumps to outchan.  If you want to dump to a file,
   just change outchan.
   Not terribly tidy, but simple.  Change outchan to a function arg
   whenever you happen to feel like rewriting everything.
*)
let outchan = ref stdout;;

let pf x n = 
  fprintf !outchan x n
;;

let dumpvar = function
    Vardecl( nm, tp, _ ) ->
      (pf "(%s: %s) " nm tp)
  | Varlenvar nm ->
      (pf "~%s " nm) 
  | Nil -> 
      ()
;;

(* Really oogly, but it works.  Kinda. 
   It also reverses the order of the interface list in the output,
   so...  I think it's okay, but still.
*)
let retdelimitedlist l =
  let n = ref l
  and retstr = ref "" in
    while !n <> [] do
      retstr := (List.hd !n) ^ ", " ^ !retstr;
      n := List.tl !n;
    done;
    retstr := !retstr ^ "\b\b";
    !retstr;
;;

let dumpmethod t x =
  match x with
    Methoddecl( classmeth, name, rettype, args, _, _ ) ->
      if classmeth then pf "   + %s" ""  else pf "   - %s" "";
      pf "(%s: %s) " name rettype; 
      List.iter dumpvar args;
      if t then 
	fprintf !outchan ": ;\n"
      else
	fprintf !outchan ": \n"
      
  | Nomethod -> ()
;;

let dumpclass = function
    Classdecl( name, super, interfaces, methods, vars ) ->
      pf "class %s (%s: %s):\n" name super (retdelimitedlist interfaces);
      List.iter (fun x ->
		   match x with
		       Nil -> ()
		     | _ -> (fprintf !outchan "   var ";
			     dumpvar x;
			     fprintf !outchan ";\n")) 
	vars;
      List.iter (dumpmethod true) methods;
;;

let dumpinterface = function
    Interfacedecl( name, super, methods ) ->
      pf "interface %s (%s):\n" name super;
      List.iter (dumpmethod false) methods;
;;
      

let dumppkg = function
    Package( nm, files, imports, uses, exports, globals, consts, classes,
	     interfaces )
      ->
	pf "in %s;\n" nm;
	printList files 0
	  (fun x d -> fprintf !outchan "_file %s;\n" x);
	printList imports 0 
	  (fun x d -> pf "import %s;\n" x);
	printList uses 0 
	  (fun x d -> pf "use %s;\n" x);
	printList exports 0 
	  (fun x d -> pf "export %s;\n" x);
	printList globals 0 
	  (fun x d -> fprintf !outchan "global ";
	     dumpvar x;
	     fprintf !outchan " <- Nil;\n");
	printList consts 0 
	  (fun x d -> fprintf !outchan "const ";
	     dumpvar x;
	     fprintf !outchan " <- Nil;\n");
	printList classes 0 
	  (fun x d -> dumpclass x);
	printList interfaces 0 
	  (fun x d -> dumpinterface x);

;;

(*
and dumpexpr x d =
  match x with
      Int i -> pf d "%s" (Int64.to_string i)
    | Float f -> pf d "%f" f
    | Char c -> pf d "%c" c
    | String s -> pf d "\"%s\"" s
    | Var s -> pf d "%s" s
    | Array -> 
	pf d 
*)



