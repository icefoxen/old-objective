(* symtable.ml
   Symbol table.

   Entries need: Name, scope number, type, address,

   Process:
   1) Walk through the package def, grab globals, constants and imports.
   2) Walk through classes, grab instance vars.  For each class:
   3) Walk through methods, grab local vars, go through each statement
   and see if it references anything that doesn't exist.
   4) Add new scopes for for, foreach, try/with, and block.

   Simon Heath
   14/05/2004
*)

open Hashtbl;;


exception SymbolException of string;;

let mergepackages a b =
  match a with
      Package( n1, f1, i1, u1, e1, g1, c1, cl1, in1 ) ->
	match b with
	    Package( n2, f2, i2, u2, e2, g2, c2, cl2, in2 ) ->
	      let nn = 
		if n1 = n2 then n1 
		else raise (SymbolException
		  (sprintf 
		     "mergepackages: %s has a different name from %s!" n1 n2))
	      and nf = f1 @ f2
	      and ni = i1 @ i2
	      and nu = u1 @ u2
	      and ne = e1 @ e2
	      and ng = g1 @ g2
	      and nc = c1 @ c2
	      and cln = cl1 @ cl2
	      and inn = in1 @ in2 in
		Package( nn, nf, ni, nu, ne, ng, nc, cln, inn )
;;

let subclass parent child =
  match parent with
      Classdecl( n1, s1, i1, m1, v1 ) ->
	match child with
	    ClassDecl( n2, s2, i2, m2, v2 ) ->
	      let nn = n2
	      and ns = n1
	      and ni = i1 @ i2   (* Ni!  Ni! *)
	      and nm = m1 @ m2
	      and nv = v1 @ vt
	      in
		ClassDecl( nn, ns, ni, nm, nv )
;;

(* Okay.  So, the algorithm we want for checking symbol existance goes thus:
  *Grab the appropriate fields out of the package object
  *Grab the other packages it imports and uses.
  *Set the global symbol table: globals, constants, classes, methods (?), 
   plus used package globals, constants, classes and methods.
  *Set up symbol tables for imported packages.
  *Go through each class, push a new frame, and:
   *For each instance var, add it to the top frame
   *For each method, push a new frame and:
    *For each local var, add it to the top frame
    *Go through the statement list in order and:
     *For each ID, see if it's declared
     *For each block, add a new frame and go through the statements
     *For each pkgref, check the appropriate package symtable
     *For each operation, check each arg, checking varrefs also
     *For each with in a try/with, push the var on a new scope (or just add
      and remove the var from the scope, as it's only one var)
     *For each methodcall, check name of the method and it's number of args,
      if the name doesn't exist, emit a WARNING.
     *For each for and foreach stm, push a new frame with the appropriate vars.
 
  Each symbol object must contain: Name, type, generated name?, initializer?,
  address?
*)

(* For the local scope we use a single hashtable, then keep a
   stack of lists that represents each scope.  Each new frame means a new list
   is pushed onto the stack, and as each symbol is added to the symtable, it's
   also added to the top list.  Then when a scope is removed, you just go 
   through the list and remove each symbol in it from the symtable.

   Maybe as we add each new var, we check to see whether or not it already
   exists in the top scope?  Then we should use another hashtbl instead of a
   list...  It can be done.  Yeah.
*)
let importTbl = Hashtbl.create 5;;
let useTbl = Hashtbl.create 5;;
let curPkg = Hashtbl.create 40;;
let remLst = Stack.create ();;



(* Other things we have to do:
   *Check for overlapping var names in the same scope
   *Check to make sure no continue or break happens outside a loop
   *Figure out how to initialize packages and objects
   *Check to make sure no constants are modified.
   *Check that interfaces are implemented.
   *For methods and blocks, we need to go through the statement list and
   dike out all the local vars, collecting them in one place.  List.filter
   should do it.
   *Build runtime!!
   *Check types --but not now
*)
