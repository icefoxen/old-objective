(* syntree.ml
   All the great flippin' wodgers of types necessary to build an
   abstract syntax tree for Objective.  Also a function or two for support,
   id est printing the tree out.
   Yay!  This means I'm freakin' done with the bloody parser!
   It only took... what, three weeks?  o_O

   Simon Heath
   07/05/2004
*)

open Printf

type binop =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | Concat
  | And
  | Or
  | Xor
  | Band
  | Bor
  | Bxor
  | Eq
  | Neq
  | Seq
  | SNeq
  | Shl
  | Shr
  | Varref
  | Arrayref
  | Gt
  | Lt
  | Gte
  | Lte

and uniop =
    Bnot
  | Bcomp
  | Not

and expr =
    Int of Int64.t
  | Float of float
  | Char of char
  | String of string
  | Var of string
  | Array of expr list
  | Dict of (expr * expr) list
  | Block of vardecl list * stm list
  | Pkgref of string * string
  | Binop of (binop * expr * expr)
  | Uniop of (uniop * expr)
  | Methodcall of string * expr * vardecl list
  | Assign of expr * expr
  | Voidexpr

(* Var( name, type, default val ) *)
and vardecl = 
    Vardecl of string * string * expr
  | Varlenvar of string 
  | Nil

and stm =
    Expr of expr
(* Ifstm( ifexpr, thenlist, (elifexpr * thenlist) list, elselist ) *)
  | Ifstm of expr * stm list * (expr * stm list) list * stm list

  | Whilestm of expr * stm list
  | Forstm of vardecl * expr * expr * stm list
  | Foreachstm of vardecl * expr * stm list
  | Continuestm
  | Breakstm

  | Raisestm of expr
  | Trystm of stm list * vardecl * stm list * stm list
  | Returnstm of expr
  | Varstm of vardecl

(* The first bool is true if it's a class method, false if it's
   an instance method.
   Methoddecl( instance/class, name, returntype, args, locals, body )
   Nomethod is used as a bit of a hack for parsing classmembers.
*)
and methoddecl =
    Methoddecl of bool * string * string * vardecl list * vardecl list *
                  stm list
  | Nomethod

(* Classdecl( name, superclass, [interfaces], [methods], [instance vars] ) *)
and classdecl =
    Classdecl of string * string * string list * methoddecl list * vardecl list

(* Interfacedecl( name, superclass, [methods] ) *)
and interfacedecl =
    Interfacedecl of string * string * methoddecl list

(* Package( name, [file], [imports], [uses], [exports], [globals], [consts], 
   [classes], [interfaces] )
*)
and package = 
    Package of string * string list * string list * string list * 
      string list * vardecl list * vardecl list * classdecl list *
      interfacedecl list
;;

let pl x d =
  print_string (String.make d ' ');
  print_endline x
;;

let printList x d f =
  List.iter (fun y -> f y d) x
;;

let printList2 x f =
  List.iter (fun y -> f y) x
;;


let printBinop x d =
  match x with
      Add    -> pl "Add" d
    | Sub    -> pl "Sub" d
    | Mul    -> pl "Mul" d
    | Div    -> pl "Div" d
    | Mod    -> pl "Mod" d
    | Concat -> pl "Concat" d
    | And    -> pl "And" d
    | Or     -> pl "Or" d
    | Xor    -> pl "Xor" d
    | Band   -> pl "Band" d
    | Bor    -> pl "Bor" d
    | Bxor   -> pl "Bxor" d
    | Eq     -> pl "Eq" d
    | Neq    -> pl "Neq" d
    | Seq    -> pl "Seq" d
    | SNeq   -> pl "Sneq" d
    | Shl    -> pl "Shl" d
    | Shr    -> pl "Shr" d
    | Varref -> pl "Varref" d
    | Arrayref -> pl "Arrayref" d
    | Gt     -> pl "Gt" d
    | Lt     -> pl "Lt" d
    | Gte    -> pl "Gte" d
    | Lte    -> pl "Lte" d
;;

let printUniop x d =
  match x with
      Bnot  -> pl "Bnot" d
    | Bcomp -> pl "Bcomp" d
    | Not   -> pl "Not" d
;;


let rec printExpr x d = 
  match x with
      Int n
        -> pl (sprintf "Int %s" (Int64.to_string n)) d
    | Float n
	-> pl (sprintf "Float %f" n) d
    | Char n 
	-> pl (sprintf "Char %c" n) d
    | String n
	-> pl (sprintf "String %s" n) d
    | Var n
	-> pl (sprintf "Var %s" n) d
    | Array n
	-> pl "Array:" d; 
	  printList n (d + 1) printExpr
    | Dict n
	-> pl "Dict:" d; 
	  printList n (d + 1) 
	    (fun a d -> 
	       let x, y = a in 
		 printExpr x d;
		 printExpr y d;)
    | Block( n, m )
	-> pl "Block:" d;
	  printList n (d + 1) printVar;
	  printList m (d + 1) printStm;
    | Pkgref( n, m )
	-> pl (sprintf "Pkgref: %s :: %s" n m) d
    | Binop( n, m, x )
	-> pl "Binop:" d;
	  printBinop n (d + 1);
	  printExpr m (d + 1);
	  printExpr x (d + 1);
    | Uniop( n, m )
	-> pl "Uniop:" d;
	  printUniop n (d + 1);
	  printExpr m (d + 1);
    | Methodcall( n, m, x )
	-> pl (sprintf "Methodcall %s" n) d;
	  printExpr m (d + 1);
	  printList x (d + 1) printVar;
    | Assign( n, m )
	-> pl "Assignment:" d;
	  printExpr n (d + 1);
	  printExpr m (d + 1);
    | Voidexpr
	-> pl "Voidexpr" d;




(* Var( name, type, default val ) *)
and printVar x d =
  match x with
      Vardecl( x, y, z ) ->
	pl "Vardecl:" d;
	pl x (d + 1);
	pl y (d + 1);
	printExpr z (d + 1);
    | Varlenvar x ->
	pl "Var-length vardecl:" d;
	pl x (d + 1);
    | Nil ->
	pl "Nil var" d;


and printElif x d =
  let a, b = x in
    printExpr a d;
    printList b d printStm;



and printStm x d =
  match x with
    Expr x ->
      pl "Exprstm:" d;
      printExpr x (d + 1);
  | Ifstm( x, y, z, a ) ->
      pl "Ifstm:" d;
      printExpr x (d + 1);
      printList y (d + 1) printStm;
      printList z (d + 1) printElif;
      printList a (d + 1) printStm;
  | Whilestm( x, y ) ->
      pl "Whilestm:" d;
      printExpr x (d + 1);
      printList y (d + 1) printStm;
  | Forstm( x, y, z, a ) ->
      pl "Forstm:" d;
      printVar x (d + 1);
      printExpr y (d + 1);
      printExpr z (d + 1);
      printList a (d + 1) printStm;
  | Foreachstm( x, y, z ) ->
      pl "Foreachstm:" d;
      printVar x (d + 1);
      printExpr y (d + 1);
      printList z (d + 1) printStm;
  | Continuestm ->
      pl "Continuestm" d;
  | Breakstm ->
      pl "Breakstm" d;
  | Raisestm x ->
      pl "Raisestm:" d;
      printExpr x (d + 1);
  | Trystm( x, y, z, a ) ->
      pl "Trystm:" d;
      printList x (d + 1) printStm;
      printVar y (d + 1);
      printList z (d + 1) printStm;
      printList a (d + 1) printStm;
  | Returnstm x ->
      pl "Returnstm:" d;
      printExpr x (d + 1);
  | Varstm z ->
      pl "Varstm:" d;
      printVar z (d + 1);
;;

(* The first bool is true if it's a class method, false if it's
   an instance method.
   Methoddecl( instance/class, name, returntype, args, local vars, body )
   Nomethod is used as a bit of a hack for parsing classmembers.

   Looking back on this... ye gods it's unreadable.
*)
let printMethodDecl x d =
  match x with
      Methoddecl( isclass, name, rettype, args, localvars, body ) ->
	pl "Methoddecl:" d;
	pl (sprintf "%b" isclass) (d + 1);
	pl name (d + 1);
	pl rettype (d + 1);
	printList args (d + 1) printVar;
	printList localvars (d + 1) printVar;
	printList body (d + 1) printStm;
    | Nomethod ->
      pl "Nomethod" d;
;;

(* Classdecl( name, superclass, [interfaces], [methods], [instance vars] ) *)
let printClassDecl x z = 
  let nd = z + 1 in
  match x with
    Classdecl( a, b, c, d, e ) ->
      pl "Classdecl:" z;
      pl a nd;
      pl b nd;
      printList c nd pl;
      printList d nd printMethodDecl;
      printList e nd printVar;
;;

let printInterfaceDecl x d =
  match x with
    Interfacedecl( a, b, c ) ->
      pl "Interfacedecl" d;
      pl a (d + 1);
      pl b (d + 1);
      printList c (d + 1) printMethodDecl;
;;

(* Package( name, [imports], [uses], [globals], [exports] [consts], [classes], 
            [interfaces] )
*)
let printPackage x z = 
  let nd = z + 1 in
  match x with
      Package( a, ahalf, b, c, d, e, f, g, h ) ->
	pl "Package:" z;
	pl a nd;
	pl "Files:" nd;
	printList ahalf (nd + 1) pl;
	printList b nd pl;
	printList c nd pl;
	printList d nd pl;
	printList e nd printVar;
	printList f nd printVar;
	printList g nd printClassDecl;
	printList h nd printInterfaceDecl

;;

let printParseTree n =
  printPackage n 0
;;
