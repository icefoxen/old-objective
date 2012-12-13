%{
(* parser.mly
   A parser for Objective.
   Yay.

   Ya know, what would be really nice would be for newlines to
   be statement-ends, so as not to require ;'s.  It can certainly be done
   (see Python :-), but makes things complex and error-prone.  At least
   here.

   Special symbols: "_NONE" showing no superclass or package, 
   "_UNKNOWN" showing an
   unknown var type --dynamic, in other words.


   Simon Heath
   10/05/2004
*)

open ErrorReport
open Syntree

(* This parses out a list of (methoddecl * vardecl) pairs into 
   ([methoddecls] * [vardecls])

   Gods I love functional languages.  This would've taken at least 10
   lines in C, and would be exceedingly painful to think about.
*)
let classSeperate n =
   let methodlist = List.map (fun x -> match x with x,y -> x) n
   and varlist = List.map (fun x -> match x with x,y -> y) n
   in
      (methodlist, varlist)
;;

(* Having these globals is probably bad form, but oh well.  It's easier than
   shifting everything around.
*)

let name = ref "_NONE"
and imports = ref []
and uses = ref []
and exports = ref []
and globals = ref []
and consts = ref []
and classes = ref []
and interfaces = ref []
and files = ref []
;;


let isvar = function
    Varstm( _ ) -> true
  | _ -> false
;;

let unbox_varstm = function
    Varstm( x ) -> x
  | n -> Nil
;;

let unbox_varstm_lst x = 
  List.map unbox_varstm x
;;


%}


%token INDENT DEDENT ENDSTM
%token ASSIGN ASSIGNADD ASSIGNSUB ASSIGNMUL ASSIGNDIV ASSIGNMOD
%token GTE LTE GT LT EQ NEQ SEQ SNEQ
%token LPAREN RPAREN COLON LBRACK RBRACK LBRACE RBRACE BLOCKSTART BLOCKEND
%token PERIOD COMMA AND OR NOT XOR BAND BOR BNOT BXOR BCOMP CONCAT SHL SHR
%token ADD SUB MUL DIV MOD
%token RAISE TRY WITH FINALLY
%token BREAK CONTINUE RETURN IF ELIF ELSE FOR FOREACH TO BY IN WHILE PASS
%token NIL IMPORT USE EXPORT
%token TRUE FALSE CLASS SELF SUPER GLOBAL CONST VAR INTERFACE
%token PKGREF
%token FILEDEC EOF

%token <string> STRING ID
%token <char> CHAR
%token <Int64.t> INT
%token <float> FLOAT

%type <Syntree.package> main
%start main

%%
main: 
	  decls EOF
	  	{ Package( !name, !files, !imports, !uses, !exports, !globals,
		           !consts, !classes, !interfaces ) }
	;


symlst:
	  ID 
	  	{ [$1] }
	| symlst COMMA ID
		{ $1 @ [$3] }
	;

endsymlst:
	  symlst ENDSTM
	  	{ $1 }
	;



/* XXX:
Ah, hahahaha...  These build these lists BACKWARDS.  Hahaha...  Better fix
that.
*/
importstm:
	  IMPORT endsymlst
	  	{ imports := $2 @ !imports  }
	;

usestm:
	  USE endsymlst
	  	{ uses := $2 @ !uses }
	;

exportstm:
	  EXPORT endsymlst
	  	{ exports := $2 @ !exports }
	;

instm:
	  IN ID ENDSTM
		{ name := $2 }
	;

superclassdecl:
	  ID
	  	{ [$1] }
	| ID COLON symlst
		{ $1 :: $3 }
	| /* NOTHING */
		{ ["_NONE"] }
	;

classstm:
	  CLASS ID LPAREN superclassdecl RPAREN COLON INDENT classmembers DEDENT
	  	{ let methods, vars = classSeperate $8 in
		   Classdecl( $2, (List.hd $4), (List.tl $4), methods, vars )
		}
	;

globalstm:
	  GLOBAL typedecl ENDSTM
	  	{ let nm, tp = $2 in 
		   (globals := !globals @ [Vardecl( nm, tp, Voidexpr )] ) }
	| GLOBAL typedecl ASSIGN value ENDSTM
	  	{ let nm, tp = $2 in 
		   (globals := !globals @ [Vardecl( nm, tp, $4 )] ) }
	;

conststm:
	  CONST typedecl ASSIGN value ENDSTM
	  	{ let nm, tp = $2 in 
		     (consts := !consts @ [Vardecl( nm, tp, $4 )] )
		}
	;

interfacestm:
	  INTERFACE ID LPAREN RPAREN COLON INDENT interfacebody DEDENT
	  	{ Interfacedecl( $2, "_NONE", $7 ) }
	| INTERFACE ID LPAREN ID RPAREN COLON INDENT interfacebody DEDENT
	  	{ Interfacedecl( $2, $4, $8 ) }
	;


interfacebody:
	  interfacemethod
	  	{ [$1] }
	| interfacebody interfacemethod
		{ $1 @ [$2] }
	;

/* The first item is whether the method IS A CLASS METHOD.
   + foo:  ==> Methoddecl( true, ... )
*/
interfacemethod:
	  ADD typedecl COLON
	  	{let nm, tp = $2 in
		   Methoddecl( true, nm, tp, [], [], [] ) }
	| ADD typedecl arglst COLON
	  	{let nm, tp = $2 in
		   Methoddecl( true, nm, tp, $3, [], [] ) }
	| SUB typedecl COLON
	  	{let nm, tp = $2 in
		   Methoddecl( false, nm, tp, [], [], [] ) }
	| SUB typedecl arglst COLON
	  	{let nm, tp = $2 in
		   Methoddecl( false, nm, tp, $3, [], [] ) }
	;

filedec:
	  FILEDEC ID ENDSTM
	  	{files := !files @ [$2]}

decl:
	  instm
		{}
	| importstm
		{}
	| exportstm
		{}
	| usestm
		{}
	| classstm
		{ classes := !classes @ [$1] }
	| globalstm
		{}
	| conststm
		{}
	| interfacestm
		{ interfaces := !interfaces @ [$1] }
	| filedec
		{}
	;

decls:
	  decl
		{ [$1] }
	| decls decl
		{ $1 @ [$2] }
	;


classmember:
	  methoddecl
	  	{ ($1, Nil) }
	| vardecl
		{ (Nomethod, $1) }
	;

classmembers:
	  classmember
	  	{ [$1] }
	| classmembers classmember
		{ $1 @ [$2] }
	;


typedecl:
	  LPAREN ID COLON ID RPAREN
	  	{ ($2, $4) }
	| ID
		{ ($1, "_UNKNOWN") }
	;

vardecl:
	  VAR typedecl ENDSTM
		{ let name, tp = $2 in
		  Vardecl( name, tp, Voidexpr ) }
	| VAR typedecl ASSIGN value ENDSTM
		{ let name, tp = $2 in
		  Vardecl( name, tp, $4 ) }
	;


methoddecl:
	  ADD methodbody
		{ let a, b, c, d, e = $2 in
		  let f = unbox_varstm_lst d in
		  Methoddecl( true, a, b, c, f, e ) }
	| SUB methodbody
		{ let a, b, c, d, e = $2 in
		  let f = unbox_varstm_lst d in
		  Methoddecl( false, a, b, c, f, e ) }
	;

argdefault:
	  /* NOTHING */
	  	{ Voidexpr }
	| EQ value
		{ $2 }
	;

/* The foo=| syntax is kinda messy, and varlength args don't mesh 
well with the optional type-checking in general, but...
The ID EQ bit also conflicts with argdefault, but it's RIGHT, so I won't
care about it right now.
*/
argitm:
	  typedecl argdefault
	  	{ let a, b = $1 in Vardecl( a, b, $2 ) }
	| BCOMP ID
		{ Varlenvar( $2 ) }
	;

arglst:
	  argitm
	  	{ [$1] }
	| arglst argitm
		{ $1 @ [$2] }
	;

/* (name, returntype, vars, body) */
methodbody:
	  typedecl arglst COLON indentblock
	  	{ let a, b = $1 
		  and c, d = List.partition isvar $4 in (a, b, $2, c, d ) } 
	| typedecl COLON indentblock
	  	{ let a, b = $1
		  and c, d = List.partition isvar $3 in (a, b, [], c, d ) }
	;


indentblock:
	  INDENT stms DEDENT
		{ $2 }
	| stm
		{ [$1] }
	;


stms:
	  stm
	  	{ [$1] }
	| stms stm
		{ $1 @ [$2] }
	;

stm:
	  expr ENDSTM
	  	{ Expr( $1 ) }
	| ifstm
		{ $1 }
	| PASS
		{ Expr( Voidexpr ) }
	| vardecl
		{ Varstm( $1 ) }
	| forstm
		{ $1 }
	| foreachstm
		{ $1 }
	| whilestm
		{ $1 }
	| BREAK ENDSTM
		{ Breakstm }
	| CONTINUE ENDSTM
		{ Continuestm }
	| RETURN ENDSTM
		{ Returnstm( Voidexpr ) }
	| RETURN expr ENDSTM
		{ Returnstm( $2 ) }
	| raisestm ENDSTM
		{ $1 }
	| trystm
		{ $1 }
	| ENDSTM
		{ Expr( Voidexpr ) }
	;


/* This causes the classic dangling-else, compounded with a dangling-elif.
   Works just fine though.
*/
ifstm:
	  IF expr COLON indentblock
	  	{ Ifstm( $2, $4, [], [] ) }
	| IF expr COLON indentblock elifstm ELSE COLON indentblock
		{ Ifstm( $2, $4, $5, $8 ) }
	| IF expr COLON indentblock ELSE COLON indentblock
		{ Ifstm( $2, $4, [], $7 )  }
	;


elifstm:
	  ELIF expr COLON indentblock
	  	{ [($2, $4 )] }
	| elifstm ELIF expr COLON indentblock
		{ $1 @ [($3, $5)] } 
	;

forclause:
	  BY INT
	  	{ Int( $2 ) }
	| /* NOTHING */
		{ Int( Int64.one ) }
	;

forstm:
	  FOR ID ASSIGN expr TO expr forclause COLON indentblock
	  	{ Forstm( Vardecl( $2, "Int", $4 ), $6, $7, $9 ) }
	;

/* foreach x in y: dostuff
   ====>>
   while (hasNext y):
      x <- (next y);
      dostuff
		{ [Varstm( Vardecl( $2, "_UNKNOWN", Voidexpr ) ),
		   Whilestm( Methodcall( "hasNext", $4, [] ),
		            [Assign( $2, Methodcall( "next", $4, [] ) );
			     $6] 
			    ) 
		  ] }
*/
foreachstm:
	  FOREACH ID IN expr COLON indentblock
	  	{ Foreachstm( Vardecl( $2, "_UNKNOWN", Voidexpr ), $4, $6 ) }
	;
whilestm:
	  WHILE expr COLON indentblock
	  	{ Whilestm( $2, $4 ) }
	;


raisestm:
	  RAISE expr
		{ Raisestm( $2 ) }
	;

trystm:
	  TRY COLON indentblock withstm
	  	{ let withvar, withbody, finallybody = $4 in
		     Trystm( $3, withvar, withbody, finallybody ) }
	| TRY COLON indentblock finallystm
		{ Trystm( $3, Nil, [], $4 ) }
	;

withstm:
	  WITH typedecl COLON indentblock
		{ let v, t = $2 in (Vardecl( v, t, Voidexpr ), $4, [] ) }
	| WITH typedecl COLON indentblock finallystm
		{ let v, t = $2 in (Vardecl( v, t, Voidexpr ), $4, $5 ) }
	;

finallystm:
	  FINALLY COLON indentblock
	  	{ $3 } 
	;
	

arraylst:
	  expr
	  	{ [$1] }
	| arraylst COMMA expr
		{ $1 @ [$3] }
	;

arraydecl:
	  LBRACK RBRACK
		{ Array( [] ) }
	| LBRACK arraylst RBRACK
		{ Array( $2 ) }
	;

dictlst:
	  expr COLON expr
	  	{[($1, $3)]}
	| dictlst COMMA expr COLON expr
		{ $1 @ [($3, $5)] }
	;

dictdecl:
	  LBRACE RBRACE
	  	{ Dict( [] ) }
	| LBRACE dictlst RBRACE
		{ Dict( $2 ) }
	;

/* XXX:
Okay.  This style of block is quite ugly and somewhat broken,
but it's better than nothing.
And you CAN indent blocks and spread them out!  That makes it a bit nicer.
*/

blockdecl:
	  BLOCKSTART arglst COLON stms BLOCKEND
	  	{ Block( $2, $4 ) }
	| BLOCKSTART arglst COLON INDENT stms DEDENT BLOCKEND
	  	{ Block( $2, $5 ) }
	;




value:
	  STRING
	  	{ String( $1 ) }
	| ID
	  	{ Var( $1 ) }
	| CHAR
	  	{ Char( $1 ) }
	| INT
	  	{ Int( $1 ) }
	| FLOAT
	  	{ Float( $1 ) }
	| arraydecl
	  	{ $1 }
	| dictdecl
	  	{ $1 }
	| blockdecl
	  	{ $1 }
	| ID PKGREF ID
		{ Pkgref( $1, $3 ) }
	;



/* Okay, here's the precedence stack:
expr
methodcall  (binds tightest)
postfix
prefix
factor
sum
bin
log
comp
assign      (binds loosest)
baseexp
expr

So, the grammar starts with the loosest binding, and each ruleset has
as reference to the next ruleset which binds tighter.
*/

expr:
	assignexp
		{$1}
	;

assignexp:
	  assignexp ASSIGN compexp
	  	{ Assign( $1, $3 ) }
	| assignexp ASSIGNADD compexp
	  	{ Assign( $1, Binop( Add, $1, $3 ) ) }
	| assignexp ASSIGNSUB compexp
	  	{ Assign( $1, Binop( Sub, $1, $3 ) ) }
	| assignexp ASSIGNMUL compexp
	  	{ Assign( $1, Binop( Mul, $1, $3 ) ) }
	| assignexp ASSIGNDIV compexp
	  	{ Assign( $1, Binop( Div, $1, $3 ) ) }
	| assignexp ASSIGNMOD compexp
	  	{ Assign( $1, Binop( Mod, $1, $3 ) ) }
	| compexp
		{ $1 }
	;

compexp:
	  compexp EQ logexp
		{ Binop( Eq, $1, $3 ) } 
	| compexp NEQ logexp
		{ Binop( Neq, $1, $3 ) } 
	| compexp SEQ logexp
		{ Binop( Seq, $1, $3 ) }
	| compexp SNEQ logexp
		{ Binop( SNeq, $1, $3 ) }
	| compexp GT logexp
		{ Binop( Gt, $1, $3 ) }
	| compexp LT logexp
		{ Binop( Lt, $1, $3 ) }
	| compexp GTE logexp
		{ Binop( Gte, $1, $3 ) }
	| compexp LTE logexp
		{ Binop( Lte, $1, $3 ) }
	| logexp
		{ $1 }
	;

logexp:
	  logexp AND binexp
		{ Binop( And, $1, $3 ) }
	| logexp OR binexp
		{ Binop( Or, $1, $3 ) }
	| logexp XOR binexp
		{ Binop( Xor, $1, $3 ) }
	| logexp SHL binexp
		{ Binop( Shl, $1, $3 ) }
	| logexp SHR binexp
		{ Binop( Shr, $1, $3 ) }
	| binexp
		{ $1 }
	;

binexp:
	  binexp BAND sum
		{ Binop( Band, $1, $3 ) } 
	| binexp BOR sum
		{ Binop( Bor, $1, $3 ) } 
	| binexp BXOR sum
		{ Binop( Bxor, $1, $3 ) } 
	| sum
		{ $1 }
	;

sum:
	  sum ADD factor
		{ Binop( Add, $1, $3 ) }
	| sum SUB factor
		{ Binop( Sub, $1, $3 ) }
	| factor
		{ $1 }
	;

factor:
	  factor MUL prefixexp
		{ Binop( Mul, $1, $3 ) }
	| factor DIV prefixexp
		{ Binop( Div, $1, $3 ) }
	| factor MOD prefixexp
		{ Binop( Mod, $1, $3 ) }
	| factor CONCAT prefixexp
		{ Binop( Concat, $1, $3 ) }
	| prefixexp
		{ $1 }
	;

prefixexp:
	  BNOT prefixexp
	  	{ Uniop( Bnot, $2 ) }
	| BCOMP prefixexp
		{ Uniop( Bcomp, $2 ) }
	| NOT prefixexp
		{ Uniop( Not, $2 ) }
	| postfixexp
		{ $1 }
	;


/* XXX:
This creates a shift-reduce conflict with arrays in value.
I think I can live with it; shift is correct.
It usually is.  ^_^
*/
postfixexp:
	  postfixexp LBRACK baseexp RBRACK
	 	{ Binop( Arrayref, $1, $3 ) }
	| postfixexp PERIOD ID
		{ Binop( Varref, $1, String( $3 ) ) }
	| methodcall
		{$1}
	;


/* XXX:
This makes a shift-reduce conflict between shifting as a methodcall
or reducing as a value.  Shifting is, of course, the correct thing.
*/
mcargitem:
	  expr
	  	{ Vardecl( "", "_UNKNOWN", $1 ) }
	| ID COLON expr
		{ Vardecl( $1, "_UNKNOWN", $3 ) }
	;

mcarglst:
	  mcargitem
	  	{ [$1] }
	| mcarglst mcargitem
		{ $1 @ [$2] }
	;

methodcall:
	  LPAREN ID expr RPAREN
		{ Methodcall( $2, $3, [] ) }
	| LPAREN ID expr mcarglst RPAREN
		{ Methodcall( $2, $3, $4 ) }
	| baseexp 
		{ $1 }
	;

baseexp:
	  LPAREN expr RPAREN
		{ $2 }
	| value
		{ $1 }
	;
