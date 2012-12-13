(* runtime.ml
   The Objective runtime.
   It's really quite similar to Objective C, except without the C...
   Kinda Smalltalk-ish then, with hints of Lisp and Python.

   Simon Heath
   15/05/2004
*)

(* NOT everything is a methodcall, but MOST things are, including 
   non-optimized mathematical operations.
   The way methodcalls work is thus:
   
   Each instance has a vector representing its instance variables,
   and a reference to a class object.
   Each class object has a hashtable method definitions, a hashtable of class
   methods, and superclass reference.  Interfaces have no existance at runtime.
   When a method is called, the class object is grabbed, and checked to
   see whether the method definition exists.  If it does, it's called.
   If not, the superclass is checked until it hits the top of the object
   tree, then an exception is thrown.  If a method is found in a superclass,
   a reference to it is added in the called object's class (caching).

   Naming conventions on types go thus:
   Foo for an instance of Foo
   FooClass for foo's class.  This is an instance of type Class.

   Object creation conventions go thus:
   All Class objects are instantiated when the program starts.   
   All constants and globals are initialized.
   When a new instance of a class is created, (new *Class ...) is called,
   which creates an object and sends it the 'init' message, then returns it.

   The naming conventions for methods go "Package_Class_methodName".
   The naming conventions for globals/consts go "Package_name"

   When an object is GC'ed, the 'fini' message is sent to it with no args.
   XXX: Can I actually implement this at the moment?

   XXX: Work on my own exception system instead of using Ocaml's?


   The Object object should have the following methods (kinda a kitchen
   sink, but oh well):
   - init
   - fini
   - copy
   - deepCopy
   - class
   - superClass
   - understands
   - implements
   - eq
   - isKind
   - methodFor
   - perform
   - doesNotUnderstand
   - error 
   The Class object should have the following methods:
   - new
   - name
   - superClass
   - understands
   - implements
   The Interface object should have the following methods:
   - name
   - conformsTo
   - superInterface
   - understands
   

   An internal class object needs to have:
   name
   super
   instance var num
   class methods
   instance methods
   interfaces

   An internal interface object needs to have:
   name
   selector list
   super
  
   An instance object needs to have:
   class
   super
   instance var array
   instance method ref

   ...Let's not bother with interfaces at the moment...  o_O

   Methods are functions of type
   instance -> instance array -> instance

   Grr.  It's tricky.  Issues:
   What knows methods?
   What knows class types?
   Where are class methods kept?
   How are classes instances as well?
   What knows superclass?

   In objc, it works thus:
   instance has instance vars and an "isa" pointer.
   isa points to a class object, which has methods and a superclass pointer.
   
*)

exception RuntimeException of string;;


(* All the appropriate types, hopefully... *)
type primative = 
    Bool of bool
  | Float of float
  | Int of int
  | Char of char
  | String of string
  | Instance of instanceType
  | Nil

and methodfunc = instanceType -> primative array -> primative

(* 'Tis kinda a metaclass type... *)
and classType = {
  mutable cname : string;
  mutable csuper : classType;
  mutable cinstanceVarNum : int;
  mutable cmethods : (string, methodfunc) Hashtbl.t;
(*  mutable cmethods : methodfunc array; *)
(*  mutable cinterfaces : interfaceType array; *)
}

(* Not used yet...  Get it working first.
and interfaceType = {
  mutable iname : string;
  mutable imethods : string array;
  mutable isuper : interfaceType;
}
*)

and instanceType = {
  mutable ncls : classType;
  mutable nvars : primative array;
}

;;

(* The class hashtable *)
let (classes : (string, classType) Hashtbl.t) = 
  Hashtbl.create 256;;
(*let (interfaces : (string, interfaceType) Hashtbl.t) = 
  Hashtbl.create 64;;
*)

let registerMethod name meth cls = 
  Hashtbl.add cls.cmethods name meth
;;


(* The root of all evil... erm, objects, even.  *)
let rec nilClass = {
  cname = "Nil";
  csuper = nilClass;
  cinstanceVarNum = 0;
  cmethods = Hashtbl.create 1;
}
;;

let nilClassInit _ _ =
  Nil
;;

registerMethod "init" nilClassInit nilClass;;


(* Take a class, and find out if it understands msg. *)
let classUnderstands cls msg =
  Hashtbl.mem cls.cmethods msg
;;

(* Take an instance, grab it's class, find a method, and call it. *)
let callMethod msg ins cls args =
  (* Cache the method *)
  let meth = Hashtbl.find cls.cmethods msg in
    if not (Hashtbl.mem ins.ncls.cmethods msg) then
      Hashtbl.add ins.ncls.cmethods msg meth;
    (* Call the method *)
    meth ins args
;;


let rec sendMessageToClass message ins cls args =
  if classUnderstands cls message then
    callMethod message ins cls args
  else if cls = nilClass then
    raise 
      (RuntimeException 
	 (Printf.sprintf 
	    "Class %s does not understand message %s" cls.cname message))
  else
    sendMessageToClass message ins cls.csuper args
;;

let sendMessage message instance args =
  sendMessageToClass message instance instance.ncls args
;;

let createInstance cls = 
  let ins = {
  ncls = cls;
  nvars = Array.make cls.cinstanceVarNum Nil } in
    sendMessage "init" ins [||];
    ins
;;



(* Testing stuff... *)



let testClass = {
  cname = "Test";
  csuper = nilClass;
  cinstanceVarNum = 0;
  cmethods = Hashtbl.create 2;
};;

let printIntMethod (ins : instanceType) args =
  match args.(0) with
      Int a  -> print_int a; Nil
    | _ -> Nil;

;;

let testClassInit (ins : instanceType) (args : primative array) =
  Nil
;;



registerMethod "init" testClassInit;;
registerMethod "printInt" printIntMethod testClass;;


let testobj = createInstance testClass;;

let testClass2 = {
  cname = "Test2";
  csuper = testClass;
  cinstanceVarNum = 1;
  cmethods = Hashtbl.create 2;
};;

let testClass2Init (ins : instanceType) (args : primative array) =
  ins.nvars.(0) <- Int( 5 );
  Nil
;;

let addMethod ins args =
  match ins.nvars.(0) with
      Int a ->
	(match args.(0) with
	    Int b -> Int( a + b )
	  | _ -> Nil)
    | _ -> Nil
;;

registerMethod "init" testClass2Init testClass2;;
registerMethod "add" addMethod testClass2;;

let testobj2 = createInstance testClass2;;

let rgs = [| Int( 10 ) |];;
	      

  
  










(*
type methodtype = {
  (* The selector is the hashed value of the method's name. *)
  (* XXX: Hashtbl doesn't take pure selectors... Ah well, can't hurt.  *)
  mutable sel : int;
  mutable args : int;
  mutable vararg : bool;
  mutable func : instance array -> instance;
}
  
and classtype = {
  mutable name : string;
  mutable super : classtype;
  (* We don't differentiate the types of instance vars in the runtime because
     one way or another the compiler's already handled that.
  *)
  mutable insvarnum : int;  
  mutable insmethods : (int, methodtype) Hashtbl.t;
  mutable classmethods : (int, methodtype) Hashtbl.t;
}

and instance = {
  mutable cls : classtype;
  mutable insvars : instance array;
}


(* Nil class --fundamental root of everything.
   Money is the root of all evil, Nil is the root of all objects.
   Nil is defined as:
class Nil:
   - notUnderstand (nm : String):
      raise RuntimeException "Does not understand ~A" nm
   - init:
      pass   
   + new:
      pass
*)
let rec _classNil = {
  name = "Nil";
  super = _classNil;
  insvarnum = 0;
  insmethods = Hashtbl.create 2;
  classmethods = Hashtbl.create 2;
}

(* The initializing and such are a bit wiggy...
   XXX: A class object should be an instance!!!!!
*)
(*
let rec _classNil_new 

let rec _classNil_init cls =
  Hashtbl.add cls.methods (Hashtbl.hash _classNil_init) _classNil_init;
  Hashtbl.add cls.methods (Hashtbl.hash _classNil_new) _classNil_new;
  Hashtbl.add cls.methods (Hashtbl.hash _classNil_notUnderstahd) 
    _classNil_notUnderstand;
;;
*)

let sendMsg inst msg args =
  let sel = Hashtbl.hash msg
  and  cls = inst.cls in
  let rec findmethod cls sel  =
    if not (Hashtbl.mem cls.insmethods sel) then
      if cls = _classNil then
	raise (RuntimeException 
		 (Printf.sprintf "Message %s is not understood!\n" msg ))
      else
	findmethod cls.super sel
    else
      (* We return a pair of a method and a reference to the class that had it
      *)
      ((Hashtbl.find cls.insmethods sel), cls)
  in
    (* We search for the method; if we find it, we cache it and call it. *)
  let mt, cs = findmethod cls sel in
    if cs != cls then
      if not (Hashtbl.mem cls.insmethods sel) then
	Hashtbl.add cls.insmethods sel mt;
    mt.func args
;;
    
	

*)
