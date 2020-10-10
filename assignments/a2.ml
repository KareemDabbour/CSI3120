(*** CSI 3120 Assignment 2 ***)
(*** Kareem Dabbour ***)
(*** 300082990 ***)
(*** OCAML VERSION 4.05.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)


let exp1a : string = "The orginal declaration had this as a list(using ;) of different types where it was expecting a list of tuples of type '(string * int * char)'. 
                     This is easily fixed by switching the ';' with a ',' and encapsulating the three elements in brackets."
let prob1a : (string * int * char) list = [("7", 8, '9')];;


(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)


let exp1b : string = "The original type of variable that was being used wanted a list of tuples of type '(string * int)'.
                      A tuple containing a single element of each type was expected. This is opposing the list being declared as it is a tuple of lists.
                      A fix for this was to change the type to a tuple of a list of strings paired with a list of ints."

let prob1b : (string list * int list) = (["apples";"bananas";"carrots"],[3;2;1]);;


(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.  (Do not change the left-hand-side.)
 *)


let exp1c : string = "To create a 2D list in ocaml, you have to concatinate (starting from an empty list) all elements such that proper nesting is acheived.
                      To acheive this in ocaml, '::' is used to concate elements from on list to the front of another list."

let prob1c : string list list = ["2"; "b"] :: ["or"; "not"; "2b"] :: ["that is"; "the"] :: ["question"] :: [];;


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problem 2a *)

let prob2a : (int * ((string * float) option list)) list =
[(7, [Some ("Kareem", 69.); Some ("Muna",54.)]); (3, [Some ("Yaser", 9.); Some ("Noor", 89.)])];;

(* Problem 2b *)
(* a pet is a (name, animal_type, age option) tuple *)

type pet = string * string * int option


let prob2b : string * pet list option =
("string", Some[("Fido", "Dog", Some 6); ("Fluffy", "Cat", Some 3); ("Huff", "Lion", Some 8)]);;


(* Problem 2c *)
(* Fill in a valid function call to f to make prob2c typecheck *)


let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b ^ (f xs)) else f xs
    | _ -> ""
  in f[(true, "arg1"); (false, "arg2"); (false, "arg3"); (true, "arg4")];;



(*************)
(* PROBLEM 3 *)
(*************)

(* Problem 3a.  You have been asked to write a text filter,
   where you want to find all search characters in your text
   if they appear the right order.

   Write a function text_filter that takes two lists of characters
   and checks to see if all the characters in the first list are
   included in the second list AND in the same order, BUT possibly
   with other characters in between.  For example

   text_filter ['a';'m';'z'] ['1';'a';'2';'m';'3';'z'] = true
   text_filter ['a';'m';'z'] ['1';'a';'3';'z'] = false
   text_filter ['a';'m';'z'] ['1';'z';'2';'m';'3';'a'] = false

let rec text_filter (xs:char list) (ys:char list) : bool =
 *)

 let rec text_filter (xs:char list) (ys:char list) : bool =
   match xs, ys with
   | _, [] -> false
   | [], _ -> true
   |x_h::x_t, y_h::y_t -> if x_h = y_h then text_filter x_t y_t else text_filter xs y_t;;


(* Problem 3b. Rewrite the function above so that is is polymorphic,
   i.e., it should work on lists whose elements are any types.  Give
   at least one test case (call your function at least once) with a
   type that is different from chars. *)
   let rec any_list_filter (xs:'a list) (ys:'a list) : bool =
      match xs, ys with
      | _, [] -> false
      | [], _ -> true
      |x_h::x_t, y_h::y_t -> if x_h = y_h then any_list_filter x_t y_t else any_list_filter xs y_t;;

(* should return true *)
any_list_filter [0;2;3;4] [1;0;5;2;5;37;3;4];;  

(* should return false *)
any_list_filter [0;2;4;6;8] [5;3;34;534;9];;

(*************)
(* PROBLEM 4 *)
(*************)

(* Write a function (int_to_whole) that converts an integer
   into a whole number if one exists
   (a whole number is 1, 2, 3, ...).
   Use an option type because not all integer inputs can
   be converted. *)

type whole = One | Succ of whole

let rec int_to_whole (i:int) : (whole option) = 
   match i > 0 with
   | false -> None
   | true -> match i = 1 with 
            | true -> Some One
            | false -> match int_to_whole (i -1) with 
                      | Some num -> Some (Succ num)
                      | None -> None;;
