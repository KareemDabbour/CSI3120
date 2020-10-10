(* Goal for this lab: more practice with Ocaml, in particular,
   with regard to recursion, higher order functions, and options *)

(* 1a. Write a function that takes a list of boolean values
   [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
   For simplicity, assume and_list [] is TRUE. *)


let rec and_list (lst: bool list) : bool =
 match lst with
   | [] -> true
   | hd::tl -> hd && and_list tl

(* 1b. Do the same as above, with OR.
   Assume or_list [] is FALSE. *)


let rec or_list (lst: bool list) : bool = 
match lst with
   | [] -> false
   | hd::tl -> hd || or_list tl


(* 2. The functions and_option, or_option, and calc_option below are
   possible solutions to optional questions 7 and 8 in Lab 2.  You are
   asked to implement new versions of and_option and or_option using
   calc_option.  Note that this is a minor variation of questions from
   Lab 2.  In particular, using calc_option, write a function to
   return the boolean AND/OR of two bool options, or None if both are
   None. If exactly one is None, return the other. *)

let and_option (x:bool option) (y: bool option) : bool option = 
  match x with
  | Some a -> (match y with
               | Some b -> if a then y else Some false
               | None -> x)
  | None -> y
           
let or_option (x:bool option) (y: bool option) : bool option = 
  match x with
  | Some a -> (match y with
               | Some b -> if a then Some true else y
               | None -> x)
  | None -> y

let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
  match x with
  | Some a -> (match y with
               | Some b -> Some (f a b)
               | None -> x)
  | None -> y


let op1 = Some true;;
let op2 = Some false;;
let op3 = None;;

let and_option' (x:bool option) (y: bool option) : bool option =
   calc_option (fun a b -> a && b) x y;;

let or_option' (x:bool option) (y: bool option) : bool option =
   calc_option (fun a b -> a || b) x y;;


(* 3. The following code is a possible solution to optional question 9 in Lab 2. *)

let min (a:int) (b:int) : int = if a < b then a else b
let max (a:int) (b:int) : int = if a < b then b else a

let min_option2 (x: int option) (y: int option) : int option = 
  calc_option min x y
    
let max_option2 (x: int option) (y: int option) : int option = 
  calc_option max x y
                  
(* Write a recursive function that returns the max of a list, or None
   if the list is empty. You may use the code above but you don't have
   to. *)


let rec max_of_list (lst:int list) : int option =
match lst with 
| [] -> None
| head::[] -> Some(head)
| f::s::tl -> if f>s then max_of_list(f::tl) else max_of_list(s::tl);;


(* 4. In the following exercises, we will use map (as seen in class)
   to implement some functions. *)

let rec map (f:'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)

(* 4a. Write a function that takes an int list and multiplies every
   int by 3.  Use map. *)


let times_3 (lst: int list): int list = 
   map (fun x -> x * 3) lst;;

(* 4b. Write a function that takes an int list and an int and
   multiplies every entry in the list by the int. Use map. *)

let times_x (x: int) (lst: int list) : int list =
   map (fun a -> a * x) lst;;

(* 4c. Rewrite times_3 in terms of times_x.  This should take very
   little code. *)

            
let times_3_shorter = times_x 3;;
 


(* 5. Consider the following higher-order function reduce *)

let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
  | [] -> u
  | hd::tl -> f hd (reduce f u tl);;

(* Consider the following functions defined using reduce *)

let sum xs = reduce (fun x y -> x+y) 0 xs
let prod xs = reduce (fun x y -> x*y) 1 xs

(* What do the sum and prod functions do?  What does the reduce
   function do?  Trace the following code to help figure this out.
   Show your trace and provide your explanation here:

ANSWER HERE

reduce:
The function reduce is an accumulator that applies a function to the elements in a list that is passed in.

sum:
The function sum gives a list to function reduce and provides an initial val of 0. It then adds each element in the list to the prev elememt. The result would then be a single value.
stack trace:  
List is: [2;5;6]
   reduce is called with [2;5;6] 0
   The list is not empty so it calls reduce with the values [5;6] 0 
   The list is not empty so it calls reduce with the values [6] 0
   The list is now empty so it returns the initial value of 0, and works itself up the stack:
   0 --> (6 + 0) = 6 --> (5 + 6) = 11 -> (2 + 11)  = 13  

 Prod: 
 Thefucntion prod gives a list to function reduce and provides an initual value of 1. It then multiplies each element to the previous element. The result would be a single value.
   List is: [2;5;6]
   reduce is called with [2;5;6] 1
   The list is not empty so it calls reduce with values [5;6] 1
   The list is not empty so it calls reduce with values [6] 1
   The list is now empty so it returns the initial value of 0, and works itself up the stack:
   1 --> (6 * 1) = 6 --> (5 * 6) = 30 -> 2 *  = 60
 *)

let mysum = sum [2;5;6]
let myprod = prod [2;5;6]


