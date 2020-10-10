(* 

  Agenda:
   * tuples and lists
   * options
   * higher order functions

  Note that questions 2, 8, and 9 are optional.

*)

(* An employee is a tuple of name, age, and boolean indicating marriage status *)
type employee = string * int * bool
                                 
(* 1. Write a function that takes an employee, and prints out the information in some readable form. *)


let print_employee_info (t:employee) = 
   let (n, age, mar) = t in 
   (
      Printf.printf "Name: %s\n" n;
      Printf.printf "Age: %d\n" age;
      print_string("Status of marriage: " ^ (if mar then "Married" else "Single"))
   )
 

(* 2. Reimplement the OCaml standard functions List.length and List.rev.
   This question is optional, but is good practice for the next one. *)


let rec length (l:'a list) : int = 
   match l with
   | [] -> 0
   | hd:: t1 -> length t1 + 1


let rec rev (l:'a list) : 'a list = 
   match l with
   | [] -> []
   | hd :: t1 -> (rev t1) @ [hd]

(* 3. Remove the kth element from a list. Assume indexing from 0 *)
(* example: rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] 
 * results in: [["to" ; "be" ; "not" ; "to" ; "be"] *)
 let rec rmk (k:int) (l:'a list) : 'a list =  
   match l with
   | [] -> []
   |hd :: t1 -> if k = 0 then t1 else hd :: rmk (k-1) t1;;

(* 4. Write a function that returns the final element of a list, 
   if it exists, and None otherwise *)

let rec final (l: 'a list) : 'a option = 
   match l with
   | [] -> None
   | [x] -> Some x
   | hd::t1 -> final t1;;


(* 5. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. Do 
 * the same for the larger of two int options.*)


let min_option (x: int option) (y: int option) : int option = 
   match x, y with
   | Some x, Some y -> if x < y then Some x else Some y
   | _, Some y -> Some y
   | Some x, None -> Some x 
   | None, None -> None 

let max_option (x: int option) (y: int option) : int option = 
   match x, y with
   | Some x, Some y -> if y > x then Some x else Some y
   | _, Some y -> Some y 
   | None, None -> None
   | Some x, None -> Some x


(* 6. Write a function that returns the integer buried in the argument
 * or None otherwise *)  

let get_option (x: int option option option option) : int option = 
   match x with 
   | Some x1 -> (match x1 with
      | Some x2 -> (match x2 with
         | Some x3 -> (match x3 with
            | None -> None
            | Some res -> Some res
            )
         | None -> None)
      | None -> None)
   | None -> None

(* 7. Write a function to return the boolean AND/OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)


let and_option (x:bool option) (y: bool option) : bool option = 
   match x, y with 
   | Some x, None -> Some x
   | Some x, Some y -> Some (y && x) 
   | _, Some y -> Some y
   | None, None -> None

let or_option (x:bool option) (y: bool option) : bool option = 
   match x, y with 
   | Some x, None -> Some x
   | Some x, Some y -> Some (y || x) 
   | _, Some y -> Some y
   | None, None -> None

(* What's the pattern? How can we factor out similar code? *)

(* 8. Optional (but important for preparation for next week's lab):
 * Write a higher-order function for binary operations on options.
 * If both arguments are present, then apply the operation.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
(* What is the type of the calc_option function? *)


let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
   match x, y with
   | None, None -> None
   | None, _ -> y
   | _, None -> x
   | Some r, Some r1 -> Some (f r r1);;

(* 9. Optional (but important for preparation for next week's lab):
 * Now rewrite the following functions using the above higher-order function
 * Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other.
 * Do the same for the larger of two int options. *)


let min_option2 (x: int option) (y: int option) : int option = 
let min r r1 = if r < r1 then r else r1 in 
(calc_option min x y);;

let max_option2 (x: int option) (y: int option) : int option = 
   let min r r1 = if r < r1 then r1 else r in 
   (calc_option min x y);;
