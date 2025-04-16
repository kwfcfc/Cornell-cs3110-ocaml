(** Exercise: values *)
let _:int = 7 * (1 + 2 + 3)
(* - : int = 42 *)

let _:string = "CS " ^ string_of_int 3110
(* - : string = "CS 3119" *)

(** Exercise: operators *)
let _ = 42 * 10
let _ = 3.14 /. 2.
let _ = 4.2 ** 7.

(** Exercise: equality *)
let _ = 42 = 42
let _ = "hi" = "hi"
let _ = "hi" == "hi"

(** Exercise: assert *)
(* assert true;;
   - : unit = () *)
(* assert false;;
  Exception: Assert_failure ... *)
let _ = assert (2110 <> 3110)

(** Exercise: if *)
let _ = if 2 > 1 then 42 else 7

(** Exercise: double fun *)
let double x = x * 2

let _ = assert (double 7 = 14)
let _ = assert (double 0 = 0)
let _ = assert (double (-2) = -4)

(** Exercise: more fun *)
let cube x = x *. x *. x

let _ = assert (cube 1.0 = 1.0)
let _ = assert (cube 2.5 = 15.625)

let sign x = if x > 0 then 1 
  else if x == 0 then 0 else -1

let _ = assert (sign 3 = 1)
let _ = assert (sign 0 = 0)
let _ = assert (sign (-6) = -1)

let area r = Float.pi *. r *. r

let close_enough ?(delta = 1e-5) a b = 
  Float.abs (a -. b) < delta

let _ = assert (close_enough (area 1.) Float.pi)
let _ = assert (close_enough (area 2.) (Float.pi *. 4.))
let _ = assert (close_enough (area 1.5) (Float.pi *. 2.25))

(** Exercise: RMS *)
let rms x y = ((x *. x +. y *. y) /. 2.) ** 0.5

let _ = assert (close_enough (rms 7. 1.) 5.)

(** Exercise: date fun*)
let date d m = 
  match m with
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 1 <= d && d <= 31
  | "Apr" | "Jun" | "Sept" | "Nov" -> 1 <= d && d <= 30
  | "Feb" -> 1 <= d && d <= 28
  | _ -> false 

let _ = assert (date 4 "Jun")
let _ = assert ((date (35) "May") <> true)

(** Exercise: fib *)
let rec fib n = 
  match n with
  | 1 -> 1
  | 2 -> 1
  | _  -> fib(n - 1) + fib(n - 2)

let _ = assert ((fib 3) = 2)
let _ = assert ((fib 12) = 144)

let fib_fast n = 
  let rec helper (number:int) (pprevFib:int) (prevFib:int) : int =
    match number with
    | 1 -> prevFib
    | _ -> helper (number - 1) prevFib (pprevFib + prevFib)
  in helper n 0 1

let _ = assert ((fib_fast 15) = 610)

(** Exercise: divide *)
let divide numerator denominator = 
  if denominator = 0. then failwith "Zero denominator error"
  else numerator /. denominator

let _ = assert (close_enough (divide 3.2 2.0) 1.6)

(** Exercise: associativity *)
let add x y = x + y

(* an integer of the application *)
let _integer1 = assert ((add 5 1) = 6)

(* a function (int -> int) that takes in an integer and add 5 to it *)
let _funtion = (add 5)

(* apply the add5 function *)
let _funtion_applied = (add 5) 1

(** an error, as 5 cannot apply to 1
let _error = add (5 1) *)

(** Exercise: average *)
let ( +/. ) x y = (x +. y) /. 2.

let _ = assert (close_enough (3.2 +/. 9.6) 6.4)

(** Exercise: hello world *)
let () = print_endline "Hello world!"

(* this print will not start a new line*)
let () = print_string "Hello world!"