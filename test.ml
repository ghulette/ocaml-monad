module OptionMonad = Monad.Make(Option_monad.Def)
open OptionMonad

let from_some = function
  | Some x -> x
  | None -> raise Not_found
;;

let string_of_int_option = function
  | Some x -> "Some " ^ string_of_int x
  | None -> "None"
;;

let mx = pure 5 >>= fun x -> pure (x + 1) in
let x = from_some mx in
print_int x;
print_newline ();;

let xms = [Some 1; Some 2; Some 3] in
let mxs = sequence xms in
let xs = from_some mxs in
print_string (String.concat "," (List.map string_of_int xs));
print_newline ();;

let mmx = Some (None) in
let mx = join mmx in
print_string (string_of_int_option mx);
print_newline ();;
