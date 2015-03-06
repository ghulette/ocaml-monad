module type S = sig
  include Functor.S
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Make (M : S) = struct
  open M

  let join mmx = 
    mmx >>= fun mx -> 
    mx >>= fun x -> 
    return x

  let rec sequence = function
    | [] -> return []
    | m::ms ->
       m >>= fun x ->
       sequence ms >>= fun xs ->
       return (x::xs)

  let map f xs = 
    sequence (List.map f xs)

  let rec fold f x = function
    | [] -> return x
    | y::ys ->
       f x y >>= fun x' ->
       fold f x' ys
end

module Failure = struct
  include Functor.Option
  let return x = Some x
  let (>>=) m f = 
    match m with
    | Some x -> f x
    | None -> None
end

module Nondet = struct
  include Functor.List
  let return x = [x]
  let (>>=) m f = List.map f m |> List.concat
end

module Lazy = struct
  include Functor.Lazy
  let return x = lazy x
  let (>>=) m f = Lazy.force m |> f
end
