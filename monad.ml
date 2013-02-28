module type MonadDef = sig
  type 'a t
  val pure : 'a -> 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  include MonadDef
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t
  val sequence : ('a t) list -> ('a list) t
  val map : ('a -> 'b t) -> 'a list -> ('b list) t
  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  val onlyif : bool -> unit t -> unit t
end

module Make (M : MonadDef) : (Monad with type 'a t = 'a M.t) = struct
  include M

  let (>>=) = bind

  let join mmx = 
    mmx >>= fun mx -> (mx >>= fun x -> pure x)

  let rec sequence = function
    | [] -> pure []
    | m::ms -> begin
        m >>= fun x ->
        sequence ms >>= fun xs ->
        pure (x::xs)
      end

  let map f xs = 
    sequence (List.map f xs)

  let rec fold f x = function
    | [] -> pure x
    | y::ys -> begin
        f x y >>= fun x' ->
        fold f x' ys
      end

  let onlyif b m = if b then m else pure ()
end
