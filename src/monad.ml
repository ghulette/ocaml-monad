module type MonadDef = sig
  type 'a t
  val return : 'a -> 'a t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val (<*>)  : ('a -> 'b) t -> 'a t -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t
  val sequence : 'a t list -> 'a list t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  val filter : ('a -> bool t) -> 'a list -> 'a list t

  val product : 'a t -> 'b t -> ('a * 'b) t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t ) -> 'b t (* bind *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t (* fmap *)
    val ( and+ ) : 'a t -> 'b t-> ('a * 'b) t (* product *)
  end
end

module MonadF(M : MonadDef) = struct
  include M

  let fmap = (<$>)
  let bind = (>>=)
  let apply = (<*>)

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

  let rec filter p = function
    | [] -> return []
    | x::xs ->
       p x >>= fun flg ->
       filter p xs >>= fun xs' ->
       return (if flg then x::xs' else xs')

  let product xa ya = return (fun x y -> (x, y)) <*> xa <*> ya

  module Syntax = struct
    let ( let+ ) x f = fmap f x
    let ( and+ ) o1 o2 = product o1 o2
    let ( let* ) x f = bind x f
  end
end

module OptionDef = struct
  type 'a t = 'a option

  let return x = Some x

  let (<$>) f = function
    | None -> None
    | Some x -> Some (f x)

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

  let (<*>) fo xo =
    match fo, xo with
    | Some f, Some x  -> Some (f x)
    | _               -> None
end

module Option = MonadF(OptionDef)

module ListDef = struct
  type 'a t = 'a list
  let return x = [x]
  let (<$>) = List.map
  let (>>=) m f = List.concat_map f m

  let (<*>) fs xs =
    fs >>= fun f ->
    xs >>= fun x ->
    return (f x)
end

module List = MonadF(ListDef)

module LazyDef = struct
  type 'a t = 'a Lazy.t
  let return x = lazy x
  let (<$>) f x = lazy (f (Lazy.force x))
  let (>>=) m f = lazy (Lazy.force (f (Lazy.force m)))
  let (<*>) lf lx =
    lf >>= fun f ->
    lx >>= fun x ->
    return (f x)
end

module Lazy = MonadF(LazyDef)

module type Exist = sig
  type t
end

module ResultDef (Err : Exist) = struct
  type 'a t = ('a, Err.t) result

  let return x = Ok x

  let (<$>) f m =
    match m with
    | Ok x -> Ok (f x)
    | Error e -> Error e

  let (>>=) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e

  let (<*>) lf lx =
    lf >>= fun f ->
    lx >>= fun x ->
    return (f x)

end

module Result(Err:Exist) = struct
  include MonadF(ResultDef(Err))

  let throw err = Error err

  let catch m f =
    match m with
    | Ok x -> Ok x
    | Error err -> f err
end

module SeqDef = struct
  type 'a t = 'a Seq.t

  let return x = Seq.return x
  let (<$>) f m = Seq.map f m

  let (>>=) m f = Seq.flat_map f m

  let (<*>) lf lx =
    lf >>= fun f ->
    lx >>= fun x ->
    return (f x)
end

module Seq = MonadF(SeqDef)
