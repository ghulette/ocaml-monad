module type S = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Make (M : S) : sig
  val join : 'a M.t M.t -> 'a M.t
  val sequence : 'a M.t list -> 'a list M.t
  val map : ('a -> 'b M.t) -> 'a list -> 'b list M.t
  val fold : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> 'a M.t
  val filter : ('a -> bool M.t) -> 'a list -> 'a list M.t
end

module Option : S with type 'a t = 'a option
module List : S with type 'a t = 'a list
module Lazy : S with type 'a t = 'a Lazy.t
