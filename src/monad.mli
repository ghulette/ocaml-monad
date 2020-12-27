module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (* Infix operators *)
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t (* fmap *)
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t (* fmap with args reversed *)
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t (* apply *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t (* bind *)
  val (>>) : 'a t -> 'b t -> 'b t (* bind and ignore first result *)

  val join : 'a t t -> 'a t
  val sequence : 'a t list -> 'a list t
  val sequence_map : ('a -> 'b t) -> 'a list -> 'b list t
  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
  val filter : ('a -> bool t) -> 'a list -> 'a list t
  val product : 'a t -> 'b t -> ('a * 'b) t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t ) -> 'b t (* bind *)
    val ( and* ) : 'a t -> 'b t-> ('a * 'b) t (* product *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t (* fmap *)
    val ( and+ ) : 'a t -> 'b t-> ('a * 'b) t (* product *)
  end
end

module type Exists = sig
  type t
end

module Option : S with type 'a t = 'a option
module List : S with type 'a t = 'a list
module Seq : S with type 'a t = 'a Seq.t
module Lazy : S with type 'a t = 'a Lazy.t

module Result (Err : Exists) : sig
  include S with type 'a t = ('a, Err.t) result
  val throw : Err.t -> ('a, Err.t) result
  val catch : ('a, Err.t) result -> (Err.t -> ('a, Err.t) result) -> ('a, Err.t) result
end
