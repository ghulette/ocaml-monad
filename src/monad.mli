module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t

  val join : 'a t t -> 'a t
  val sequence : 'a t list -> 'a list t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
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

module type EX = sig
  type t
end

module Option : S with type 'a t = 'a option
module List : S with type 'a t = 'a list
module Seq : S with type 'a t = 'a Seq.t
module Lazy : S with type 'a t = 'a Lazy.t

module Result (Err : EX) : sig
  include S with type 'a t = ('a, Err.t) result
  val throw : Err.t -> ('a, Err.t) result
  val catch : ('a, Err.t) result -> (Err.t -> ('a, Err.t) result) -> ('a, Err.t) result
end

module Reader (Env : EX) : sig
  include S with type 'a t = Env.t -> 'a
  val get : Env.t t
end
