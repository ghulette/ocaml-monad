module type S = sig

  (** Monad functor type *)
  type 'a t

  (** {1 Basic definitions} *)

  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** {1 Infix operators} *)

  (** Infix {!map}. *)
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t

  (** Infix {!map} with arguments reversed. *)
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  (** Infix {!apply}. *)
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

  (** Infix {!bind}. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** Infix {!bind} but ignoring the first result. *)
  val (>>) : 'a t -> 'b t -> 'b t

  (** {1 Derived functions} *)

  val join : 'a t t -> 'a t

  val sequence : 'a t list -> 'a list t

  (** In Haskell this is called [mapM]. *)
  val sequence_map : ('a -> 'b t) -> 'a list -> 'b list t

  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  val filter : ('a -> bool t) -> 'a list -> 'a list t

  val product : 'a t -> 'b t -> ('a * 'b) t

  (** Binding operators *)
  module Syntax : sig

    (** Binding operator for {!bind}. *)
    val ( let* ) : 'a t -> ('a -> 'b t ) -> 'b t

    (** Binding operator for {!product}. *)
    val ( and* ) : 'a t -> 'b t-> ('a * 'b) t (* product *)

    (** Binding operator for {!map}. *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t (* fmap *)

    (** Binding operator for {!product}. *)
    val ( and+ ) : 'a t -> 'b t-> ('a * 'b) t (* product *)
  end
end

module type Exists = sig
  type t
end

module Option : S with type 'a t := 'a option
module List : S with type 'a t := 'a list
module Seq : S with type 'a t := 'a Seq.t
module Lazy : S with type 'a t := 'a Lazy.t

module Result (Err : Exists) : sig
  include S with type 'a t := ('a, Err.t) result
  val throw : Err.t -> ('a, Err.t) result
  val catch : ('a, Err.t) result -> (Err.t -> ('a, Err.t) result) -> ('a, Err.t) result
end
