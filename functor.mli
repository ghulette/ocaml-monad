module type S = sig 
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module List : S with type 'a t = 'a list
module Option : S with type 'a t = 'a option
module Lazy : S with type 'a t = 'a Lazy.t
