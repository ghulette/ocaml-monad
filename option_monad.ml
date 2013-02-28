module Def : (Monad.MonadDef with type 'a t = 'a option) = struct
  type 'a t = 'a option

  let pure x = Some x

  let fmap f = function
    | Some x -> Some (f x)
    | None -> None

  let bind x f = match x with
    | Some x -> f x
    | None -> None
end
