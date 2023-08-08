module Specification exposing (Specification, fromPredicate)


type alias Specification e a =
    Result e a -> Result e a


fromPredicate : e -> (a -> Bool) -> Specification e a
fromPredicate error predicate source =
    let
        filter : a -> Result e a
        filter a =
            if predicate a then
                Ok a

            else
                Err error
    in
    source
        |> Result.andThen filter
