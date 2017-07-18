module Infer.Monad exposing (..)

{-|


# Construction

@docs pure, err, Monad, fromResult


# Mapping

@docs andThen, map, map2, map3, map4, andMap


# Combining

@docs combine, finalValue

-}

import State exposing (State)


{-| Represents a stateful computation that can fail.
-}
type alias Monad a =
    State Int (Result String a)


{-| Put a value into the monad. Will not advance the fresh name supply nor cause an error.
-}
pure : a -> Monad a
pure x =
    State.state (Ok x)


{-| Represents a failed computation.
-}
err : String -> Monad a
err e =
    State.state (Err e)


{-| Un-specialize a Result.
-}
fromResult : Result String a -> Monad a
fromResult res =
    State.state res


{-| `map` for this particular monad.
-}
map : (a -> value) -> Monad a -> Monad value
map f =
    State.map (Result.map f)


{-| `andThen` for this particular monad.
-}
andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f =
    State.andThen
        (\r ->
            case r of
                Ok v ->
                    f v

                Err e ->
                    State.state <| Err e
        )


{-| map over one more value. Arguments are reversed in order to work
well with `<|`.
-}
andMap : Monad y -> Monad (y -> z) -> Monad z
andMap y =
    andThen (\g -> map g y)


{-| `map2` for this particular monad.
-}
map2 : (a -> b -> c) -> Monad a -> Monad b -> Monad c
map2 f x y =
    map f x
        |> andMap y


{-| `map3` for this particular monad.
-}
map3 : (a -> b -> c -> d) -> Monad a -> Monad b -> Monad c -> Monad d
map3 f a b c =
    map2 f a b
        |> andMap c


{-| `map4` for this particular monad.
-}
map4 : (a -> b -> c -> d -> e) -> Monad a -> Monad b -> Monad c -> Monad d -> Monad e
map4 f a b c d =
    map3 f a b c
        |> andMap d


{-| Lifts the monads out of a list.
-}
combine : List (Monad a) -> Monad (List a)
combine =
    List.foldr (map2 (::)) (pure [])


{-| Computes the value of a computation.
-}
finalValue : Int -> Monad a -> Result String a
finalValue =
    State.finalValue
