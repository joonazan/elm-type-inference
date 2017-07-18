module Infer.Expression exposing (..)

{-|


#

@docs Expression

-}

import Infer.Type exposing (Type)


{-| Translate your expressions to this type in order to be able to perform type inference on them.
The Spy variant has no effect on type inference, but can be used to find the type of a subexpression.
-}
type Expression
    = Literal Type
    | Lambda String Expression
    | Call Expression Expression
    | Let String Expression Expression
    | Name String
    | Spy Expression Int
