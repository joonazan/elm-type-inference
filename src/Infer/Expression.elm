module Infer.Expression exposing (..)

{-|


#

@docs Expression

-}

import Infer.Type exposing (Type)


{-| Translate your expressions to this type in order to be able to perform type inference on them.
-}
type Expression
    = Lambda String Expression
    | Call Expression Expression
    | Name String
    | Literal Type
    | Let String Expression Expression
    | If Expression Expression Expression
