module Infer.ConstraintGen exposing (..)

import Dict exposing (Dict)
import Infer.Expression exposing (Expression(..))
import Infer.Monad exposing (..)
import Infer.Scheme exposing (Environment, Scheme, freshTypevar, generalize, instantiate)
import Infer.Type as Type exposing (($), Type(..))


type alias Constraint =
    ( Type, Type )


variable : Environment -> String -> Monad Type
variable env name =
    Dict.get name env
        |> Result.fromMaybe ("variable " ++ name ++ " not found")
        |> fromResult
        |> andThen instantiate


extendGeneralized : Environment -> String -> Type -> Environment
extendGeneralized environment name t =
    Dict.insert name (generalize environment t) environment


extend : Environment -> String -> Type -> Environment
extend environment name t =
    Dict.insert name ( [], t ) environment
