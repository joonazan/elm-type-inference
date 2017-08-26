module Infer exposing (typeOf)

{-| This is the module implementing type inference. You'll also need at least `Infer.Expression`.
@docs typeOf
-}

import Infer.Expression exposing (Expression)
import Infer.ConstraintGen exposing (Constraint, generateConstraints)
import Infer.Scheme exposing (Environment)
import Infer.Type as Type exposing (Substitution, ($), Type)
import Infer.Monad as Infer
import Dict


types : Environment -> Expression -> Int -> Bool
types env exp s =
    typeOf env exp
        |> Infer.finalValue s
        |> Result.map (always True)
        |> Result.withDefault False


{-| Returns a computation that yields the type of the input expression
with the specified environment.
-}
typeOf : Environment -> Expression -> Infer.Monad ( Type, Type -> Type )
typeOf env exp =
    generateConstraints env exp
        |> Infer.andThen
            (\( t, cs ) ->
                solve Dict.empty cs
                    |> Result.map (\s -> ( Type.substitute s t, Type.substitute s ))
                    |> Infer.fromResult
            )


solve : Substitution -> List Constraint -> Result String Substitution
solve substitution constraints =
    case constraints of
        [] ->
            Ok substitution

        ( t1, t2 ) :: tail ->
            Type.unify t1 t2
                |> Result.andThen
                    (\new ->
                        solve
                            (new $ substitution)
                            (List.map (substituteConstraint new) tail)
                    )


substituteConstraint : Substitution -> Constraint -> Constraint
substituteConstraint substitution ( l, r ) =
    let
        f =
            Type.substitute substitution
    in
        ( f l, f r )
