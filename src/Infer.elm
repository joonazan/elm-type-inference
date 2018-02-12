module Infer exposing (typeOf)

{-| This is the module implementing type inference. You'll also need at least `Infer.Expression`.
@docs typeOf
-}

import Dict
import Infer.Bindings as Bindings
import Infer.ConstraintGen exposing (..)
import Infer.Expression exposing (Expression(..))
import Infer.Monad as Infer exposing (..)
import Infer.Scheme exposing (Environment, Scheme, freshTypevar, generalize, instantiate)
import Infer.Type as Type exposing (($), Substitution, Type(..), substitute)


{-| Returns a computation that yields the type of the input expression
with the specified environment.
-}
typeOf : Environment -> Expression -> Infer.Monad ( Type, Type -> Type )
typeOf env exp =
    generateConstraints env exp
        |> Infer.andThen
            (\( t, cs ) ->
                solve Dict.empty cs
                    |> Result.map (\s -> ( Type.substitute s t, s ))
                    |> Infer.fromResult
            )
        |> getSubstitution (\s ( t, s2 ) -> ( t, Type.substitute (Dict.union s s2) ))


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


generateConstraints : Environment -> Expression -> Monad ( Type, List Constraint )
generateConstraints environment exp =
    case exp of
        Name name ->
            variable environment name
                |> map (\x -> ( x, [] ))

        Literal t ->
            pure ( t, [] )

        Call function argument ->
            map3
                (\this ( f, fc ) ( a, ac ) ->
                    ( this
                    , fc ++ ac ++ [ ( f, TArrow a this ) ]
                    )
                )
                freshTypevar
                (generateConstraints environment function)
                (generateConstraints environment argument)

        Lambda argument body ->
            freshTypevar
                |> andThen
                    (\argType ->
                        generateConstraints (extend environment argument argType) body
                            |> map
                                (\( bodyType, bodyCons ) ->
                                    ( TArrow argType bodyType, bodyCons )
                                )
                    )

        Let bindings body ->
            Bindings.group bindings
                |> List.foldl (addBindingGroupToEnv >> andThen) (pure environment)
                |> andThen
                    (\env -> generateConstraints env body)

        Spy exp tag ->
            generateConstraints environment exp
                |> map
                    (\( typ, constraints ) ->
                        ( typ, constraints ++ [ ( TAny tag, typ ) ] )
                    )


addBindingGroupToEnv : List ( String, Expression ) -> Environment -> Monad Environment
addBindingGroupToEnv bindings origEnv =
    let
        bindings_ =
            List.map (\( n, e ) -> map (\tv -> ( n, e, tv )) freshTypevar) bindings
                |> combine

        extendedEnv =
            map (List.foldl (\( n, _, tv ) env -> extend env n tv) origEnv) bindings_

        typesAndConstraints =
            map2
                (\bin env ->
                    bin
                        |> List.map
                            (\( _, e, tv ) ->
                                map (\( t, cs ) -> ( t, ( tv, t ) :: cs )) <|
                                    generateConstraints env e
                            )
                        |> combine
                )
                bindings_
                extendedEnv
                |> andThen identity

        subs =
            List.map Tuple.second
                >> List.concat
                >> solve Dict.empty
                >> Infer.fromResult
    in
        typesAndConstraints
            |> andThen
                (\tcs ->
                    subs tcs
                        |> andThen
                            (\subs ->
                                List.map Tuple.first tcs
                                    |> List.map (substitute subs >> generalize origEnv)
                                    |> List.map2 (,) (List.map Tuple.first bindings)
                                    |> Dict.fromList
                                    |> (\new -> Dict.union new origEnv)
                                    |> pure
                                    |> addSubstitution subs
                            )
                )
