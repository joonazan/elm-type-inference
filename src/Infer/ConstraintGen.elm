module Infer.ConstraintGen exposing (..)

import Infer.Type as Type exposing (Type(..), ($))
import Infer.Scheme exposing (Environment, Scheme, generalize, instantiate, freshTypevar)
import Infer.Expression exposing (Expression(..))
import Dict exposing (Dict)
import Infer.Monad exposing (..)


type alias Constraint =
    ( Type, Type )


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
                    , fc ++ ac ++ [ ( f, (TArrow a this) ) ]
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

        Let name value body ->
            generateConstraints environment value
                |> andThen
                    (\( valueT, valueC ) ->
                        map2
                            (\this ( bodyT, bodyC ) ->
                                ( this
                                , valueC ++ bodyC ++ [ ( this, bodyT ) ]
                                )
                            )
                            freshTypevar
                            (generateConstraints (extend environment name valueT) body)
                    )

        Spy exp tag ->
            generateConstraints environment exp
                |> map
                    (\( typ, constraints ) ->
                        ( typ, constraints ++ [ ( TAny tag, typ ) ] )
                    )


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
