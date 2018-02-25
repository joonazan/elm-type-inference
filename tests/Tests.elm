module Tests exposing (typeInference)

import Dict
import Expect
import Infer
import Infer.Expression exposing (Expression(..))
import Infer.Monad as Infer
import Infer.Scheme exposing (generalize, instantiate)
import Infer.Type as Type exposing (Type, unconstrained, RawType(..), Constraint(..))
import Test exposing (..)


typeOf env exp =
    Infer.typeOf env exp
        |> Infer.finalValue 0
        |> Result.map Tuple.first


equal : a -> a -> () -> Expect.Expectation
equal a b =
    \() -> Expect.equal a b


variablesDiffer a b =
    \() ->
        Expect.true "parts other than type variables differ"
            (Type.unify a b
                |> Result.map (Dict.values >> List.all (Tuple.second >> isTAny))
                |> Result.withDefault False
            )


isTAny x =
    case x of
        TAny _ ->
            True

        _ ->
            False


stringLiteral =
    Literal <| unconstrained Type.string


intLiteral =
    Literal <| unconstrained Type.int


typeInference : Test
typeInference =
    describe "Type inference"
        [ test "trivial inference" <|
            equal
                (typeOf Dict.empty stringLiteral)
                (Ok <| unconstrained Type.string)
        , test "identity construction" <|
            equal
                (typeOf
                    (Dict.singleton "identity" ( [ 1 ], unconstrained <| TArrow (TAny 1) (TAny 1) ))
                    (Call (Name "identity")
                        (Call (Name "identity")
                            stringLiteral
                        )
                    )
                )
            <|
                Ok (unconstrained Type.string)
        , test "string concat" <|
            equal
                (typeOf
                    (Dict.singleton "(++)" ( [ 1 ], unconstrained <| TArrow Type.string (TArrow Type.string Type.string) ))
                    (Call
                        (Call (Name "(++)")
                            stringLiteral
                        )
                        stringLiteral
                    )
                )
            <|
                Ok (unconstrained Type.string)
        , test "let binding" <|
            equal
                (typeOf
                    Dict.empty
                    (Let
                        [ ( "x", stringLiteral ) ]
                        (Name "x")
                    )
                )
            <|
                Ok (unconstrained Type.string)
        , test "recursion with let" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "f"
                          , Lambda "x" <|
                                if_ (Literal <| unconstrained Type.bool)
                                    (Call (Name "f") (Call (Call (Name "+") (Name "x")) (Name "x")))
                                    stringLiteral
                          )
                        ]
                        (Call (Name "f") intLiteral)
                    )
                )
                (Ok <| unconstrained Type.string)
        , test "mutual recursion with let" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "f"
                          , Lambda "x" <|
                                if_ (Literal <| unconstrained Type.bool)
                                    (Call (Name "g") (Call (Call (Name "+") (Name "x")) (Name "x")))
                                    stringLiteral
                          )
                        , ( "g"
                          , Name "f"
                          )
                        ]
                        (Call (Name "f") intLiteral)
                    )
                )
                (Ok <| unconstrained Type.string)
        , test "polymorphic let" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "id", Lambda "x" <| Name "x" )
                        ]
                        (tuple
                            (Call (Name "id") intLiteral)
                            (Call (Name "id") stringLiteral)
                        )
                    )
                )
                (Ok <| unconstrained <| TOpaque "Tuple" [ Type.int, Type.string ])
        , test "polymorphic let2" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "id", Lambda "x" <| Name "x" )
                        , ( "a", Call (Name "id") intLiteral )
                        , ( "b", Call (Name "id") stringLiteral )
                        ]
                        (tuple (Name "a") (Name "b"))
                    )
                )
                (Ok <| unconstrained <| TOpaque "Tuple" [ Type.int, Type.string ])
        , test "recursive type error when there should be none" <|
            equal
                (typeOf
                    testEnv
                    (if_
                        (Literal <| unconstrained Type.bool)
                        (Name "+")
                        (Name "+")
                    )
                    |> Result.andThen
                        (generalize Dict.empty
                            >> instantiate
                            >> Infer.finalValue 1
                        )
                )
            <|
                Ok (Tuple.second arith)
        , test "spies on lets should work" <|
            variablesDiffer
                (Infer.typeOf
                    (Dict.singleton "Just"
                        ( [ 1 ], unconstrained <| TArrow (TAny 1) (TOpaque "Maybe" [ TAny 1 ]) )
                    )
                    (Let [ ( "x", Spy (Name "Just") 900 ) ] (Name "x"))
                    |> Infer.finalValue 0
                    |> Result.map Tuple.second
                    |> Result.withDefault identity
                    |> (\x -> x (unconstrained <| TAny 900))
                )
                (unconstrained (TArrow (TAny 1) (TOpaque "Maybe" [ TAny 1 ])))
        , test "number should propagate" <|
            equal
                (typeOf
                    (Dict.singleton "+" ( [ 1 ], ( Dict.singleton 1 Number, (TArrow (TAny 1) <| TArrow (TAny 1) (TAny 1)) ) ))
                    (Lambda "x" <| Call (Call (Name "+") (Name "x")) (Name "x"))
                )
                (Ok ( Dict.singleton 1 Number, TArrow (TAny 1) (TAny 1) ))
        ]


if_ a b c =
    Call (Call (Call (Name "if") a) b) c


testEnv =
    Dict.fromList
        [ ( "if"
          , ( [ 1 ]
            , unconstrained <|
                TArrow Type.bool <|
                    TArrow (TAny 1) <|
                        TArrow (TAny 1) (TAny 1)
            )
          )
        , ( "+", arith )
        , ( "tuple2", ( [ 1, 2 ], unconstrained <| TArrow (TAny 1) (TArrow (TAny 2) (TOpaque "Tuple" [ TAny 1, TAny 2 ])) ) )
        ]


tuple a b =
    Call (Call (Name "tuple2") a) b


arith =
    ( [ 1 ], unconstrained <| TArrow (TAny 1) <| TArrow (TAny 1) (TAny 1) )
