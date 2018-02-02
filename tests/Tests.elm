module Tests exposing (typeInference)

import Dict
import Expect
import Infer
import Infer.Expression exposing (Expression(..))
import Infer.Monad as Infer
import Infer.Scheme exposing (generalize, instantiate)
import Infer.Type as Type exposing (Type(..))
import Test exposing (..)


typeOf env exp =
    Infer.typeOf env exp
        |> Infer.finalValue 0
        |> Result.map Tuple.first


equal : a -> a -> () -> Expect.Expectation
equal a b =
    \() -> Expect.equal a b


typeInference : Test
typeInference =
    describe "Type inference"
        [ test "function type union" <|
            equal
                (Type.union
                    (TArrow (TAny 1) (TArrow (TAny 2) (TAny 2)))
                    (TArrow (TAny 3) (TArrow (TAny 3) Type.int))
                )
                (Ok <| TArrow Type.int (TArrow Type.int Type.int))
        , test "trivial inference" <|
            equal
                (typeOf Dict.empty (Literal Type.string))
                (Ok Type.string)
        , test "identity construction" <|
            equal
                (typeOf
                    (Dict.singleton "identity" ( [ 1 ], TArrow (TAny 1) (TAny 1) ))
                    (Call (Name "identity")
                        (Call (Name "identity")
                            (Literal Type.string)
                        )
                    )
                )
            <|
                Ok Type.string
        , test "string concat" <|
            equal
                (typeOf
                    (Dict.singleton "(++)" ( [ 1 ], TArrow Type.string (TArrow Type.string Type.string) ))
                    (Call
                        (Call (Name "(++)")
                            (Literal Type.string)
                        )
                        (Literal Type.string)
                    )
                )
            <|
                Ok Type.string
        , test "let binding" <|
            equal
                (typeOf
                    Dict.empty
                    (Let
                        [ ( "x", Literal Type.string ) ]
                        (Name "x")
                    )
                )
            <|
                Ok Type.string
        , test "recursion with let" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "f"
                          , Lambda "x" <|
                                if_ (Literal Type.bool)
                                    (Call (Name "f") (Call (Call (Name "+") (Name "x")) (Name "x")))
                                    (Literal Type.string)
                          )
                        ]
                        (Call (Name "f") <| Literal Type.int)
                    )
                )
                (Ok Type.string)
        , test "polymorphic let" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "id", Lambda "x" <| Name "x" )
                        ]
                        (tuple
                            (Call (Name "id") <| Literal Type.int)
                            (Call (Name "id") <| Literal Type.string)
                        )
                    )
                )
                (Ok <| TOpaque "Tuple" [ Type.int, Type.string ])
        , test "polymorphic let2" <|
            equal
                (typeOf
                    testEnv
                    (Let
                        [ ( "id", Lambda "x" <| Name "x" )
                        , ( "a", Call (Name "id") <| Literal Type.int )
                        , ( "b", Call (Name "id") <| Literal Type.string )
                        ]
                        (tuple (Name "a") (Name "b"))
                    )
                )
                (Ok <| TOpaque "Tuple" [ Type.int, Type.string ])
        , test "recursive type error when there should be none" <|
            equal
                (typeOf
                    testEnv
                    (if_
                        (Literal Type.bool)
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
        ]


if_ a b c =
    Call (Call (Call (Name "if") a) b) c


testEnv =
    Dict.fromList
        [ ( "if"
          , ( [ 1 ]
            , TArrow Type.bool <|
                TArrow (TAny 1) <|
                    TArrow (TAny 1) (TAny 1)
            )
          )
        , ( "+", arith )
        , ( "tuple2", ( [ 1, 2 ], TArrow (TAny 1) (TArrow (TAny 2) (TOpaque "Tuple" [ TAny 1, TAny 2 ])) ) )
        ]


tuple a b =
    Call (Call (Name "tuple2") a) b


arith =
    ( [ 1 ], TArrow (TAny 1) <| TArrow (TAny 1) (TAny 1) )
