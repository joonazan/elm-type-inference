module Tests exposing (all)

import Test exposing (..)
import Expect
import Infer.Type as Type exposing (Type(..))
import Infer
import Infer.Expression exposing (Expression(..))
import Infer.Monad as Infer
import Dict


typeOf env exp =
    Infer.typeOf env exp
        |> Infer.finalValue 0
        |> Result.map Tuple.first


all : Test
all =
    typeInference


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
                    (Dict.singleton "(++)" ( [ 1 ], (TArrow Type.string (TArrow Type.string Type.string)) ))
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
                    (Let "x"
                        (Literal Type.string)
                        (Name "x")
                    )
                )
            <|
                Ok Type.string
        , test "recursive type error when there should be none" <|
            equal
                (typeOf
                    (Dict.fromList
                        [ ( "if"
                          , ( [ 1 ]
                            , (TArrow Type.bool <|
                                TArrow (TAny 1) <|
                                    TArrow (TAny 1) (TAny 1)
                              )
                            )
                          )
                        , ( "+", ( [ 1 ], TArrow (TAny 1) <| TArrow (TAny 1) (TAny 1) ) )
                        ]
                    )
                    (Call (Call (Call (Name "if") (Literal Type.bool)) (Name "+")) (Name "+"))
                )
            <|
                Ok (TArrow (TAny 1) <| TArrow (TAny 1) (TAny 1))
        ]
