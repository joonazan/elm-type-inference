module Tests exposing (all)

import Test exposing (..)
import Expect
import Infer.Type as Type exposing (Type(..))
import Infer exposing (typeOf)
import Infer.Expression exposing (Expression(..))
import Infer.Monad as Infer
import Dict


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
                (typeOf Dict.empty (Literal Type.string) |> Infer.finalValue 0)
            <|
                Ok Type.string
        , test "identity construction" <|
            equal
                (typeOf
                    (Dict.singleton "identity" ( [ 1 ], TArrow (TAny 1) (TAny 1) ))
                    (Call (Name "identity")
                        (Call (Name "identity")
                            (Literal Type.string)
                        )
                    )
                    |> Infer.finalValue 0
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
                    |> Infer.finalValue 0
                )
            <|
                Ok Type.string
        , test "let binding" <|
            equal
                (typeOf
                    (Dict.empty)
                    (Let "x"
                        (Literal Type.string)
                        (Name "x")
                    )
                    |> Infer.finalValue 0
                )
            <|
                Ok Type.string
        ]
