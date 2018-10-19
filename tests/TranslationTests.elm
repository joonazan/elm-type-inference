module TranslationTests exposing (lets, literals)

import Ast
import Ast.Statement as Statement
import Ast.Translate as Translate
import Dict
import Expect exposing (equal)
import Infer
import Infer.Expression as Expression exposing (Expression)
import Infer.Monad as Infer
import Infer.Scheme
import Infer.Type as Type exposing ((=>), Constraint(..), RawType(..), Type, unconstrained)
import Result exposing (..)
import Test exposing (..)


literals : Test
literals =
    describe "Literals translation"
        [ test "Int" <| code "1" Type.int
        , test "String" <| code "\"a\"" Type.string
        ]


lets : Test
lets =
    describe "Lets translation"
        [ test "Int" <| code "let a = 1 in a" Type.int
        , test "String" <| code "let a = \"a\" in a" Type.string
        ]



--- HELPERS ---


typeOf : Infer.Scheme.Environment -> Expression -> Result String Type
typeOf env exp =
    Infer.typeOf env exp
        |> Infer.finalValue 0
        |> Result.map Tuple.first


code : String -> RawType -> (() -> Expect.Expectation)
code input t =
    Ast.parse ("a = " ++ input)
        |> Result.mapError (always "Parsing failed")
        |> Result.andThen
            (\res ->
                case res of
                    ( _, _, [ Statement.FunctionDeclaration "a" [] body ] ) ->
                        Ok body

                    _ ->
                        Err "Imparsable code"
            )
        |> Result.map Translate.expression
        |> Result.andThen (typeOf Dict.empty)
        |> equal (Ok <| unconstrained t)
        |> (\a -> \() -> a)
