module Ast.Translate exposing (expression)

import Ast.Expression as AstExp exposing (..)
import Infer.Expression as InferExp
import Infer.Type as Type exposing (Type, unconstrained)


type alias ExMeta =
    {}


expression : Expression -> InferExp.Expression
expression e =
    case e of
        Let bindingList body ->
            InferExp.Let (bindings bindingList) (expression body)

        Variable [ name ] ->
            InferExp.Name name

        Integer _ ->
            literal Type.int

        Float _ ->
            literal Type.float

        String _ ->
            literal Type.string

        --List elems ->
        --    literal <| Type.list (getCommonType elems)
        -- Lambdas
        Lambda ((Variable [ only ]) :: []) body ->
            InferExp.Lambda only (expression body)

        Lambda ((Variable [ first ]) :: rest) body ->
            InferExp.Lambda first (expression <| Lambda rest body)

        -- Aplication
        Application l r ->
            InferExp.Call (expression l) (expression r)

        _ ->
            Debug.crash "Not implemented"


bindings :
    List ( Expression, Expression )
    -> List ( String, InferExp.Expression )
bindings list =
    list
        |> List.map
            (\binding ->
                case binding of
                    ( Variable [ name ], r ) ->
                        ( name, expression r )

                    e ->
                        Debug.crash (toString e ++ " is not a supported variable definition yet")
            )


literal : Type.RawType -> InferExp.Expression
literal t =
    InferExp.Literal (unconstrained t)
