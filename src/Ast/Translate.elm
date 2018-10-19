module Ast.Translate exposing (expression)

import Ast.Expression as AstExp
import Ast.Statement
import Infer.Expression as InferExp
import Infer.Type as Type exposing (Type, unconstrained)


expression : AstExp.Expression -> InferExp.Expression
expression e =
    case e of
        AstExp.Let bindingList body ->
            InferExp.Let (bindings bindingList) (expression body)

        AstExp.Variable [ name ] ->
            InferExp.Name name

        AstExp.Integer _ ->
            literal Type.int

        AstExp.String _ ->
            literal Type.string

        _ ->
            Debug.crash "Not implemented"


bindings : List ( AstExp.Expression, AstExp.Expression ) -> List ( String, InferExp.Expression )
bindings list =
    list
        |> List.map
            (\binding ->
                case binding of
                    ( AstExp.Variable [ name ], r ) ->
                        ( name, expression r )

                    e ->
                        Debug.crash (toString e ++ " is not a support binding type")
            )


literal : Type.RawType -> InferExp.Expression
literal t =
    InferExp.Literal <| unconstrained t
