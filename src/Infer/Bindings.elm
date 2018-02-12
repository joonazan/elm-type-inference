module Infer.Bindings exposing (group)

import Dict exposing (Dict)
import Infer.Expression exposing (Expression(..))
import List.Extra exposing (dropWhile, takeWhile)
import Set exposing (Set)


group : List ( String, Expression ) -> List (List ( String, Expression ))
group bindings_ =
    let
        bindings =
            Dict.fromList bindings_

        nodes =
            Dict.keys bindings

        neighbors =
            Dict.map
                (always (freeVariables >> Set.filter (\x -> List.member x nodes)))
                bindings
    in
    stronglyConnected nodes neighbors
        |> List.map
            (List.map
                (\name ->
                    ( name
                    , Dict.get name bindings
                        |> Maybe.withDefault (Name "error")
                    )
                )
            )


freeVariables : Expression -> Set String
freeVariables e =
    case e of
        Name x ->
            Set.singleton x

        Lambda arg exp ->
            freeVariables exp
                |> Set.remove arg

        Let bindings exp ->
            List.map Tuple.first bindings
                |> Set.fromList
                |> Set.diff (freeVariables exp)

        Call e1 e2 ->
            Set.union (freeVariables e1) (freeVariables e2)

        Literal _ ->
            Set.empty

        Spy exp _ ->
            freeVariables exp


stronglyConnected : List comparable -> Dict comparable (Set comparable) -> List (List comparable)
stronglyConnected nodes neighbors =
    let
        dfs v s p ( labeling, n, components ) =
            let
                ( s_, p_, ( labeling_, n_, components_ ) ) =
                    Set.foldl
                        (\w ( s, p, ( labeling, n, components ) ) ->
                            case Dict.get w labeling of
                                Nothing ->
                                    dfs w s p ( labeling, n, components )

                                Just o ->
                                    ( s, dropWhile ((<) o) p, ( labeling, n, components ) )
                        )
                        ( v :: s, v :: p, ( Dict.insert v n labeling, n + 1, components ) )
                        (Dict.get v neighbors
                            |> Maybe.withDefault Set.empty
                        )
            in
            if List.head p_ == Just v then
                let
                    newC =
                        takeWhile ((<=) v) s_

                    restS =
                        dropWhile ((<=) v) s_
                in
                ( restS, List.drop 1 p_, ( labeling_, n_, newC :: components_ ) )
            else
                ( p_, s_, ( labeling_, n_, components_ ) )

        ( _, _, components ) =
            List.foldl
                (\v (( labeling, _, _ ) as state) ->
                    if Dict.get v labeling == Nothing then
                        let
                            ( _, _, state_ ) =
                                dfs v [] [] state
                        in
                        state_
                    else
                        state
                )
                ( Dict.empty, 0, [] )
                nodes
    in
    components
