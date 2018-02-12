module Infer.Bindings exposing (group)

import Dict exposing (Dict)
import Infer.Expression exposing (Expression(..))
import List.Extra as List exposing (dropWhile, unique)
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
            |> sortGroups neighbors
            |> List.map
                (List.map
                    (\name ->
                        ( name
                        , Dict.get name bindings
                            |> Maybe.withDefault (Name "error")
                        )
                    )
                )


sortGroups : Dict comparable (Set comparable) -> List (List comparable) -> List (List comparable)
sortGroups neighborDict groups =
    let
        groupNeighbors group =
            List.map neighbors group
                |> List.foldl Set.union Set.empty
                |> Set.map groupContaining
                |> Set.remove group
                |> Set.toList

        neighbors x =
            Dict.get x neighborDict
                |> Maybe.withDefault Set.empty

        groupContaining x =
            List.find (List.member x) groups
                -- this should be impossible, as the groups do not overlap
                |> Maybe.withDefault []
    in
        depsFirst groups groupNeighbors


{-| Returns the nodes of a directed acyclic graph (a tree) in an order where
all children of a node come before the node
-}
depsFirst : List comparable -> (comparable -> List comparable) -> List comparable
depsFirst nodes neighbors =
    let
        dependencies node =
            (neighbors node
                |> List.concatMap dependencies
                |> unique
            )
                ++ [ node ]
    in
        List.concatMap dependencies nodes
            |> unique


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
                                    ( s
                                    , if List.member w s then
                                        dropWhile
                                            (\x ->
                                                Dict.get x labeling
                                                    |> Maybe.map (\x -> x > o)
                                                    |> Maybe.withDefault False
                                            )
                                            p
                                      else
                                        p
                                    , ( labeling, n, components )
                                    )
                        )
                        ( v :: s, v :: p, ( Dict.insert v n labeling, n + 1, components ) )
                        (Dict.get v neighbors
                            |> Maybe.withDefault Set.empty
                        )
            in
                if List.head p_ == Just v then
                    let
                        ( newC, restS ) =
                            pop v s_
                    in
                        ( restS, List.drop 1 p_, ( labeling_, n_, newC :: components_ ) )
                else
                    ( s_, p_, ( labeling_, n_, components_ ) )

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


{-| splits the list after the first occurence of x
-}
pop : a -> List a -> ( List a, List a )
pop x list =
    case list of
        h :: t ->
            if h == x then
                ( [ x ], t )
            else
                pop x t
                    |> Tuple.mapFirst ((::) h)

        [] ->
            ( [], [] )
