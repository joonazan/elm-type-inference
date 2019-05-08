module Helpers exposing (checkLetCase, checkListAndExp, checkListUniqueIds, checkNameId, checkNameListUniqueIds, checkRecordsIds, generateStatementIds, generateStatementsIds, hasUniqueIds, hasUniqueIds_, listHasUniqueIds, listHasUniqueIds_, statementHasUniqueIds, statementsHaveUniqueIds)

import Ast.Statement exposing (Statement, StatementBase(..))
import Infer.Expression exposing (..)
import Set exposing (Set)


checkListUniqueIds : List AstMExp -> Set Id -> Maybe (Set Id)
checkListUniqueIds li ids =
    List.foldl (\x acc -> Maybe.andThen (hasUniqueIds_ x) acc) (Just ids) li


checkNameId : MName -> Set Id -> Maybe (Set Id)
checkNameId ( _, { id } ) ids =
    if Set.member id ids then
        Nothing

    else
        Just <| Set.insert id ids


checkNameListUniqueIds : List MName -> Set Id -> Maybe (Set Id)
checkNameListUniqueIds li ids =
    case li of
        [] ->
            Just ids

        n :: xs ->
            checkNameId n ids |> Maybe.andThen (checkNameListUniqueIds xs)


checkListAndExp : List AstMExp -> AstMExp -> Set Id -> Maybe (Set Id)
checkListAndExp li ex ids =
    hasUniqueIds_ ex ids |> Maybe.andThen (checkListUniqueIds li)


checkRecordsIds : List ( MName, AstMExp ) -> Set Id -> Maybe (Set Id)
checkRecordsIds records ids =
    List.unzip records
        |> (\( names, exps ) ->
                checkNameListUniqueIds names ids
                    |> Maybe.andThen
                        (\newIds -> checkListUniqueIds exps newIds)
           )


checkLetCase : List ( AstMExp, AstMExp ) -> AstMExp -> Set Id -> Maybe (Set Id)
checkLetCase li exp ids =
    let
        ( li1, li2 ) =
            List.unzip li
    in
    checkListUniqueIds li1 ids |> Maybe.andThen (checkListAndExp li2 exp)


hasUniqueIds_ : AstMExp -> Set Id -> Maybe (Set Id)
hasUniqueIds_ ( e, { id } ) ids =
    if Set.member id ids then
        Nothing

    else
        case e of
            ECharacter _ ->
                Just (Set.insert id ids)

            EString _ ->
                Just (Set.insert id ids)

            EInteger _ ->
                Just (Set.insert id ids)

            EFloat _ ->
                Just (Set.insert id ids)

            EVariable _ ->
                Just (Set.insert id ids)

            EList li ->
                checkListUniqueIds li ids

            ETuple li ->
                checkListUniqueIds li ids

            EAccess exp li ->
                hasUniqueIds_ exp ids |> Maybe.andThen (checkNameListUniqueIds li)

            EAccessFunction _ ->
                Just (Set.insert id ids)

            ERecord records ->
                checkRecordsIds records ids

            ERecordUpdate n records ->
                checkNameId n ids
                    |> Maybe.andThen (checkRecordsIds records)

            EIf e1 e2 e3 ->
                hasUniqueIds_ e1 ids
                    |> Maybe.andThen (hasUniqueIds_ e2)
                    |> Maybe.andThen (hasUniqueIds_ e3)

            ELet li exp ->
                checkLetCase li exp ids

            ECase exp li ->
                checkLetCase li exp ids

            ELambda li exp ->
                hasUniqueIds_ exp ids |> Maybe.andThen (checkListUniqueIds li)

            EApplication e1 e2 ->
                hasUniqueIds_ e1 ids
                    |> Maybe.andThen (hasUniqueIds_ e2)

            EBinOp e1 e2 e3 ->
                hasUniqueIds_ e1 ids
                    |> Maybe.andThen (hasUniqueIds_ e2)
                    |> Maybe.andThen (hasUniqueIds_ e3)


listHasUniqueIds_ : List AstMExp -> Set Id -> Maybe (Set Id)
listHasUniqueIds_ list ids =
    case list of
        [] ->
            Just ids

        exp :: rest ->
            case hasUniqueIds_ exp ids of
                Nothing ->
                    Nothing

                Just newIds ->
                    listHasUniqueIds_ rest newIds


listHasUniqueIds : List AstMExp -> Bool
listHasUniqueIds l =
    case listHasUniqueIds_ l Set.empty of
        Nothing ->
            False

        _ ->
            True


hasUniqueIds : AstMExp -> Bool
hasUniqueIds exp =
    case hasUniqueIds_ exp Set.empty of
        Nothing ->
            False

        _ ->
            True


generateStatementIds : Statement -> List AstMExp
generateStatementIds ( s, _ ) =
    case s of
        FunctionDeclaration _ vars body ->
            generateListIds (body :: vars)

        _ ->
            []


generateStatementsIds : List Statement -> List AstMExp
generateStatementsIds statements =
    List.foldr
        (\( x, _ ) acc ->
            case x of
                FunctionDeclaration _ vars body ->
                    body :: vars ++ acc

                _ ->
                    acc
        )
        []
        statements
        |> generateListIds


statementHasUniqueIds : Statement -> Bool
statementHasUniqueIds ( s, _ ) =
    case s of
        FunctionDeclaration _ vars body ->
            listHasUniqueIds (generateListIds (body :: vars))

        _ ->
            True


statementsHaveUniqueIds : List Statement -> Bool
statementsHaveUniqueIds =
    generateStatementsIds >> listHasUniqueIds
