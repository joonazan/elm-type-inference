module Infer.Type exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

type Type
  = TArrow Type Type
  | TRecord (Dict String Type)
  | TUnion String (Dict String (List Type))
  | TTuple (List Type)

  | TOpaque String (List Type)

  | TAny Int

string = TOpaque "String" []
char = TOpaque "Char" []
bool = TOpaque "Bool" []
int = TOpaque "Int" []
float = TOpaque "Float" []


toString : Type -> String
toString t =
  case t of
    TOpaque name args ->
      name :: List.map toString args
      |> String.join " "
      |> brace

    TArrow l r -> toString l ++ " -> " ++ toString r

    TAny x -> Basics.toString x

    TRecord d ->
      Basics.toString <| Dict.map (always toString) d

    TUnion name options ->
      Dict.toList options
      |> List.map (\(name, types) ->
        name :: List.map toString types
        |> String.join " "
      )
      |> String.join " | "
      |> (++) (name ++ " = ")

    TTuple types ->
      List.map toString types
      |> String.join ","
      |> brace

brace : String -> String
brace x =
  "(" ++ x ++ ")"

variables : Type -> Set Int
variables t =
  case t of
    TAny x ->
      Set.singleton x

    TArrow l r ->
      Set.union (variables l) (variables r)

    TRecord d ->
      Dict.values d
      |> variablesFromList

    TUnion _ options ->
      Dict.values options
      |> List.concat
      |> variablesFromList

    TTuple types ->
      variablesFromList types

    TOpaque _ args ->
      variablesFromList args

variablesFromList : List Type -> Set Int
variablesFromList =
  List.map variables
  >> List.foldl Set.union Set.empty

{-|
  Returns the substitutions necessary to transform either type
  into a type that is compatible with both.
  Returns an error if not possible.
-}
unify : Type -> Type -> Result String Substitution 
unify context content =
  case (context, content) of
    (TOpaque a at, TOpaque b bt) ->
      if a == b then
        unifyMany at bt
      else
        mismatch a b

    (TArrow head1 tail1, TArrow head2 tail2) ->
      unify head1 head2
      |> Result.andThen (\sub1 ->
        unify (substitute sub1 tail1) (substitute sub1 tail2)
        |> Result.map (\sub2 -> sub2 $ sub1)
      )

    (TAny id, x) -> bind id x
    (x, TAny id) -> bind id x

    (x, y) -> mismatch (toString x) (toString y)

unifyMany : List Type -> List Type -> Result String Substitution
unifyMany context content =
  case (context, content) of
    ([], []) ->
      Ok Dict.empty

    (h1::t1, h2::t2) ->
      unify h1 h2
      |> Result.andThen (\sub1 ->
        unifyMany (substituteMany sub1 t1) (substituteMany sub1 t2)
        |> Result.map (flip ($) <| sub1)
      )

    _ -> Err "unifyMany with differing list lengths"

substituteMany : Substitution -> List Type -> List Type
substituteMany s =
  List.map (substitute s)

bind id x =
  if Set.member id (variables x) then
    Err "recursive type"
  else
    Ok <| Dict.singleton id x

mismatch : String -> String -> Result String a
mismatch a b =
  Err <| "Mismatch: " ++ a ++ " and " ++ b

($) : Substitution -> Substitution -> Substitution
($) a b =
  Dict.union (Dict.map (always <| substitute a) b) a
infixl 9 $

union : Type -> Type -> Result String Type
union a b =
  unify a b
    |> Result.map (\r -> substitute r a)

type alias Substitution =
  Dict Int Type

substitute : Substitution -> Type -> Type
substitute substitution t =
  case t of
    TAny x ->
      Dict.get x substitution
        |> Maybe.withDefault (TAny x)

    TArrow h t ->
      TArrow (substitute substitution h) (substitute substitution t)

    x -> x
