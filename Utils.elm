module Utils where

import Html exposing (..)
import Html.Attributes exposing (rel, href)
import Array as A

css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []

range : Int -> Int -> List Int
range start end =
    if start >= end
    then []
    else start :: range (start + 1) end

concat : A.Array (A.Array a) -> A.Array a
concat xss =
    xss |> A.map A.toList
        |> A.toList
        |> List.concat
        |> A.fromList
