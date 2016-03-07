module Board where

import Utils exposing (css, concat, range)
import Html exposing (..)
import Html.Attributes exposing (style, class, rel, href)
import Html.Events exposing (onClick)
import Array as A
import List
import Maybe exposing (Maybe(..))

-- Model

type alias Position = { x : Int, y : Int }

type alias Model = A.Array (A.Array Bool)

-- Model helpers

position : Int -> Int -> Position
position i j = { x = i, y = j }

positions : Model -> List Position
positions model =
    let w = width model
        h = height model
    in List.concatMap (\i -> (List.map (position i) (range 0 w))) (range 0 h)

intToPosition : Int -> Int -> Position
intToPosition n width = position (n // width) (n % width)

height : Model -> Int
height model = A.length model

width : Model -> Int
width model =
    case A.get 0 model of
        Nothing -> 0
        Just row -> A.length row

getIn : A.Array (A.Array a) -> Position -> Maybe a
getIn model position =
    case A.get position.x model of
        Nothing -> Nothing
        Just row -> A.get position.y row

assocIn : A.Array (A.Array a) -> Position -> a -> A.Array (A.Array a)
assocIn model position x =
    case A.get position.x model of
        Nothing -> model
        Just row ->
            let updatedRow = A.set position.y x row
            in
                A.set position.x updatedRow model

updateIn : A.Array (A.Array a) -> Position -> (a -> a) -> A.Array (A.Array a)
updateIn model position f =
    case getIn model position of
        Nothing -> model
        Just b -> assocIn model position (f b)


pulsar : Model
pulsar = A.fromList [ A.fromList [ False, False, False, False, False, False, False, False, False, False, False, False, False, False, False ]
                    , A.fromList [ False, False, False, True,  True,  True,  False, False, False, True,  True,  True,  False, False, False ]
                    , A.fromList [ False, False, False, False, False, False, False, False, False, False, False, False, False, False, False ]
                    , A.fromList [ False, True,  False, False, False, False, True,  False, True,  False, False, False, False, True,  False ]
                    , A.fromList [ False, True,  False, False, False, False, True,  False, True,  False, False, False, False, True,  False ]
                    , A.fromList [ False, True,  False, False, False, False, True,  False, True,  False, False, False, False, True,  False ]
                    , A.fromList [ False, False, False, True,  True,  True,  False, False, False, True,  True,  True,  False, False, False ]
                    , A.fromList [ False, False, False, False, False, False, False, False, False, False, False, False, False, False, False ]
                    , A.fromList [ False, False, False, True,  True,  True,  False, False, False, True,  True,  True,  False, False, False ]
                    , A.fromList [ False, True,  False, False, False, False, True,  False, True,  False, False, False, False, True,  False ]
                    , A.fromList [ False, True,  False, False, False, False, True,  False, True,  False, False, False, False, True,  False ]
                    , A.fromList [ False, True,  False, False, False, False, True,  False, True,  False, False, False, False, True,  False ]
                    , A.fromList [ False, False, False, False, False, False, False, False, False, False, False, False, False, False, False ]
                    , A.fromList [ False, False, False, True,  True,  True,  False, False, False, True,  True,  True,  False, False, False ]
                    , A.fromList [ False, False, False, False, False, False, False, False, False, False, False, False, False, False, False ]
                    ]

glider : Model
glider = A.fromList [ A.fromList [ False, True,  False, False, False, False ]
                    , A.fromList [ False, False, True,  False, False, False ]
                    , A.fromList [ True,  True,  True,  False, False, False ]
                    , A.fromList [ False, False, False, False, False, False ]
                    , A.fromList [ False, False, False, False, False, False ]
                    , A.fromList [ False, False, False, False, False, False ]
                    , A.fromList [ False, False, False, False, False, False ]
                    ]

boards : List (String, Model)
boards = [ ("Glider", glider)
         , ("Pulsar", pulsar)
         ]

-- Update

type Action = Next
            | LoadBoard Model
            | ToggleValue Position

update : Action -> Model -> Model
update action model =
    case action of
        Next -> next model
        LoadBoard model -> model
        ToggleValue pos -> updateIn model pos not

-- Business Logic

isValid : Model -> Position -> Bool
isValid model position =
    let w = width model
        h = height model
        x = position.x
        y = position.y
    in
        if x >= h then False
        else if y >= w then False
        else if x < 0 then False
        else if y < 0 then False
        else True

neighbors : Model -> Position -> List Position
neighbors model pos =
    let w = width model
        h = height model
        x = pos.x
        y = pos.y
    in
        [ (x + 1, y), (x + 1, y + 1), (x + 1, y - 1)
        , (x , y + 1), (x, y - 1)
        , (x - 1, y), (x - 1, y + 1), (x - 1, y - 1)
        ]
            |> List.map (\(i, j) -> position i j)
            |> List.filter (isValid model)

isDead : Model -> Position -> Bool
isDead model position =
    case getIn model position of
        Nothing -> False
        Just b -> not b

isAlive : Model -> Position -> Bool
isAlive model = not << isDead model

rules : Bool -> Int -> Int -> Bool
rules self deadCellCount aliveCellCount =
    if self && aliveCellCount < 2 then False
    else if self && aliveCellCount >= 2 && aliveCellCount <= 3 then True
    else if not self && aliveCellCount == 3 then True
    else False

next : Model -> Model
next model =
    positions model
        |> List.foldl (\position m -> assocIn m position (nextCellState model position)) model

nextCellState : Model -> Position -> Bool
nextCellState model position =
    let neighbs = neighbors model position
        deadCellCount = List.length (List.filter (isDead model) neighbs)
        aliveCellCount = List.length (List.filter (isAlive model) neighbs)
    in case (getIn model position) of
        Nothing -> False
        Just self -> rules self deadCellCount aliveCellCount

-- View

viewCell : Signal.Address Action -> Position -> Int -> Bool -> Html
viewCell address position width b =
    div [ viewCellStyle width b
        , onClick address (ToggleValue position)
        , class ("cell-item" ++ " " ++ (if b then "cell-alive" else "cell-dead"))
        ] []

selectBoardView : Signal.Address Action -> List (String, Model) -> Html
selectBoardView address models =
    div [ class "btn-group pull-right" ]
        (List.map (\(s, m) -> div [ class "btn btn-default"
                                  , onClick address (LoadBoard m) ]
                                  [ text s ]) models)

view : Signal.Address Action -> Model -> Html
view address model =
    let w = width model
    in
        div [class "game-container" ]
            [ css "game.css"
            , css "//maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css"
            , div [ class "button-container" ]
                  [div [ class "btn btn-default"
                       , onClick address Next]
                        [text "Next"]
                  , selectBoardView address boards
                ]
            , div [ class "board-container" ]
                  (List.indexedMap (\k b -> viewCell address (intToPosition k w) w b) (A.toList (concat model)))
            ]

viewCellStyle : Int -> Bool -> Attribute
viewCellStyle width b =
    style [ ("flex-basis", toString (100.0 / (toFloat width)) ++ "%") ]
