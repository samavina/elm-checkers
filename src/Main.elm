module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attr
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MAIN
main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type GameState
  = Initial
  | Win
  | Loss
  | PlayerOneTurn
  | PlayerTwoTurn

type PieceColor
  = Black
  | Red

type PieceState
  = Unkinged
  | Kinged

type alias Piece
  = { color : PieceColor
    , state : PieceState
    }

type SquareColor
  = Dark
  | Light

type alias Square
  = { color : SquareColor
    , piece : Maybe Piece
    }

type alias Board = List (List Square)

type alias Model =
  { board : Board
  , state : GameState
  }

type alias Flags =
  ()

{-
X = black
O = red

_ O _ O _ O _ O
O _ O _ O _ O _
_ O _ O _ O _ O
_ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _
X _ X _ X _ X _
_ X _ X _ X _ X
X _ X _ X _ X _
-}

newBoard : Board
newBoard =
  let
    emptyLightSquare = {color = Light, piece = Nothing}
    emptyDarkSquare = {color = Dark, piece = Nothing}
    redPiece = Just {color = Red, state = Unkinged}
    redPieceSquare = {color = Dark, piece = redPiece}
    redOne = emptyLightSquare :: (List.intersperse emptyLightSquare (List.repeat 4 redPieceSquare))
    redTwo = redPieceSquare :: (List.intersperse redPieceSquare (List.repeat 4 emptyLightSquare))
    blackPiece = Just {color = Black, state = Unkinged}
    blackPieceSquare = {color = Dark, piece = blackPiece}
    blackOne = blackPieceSquare :: (List.intersperse blackPieceSquare (List.repeat 4 emptyLightSquare))
    blackTwo = emptyLightSquare :: (List.intersperse emptyLightSquare (List.repeat 4 blackPieceSquare))
    emptyOne = emptyDarkSquare :: (List.intersperse emptyDarkSquare (List.repeat 4 emptyLightSquare))
    emptyTwo = emptyLightSquare :: (List.intersperse emptyLightSquare (List.repeat 4 emptyDarkSquare))
  in
    [redOne, redTwo, redOne, emptyOne, emptyTwo, blackOne, blackTwo, blackOne]

init : Flags -> (Model, Cmd Msg)
init () =
  let
    initModel =
      { board = newBoard
      , state = Initial
      }
  in
    (initModel, Cmd.none)

-- UPDATE

type Msg
  = NewGame
  | PlayerOneMove
  | PlayerTwoMove

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewGame ->
      init ()
    _ ->
      (model, Cmd.none)

-- VIEW

squareSide : String
squareSide = "85"

squareAttr : List (Svg.Attribute msg)
squareAttr = [ width squareSide
             , height squareSide
             , viewBox "0 0 squareSide squareSide"
             ]

blackRect : Svg msg
blackRect = rect
              [ x "0"
              , y "0"
              , width squareSide
              , height squareSide
              , fill "black"
              ]
              []

redRect : Svg msg
redRect = rect
            [ x "0"
            , y "0"
            , width squareSide
            , height squareSide
            , fill "red"
            ]
            []

centerX : String
centerX = "43"

centerY : String
centerY = "43"

pRadius : String
pRadius = "38"

bPiece : Svg msg
bPiece = circle
           [ cx centerX
           , cy centerY
           , r pRadius
           , fill "black"
           , stroke "white"
           ]
           []

bKingedPiece : Svg msg
bKingedPiece = circle
                 [ cx centerX
                 , cy centerY
                 , r pRadius
                 , fill "black"
                 , stroke "white"
                 ]
                 []

rPiece : Svg msg
rPiece = circle
           [ cx centerX
           , cy centerY
           , r pRadius
           , fill "red"
           , stroke "white"
           ]
           []

rKingedPiece : Svg msg
rKingedPiece = circle
                 [ cx centerX
                 , cy centerY
                 , r pRadius
                 , fill "red"
                 , stroke "white"
                 ]
                 []

convertSquare : Square -> Element msg
convertSquare sq =
  let
    boardRect = if sq.color == Light then redRect else blackRect
  in
  case sq.piece of
    Nothing ->
      Element.html (svg squareAttr [boardRect])
    Just p->
      case (p.color, p.state) of
        (Black, Unkinged) ->
          Element.html (svg squareAttr [boardRect, bPiece])
        (Red, Unkinged) ->
          Element.html (svg squareAttr [boardRect, rPiece])
        (Black, Kinged) ->
          Element.html (svg squareAttr [boardRect, bKingedPiece])
        (red, Kinged) ->
          Element.html (svg squareAttr [boardRect, rKingedPiece])


convertRow : List Square -> List (Element msg) -> Element msg
convertRow row acc =
  case row of
    [] ->
      Element.row [] acc
    e :: es ->
      convertRow es (acc ++ [convertSquare e])

populateBoard : Board -> List (Element msg)
populateBoard board =
  case board of
    [] ->
      []
    r :: rs ->
      (convertRow r []) :: (populateBoard rs)

view : Model -> Html Msg
view model =
  let
    rows = populateBoard model.board
  in
    Element.layout
      [ Font.family
          [ Font.external
              { name = "Droid Sans"
              , url = "https://fonts.googleapis.com/css?family=Droid+Sans"
              }
          ]
      , Element.focused []
      ] <|
      Element.column
        [ Element.centerX, Element.centerY, Element.spacing 0 ]
        rows

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none








