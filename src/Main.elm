module Main exposing (Autoplay(..), Cell(..), Model, Msg(..), aliveNeighbors, getCell, getCellStatusBasedOnAliveNeighbors, init, isMaybeAlive, main, neighborIndexes, neighbors, normalizeSize, previousStep, toggleAutoplay, toggleCell, toggleCellStatus, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, section, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe
import Platform.Cmd as Cmd
import Random
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Cell
    = Alive
    | Dead


type Autoplay
    = Playing
    | Paused


type alias Model =
    { size : Int
    , board : Array Cell
    , history : List (Array Cell)
    , autoplay : Autoplay
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 8 (initCellList 8) [] Paused
    , Cmd.none
    )


initCellList : Int -> Array Cell
initCellList size =
    Array.repeat (size ^ 2) Dead



-- UPDATE


type Msg
    = UpdateSize String
    | NewGame
    | NewRandomGame
    | RandomSeed (List Cell)
    | ToggleCell Int
    | NextStep (Array Cell)
    | PreviousStep
    | ToggleAutoplay
    | AutoplayTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSize newSize ->
            update NewGame { model | size = normalizeSize newSize }

        NewGame ->
            ( { model | board = initCellList model.size, history = [] }
            , Cmd.none
            )

        NewRandomGame ->
            ( model
            , Random.generate RandomSeed (randomSeed model.size)
            )

        RandomSeed randomBoard ->
            ( { model | board = Array.fromList randomBoard, history = [] }
            , Cmd.none
            )

        ToggleCell index ->
            ( { model | board = toggleCell index model.board }
            , Cmd.none
            )

        NextStep currentBoard ->
            ( { model | board = nextStep model.board model.size, history = currentBoard :: model.history }
            , Cmd.none
            )

        PreviousStep ->
            ( { model | board = previousStep model.history model.board, history = List.drop 1 model.history }
            , Cmd.none
            )

        ToggleAutoplay ->
            ( { model | autoplay = toggleAutoplay model.autoplay }
            , Cmd.none
            )

        AutoplayTick _ ->
            case model.autoplay of
                Playing ->
                    update (NextStep model.board) model

                Paused ->
                    ( model, Cmd.none )


normalizeSize : String -> Int
normalizeSize sizeString =
    case String.toInt sizeString of
        Nothing ->
            8

        Just size ->
            if size < 8 then
                8

            else if size > 96 then
                96

            else
                size


randomSeed : Int -> Random.Generator (List Cell)
randomSeed size =
    Random.list (size ^ 2) (Random.uniform Alive [ Dead ])


toggleCell : Int -> Array Cell -> Array Cell
toggleCell index board =
    Array.set index (toggleCellStatus (getCell index board)) board


getCell : Int -> Array Cell -> Cell
getCell index board =
    Maybe.withDefault Dead (Array.get index board)


toggleCellStatus : Cell -> Cell
toggleCellStatus cellStatus =
    case cellStatus of
        Alive ->
            Dead

        Dead ->
            Alive


isMaybeAlive : Maybe Cell -> Bool
isMaybeAlive cell =
    case cell of
        Just Alive ->
            True

        _ ->
            False


nextStep : Array Cell -> Int -> Array Cell
nextStep board size =
    Array.indexedMap (getCellStatusBasedOnAliveNeighbors board size) board


getCellStatusBasedOnAliveNeighbors : Array Cell -> Int -> Int -> Cell -> Cell
getCellStatusBasedOnAliveNeighbors board size index cellStatus =
    let
        aliveNeighborsCount =
            aliveNeighbors board size index
    in
    case aliveNeighborsCount of
        2 ->
            cellStatus

        3 ->
            Alive

        _ ->
            Dead


aliveNeighbors : Array Cell -> Int -> Int -> Int
aliveNeighbors board size index =
    List.length <|
        List.filter isMaybeAlive <|
            List.map (\neighborIndex -> Array.get neighborIndex board) <|
                neighbors size index


neighbors : Int -> Int -> List Int
neighbors size index =
    List.map Tuple.second (List.filter Tuple.first (neighborIndexes size index))


neighborIndexes : Int -> Int -> List ( Bool, Int )
neighborIndexes size index =
    let
        notLeft =
            modBy size index /= 0

        notRight =
            modBy size index /= size - 1
    in
    [ ( notLeft, index - size - 1 )
    , ( True, index - size )
    , ( notRight, index - size + 1 )
    , ( notLeft, index - 1 )
    , ( notRight, index + 1 )
    , ( notLeft, index + size - 1 )
    , ( True, index + size )
    , ( notRight, index + size + 1 )
    ]


previousStep : List (Array Cell) -> Array Cell -> Array Cell
previousStep history board =
    case List.head history of
        Just previousBoard ->
            previousBoard

        Nothing ->
            board


toggleAutoplay : Autoplay -> Autoplay
toggleAutoplay autoplay =
    case autoplay of
        Playing ->
            Paused

        Paused ->
            Playing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 AutoplayTick



-- VIEW


view : Model -> Html Msg
view model =
    section []
        [ div
            [ id "controls" ]
            [ input
                [ type_ "number"
                , value (String.fromInt model.size)
                , onInput UpdateSize
                , Html.Attributes.min "8"
                , Html.Attributes.max "96"
                ]
                []
            , createButton NewGame "New Game" "primary" 'n' False
            , createButton NewRandomGame "Random" "primary" 'r' False
            , createButton PreviousStep "Previous" "secondary" 'h' (List.length model.history == 0)
            , createButton ToggleAutoplay "Autoplay" "secondary" 'a' False
            , createButton (NextStep model.board) "Next" "secondary" 'l' False
            ]
        , div
            [ id "board"
            , style "grid-template-columns" ("repeat(" ++ String.fromInt model.size ++ ", minmax(2px, 50px))")
            ]
            (viewBoard model.board)
        ]


createButton : Msg -> String -> String -> Char -> Bool -> Html Msg
createButton msg buttonText buttonClass buttonAccessKey buttonDisabled =
    button
        [ onClick msg
        , class buttonClass
        , accesskey buttonAccessKey
        , disabled buttonDisabled
        ]
        [ span
            [ class "font" ]
            [ text buttonText ]
        ]


viewBoard : Array Cell -> List (Html Msg)
viewBoard board =
    Array.toList (Array.indexedMap createCellHtml board)


createCellHtml : Int -> Cell -> Html Msg
createCellHtml index cell =
    case cell of
        Alive ->
            createCellDiv index "alive"

        Dead ->
            createCellDiv index "dead"


createCellDiv : Int -> String -> Html Msg
createCellDiv index classText =
    div
        [ id (String.fromInt index)
        , class ("cell " ++ classText)
        , onClick (ToggleCell index)
        ]
        []
