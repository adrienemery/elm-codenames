module Main exposing (..)

import Array exposing (Array)
import Browser
import Color.OneDark as OneDark
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Random
import Words exposing (words)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type PlayerMode
    = Spy
    | Player


type GameState
    = RedsTurn
    | BluesTurn
    | BlueWins
    | RedWins


type CardColor
    = Red
    | Blue
    | Yellow
    | Black


type CardState
    = Hidden
    | Visible


type alias Card =
    { color : CardColor
    , word : String
    , state : CardState
    }


type alias Model =
    { state : GameState
    , mode : PlayerMode
    , redCardsLeft : Int
    , blueCardsLeft : Int
    , cards : List Card
    , seed : Random.Seed
    }


cardColors : Array CardColor
cardColors =
    List.repeat 9 Red
        |> List.append (List.repeat 8 Blue)
        |> List.append (List.repeat 7 Yellow)
        |> List.append (List.repeat 1 Black)
        |> Array.fromList


generateRandomIndexes : Random.Seed -> List Int -> Int -> ( List Int, Random.Seed )
generateRandomIndexes seed indexes maxSize =
    let
        ( index, newSeed ) =
            Random.step (Random.int 0 maxSize) seed

        newIndexes =
            if List.member index indexes then
                indexes

            else
                indexes ++ [ index ]
    in
    if List.length newIndexes < 25 then
        generateRandomIndexes newSeed newIndexes maxSize

    else
        ( newIndexes, newSeed )


pickWord : Int -> String
pickWord index =
    case Array.get index words of
        Nothing ->
            "***Error***"

        Just word ->
            word


generateWords : Random.Seed -> ( Random.Seed, List String )
generateWords seed =
    let
        ( indexes, newSeed ) =
            generateRandomIndexes seed [] (Array.length words)
    in
    indexes
        |> List.map pickWord
        |> Tuple.pair newSeed


pickColor : Int -> CardColor
pickColor index =
    case Array.get index cardColors of
        Nothing ->
            Yellow

        Just color ->
            color


generateColors : Random.Seed -> List CardColor
generateColors seed =
    let
        ( indexes, _ ) =
            generateRandomIndexes seed [] (Array.length cardColors)
    in
    List.map pickColor indexes


buildCard : String -> CardColor -> Card
buildCard word color =
    { color = color
    , word = String.toLower word
    , state = Hidden
    }


generateCards : Random.Seed -> ( List Card, Random.Seed )
generateCards seed =
    let
        ( newSeed, randomWords ) =
            generateWords seed

        randomColors =
            generateColors seed
    in
    ( List.map2 buildCard randomWords randomColors, newSeed )


init : Int -> ( Model, Cmd Msg )
init seedValue =
    let
        ( initialCards, newSeed ) =
            generateCards (Random.initialSeed seedValue)
    in
    ( { state = RedsTurn
      , mode = Player
      , redCardsLeft = 9
      , blueCardsLeft = 8
      , cards = initialCards
      , seed = newSeed
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Reveal Int Card
    | ToggleTurn
    | ChangeMode PlayerMode
    | NewGame


toggleCard : Int -> Int -> Card -> Card
toggleCard toggleIndex index card =
    if toggleIndex == index then
        { card | state = Visible }

    else
        card


updateCardCount : Card -> Model -> Model
updateCardCount card model =
    { model
        | redCardsLeft =
            if card.color == Red && card.state == Hidden then
                model.redCardsLeft - 1

            else
                model.redCardsLeft
        , blueCardsLeft =
            if card.color == Blue && card.state == Hidden then
                model.blueCardsLeft - 1

            else
                model.blueCardsLeft
    }


updateGameState : Card -> Model -> Model
updateGameState card model =
    { model
        | state =
            if model.blueCardsLeft <= 0 then
                BlueWins

            else if model.redCardsLeft <= 0 then
                RedWins

            else if model.state == BluesTurn then
                case card.color of
                    Blue ->
                        BluesTurn

                    Red ->
                        RedsTurn

                    Yellow ->
                        RedsTurn

                    Black ->
                        RedWins

            else if model.state == RedsTurn then
                case card.color of
                    Blue ->
                        BluesTurn

                    Red ->
                        RedsTurn

                    Yellow ->
                        BluesTurn

                    Black ->
                        BlueWins

            else
                model.state
    }


updatePlayerMode : Model -> Model
updatePlayerMode model =
    { model
        | mode =
            if model.state == RedWins || model.state == BlueWins then
                Spy

            else
                model.mode
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            let
                ( newCards, newSeed ) =
                    generateCards model.seed
            in
            ( { model
                | cards = newCards
                , seed = newSeed
                , state = RedsTurn
                , mode = Player
                , redCardsLeft = 9
                , blueCardsLeft = 8
              }
            , Cmd.none
            )

        ChangeMode newMode ->
            ( { model
                | mode = newMode
              }
            , Cmd.none
            )

        ToggleTurn ->
            ( { model
                | state =
                    if model.state == RedsTurn then
                        BluesTurn

                    else if model.state == BluesTurn then
                        RedsTurn

                    else
                        model.state
              }
            , Cmd.none
            )

        Reveal index card ->
            case model.mode of
                Player ->
                    ( { model | cards = List.indexedMap (toggleCard index) model.cards }
                        |> updateCardCount card
                        |> updateGameState card
                        |> updatePlayerMode
                    , Cmd.none
                    )

                Spy ->
                    ( model, Cmd.none )



-- VIEW
-- Color package; download instead of customizing
-- https://package.elm-lang.org/packages/cappyzawa/elm-ui-colors/latest/Color-Dracula


getCardColor : Card -> Model -> Element.Color
getCardColor card model =
    if card.state == Hidden && model.mode == Player then
        OneDark.gutterGrey

    else
        case card.color of
            Red ->
                OneDark.darkRed

            Blue ->
                OneDark.blue

            Yellow ->
                OneDark.lightYellow

            Black ->
                OneDark.magenta


cardElement : Model -> Int -> Int -> Card -> Element.Element Msg
cardElement model rowNum index card =
    case model.mode of
        Player ->
            Element.el
                [ Background.color (getCardColor card model)
                , width (px 150)
                , Font.center
                , Font.color (Element.rgb 1 1 1)
                , paddingXY 5 30
                , onClick (Reveal (rowNum * 5 + index) card)
                ]
                (text card.word)

        Spy ->
            let
                backgroundColor =
                    if card.state == Visible then
                        getCardColor card model

                    else
                        OneDark.gutterGrey
            in
            Element.el
                [ Background.color backgroundColor
                , width (fill |> minimum 150)
                , Font.center
                , Font.color (Element.rgb 1 1 1)
                , paddingXY 5 25
                , Element.Border.width 5
                , Element.Border.color (getCardColor card model)
                , Element.Border.dashed
                ]
                (text card.word)


whosTurnString : GameState -> String
whosTurnString state =
    case state of
        RedsTurn ->
            "Red's Turn"

        BluesTurn ->
            "Blue's Turn"

        BlueWins ->
            "Blue Wins!!!!"

        RedWins ->
            "Red Wins!!!!"


getRowOfCards : Int -> List Card -> List Card
getRowOfCards rowIndex cards =
    List.drop (rowIndex * 5) cards |> List.take 5


cardRow : Int -> Model -> Element.Element Msg
cardRow rowNum model =
    Element.row [ spacing 30, padding 10 ]
        (List.indexedMap (cardElement model rowNum) (getRowOfCards rowNum model.cards))


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color OneDark.black
        , Font.color OneDark.white
        , Font.regular
        ]
    <|
        Element.column [ centerX ]
            [ el [ centerX, padding 40 ] (text "CODENAMES")

            -- Top Controls
            , Element.row [ padding 10, width fill ]
                -- Left
                [ Element.row [ width (fill |> maximum 150) ]
                    [ el [ Font.color OneDark.lightRed, alignLeft ] (text ("Red: " ++ String.fromInt model.redCardsLeft))
                    , el [] (text " | ")
                    , el [ Font.color OneDark.blue ] (text ("Blue: " ++ String.fromInt model.blueCardsLeft))
                    ]

                -- Center
                , el [ centerX ]
                    (text (whosTurnString model.state))

                -- Right
                , Element.row
                    [ width (fill |> maximum 150) ]
                    [ Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 10, alignRight ]
                        { onPress = Just ToggleTurn
                        , label = text "End Turn"
                        }
                    ]
                ]

            -- Rows of Cards
            , cardRow 0 model
            , cardRow 1 model
            , cardRow 2 model
            , cardRow 3 model
            , cardRow 4 model

            -- Bottom Controls
            , Element.row [ width fill, padding 10, spacing 5 ]
                [ Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 10, alignRight ]
                    { onPress = Just (ChangeMode Player)
                    , label = text "Player"
                    }
                , el [] (text " | ")
                , Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 10 ]
                    { onPress = Just (ChangeMode Spy)
                    , label = text "Spy"
                    }
                , el [] (text " | ")
                , Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 10 ]
                    { onPress = Just NewGame
                    , label = text "New Game"
                    }
                ]
            ]
