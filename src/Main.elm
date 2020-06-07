module Main exposing (..)

import Browser
import Color.OneDark as OneDark
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Words exposing (words)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Mode
    = Spy
    | Player


type Status
    = Started
    | GameOver


type WhosTurn
    = RedsTurn
    | BluesTurn


type CardColor
    = Red
    | Blue
    | Yellow


type CardState
    = Hidden
    | Visible


type alias Card =
    { color : CardColor
    , word : String
    , state : CardState
    }


type alias Model =
    { status : Status
    , mode : Mode
    , whosTurn : WhosTurn
    , redCardsLeft : Int
    , blueCardsLeft : Int
    , cards : List Card
    }


generateWords : List String
generateWords =
    let
        rowIndexes =
            List.range 0 4

        pickWordsForRow rowIndex =
            List.drop (rowIndex * 5) words |> List.take 5
    in
    List.concatMap pickWordsForRow rowIndexes


buildCard : String -> Card
buildCard word =
    { color = Red
    , word = word
    , state = Hidden
    }


generateCards : List Card
generateCards =
    List.map buildCard generateWords


init : Model
init =
    { status = Started
    , mode = Player
    , whosTurn = RedsTurn
    , redCardsLeft = 9
    , blueCardsLeft = 8
    , cards = generateCards
    }



-- UPDATE


type Msg
    = Reveal Int Card
    | ToggleTurn
    | ChangeMode Mode


toggleCard : Int -> Int -> Card -> Card
toggleCard toggleIndex index card =
    if toggleIndex == index then
        { card | state = Visible }

    else
        card


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMode newMode ->
            { model | mode = newMode }

        ToggleTurn ->
            { model
                | whosTurn =
                    case model.whosTurn of
                        RedsTurn ->
                            BluesTurn

                        BluesTurn ->
                            RedsTurn
            }

        Reveal index card ->
            case model.mode of
                Player ->
                    { model
                        | cards = List.indexedMap (toggleCard index) model.cards
                        , whosTurn =
                            if card.state == Hidden then
                                case card.color of
                                    Red ->
                                        RedsTurn

                                    Blue ->
                                        BluesTurn

                                    Yellow ->
                                        case model.whosTurn of
                                            RedsTurn ->
                                                BluesTurn

                                            BluesTurn ->
                                                RedsTurn

                            else
                                model.whosTurn
                        , redCardsLeft =
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

                Spy ->
                    model



-- VIEW
-- Color package; download instead of customizing
-- https://package.elm-lang.org/packages/cappyzawa/elm-ui-colors/latest/Color-Dracula


getCardColor : Card -> Model -> Element.Color
getCardColor card model =
    if card.state == Hidden && model.mode == Player then
        OneDark.gutterGrey

    else if card.color == Red then
        OneDark.darkRed

    else if card.color == Blue then
        OneDark.blue

    else
        OneDark.lightYellow


cardElement : Model -> Int -> Int -> Card -> Element.Element Msg
cardElement model rowNum index card =
    case model.mode of
        Player ->
            Element.el
                [ Background.color (getCardColor card model)
                , centerY
                , centerX
                , padding 25
                , onClick (Reveal (rowNum * 5 + index) card)
                ]
                (text card.word)

        Spy ->
            Element.el
                [ Background.color OneDark.gutterGrey
                , centerY
                , centerX
                , padding 20
                , Element.Border.width 5
                , Element.Border.color (getCardColor card model)
                , Element.Border.dashed
                ]
                (text card.word)


whosTurnString : WhosTurn -> String
whosTurnString whosTurn =
    case whosTurn of
        RedsTurn ->
            "Red's Turn"

        BluesTurn ->
            "Blue's Turn"


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
            [ el [ centerX, padding 50 ] (text "CODENAMES")

            -- Top Controls
            , Element.row [ width fill, padding 10 ]
                [ el [ Font.color OneDark.lightRed ] (text ("Red: " ++ String.fromInt model.redCardsLeft))
                , el [] (text " | ")
                , el [ Font.color OneDark.blue ] (text ("Blue: " ++ String.fromInt model.blueCardsLeft))
                , el [ centerX ]
                    (text (whosTurnString model.whosTurn))
                , Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 5 ]
                    { onPress = Just ToggleTurn
                    , label = text "End Turn"
                    }
                ]

            -- Rows of Cards
            , cardRow 0 model
            , cardRow 1 model
            , cardRow 2 model
            , cardRow 3 model
            , cardRow 4 model

            -- Bottom Controls
            , Element.row [ width fill, padding 10, spacing 10 ]
                [ Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 5, alignRight ]
                    { onPress = Just (ChangeMode Player)
                    , label = text "Player"
                    }
                , Input.button [ Background.color OneDark.blue, Font.color OneDark.lightYellow, padding 5 ]
                    { onPress = Just (ChangeMode Spy)
                    , label = text "Spy"
                    }
                ]
            ]
