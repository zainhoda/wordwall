port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Random


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- PORTS


port sendMessage : String -> Cmd msg



-- MODEL


type alias Model =
    { draft : String
    , messages : List String
    , isAdding : Bool
    , numFound : Int
    , numMissing : Int
    , whichWord : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draft = "", messages = [], isAdding = True, numFound = 0, numMissing = 1, whichWord = 0 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DraftChanged String
    | Send
    | Recv String
    | DoneAdding
    | Replay
    | NextWord
    | MoreMissing



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }
            , Cmd.none
            )

        Send ->
            ( { model | draft = "", messages = model.messages ++ [ model.draft ] }
            , sendMessage model.draft
            )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }
            , Cmd.none
            )

        DoneAdding ->
            ( { model | isAdding = False }
            , sendMessage (("What are the missing letters in " ++ (getMysteryWord model) ++"?"))
            )

        Replay ->
            ( model
            , sendMessage (getMysteryWord model)
            )

        NextWord ->
            let newModel = {model | whichWord = 
                                      if List.length model.messages > model.whichWord+1 then
                                        model.whichWord + 1
                                      else
                                        0
                           }
            in
            ( newModel
            , sendMessage (("What are the missing letters in " ++ (getMysteryWord newModel) ++"?"))
            )

        MoreMissing ->
          ( { model | numMissing = model.numMissing + 1}
          , sendMessage (("What are the missing letters in " ++ (getMysteryWord model) ++"?"))
          )

-- SUBSCRIPTIONS
-- VIEW


view : Model -> Html Msg
view model =
    if model.isAdding then
        wordInputView model
    else
        mysteryWordView model


getMysteryWord : Model -> String
getMysteryWord model =
    case (List.head <| List.drop model.whichWord model.messages) of
        Nothing ->
          ""
        Just word ->
          word

mysteryWordView : Model -> Html Msg
mysteryWordView model =
    div [ class "container is-fluid" ]
        [ h1 [ class "title" ] [ text ("What are the missing letters?") ]
        , mysteryLetter (getMysteryWord model) model.numFound model.numMissing
        , button [class "button is-info", onClick Replay] [text "🔊 Replay"]
        , button [class "button is-primary", onClick NextWord] [text "➡️ Next Word"]
        , button [class "button is-warning", onClick MoreMissing] [text "🆙 Next Level"]
        ]


mysteryLetter : String -> Int -> Int -> Html Msg
mysteryLetter word numberFound numberMissing =
    div []
        (List.indexedMap
            (\i ->
                \letter ->
                    if i < (numberMissing - numberFound) then
                        div [ class "button is-light is-large m-2" ] [ text "🤔" ]
                    else
                        div [ class "button is-light is-large m-2" ] [ text (String.fromChar letter) ]
            )
            (String.toList word)
        )


wordInputView : Model -> Html Msg
wordInputView model =
    div [ class "container is-fluid" ]
        [ h1 [ class "title is-1" ] [ text "Enter Your Words" ]
        , wordAdder model.draft
        , doneButton
        , wordList model.messages
        ]


doneButton : Html Msg
doneButton =
    button [ class "button is-danger", onClick DoneAdding ] [ text "Done Adding Words" ]


wordAdder : String -> Html Msg
wordAdder newPartialWord =
    div []
        [ input
            [ type_ "text"
            , class "input is-primary"
            , placeholder "Add a word here"
            , onInput DraftChanged
            , on "keydown" (ifIsEnter Send)
            , value newPartialWord
            ]
            []
        , button [ class "button is-primary", onClick Send ] [ text "Add" ]
        ]


wordList : List String -> Html Msg
wordList listOfWords =
    ul [] (List.map (\word -> li [ class "title" ] [ text word ]) listOfWords)



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg
                else
                    D.fail "some other key"
            )
