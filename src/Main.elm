port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, br, input)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (onInput, onClick)
import Html.Lazy exposing (lazy)
import List exposing (foldr)


main =
  Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type MessageType
    = Incoming
    | Outgoing

      
type alias Message =
    { contents :  String
    , messageType : MessageType }

      
type alias Model =
    { messages : List Message
    , messageBoxValue : String
    }


init : () -> (Model, Cmd Msg)
init f =
    ({ messages = [], messageBoxValue = "" }
    , Cmd.none)


type Msg
    = GotMessage String
    | SendMessage
    | MessageBoxChanged String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotMessage messageText ->
            let
                newMessage =
                    { contents = messageText
                    , messageType = Incoming
                    }
                newModel = { model | messages = [newMessage] ++ model.messages }
            in
                (newModel, Cmd.none)

        SendMessage ->
            let
                newMessage =
                    { contents = model.messageBoxValue
                    , messageType = Outgoing
                    }
                newModel = { model |
                                 messageBoxValue = "",
                                 messages = [newMessage] ++ model.messages
                           }
            in
                (newModel, outgoingMessage model.messageBoxValue)

        MessageBoxChanged newText -> ({ model | messageBoxValue = newText }, Cmd.none)

    
renderMessage : Message -> Html Msg
renderMessage msg =
    let
        arrow =
            case msg.messageType of
                Outgoing -> "->"
                Incoming -> "<-"
        contents =
            arrow  ++ " " ++ msg.contents
    in
        text contents


renderMessages : List Message -> Html Msg
renderMessages messages =
    div []
        ( messages
        |> List.map renderMessage
        |> List.intersperse (br [] [])
        )
                        

renderModel : Model -> Html Msg
renderModel model =
    div []
        [ input [ placeholder "Message to send", value model.messageBoxValue, onInput MessageBoxChanged ] []
        , button [ onClick SendMessage ] [ text "Send message" ]
        , renderMessages model.messages
        ]
    

view : Model -> Browser.Document Msg
view model =
    { title = "Websockets example"
    , body = [renderModel model]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    incomingMessage GotMessage


port incomingMessage : (String -> msg) -> Sub msg
port outgoingMessage : String -> Cmd msg
