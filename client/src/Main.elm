module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GotText (Result Http.Error String)


auroraLeighUrl : String
auroraLeighUrl =
    "http://digital.library.upenn.edu/women/barrett/aurora/aurora.html"


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = auroraLeighUrl
        , expect = Http.expectString GotText
        }
    )


type Model
    = Loading
    | Failure Http.Error
    | HasText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok t ->
                    ( HasText t, Cmd.none )

                Err e ->
                    ( Failure e, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading full text of Aurora Leigh..." ]

        Failure e ->
            div []
                [ case e of
                    Http.BadUrl u ->
                        text <| "Bad url: " ++ u

                    Http.Timeout ->
                        text "Request timed out"

                    Http.NetworkError ->
                        text "Network error"

                    Http.BadStatus i ->
                        text <| "Bad status: " ++ String.fromInt i

                    Http.BadBody b ->
                        text <| "Bad body: " ++ b
                ]

        HasText s ->
            div [] [ text s ]
