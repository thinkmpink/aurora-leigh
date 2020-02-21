module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as JD
import Url.Builder


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GotText (Result Http.Error String)
    | Search PoemSearchParams
    | GotSearchResults (Result Http.Error (List PoemSearchResult))
    | SetAuthorNamePart String
    | SetAuthorNameMatch MatchType
    | SetTitlePart String
    | SetTitleMatch MatchType
    | SetLinePart String
    | SetLineMatch MatchType
    | SetLineCount Int
    | NoOp


type alias PoemSearchResult =
    { poemTitle : String
    , poemAuthor : String
    , poemText : List String
    , numLines : Int
    }


type alias PoemSearchParams =
    { authorNamePart : String
    , authorNameMatch : MatchType
    , titlePart : String
    , titleMatch : MatchType
    , linePart : String
    , lineMatch : MatchType
    , lineCount : Maybe Int
    }


type alias PoetryDBQuery =
    { poetryDBQueryParams : List PoetryDBParam }


queryableFields : PoemSearchParams -> List String
queryableFields params =
    []
        ++ (case params.authorNamePart of
                "" ->
                    []

                n ->
                    [ "author" ]
           )
        ++ (case params.titlePart of
                "" ->
                    []

                n ->
                    [ "title" ]
           )
        ++ (case params.linePart of
                "" ->
                    []

                n ->
                    [ "lines" ]
           )
        ++ (case params.lineCount of
                Nothing ->
                    []

                Just n ->
                    [ "linecount" ]
           )


queryableVals : PoemSearchParams -> List String
queryableVals params =
    []
        ++ (case params.authorNamePart of
                "" ->
                    []

                n ->
                    case params.authorNameMatch of
                        Exact ->
                            [ n ++ ":abs" ]

                        Inexact ->
                            [ n ]
           )
        ++ (case params.titlePart of
                "" ->
                    []

                n ->
                    case params.titleMatch of
                        Exact ->
                            [ n ++ ":abs" ]

                        Inexact ->
                            [ n ]
           )
        ++ (case params.linePart of
                "" ->
                    []

                n ->
                    case params.lineMatch of
                        Exact ->
                            [ n ++ ":abs" ]

                        Inexact ->
                            [ n ]
           )
        ++ (case params.lineCount of
                Nothing ->
                    []

                Just n ->
                    [ String.fromInt n ]
           )



-- sort of want a non-empty list of these, otherwise json may differ?
-- list of these is more flexible than a record with one of each
-- what if there is meaningful support for multiple author name parts
-- what is the message (kinda depends on UI? what is the least
-- dependent on the UI???)
-- sum type for param is good for disambiguation


type PoetryDBParam
    = AuthorNamePart String MatchType
    | TitlePart String MatchType
    | LinePart String MatchType
    | LineCount Int MatchType


type MatchType
    = Exact
    | Inexact


flipMatch : MatchType -> MatchType
flipMatch match =
    case match of
        Exact ->
            Inexact

        Inexact ->
            Exact


boolToMatch : Bool -> MatchType
boolToMatch b =
    case b of
        True ->
            Exact

        False ->
            Inexact


poetryDBInputField : PoetryDBParam -> String
poetryDBInputField param =
    case param of
        AuthorNamePart _ _ ->
            "author"

        TitlePart _ _ ->
            "title"

        LinePart _ _ ->
            "lines"

        LineCount _ _ ->
            "linecount"


poetryDBMatchType : PoetryDBParam -> String
poetryDBMatchType param =
    case param of
        AuthorNamePart _ t ->
            case t of
                Exact ->
                    ":abs"

                _ ->
                    ""

        TitlePart _ t ->
            case t of
                Exact ->
                    ":abs"

                _ ->
                    ""

        LinePart _ t ->
            case t of
                Exact ->
                    ":abs"

                _ ->
                    ""

        LineCount _ t ->
            case t of
                Exact ->
                    ":abs"

                _ ->
                    ""


poetryDBInputVal : PoetryDBParam -> String
poetryDBInputVal param =
    case param of
        LineCount i _ ->
            String.fromInt i

        AuthorNamePart s _ ->
            s

        TitlePart s _ ->
            s

        LinePart s _ ->
            s


auroraLeighUrl : String
auroraLeighUrl =
    "http://digital.library.upenn.edu/women/barrett/aurora/aurora.html"


poetryDBRoot : String
poetryDBRoot =
    "http://poetrydb.org"


init : () -> ( Model, Cmd Msg )
init _ =
    ( ComposeSearch initModel
    , Cmd.none
    )


initModel : PoemSearchParams
initModel =
    { authorNamePart = ""
    , authorNameMatch = Inexact
    , titlePart = ""
    , titleMatch = Inexact
    , linePart = ""
    , lineMatch = Exact
    , lineCount = Nothing
    }


type Model
    = Loading
    | ComposeSearch PoemSearchParams
    | HasSearchResults (List PoemSearchResult)
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

        Search poemSearchParams ->
            ( Loading
            , Http.get
                { url = makePoetryDBQueryUrl poemSearchParams
                , expect = Http.expectJson GotSearchResults poetryDBDecoder
                }
            )

        GotSearchResults results ->
            case results of
                Err e ->
                    ( Failure e, Cmd.none )

                Ok searchResults ->
                    ( HasSearchResults searchResults
                    , Cmd.none
                      -- eventually cache??
                    )

        SetAuthorNamePart name ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | authorNamePart = name }

                _ ->
                    ComposeSearch { initModel | authorNamePart = name }
            , Cmd.none
            )

        SetTitlePart title ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | titlePart = title }

                _ ->
                    ComposeSearch { initModel | titlePart = title }
            , Cmd.none
            )

        SetLinePart line ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | linePart = line }

                _ ->
                    ComposeSearch { initModel | linePart = line }
            , Cmd.none
            )

        SetLineCount count ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | lineCount = Just count }

                _ ->
                    ComposeSearch { initModel | lineCount = Just count }
            , Cmd.none
            )

        SetAuthorNameMatch match ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | authorNameMatch = match }

                _ ->
                    ComposeSearch { initModel | authorNameMatch = match }
            , Cmd.none
            )

        SetTitleMatch match ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | titleMatch = match }

                _ ->
                    ComposeSearch { initModel | titleMatch = match }
            , Cmd.none
            )

        SetLineMatch match ->
            ( case model of
                ComposeSearch params ->
                    ComposeSearch { params | lineMatch = match }

                _ ->
                    ComposeSearch { initModel | lineMatch = match }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


makePoetryDBQueryUrl : PoemSearchParams -> String
makePoetryDBQueryUrl params =
    let
        inputFields =
            String.join "," <|
                queryableFields params

        searchTerms =
            String.join ";" <|
                queryableVals params
    in
    Url.Builder.crossOrigin
        poetryDBRoot
        [ inputFields
        , searchTerms
        ]
        []


poetryDBDecoder : JD.Decoder (List PoemSearchResult)
poetryDBDecoder =
    JD.list <|
        JD.map4 PoemSearchResult
            (JD.field "title" JD.string)
            (JD.field "author" JD.string)
            (JD.field "lines" <|
                JD.list JD.string
            )
            (JD.field "linecount" JD.int)


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading poem text..." ]

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

        ComposeSearch params ->
            div []
                [ ul []
                    [ li []
                        [ viewInput "text"
                            "Author's name, or part of their name"
                            params.authorNamePart
                            SetAuthorNamePart
                        , check "Exact Author Name" (SetAuthorNameMatch << boolToMatch)
                        ]
                    , li []
                        [ viewInput "text"
                            "Title of poem, or part of the title"
                            params.titlePart
                            SetTitlePart
                        , check "Exact Title" (SetTitleMatch << boolToMatch)
                        ]
                    , li []
                        [ viewInput "text"
                            "Line of the poem, or part of a line"
                            params.linePart
                            SetLinePart
                        , check "Exact Line" (SetLineMatch << boolToMatch)
                        ]
                    , li []
                        [ viewInput "number"
                            "Number of lines in poem"
                            (case params.lineCount of
                                Nothing ->
                                    ""

                                Just count ->
                                    String.fromInt count
                            )
                            (\countStr ->
                                case String.toInt countStr of
                                    Just count ->
                                        SetLineCount count

                                    Nothing ->
                                        NoOp
                            )
                        ]
                    ]
                , button [ onClick <| Search params ] [ text "Find poems!" ]
                ]

        HasText s ->
            div [] [ text s ]

        HasSearchResults results ->
            div []
                [ ul [] <|
                    List.map viewResult results
                ]


viewResult : PoemSearchResult -> Html msg
viewResult item =
    li []
        ([ text item.poemTitle
         , div [] [ text item.poemAuthor ]
         ]
            ++ (case List.head item.poemText of
                    Nothing ->
                        []

                    Just line ->
                        [ div [] [ text line ] ]
               )
        )


check : String -> (Bool -> msg) -> Html msg
check value mapCheck =
    label
        [ style "padding" "20px"
        ]
        [ input [ type_ "checkbox", onCheck mapCheck ] []
        , text value
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
