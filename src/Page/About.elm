module Page.About exposing (Model, Msg(..), init, update, view)

import Element exposing (Element)
import Element.Font
import Http
import Json.Decode exposing (Decoder)



-- MODEL


type alias Fact =
    { fact : String, source : String }


type Model
    = Loading
    | WithFact Fact
    | Errored


init : ( Model, Cmd Msg )
init =
    ( Loading, fetchFact )



-- MESSAGE


type Msg
    = GotFact (Result Http.Error Fact)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotFact (Ok fact) ->
            ( WithFact fact, Cmd.none )

        GotFact (Err _) ->
            ( Errored, Cmd.none )



-- VIEW


view : Model -> { title : String, body : List (Element Msg) }
view model =
    { title = "About"
    , body =
        [ Element.paragraph
            [ Element.Font.bold
            , Element.Font.size 20
            ]
            [ Element.text "This is just an about page" ]
        , Element.paragraph [] [ Element.text "There isn't much here, it's just supposed to demonstrate routing, so here's a random fact to reward you:" ]
        , case model of
            Loading ->
                Element.text "Loading"

            WithFact fact ->
                viewFact fact

            Errored ->
                Element.text "Something went wrong"
        ]
    }


factsAPIUrl : String
factsAPIUrl =
    "https://uselessfacts.jsph.pl"


viewFact : Fact -> Element msg
viewFact fact =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        , Element.paddingXY 20 0
        ]
        [ Element.paragraph [ Element.Font.center, Element.padding 20 ]
            [ Element.el [] <|
                Element.text <|
                    "\""
                        ++ fact.fact
                        ++ "\""
            ]
        , Element.paragraph []
            [ Element.text "Source: "
            , Element.newTabLink []
                { url = fact.source
                , label =
                    Element.el [ Element.Font.underline ] <| Element.text fact.source
                }
            ]
        , Element.paragraph []
            [ Element.text "Powered by "
            , Element.newTabLink [ Element.Font.underline ]
                { url = factsAPIUrl
                , label = Element.text factsAPIUrl
                }
            ]
        ]



-- HTTP


fetchFact : Cmd Msg
fetchFact =
    Http.get
        { url = factsAPIUrl ++ "/random.json?language=en"
        , expect =
            Http.expectJson GotFact decodeFact
        }


decodeFact : Decoder Fact
decodeFact =
    Json.Decode.map2 Fact
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.field "source_url" Json.Decode.string)
