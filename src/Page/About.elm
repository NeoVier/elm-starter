module Page.About exposing (Model, Msg(..), init, update, view)

import Browser
import Element exposing (Element)
import Element.Input as Input
import Route



-- MODEL


type alias Model =
    { counter : Int }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0 }, Cmd.none )



-- MESSAGE


type Msg
    = Increment
    | Decrement



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "About"
    , body =
        [ Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] <|
            Element.column []
                [ Element.text "about page"
                , Element.row []
                    [ Input.button [] { onPress = Just Decrement, label = Element.text "-" }
                    , Element.text <| String.fromInt model.counter
                    , Input.button [] { onPress = Just Increment, label = Element.text "+" }
                    ]
                , Route.linkToRoute [] { route = Route.Home, label = Element.text "Go to home" }
                ]
        ]
    }
