module Page.Home exposing (Model, Msg(..), init, update, view)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
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


view : Model -> Element.Device -> { title : String, body : List (Element Msg) }
view model device =
    let
        sections =
            [ homeSection, messagesSection, routingSection, responsivenessSection device ]
    in
    { title = "Home"
    , body =
        [ List.map (\section -> section model |> viewSection) sections
            |> Element.column [ Element.width Element.fill, Element.spacing 50 ]
        ]
    }


viewCode : String -> Element Msg
viewCode code =
    Element.paragraph
        [ Element.Font.family [ Element.Font.monospace ]
        , Element.Background.color <| Element.rgb255 234 234 234
        ]
        [ Element.text code ]


viewSection : { title : String, body : List (Element Msg) } -> Element Msg
viewSection { title, body } =
    (Element.paragraph [ Element.Font.bold, Element.padding 25, Element.Font.size 20 ]
        [ Element.text title ]
        :: body
    )
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing 20
            , Element.Font.size 18
            ]


homeSection : Model -> { title : String, body : List (Element Msg) }
homeSection _ =
    { title = "Congratulations! You're in the homepage"
    , body =
        [ Element.paragraph []
            [ Element.text "Read below to see some example usage" ]
        ]
    }


messagesSection : Model -> { title : String, body : List (Element Msg) }
messagesSection model =
    let
        customButton attrs =
            Element.Input.button
                ([ Element.paddingXY 20 10
                 , Element.Border.rounded 5
                 , Element.Font.color <| Element.rgb 1 1 1
                 ]
                    ++ attrs
                )
    in
    { title = "Using messages"
    , body =
        [ Element.paragraph []
            [ Element.text "Each page has its own kind of "
            , viewCode "Msg"
            , Element.text ", and "
            , viewCode "Main.elm"
            , Element.text " has a type that encapsulates each Page "
            , viewCode "Msg"
            , Element.text " to update the global ("
            , viewCode "Main.elm"
            , Element.text "'s) model, along with the page model. "
            , Element.text "Below is an example counter, to show the usage of "
            , viewCode "Msg"
            , Element.text " in pages:"
            ]
        , Element.row
            [ Element.centerX
            , Element.spacing 20
            ]
            [ customButton
                [ Element.Background.color <|
                    Element.rgb255 0xDB 0x5E 0x5E
                ]
                { onPress = Just Decrement
                , label = Element.text "-"
                }
            , Element.el
                [ Element.width <| Element.px 50
                , Element.Font.center
                ]
              <|
                Element.text <|
                    String.fromInt model.counter
            , customButton
                [ Element.Background.color <|
                    Element.rgb255 0x5E 0xDB 0x60
                ]
                { onPress = Just Increment
                , label = Element.text "+"
                }
            ]
        ]
    }


routingSection : Model -> { title : String, body : List (Element Msg) }
routingSection _ =
    { title = "Routing to other pages"
    , body =
        [ Element.paragraph []
            [ Element.text "Routing to other pages in the application simply means changing the "
            , viewCode "currPage"
            , Element.text " in "
            , viewCode "Main.Model"
            , Element.text ", and, if necessary, executing the appropriate "
            , viewCode "init"
            , Element.text " method."
            ]
        , Element.paragraph []
            [ Element.text "The "
            , viewCode "Route"
            , Element.text " module gives us a type-safe way of defining, parsing and selecting routes in an application. It defines every possible route in the "
            , viewCode "Route"
            , Element.text " type. Each "
            , viewCode "Route"
            , Element.text " should have a parser, and a "
            , viewCode "routeToPieces"
            , Element.text " implementation. This alone is enough for defining a new route."
            ]
        , Element.paragraph []
            [ Element.text "To change routes in the application, you should use "
            , viewCode "Route.linkToRoute"
            , Element.text ": it's the same as "
            , viewCode "Element.link"
            , Element.text ", but takes a "
            , viewCode "Route"
            , Element.text " instead of a url"
            ]
        , Element.paragraph []
            [ Element.text "So, for example, you could "
            , Route.linkToRoute [ Element.Font.underline ] { route = Route.About, label = Element.text "go to the about page" }
            , Element.text " in a type-safe way. If you ever decide to change the url of the route, you just need to change that in the "
            , viewCode "Route"
            , Element.text " module."
            ]
        ]
    }


responsivenessSection : Element.Device -> Model -> { title : String, body : List (Element Msg) }
responsivenessSection device _ =
    { title = "Handling responsiveness"
    , body =
        [ Element.paragraph []
            [ Element.text "As we use "
            , Element.newTabLink [ Element.Font.underline ]
                { url = "https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/"
                , label = viewCode "elm-ui"
                }
            , Element.text " for layout and styling, most things we make are already somewhat responsive. However, you may need to handle some special cases."
            ]
        , Element.paragraph []
            [ Element.text "For that, the global model keeps an "
            , viewCode "Element.Device"
            , Element.text " field, which specifies the "
            , viewCode "DeviceClass"
            , Element.text " ("
            , viewCode "Phone"
            , Element.text ", "
            , viewCode "Tablet"
            , Element.text ", "
            , viewCode "Desktop"
            , Element.text " or "
            , viewCode "BigDesktop"
            , Element.text "), and "
            , viewCode "Orientation"
            , Element.text " ("
            , viewCode "Portrait"
            , Element.text " or "
            , viewCode "Landscape"
            , Element.text ")"
            ]
        , Element.paragraph []
            [ Element.text <|
                "So, for example, I know you're reading this in a "
            , Element.el
                [ Element.Font.bold ]
              <|
                Element.text <|
                    deviceClassToString device.class
            , Element.text ", in "
            , Element.el [ Element.Font.bold ] <| Element.text <| deviceOrientationToString device.orientation
            , Element.text " mode (you can try resizing your browser window and see how it changes)."
            ]
        ]
    }


deviceClassToString : Element.DeviceClass -> String
deviceClassToString deviceClass =
    case deviceClass of
        Element.Phone ->
            "phone"

        Element.Tablet ->
            "tablet"

        Element.Desktop ->
            "desktop"

        Element.BigDesktop ->
            "big desktop"


deviceOrientationToString : Element.Orientation -> String
deviceOrientationToString orientation =
    case orientation of
        Element.Portrait ->
            "portrait"

        Element.Landscape ->
            "landscape"
