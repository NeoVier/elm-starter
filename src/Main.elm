module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (Element)
import Html
import Layout
import Page.About
import Page.Home
import Page.NotFound
import Route exposing (Route)
import Url



-- MODEL


type Page
    = Home Page.Home.Model
    | About Page.About.Model
    | NotFound


type alias Model =
    { navKey : Nav.Key, currPage : Page, device : Element.Device }


type alias Dimmensions =
    { width : Int, height : Int }


type alias Flags =
    Dimmensions


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url)
        { navKey = navKey
        , currPage = NotFound
        , device = Element.classifyDevice flags
        }



-- MESSAGE


type Msg
    = ChangedUrl Url.Url
    | RequestedUrl Browser.UrlRequest
    | Resized Dimmensions
    | GotHomeMsg Page.Home.Msg
    | GotAboutMsg Page.About.Msg



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = RequestedUrl
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currPage ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( RequestedUrl request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( Resized dimm, _ ) ->
            ( { model | device = Element.classifyDevice dimm }, Cmd.none )

        ( GotHomeMsg subMsg, Home subModel ) ->
            Page.Home.update subMsg subModel
                |> updateWith model Home GotHomeMsg

        ( GotAboutMsg subMsg, About subModel ) ->
            Page.About.update subMsg subModel
                |> updateWith model About GotAboutMsg

        -- Invalid messages
        ( GotHomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotAboutMsg _, _ ) ->
            ( model, Cmd.none )


updateWith :
    Model
    -> (subModel -> Page)
    -> (subMsg -> Msg)
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith model toPage toMsg ( subModel, subCmd ) =
    ( { model | currPage = toPage subModel }, Cmd.map toMsg subCmd )



-- VIEW


viewPage :
    Maybe Route
    -> { title : String, body : List (Element subMsg) }
    -> (subMsg -> Msg)
    -> Browser.Document Msg
viewPage activeRoute page toMsg =
    let
        layoutView =
            Layout.view
                activeRoute
                { title = page.title
                , body = page.body
                }
    in
    { title = layoutView.title
    , body = [ layoutView.body |> Html.map toMsg ]
    }


viewStaticPage :
    Maybe Route
    -> { title : String, body : List (Element msg) }
    -> Browser.Document msg
viewStaticPage activeRoute page =
    let
        layoutView =
            Layout.view activeRoute { title = page.title, body = page.body }
    in
    { title = layoutView.title
    , body = [ layoutView.body ]
    }


view : Model -> Browser.Document Msg
view model =
    case model.currPage of
        NotFound ->
            viewStaticPage Nothing { title = "Not Found", body = [ Page.NotFound.view ] }

        Home subModel ->
            viewPage (Just Route.Home) (Page.Home.view subModel model.device) GotHomeMsg

        About subModel ->
            viewPage (Just Route.About) (Page.About.view subModel) GotAboutMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> Resized { width = w, height = h })



-- UTILS


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | currPage = NotFound }, Cmd.none )

        Just Route.Home ->
            Page.Home.init
                |> updateWith model Home GotHomeMsg

        Just Route.About ->
            Page.About.init
                |> updateWith model About GotAboutMsg
