module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Html
import Layout
import Page.About
import Page.Home
import Page.NotFound
import Route exposing (Route)
import Shared exposing (Shared)
import Url



-- MODEL


type Page
    = Home Page.Home.Model
    | About Page.About.Model
    | NotFound


type alias Model =
    { currPage : Page
    , shared : Shared
    }


init : Shared.Dimmensions -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init dimmensions url navKey =
    changeRouteTo (Route.fromUrl url)
        { currPage = NotFound
        , shared = Shared.init dimmensions navKey
        }



-- MESSAGE


type Msg
    = ChangedUrl Url.Url
    | RequestedUrl Browser.UrlRequest
    | GotSharedMsg Shared.Msg
    | GotHomeMsg Page.Home.Msg
    | GotAboutMsg Page.About.Msg



-- MAIN


main : Program Shared.Dimmensions Model Msg
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
                    ( model, Nav.pushUrl model.shared.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotSharedMsg subMsg, _ ) ->
            let
                ( newShared, newCmd ) =
                    Shared.update subMsg model.shared
            in
            ( { model | shared = newShared }, Cmd.map GotSharedMsg newCmd )

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
            viewStaticPage Nothing
                { title = "Not Found"
                , body = [ Page.NotFound.view ]
                }

        Home subModel ->
            viewPage (Just Route.Home)
                (Page.Home.view subModel model.shared.device)
                GotHomeMsg

        About subModel ->
            viewPage (Just Route.About) (Page.About.view subModel) GotAboutMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Shared.subscriptions model.shared
        |> Sub.map GotSharedMsg



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
