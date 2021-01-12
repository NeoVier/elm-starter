module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Html exposing (Html)
import Page.About as PAbout
import Page.Home as PHome
import Page.NotFound as PNotFound
import Route exposing (Route)
import Url



-- MODEL


type Page
    = Home PHome.Model
    | About PAbout.Model
    | NotFound


type alias Model =
    { navKey : Nav.Key, currPage : Page }


type alias Flags =
    {}


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url) { navKey = navKey, currPage = NotFound }



-- MESSAGE


type Msg
    = ChangedUrl Url.Url
    | RequestedUrl Browser.UrlRequest
    | GotHomeMsg PHome.Msg
    | GotAboutMsg PAbout.Msg



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

        ( GotHomeMsg subMsg, Home subModel ) ->
            PHome.update subMsg subModel
                |> updateWith model Home GotHomeMsg

        ( GotAboutMsg subMsg, About subModel ) ->
            PAbout.update subMsg subModel
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


viewPage : Browser.Document a -> (a -> Msg) -> Browser.Document Msg
viewPage page toMsg =
    { title = page.title
    , body =
        List.map (Html.map toMsg) page.body
    }


viewStaticPage : { title : String, body : Element msg } -> Browser.Document msg
viewStaticPage { title, body } =
    { title = title
    , body = [ Element.layout [] body ]
    }


view : Model -> Browser.Document Msg
view model =
    case model.currPage of
        NotFound ->
            viewStaticPage { title = "Not Found", body = PNotFound.view }

        Home subModel ->
            viewPage (PHome.view subModel) GotHomeMsg

        About subModel ->
            viewPage (PAbout.view subModel) GotAboutMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UTILS


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | currPage = NotFound }, Cmd.none )

        Just Route.Home ->
            PHome.init
                |> updateWith model Home GotHomeMsg

        Just Route.About ->
            PAbout.init
                |> updateWith model About GotAboutMsg
