module Shared exposing (Dimmensions, Msg, Shared, init, subscriptions, update)

import Browser.Events
import Browser.Navigation as Nav
import Element



-- MODEL


type alias Dimmensions =
    { width : Int, height : Int }


type alias Shared =
    { navKey : Nav.Key
    , device : Element.Device
    }


init : Dimmensions -> Nav.Key -> Shared
init dimmensions navKey =
    { navKey = navKey
    , device = Element.classifyDevice dimmensions
    }



-- MESSAGE


type Msg
    = Resized Dimmensions



-- UPDATE


update : Msg -> Shared -> ( Shared, Cmd Msg )
update msg model =
    case msg of
        Resized newDimmensions ->
            ( { model | device = Element.classifyDevice newDimmensions }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Shared -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> Resized { width = w, height = h })
