module Route exposing (Route(..), fromUrl, linkToRoute)

import Element exposing (Element)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)



-- ROUTING


type Route
    = Home
    | About


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map About (Parser.s "about")
        ]



-- PUBLIC HELPERS


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


linkToRoute :
    List (Element.Attribute msg)
    -> { route : Route, label : Element msg }
    -> Element msg
linkToRoute attrs { route, label } =
    Element.link attrs { url = toString route, label = label }



-- INTERNAL


toString : Route -> String
toString route =
    "/" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        Home ->
            []

        About ->
            [ "about" ]
