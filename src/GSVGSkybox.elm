port module GSVGSkybox exposing (..)

import Html exposing(Html,div,button)
import Html.Events exposing(onClick)
import Html.Attributes exposing (style)
import Browser
import GraphicSVG exposing(..)
import GraphicSVG.Widget as Widget
import Html.Events exposing (onClick)

import Task


type alias WidgetID = String

-- tell JavaScript to create a PNG with a given name (must match the id of the widget)
port createPNG : (WidgetID, Int, Int) -> Cmd msg

type alias URL = String

port receivePNG : ((WidgetID, URL) -> msg) -> Sub msg

top : Shape msg
top = group
    [
        square 50 |> filled lightBlue
    ,  circle 20
            |> filled yellow
    ,  circle 5
            |> filled black
            |> move (-7,7)
    ,  circle 5
            |> filled black
            |> move (7,7)
    ,  wedge 10 0.5
            |> filled black
            |> rotate (degrees -90)
            |> move (0,-5)
    ]

bottom : Shape msg
bottom = group
    [
        square 50 |> filled darkBlue
    ]

sides : Shape msg
sides = group
    [
        rect 200 50
            |> filled (rgb 135 206 235)
    ,   text "abcdefghijklmnopqrstuvwxyz" |> centered |> filled black
    ]


type alias Model =
    { topWidget : Widget.Model
    , botWidget : Widget.Model
    , side1Widget : Widget.Model
    , side2Widget : Widget.Model
    , side3Widget : Widget.Model
    , side4Widget : Widget.Model
    , showBorders : Bool
    , topPNG : String
    , botPNG : String
    , side1PNG : String
    , side2PNG : String
    , side3PNG : String
    , side4PNG : String
    }


initialModel : (Model, Cmd Msg)
initialModel =
    let
        (topWidget, topWCmd) = Widget.init 50 50 "skyT"
        (botWidget, botWCmd) = Widget.init 50 50 "skyB"
        (side1Widget, s1WCmd) = Widget.init 50 50 "skyS1"
        (side2Widget, s2WCmd) = Widget.init 50 50 "skyS2"
        (side3Widget, s3WCmd) = Widget.init 50 50 "skyS3"
        (side4Widget, s4WCmd) = Widget.init 50 50 "skyS4"
    in
    ({ topWidget = topWidget
    , botWidget = botWidget
    , side1Widget = side1Widget
    , side2Widget = side2Widget
    , side3Widget = side3Widget
    , side4Widget = side4Widget
    , showBorders = True
    , topPNG = ""
    , botPNG = ""
    , side1PNG = ""
    , side2PNG = ""
    , side3PNG = ""
    , side4PNG = ""
    }
    ,Cmd.batch [Cmd.map TopWidgetMsg topWCmd
               ,Cmd.map BotWidgetMsg botWCmd
               ,Cmd.map Side1Widget s1WCmd
               ,Cmd.map Side2Widget s2WCmd
               ,Cmd.map Side3Widget s3WCmd
               ,Cmd.map Side4Widget s4WCmd
               ]
    )


initialModelWithLoad : (Model, Cmd Msg)
initialModelWithLoad =
    let
        (topWidget, topWCmd) = Widget.init 50 50 "skyT"
        (botWidget, botWCmd) = Widget.init 50 50 "skyB"
        (side1Widget, s1WCmd) = Widget.init 50 50 "skyS1"
        (side2Widget, s2WCmd) = Widget.init 50 50 "skyS2"
        (side3Widget, s3WCmd) = Widget.init 50 50 "skyS3"
        (side4Widget, s4WCmd) = Widget.init 50 50 "skyS4"
    in
    ({ topWidget = topWidget
    , botWidget = botWidget
    , side1Widget = side1Widget
    , side2Widget = side2Widget
    , side3Widget = side3Widget
    , side4Widget = side4Widget
    , showBorders = False
    , topPNG = ""
    , botPNG = ""
    , side1PNG = ""
    , side2PNG = ""
    , side3PNG = ""
    , side4PNG = ""
    }
    ,Cmd.batch [Cmd.map TopWidgetMsg topWCmd
               ,Cmd.map BotWidgetMsg botWCmd
               ,Cmd.map Side1Widget s1WCmd
               ,Cmd.map Side2Widget s2WCmd
               ,Cmd.map Side3Widget s3WCmd
               ,Cmd.map Side4Widget s4WCmd
               ,createPNG ("skyT", 1024, 1024)
               ,createPNG ("skyB", 1024, 1024)
               ,createPNG ("skyS1", 1024, 1024)
               ,createPNG ("skyS2", 1024, 1024)
               ,createPNG ("skyS3", 1024, 1024)
               ,createPNG ("skyS4", 1024, 1024)
               ]
    )

type Msg =
      TopWidgetMsg Widget.Msg
    | BotWidgetMsg Widget.Msg
    | Side1Widget Widget.Msg
    | Side2Widget Widget.Msg
    | Side3Widget Widget.Msg
    | Side4Widget Widget.Msg
    | GeneratePNG String
    | BordersGone String
    | ReceivePNG (String, URL)
    | AllLoaded (List (String, URL))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TopWidgetMsg wMsg ->
            let
                (newWState, wCmd) = Widget.update wMsg model.topWidget
            in
            ({ model | topWidget = newWState
                        }
            , Cmd.map TopWidgetMsg wCmd)

        BotWidgetMsg wMsg ->
            let
                (newWState, wCmd) = Widget.update wMsg model.botWidget
            in
            ({ model | botWidget = newWState
                        }
            , Cmd.map TopWidgetMsg wCmd)

        Side1Widget wMsg ->
            let
                (newWState, wCmd) = Widget.update wMsg model.side1Widget
            in
            ({ model | side1Widget = newWState
                        }
            , Cmd.map TopWidgetMsg wCmd)

        Side2Widget wMsg ->
            let
                (newWState, wCmd) = Widget.update wMsg model.side2Widget
            in
            ({ model | side2Widget = newWState
                        }
            , Cmd.map TopWidgetMsg wCmd)

        Side3Widget wMsg ->
            let
                (newWState, wCmd) = Widget.update wMsg model.side3Widget
            in
            ({ model | side3Widget = newWState
                        }
            , Cmd.map TopWidgetMsg wCmd)

        Side4Widget wMsg ->
            let
                (newWState, wCmd) = Widget.update wMsg model.side4Widget
            in
            ({ model | side4Widget = newWState
                        }
            , Cmd.map TopWidgetMsg wCmd)

        GeneratePNG id ->
            ( { model | showBorders = False },
            Cmd.batch
                [
                    Task.perform identity (Task.succeed (BordersGone "skyT"))
                ,   Task.perform identity (Task.succeed (BordersGone "skyB"))
                ,   Task.perform identity (Task.succeed (BordersGone "skyS1"))
                ,   Task.perform identity (Task.succeed (BordersGone "skyS2"))
                ,   Task.perform identity (Task.succeed (BordersGone "skyS3"))
                ,   Task.perform identity (Task.succeed (BordersGone "skyS4"))
                ]
            )

        BordersGone id ->
            ( model
            , createPNG (id, 1024, 1024)
            )

        ReceivePNG (widgetID, imageURL) ->
            let
                newModel =
                    case widgetID of
                        "skyT" -> { model | topPNG = imageURL }
                        "skyB" -> { model | botPNG = imageURL }
                        "skyS1" -> { model | side1PNG = imageURL }
                        "skyS2" -> { model | side2PNG = imageURL }
                        "skyS3" -> { model | side3PNG = imageURL }
                        "skyS4" -> { model | side4PNG = imageURL }
                        _ -> model
            in
            (newModel
            , if newModel.topPNG == "" || newModel.botPNG == "" || newModel.side1PNG == "" || newModel.side2PNG == "" || newModel.side3PNG == "" || newModel.side4PNG == "" then
                Cmd.none
              else
                Task.perform identity <| Task.succeed (AllLoaded [("skyB", newModel.botPNG), ("skyT", newModel.topPNG), ("skyS1", newModel.side1PNG), ("skyS2", newModel.side2PNG), ("skyS3", newModel.side3PNG), ("skyS4", newModel.side4PNG)] )
            )

        AllLoaded skyboxList -> --handled by larger app
            ( model , Cmd.none )



showBorder : String -> Bool -> List (Shape msg) -> List (Shape msg)
showBorder name show sh =
    if show then
        sh ++ [
           square 50
                |> outlined (solid 1) black
        , text name
            |> fixedwidth
            |> size 2
            |> filled black
            |> move (-23,21)
        ]
    else sh

view : Model -> Html Msg
view model =
    drawSkybox True model top sides bottom


drawSkybox : Bool -> Model -> Shape Msg -> Shape Msg -> Shape Msg -> Html Msg
drawSkybox show model topS sidesS bottomS =
    div ((if show then [style "display" "block"] else [style "display" "none"]))
        [ div [style "width" "24%"{-style "margin-left" "auto", style "margin-right" "auto",-}, style "margin-left" "25%"]
            [
                Widget.view model.topWidget
                ([
                    topS
                ] |> showBorder "Top" show)
            ]
            ,div [style "width" "24%", style "float" "left"]
            [
                Widget.view model.side1Widget
                ([
                    sidesS |> move (75,0)
                ] |> showBorder "Side 1" show)
            ]
            ,div [style "width" "24%", style "float" "left", style "margin-left" "1%"]
            [
                Widget.view model.side2Widget
                ([
                   sidesS |> move (25,0)

                ] |> showBorder "Side 2" show)
            ]
            ,div [style "width" "24%", style "float" "left", style "margin-left" "1%"]
            [
                Widget.view model.side3Widget
                ([
                   sidesS |> move (-25,0)

                ] |> showBorder "Side 3" show)
            ]
            ,div [style "width" "24%", style "float" "left", style "margin-left" "1%"]
            [
                Widget.view model.side4Widget
                ([
                    sidesS |> move (-75,0)
                ] |> showBorder "Side 4" show)
            ]
            ,div [style "width" "24%", style "margin-left" "25%"]
            [
                Widget.view model.botWidget
                ([
                    bottomS
                ] |> showBorder "Bottom" show)
            ]
            ,button [onClick (GeneratePNG "skyT")]
                [Html.text "Download All"]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions = \_ ->
                    Sub.batch
                        [
                            Sub.map TopWidgetMsg Widget.subscriptions
                        ,   Sub.map BotWidgetMsg Widget.subscriptions
                        ,   Sub.map Side1Widget Widget.subscriptions
                        ,   Sub.map Side2Widget Widget.subscriptions
                        ,   Sub.map Side3Widget Widget.subscriptions
                        ,   Sub.map Side4Widget Widget.subscriptions
                        ,   receivePNG ReceivePNG
                        ]