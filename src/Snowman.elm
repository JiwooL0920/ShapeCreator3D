module Snowman exposing (Msg(..), WorldCoordinates(..), camera, init, update, view)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Cone3d
import Cylinder3d
import Direction3d
import Html exposing (Html)
import Html.Attributes
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LineSegment3d
import Pixels exposing (Pixels)
import Plane3d
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Entity exposing (Entity)
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Task
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { width : Quantity Float Pixels -- Width of the browser window
    , height : Quantity Float Pixels -- Height of the browser window
    , azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    }


type Msg
    = Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | MouseDown


init : ( Model, Cmd Msg )
init =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , azimuth = Angle.degrees 30
      , elevation = Angle.degrees 30
      , orbiting = False
      }
    , Task.perform
        (\{ viewport } ->
            Resize
                (Pixels.pixels viewport.width)
                (Pixels.pixels viewport.height)
        )
        Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees 5) (Angle.degrees 85)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )


camera : Model -> Camera3d Meters WorldCoordinates
camera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.origin
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.centimeters 90
                }
        , verticalFieldOfView = Angle.degrees 30
        }


sunlight : Light WorldCoordinates Bool
sunlight =
    Light.directional (Light.castsShadows True)
        { chromaticity = Light.sunlight
        , intensity = Illuminance.lux 20000
        , direction = Direction3d.xyZ (Angle.degrees 0) (Angle.degrees -135)
        }

view : Model -> Html msg
view model =
    Html.div [ Html.Attributes.style "width" "100%", Html.Attributes.style "width" "100%"]
        [ Scene3d.custom
            { camera = camera model
            , clipDepth = Length.centimeters 0.5
            , entities = entities ++ debugs
            , whiteBalance = Light.daylight
            , lights = Scene3d.oneLight sunlight
            , antialiasing = Scene3d.multisampling
            , exposure = Scene3d.exposureValue 11
            , toneMapping = Scene3d.hableFilmicToneMapping
            , background = Scene3d.transparentBackground
            , dimensions = ( model.width, model.height )
            }
        ]


debugs : List (Entity WorldCoordinates)
debugs =
    [ Scene3d.lineSegment (Material.color Color.darkRed)
        (LineSegment3d.along Axis3d.x (Length.centimeters 0) (Length.centimeters 999))
    , Scene3d.lineSegment (Material.color Color.darkGreen)
        (LineSegment3d.along Axis3d.y (Length.centimeters 0) (Length.centimeters 999))
    , Scene3d.lineSegment (Material.color Color.darkBlue)
        (LineSegment3d.along Axis3d.z (Length.centimeters 0) (Length.centimeters 999))
    ]


entities : List (Entity WorldCoordinates)
entities =
    let
        sphere size =
            Sphere3d.withRadius (Length.centimeters size) Point3d.origin
                |> Scene3d.sphereWithShadow bodyMaterial

        bodyMaterial =
            Material.matte Color.white

        floor =
            Scene3d.quad (Material.matte Color.darkGray)
                (Point3d.centimeters -70 -70 0)
                (Point3d.centimeters 70 -70 0)
                (Point3d.centimeters 70 70 0)
                (Point3d.centimeters -70 70 0)

        bottom =
            sphere 5

        body =
            let
                stick len =
                    Cylinder3d.along Axis3d.y
                        { start = Length.centimeters 0
                        , end = len
                        , radius = Length.centimeters 0.1
                        }
                        |> Scene3d.cylinderWithShadow (Material.matte Color.darkBrown)

                rightArm =
                    Scene3d.group
                        [ stick (Length.centimeters 6)
                        , stick (Length.centimeters 0.75)
                            |> Scene3d.translateIn Direction3d.y (Length.centimeters 6)
                        , stick (Length.centimeters 0.75)
                            |> Scene3d.rotateAround Axis3d.x (Angle.degrees 45)
                            |> Scene3d.translateIn Direction3d.y (Length.centimeters 6)
                        , stick (Length.centimeters 0.75)
                            |> Scene3d.rotateAround Axis3d.x (Angle.degrees -45)
                            |> Scene3d.translateIn Direction3d.y (Length.centimeters 6)
                        ]
                        |> Scene3d.rotateAround Axis3d.x (Angle.degrees 20)

                leftArm =
                    rightArm
                        |> Scene3d.mirrorAcross Plane3d.zx
            in
            Scene3d.group
                [ sphere 3
                , rightArm
                , leftArm
                ]

        face =
            let
                rightEye =
                    Sphere3d.withRadius (Length.centimeters 0.3) Point3d.origin
                        |> Scene3d.sphere (Material.matte Color.black)
                        |> Scene3d.translateIn Direction3d.x (Length.centimeters 2)
                        |> Scene3d.rotateAround Axis3d.y (Angle.degrees -30)
                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees 30)

                leftEye =
                    rightEye
                        |> Scene3d.mirrorAcross Plane3d.zx

                nose =
                    Cone3d.along Axis3d.x
                        { base = Length.centimeters 0
                        , tip = Length.centimeters 1.5
                        , radius = Length.centimeters 0.3
                        }
                        |> Scene3d.coneWithShadow (Material.matte Color.red)
                        |> Scene3d.translateIn Direction3d.x (Length.centimeters 1.6)
                        |> Scene3d.translateIn Direction3d.z (Length.centimeters 0.4)
            in
            Scene3d.group
                [ sphere 2
                , nose
                , rightEye
                , leftEye
                ]
    in
    [ floor
    , bottom
        |> Scene3d.translateIn Direction3d.z (Length.centimeters 5)
    , body
        |> Scene3d.translateIn Direction3d.z (Length.centimeters 10)
    , face
        |> Scene3d.translateIn Direction3d.z (Length.centimeters 14)
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render the full size of the
          -- browser window
          Browser.Events.onResize
            (\width height ->
                Resize
                    (Pixels.pixels (toFloat width))
                    (Pixels.pixels (toFloat height))
            )
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
