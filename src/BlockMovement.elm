-- A lot of this file was just Frankenstein'd together from the existing elm-3d-scene examples,
-- but it should be useful as a starting point.

module BlockMovement exposing (main)

import Angle exposing (Angle)
import Array
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Length exposing (Meters)
import LuminousFlux
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Entity as Entity
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d
import Vector3d
import Viewpoint3d

type WorldCoordinates = WorldCoordinates

type Direction
    = Up
    | Down
    | Left
    | Right
    | Forward
    | Backward
    | None

speed : Float
speed = 1.0

aluminum : Material.Uniform WorldCoordinates
aluminum =
    Material.metal
        { baseColor = Color.rgb255 233 235 236
        , roughness = 0.6
        }

myMat : Color.Color -> Material.Uniform WorldCoordinates
myMat colour = Material.nonmetal { baseColor = colour, roughness = 0.2 }

floor : Scene3d.Entity WorldCoordinates
floor =
    -- Create a thin block to act as a 'floor' that shadows will be cast on;
    -- no need to use Material.uniform here since blocks _only_ support uniform
    -- materials
    Scene3d.block (myMat Color.lightBlue) <|
        Block3d.from
            (Point3d.centimeters -100 -100 -22)
            (Point3d.centimeters 100 100 -20)

-- Some useful functions to start drawing stuff

cube : Color.Color -> Float -> Scene3d.Entity WorldCoordinates
cube colour size = Scene3d.blockWithShadow (myMat colour) <|
        Block3d.from
            (Point3d.centimeters -size -size -size)
            (Point3d.centimeters size size size)

getDirection : Float -> Float -> Float -> Direction3d coords
getDirection x y z = let direction = Direction3d.from Point3d.origin (Point3d.meters x y z)
    in case direction of
       Nothing -> Direction3d.y
       Just dir -> dir

translate : Float -> (Float, Float, Float) -> Entity.Entity coords -> Entity.Entity coords
translate amount direction entity = case (amount, direction, entity) of
    (amt, (x,y,z), ent) -> ent |> Entity.translateBy (Vector3d.withLength (Length.centimeters amt) (getDirection x y z))

translateDir : Float -> Direction3d coords -> Entity.Entity coords -> Entity.Entity coords
translateDir amount direction entity = case (amount, direction, entity) of
    (amt, dir, ent) -> ent |> Entity.translateBy (Vector3d.withLength (Length.centimeters amt) dir)

makeAxis : Float -> Float -> Float -> Axis3d.Axis3d Meters coords
makeAxis x y z = Axis3d.through Point3d.origin (getDirection x y z)

-- Rotate an entity around an axis. I recommend using makeAxis above to create the axis of rotation.
rotate : Axis3d.Axis3d Meters coords -> Float -> Entity.Entity coords -> Entity.Entity coords
rotate axis angle entity = Entity.rotateAround axis (Angle.radians angle) entity

camera : Model -> Camera3d Meters WorldCoordinates
camera model =
    -- Create a perspective camera based on the current azimuth/elevation from
    -- the model
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.centimeters 0 0 -20
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }
        , verticalFieldOfView = Angle.degrees 30
        }


lightBulb : Light WorldCoordinates Bool
lightBulb =
    -- Define a light bulb similar to a 30 watt incandescent light bulb
    Light.point (Light.castsShadows True)
        { chromaticity = Light.incandescent
        , intensity = LuminousFlux.lumens 300
        , position = Point3d.centimeters 0 0 30
        }


overheadLighting : Light WorldCoordinates Never
overheadLighting =
    -- Add some soft overhead lighting
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , chromaticity = Light.fluorescent
        , intensity = Illuminance.lux 100
        }

type alias Model =
    { time : Float 
    , azimuth : Angle
    , elevation : Angle
    , orbiting : Bool
    , moving : Bool
    , blockDirection : Direction3d WorldCoordinates
    , player : Entity.Entity WorldCoordinates
    }

type Msg
    = Tick Duration -- Elapsed time since last animation frame
    | MouseDown
    | MouseUp
    | MouseMove Float Float
    | KeyDown String
    | KeyUp String

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init () =
    ( { time = 0
      , azimuth = Angle.degrees 45
      , elevation = Angle.degrees 40
      , orbiting = False
      , moving = False
      , blockDirection = getDirection 0 0 0
      , player = initialCube
      }
    , Cmd.none )

initialCube = cube (Color.rgb 0.8 0.8 0.8) 5

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        Tick t -> 
            let
                tickRate = 
                    Duration.milliseconds 1 |> Quantity.per Duration.second
                
                updatedTime = 
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                updatedCube = 
                    if model.moving then
                        model.player |> translateDir speed model.blockDirection
                    else
                        model.player

            in
                ( { model | time = Duration.inSeconds updatedTime, player = updatedCube }, Cmd.none )
        
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            -- Handle orbiting just like in the OrbitingCamera example
            if model.orbiting then
                let
                    rotation numPixels =
                        Angle.degrees (0.25 * numPixels)

                    newAzimuth =
                        model.azimuth |> Quantity.minus (rotation dx)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (rotation dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        KeyDown key ->
            let
                dir = toDirection key
            in
                case dir of
                    None ->
                        ( model, Cmd.none )
                    actualDirection ->
                        ( { model | moving = True, blockDirection = convertDirection actualDirection }, Cmd.none )

        KeyUp _ ->
            ( { model | moving = False }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    -- Subscribe to animation frames and wrap each time step (a number of
    -- seconds) into a typed Duration value
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
            , Browser.Events.onKeyDown keyDecoder
            , Browser.Events.onKeyUp keyUpDecoder
            ]
    else
        Sub.batch 
            [ Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
            , Browser.Events.onKeyDown keyDecoder
            , Browser.Events.onKeyUp keyUpDecoder
            ]

view : Model -> Html Msg
view model =
    let
        toneMapping = Scene3d.noToneMapping

    in
    Html.div []
        -- Start orbiting when the mouse is pressed on the scene
        [ Html.div [ Html.Events.onMouseDown MouseDown ]
            [ Scene3d.custom
                { lights = Scene3d.twoLights lightBulb overheadLighting
                , camera = camera model
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.pixels 1024, Pixels.pixels 768 )
                , antialiasing = Scene3d.multisampling
                , exposure = Scene3d.exposureValue 6
                , toneMapping = toneMapping
                , whiteBalance = Light.fluorescent
                , background = Scene3d.transparentBackground
                , entities =
                    [ floor
                    , model.player
                    ]
                }
            ]
            , Html.text ("Time elapsed: " ++ String.fromFloat model.time ++ " seconds.")
        , Html.div []
            [ Html.text "Use Arrow Keys to move."
            , Html.text " Space moves you up, Shift moves you down."
            ]
        ]

{-| Decode mouse movement just like in OrbitingCamera example
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)

keyDecoder : Decoder Msg
keyDecoder =
    Decode.map KeyDown (Decode.field "key" Decode.string)

keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.map KeyUp (Decode.field "key" Decode.string)

toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
      Left

    "ArrowRight" ->
      Right

    "ArrowUp" ->
      Forward

    "ArrowDown" ->
      Backward

    " " ->
      Up

    "Spacebar" ->
      Up

    "Shift" ->
      Down

    _ ->
      None

convertDirection : Direction -> Direction3d coords
convertDirection dir = case dir of
    Left -> getDirection 1 0 0

    Right -> getDirection (-1) 0 0

    Forward -> getDirection 0 (-1) 0

    Backward -> getDirection 0 1 0

    Up -> getDirection 0 0 1

    Down -> getDirection 0 0 (-1)

    _ -> getDirection 0 0 0