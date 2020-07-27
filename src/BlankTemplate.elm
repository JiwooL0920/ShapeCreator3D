-- A lot of this file was just Frankenstein'd together from the existing elm-3d-scene examples,
-- but it should be useful as a starting point.

module BlankTemplate exposing (main)

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
import Cone3d 

type WorldCoordinates = WorldCoordinates

type Axis = X | Y | Z  -- custom axis type

type alias Dimensions = (Float,Float,Float)


aluminum : Material.Uniform WorldCoordinates
aluminum =
    Material.metal
        { baseColor = Color.rgb255 233 235 236
        , roughness = 0.6
        }

myMat : Color.Color -> Material.Uniform WorldCoordinates
myMat colour = Material.nonmetal { baseColor = colour, roughness = 0.2 }

myTexturedMat : Color.Color -> Material.Textured WorldCoordinates
myTexturedMat colour = Material.nonmetal { baseColor = colour, roughness = 0.2 }

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

prism : Color.Color -> Dimensions-> Scene3d.Entity WorldCoordinates 
prism colour (x,y,z) = 
        Scene3d.block (myMat colour) <|
                Block3d.with
                    { x1 = Length.centimeters -x
                    , x2 = Length.centimeters x
                    , y1 = Length.centimeters -y
                    , y2 = Length.centimeters y
                    , z1 = Length.centimeters -z
                    , z2 = Length.centimeters z
                    } 

sphere : Color.Color -> Float -> Scene3d.Entity WorldCoordinates
sphere colour r = 
        Scene3d.sphere (myTexturedMat colour) <|
            Sphere3d.withRadius (Length.centimeters r) Point3d.origin

cone : Color.Color -> Axis -> Dimensions -> Scene3d.Entity WorldCoordinates
cone colour axis (b,t,r) = 
        let 
            along = 
                case axis of 
                    X -> Axis3d.x 
                    Y -> Axis3d.y 
                    Z -> Axis3d.z     

        in 
            Scene3d.cone (myMat colour) <|
                Cone3d.along along
                    { base = Length.centimeters b
                    , tip = Length.centimeters t
                    , radius = Length.centimeters r
                    }

cylinder : Color.Color -> Axis -> Dimensions -> Scene3d.Entity WorldCoordinates
cylinder colour axis (s,e,r) =
        let 
            along = 
                case axis of 
                    X -> Axis3d.x 
                    Y -> Axis3d.y 
                    Z -> Axis3d.z    

        in 
            Scene3d.cylinder (myMat colour) <|
                Cylinder3d.along along
                    { start = Length.centimeters s
                    , end = Length.centimeters e 
                    , radius = Length.centimeters r
                    }

--custom meshes
ringMesh : Float -> Float -> Mesh.Uniform WorldCoordinates
ringMesh radius thickness = 
    let
        pointList = List.map
                    ( \idx ->
                      let 
                        t = toFloat idx
                        slowx = sin (0.004*t)
                        slowy = cos (0.004*t)
                        fastx = radius + thickness * sin (0.4 *t)
                        fastz =          thickness * cos (0.4 *t)
                      in
                        Point3d.centimeters 
                            (slowx * fastx)
                            (slowy * fastx)
                            (fastz)
                    )
                    <| List.range 0 400
        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList pointList )
                ((List.concatMap ( \ idx ->  [(idx,idx+1,16+idx)
                                            ,(idx+1,16+idx,16+idx+1)]
                                )
                                <| List.range 0 (1572-16))
                ++
                (List.concatMap ( \ idx ->  [(idx+1572-16,idx+1572-16+1,16+idx)
                                            ,(idx+1572-16+1,16+idx,16+idx+1)]
                                )
                                <| List.range -16 0
                ))
    in
        Mesh.indexedFacets triangularMesh

pyramidMesh : Mesh.Uniform WorldCoordinates
pyramidMesh =
    let
        -- Define the vertices of our pyramid
        frontLeft =
            Point3d.centimeters 30 40 0

        frontRight =
            Point3d.centimeters 30 -40 0

        backLeft =
            Point3d.centimeters -30 40 0

        backRight =
            Point3d.centimeters -30 -40 0

        tip =
            Point3d.centimeters 0 0 30
            
        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ frontLeft -- 0
                    , frontRight -- 1
                    , backLeft -- 2
                    , backRight -- 3
                    , tip -- 4
                    ]
                )
                [ ( 1, 0, 4 ) -- front
                , ( 0, 2, 4 ) -- left
                , ( 2, 3, 4 ) -- back
                , ( 3, 1, 4 ) -- right
                , ( 1, 3, 0 ) -- bottom
                , ( 0, 3, 2 ) -- bottom
                
                ]
    in

    Mesh.indexedFacets triangularMesh

leafMesh : Float -> Float -> Mesh.Uniform WorldCoordinates
leafMesh y rotOffset =
    let
        tip =
            Point3d.centimeters 20 y 70
        apron = List.map 
            ( \ t -> 
                let 
                    radius = 5 * (4+abs(sin(0.1*pi*t + rotOffset)))
                    angle = 0.02*pi*t
                in
                Point3d.centimeters (20 + radius * cos angle)
                                    (20 + radius * sin angle)
                                    0
            )
            (List.map toFloat <| List.range 0 100)

    in
        TriangularMesh.fan tip apron
          |> Mesh.indexedFacets

getDirection : Float -> Float -> Float -> Direction3d coords
getDirection x y z = let direction = Direction3d.from Point3d.origin (Point3d.meters x y z)
    in case direction of
       Nothing -> Direction3d.y
       Just dir -> dir

translate : Float -> (Float, Float, Float) -> Entity.Entity coords -> Entity.Entity coords
translate amount direction entity = case (amount, direction, entity) of
    (amt, (x,y,z), ent) -> ent |> Entity.translateBy (Vector3d.withLength (Length.centimeters amt) (getDirection x y z))

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
    }

type Msg
    = Tick Duration -- Elapsed time since last animation frame
    | MouseDown
    | MouseUp
    | MouseMove Float Float

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
      }
    , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
        Tick t -> 
            let
                tickRate = 
                    Duration.milliseconds 1 |> Quantity.per Duration.second
                
                updatedTime = 
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)
            in
                ( { model | time = Duration.inSeconds updatedTime }, Cmd.none )
        
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

subscriptions : Model -> Sub Msg
subscriptions model =
    -- Subscribe to animation frames and wrap each time step (a number of
    -- seconds) into a typed Duration value
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
            ]
    else
        Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)

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
                    , cube (Color.rgb (0.5+0.5*cos model.time) (0.5+0.5*cos model.time) (0.5+0.5*cos model.time)) 5
                        |> rotate (makeAxis 1 1 1) (degrees 30*model.time)
                        |> translate 50 (sin model.time, cos model.time,0)
                    ]
                }
            ]
            , Html.text ("Time elapsed: " ++ String.fromFloat model.time ++ " seconds.")
        ]

{-| Decode mouse movement just like in OrbitingCamera example
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


