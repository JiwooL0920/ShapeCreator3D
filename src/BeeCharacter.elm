module BeeCharacter exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Dict exposing (Dict)
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import Sphere3d
import Task
import Temperature
import Vector3d
import Viewpoint3d
import Cone3d
import Point2d
import Arc2d
import Arc3d
import Circle3d
import TriangularMesh
import Scene3d.Entity as Entity
import Cylinder3d
import Triangle3d
import LineSegment3d



type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    }

type Axis = X | Y | Z 

type alias Dimensions = (Float,Float,Float)

type Msg
    = Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility

debugs : List (Entity WorldCoordinates)
debugs =
    [ Scene3d.lineSegment (Material.color Color.darkRed)
        (LineSegment3d.along Axis3d.x (Length.centimeters 0) (Length.centimeters 999))
    , Scene3d.lineSegment (Material.color Color.darkGreen)
        (LineSegment3d.along Axis3d.y (Length.centimeters 0) (Length.centimeters 999))
    , Scene3d.lineSegment (Material.color Color.darkBlue)
        (LineSegment3d.along Axis3d.z (Length.centimeters 0) (Length.centimeters 999))
    ]


--wrappers
myMat : Color.Color -> Material.Uniform WorldCoordinates
myMat colour = Material.nonmetal { baseColor = colour, roughness = 0.2 }

myTexturedMat : Color.Color -> Material.Textured WorldCoordinates
myTexturedMat colour = Material.nonmetal { baseColor = colour, roughness = 0.2 }

cube : Color.Color -> Float -> Scene3d.Entity WorldCoordinates
cube colour size = Scene3d.blockWithShadow (myMat colour) <|
        Block3d.from
            (Point3d.centimeters -size -size -size)
            (Point3d.centimeters size size size)

prism : Color.Color -> Dimensions-> Scene3d.Entity WorldCoordinates 
prism colour (x,y,z) = 
        Scene3d.blockWithShadow (myMat colour) <|
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
        Scene3d.sphereWithShadow (myTexturedMat colour) <|
            Sphere3d.withRadius (Length.centimeters r) Point3d.origin

cone : Color.Color -> Axis -> Dimensions -> Scene3d.Entity WorldCoordinates
cone colour axis (b,t,r) = 
        let 
            along = 
                case axis of 
                    X-> Axis3d.x 
                    Y -> Axis3d.y 
                    Z -> Axis3d.z     

        in 
            Scene3d.coneWithShadow (myMat colour) <|
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
            Scene3d.cylinderWithShadow (myMat colour) <|
                Cylinder3d.along along
                    { start = Length.centimeters s
                    , end = Length.centimeters e 
                    , radius = Length.centimeters r
                    }




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
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees -90
      , elevation = Angle.degrees 30
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

        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        VisibilityChange Browser.Events.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

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


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render full screen
          Browser.Events.onResize
            (\width height ->
                Resize
                    (Pixels.pixels (toFloat width))
                    (Pixels.pixels (toFloat height))
            )

        -- Subscribe to animation frames to animate the cubes
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)

        -- Listen for visibility change events so we can stop orbiting if the
        -- user switches to a different tab etc.
        , Browser.Events.onVisibilityChange VisibilityChange

        -- Listen for orbit-related mouse events
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]


{-| Create both a Light and an Entity (a bright glowing sphere) representing a
particular point light
-}
pointLight :
    { position : Point3d Meters WorldCoordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    }
    -> ( Light WorldCoordinates Bool, Entity WorldCoordinates )
pointLight properties =
    let
        -- Create a sphere to represent a light bulb
        lightsphere =
            Sphere3d.atPoint properties.position (Length.millimeters 100)

        -- Calculate the luminance of the sphere surface by dividing the given
        -- total luminous flux of the light by the surface area of the sphere
        -- and by the solid angle of a hemisphere (assuming that each point on
        -- the surface of the bulb emits light equally in all directions)...I
        -- am not 100% sure this is exactly correct =)
        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea lightsphere)

        -- Create an emissive (glowing) material for the sphere
        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance
    in
    ( Light.point (Light.castsShadows True) properties
    , Scene3d.sphere sphereMaterial lightsphere
    )

view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.centimeters 0 0 100
                , chromaticity = Light.incandescent
                , intensity = LuminousFlux.lumens 500
                }
    

        -- Rough approximation of unlight near sunset
        thirdLight =
            Light.directional (Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Light.colorTemperature (Temperature.kelvins 2000)
                , intensity = Illuminance.lux 30
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }

        -- Create a quad to act as a 'floor'
        plane =
            Scene3d.quad (Material.matte Color.darkGreen)
                (Point3d.centimeters -90 -90 0)
                (Point3d.centimeters 90 -90 0)
                (Point3d.centimeters 90 90 0)
                (Point3d.centimeters -90 90 0)

        -- Bee 
        bee = Scene3d.group [ 
                            black 
                                |> Scene3d.translateBy (Vector3d.centimeters 0 -8 0)
                            ,
                            yellow   
                                |> Scene3d.translateBy (Vector3d.centimeters 0 -8 0)
                            , black
                            , yellow
                            , black 
                               |> Scene3d.translateBy (Vector3d.centimeters 0 8 0)
                            , bum
                                |> Scene3d.translateBy (Vector3d.centimeters 0 -4 0)
                            , face
                            , sting
                                |> Scene3d.translateBy (Vector3d.centimeters 0 20 30)
                            , eye
                                |> Scene3d.translateBy (Vector3d.centimeters -4 -15 33)
                            , eye
                                |> Scene3d.translateBy (Vector3d.centimeters 4 -15 33)
                            , legGroup 
                                |> Scene3d.rotateAround Axis3d.y (Angle.degrees 60)  
                                |> Scene3d.translateBy (Vector3d.centimeters 5 0 21)
                            , legGroup 
                                |> Scene3d.rotateAround Axis3d.y (Angle.degrees -60)  
                                |> Scene3d.translateBy (Vector3d.centimeters -8 0 15)
                            , antenna
                            , antenna 
                                |> Scene3d.translateBy (Vector3d.centimeters 10 0 0)
                            , mouth
                                |> Scene3d.rotateAround Axis3d.x (Angle.degrees -90)  
                                |> Scene3d.rotateAround Axis3d.y (Angle.degrees 45) 
                                |> Scene3d.translateBy (Vector3d.centimeters 0 -16.3 30)
                            , 
                            leftWing (abs <| 50 * sin (5*model.time))
                            
                            , rightWing  (abs <| 50 * sin (5*model.time))
                            ]
                            |> Scene3d.translateBy (Vector3d.centimeters 0 0 20)

        black = prism Color.black (10,2,10)
            |> Scene3d.translateBy (Vector3d.centimeters 0 5 30)

        yellow = prism Color.yellow (10,2,10)
            |> Scene3d.translateBy (Vector3d.centimeters 0 9 30)
    
        bum = cube Color.black 8
            |> Scene3d.translateBy (Vector3d.centimeters 0 18 30)

        face = cube Color.yellow 9
            |> Scene3d.translateBy (Vector3d.centimeters 0 -7 30)

        sting = cone Color.darkGrey Y (2,10,2)

        eye = cube Color.black 2

        leg = cylinder Color.black X (0,7,0.7)

        antenna = Scene3d.group [
                (Scene3d.mesh (Material.nonmetal {baseColor=Color.black, roughness=0.5})
                <| ringMesh 7 0.7)
                |> Scene3d.rotateAround Axis3d.y (Angle.degrees -90)  
                |> Scene3d.translateBy (Vector3d.centimeters -5 -18 37)
                ,
                sphere Color.black 2
                |> Scene3d.translateBy (Vector3d.centimeters -5 -18 45)
                ]

        mouth = (Scene3d.mesh (Material.nonmetal {baseColor=Color.black, roughness=0.5})
                <| ringMesh 5 0.7)

        legGroup = Scene3d.group [
                    leg 
                    |> Scene3d.translateBy (Vector3d.centimeters 0 -2 0)
                    ,
                    leg
                    |> Scene3d.translateBy (Vector3d.centimeters 0 5 0)
                    ,
                    leg 
                    |> Scene3d.translateBy (Vector3d.centimeters 0 12 0)
                    ] 

        leftWing angle = prism (Color.rgba 220 220 220 0.1) (7,6,0.3)        --opacity doesn't work??
            |> Scene3d.translateBy (Vector3d.centimeters 7 0 0)
            |> Scene3d.rotateAround Axis3d.y (Angle.degrees -angle)
            |> Scene3d.translateBy (Vector3d.centimeters 5 3 40)
        
        rightWing angle = prism (Color.rgba 220 220 220 0.1) (7,6,0.3) 
            |> Scene3d.translateBy (Vector3d.centimeters -7 0 0)
            |> Scene3d.rotateAround Axis3d.y (Angle.degrees angle)
            |> Scene3d.translateBy (Vector3d.centimeters -5 3 40)            
                   
            
        
        -- Define camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 20
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }



    in
    Scene3d.custom
        { lights = Scene3d.twoLights firstLight softLighting
        , camera = camera
        , clipDepth = Length.centimeters 10
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( model.width, model.height )
        , background = Scene3d.backgroundColor (Color.lightBlue)
        , entities =  [   plane
                        , firstLightBall
                        , bee
                            -- |> Scene3d.translateBy (Vector3d.centimeters 50 0 0)
                            -- |> Scene3d.rotateAround Axis3d.z (Angle.degrees (-90*model.time))
                      ]
                      ++ debugs
        }



