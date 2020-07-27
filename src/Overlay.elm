
{- This file should provide a good starting point for the snowman. Modified from "3d-elm-camp/BeeMovement.elm".
   This code was originally for the 3D Workshop Snowman on macoutreach.rocks
 -}

{-
"q" = rotateX (+)
"w" = rotateX (-)
"a" = rotateY (+)
"s" = rotateY (-)
"z" = rotateZ (+)
"x" = rotateZ (-)
-}
 

module Overlay exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Html exposing (Html)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SolidAngle
import Sphere3d
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d
import LineSegment3d
import WebGL.Texture
import Skybox
import Wrapper3D exposing 
    ( metallic, plastic, matte, customMat
    , cube, square3D, rectangle3D, box
    , sphere, cone, cylinder
    , polyCone, polyCylinder
    , ring, ellipsoid, polygon3D
    , move3D, scale3D, rotate3D
    , rotateX3D, rotateY3D, rotateZ3D
    , repeatDuration
    , generateEllipsoid, generatePolyCone, generatePolyCylinder
    , Dimension, Mold, GeneratedMesh, MeshStore, withOverlay )

import GraphicSVG.Widget as Widget
import GraphicSVG as G
import Html.Attributes as HA

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render full screen
          Browser.Events.onResize
            (\width height ->
                Resize
                    (Pixels.pixels width)
                    (Pixels.pixels height)
            )

        -- Subscribe to animation frames to animate the cubes
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)

        -- Listen for visibility change events so we can stop orbiting if the
        -- user switches to a different tab etc.
        , Browser.Events.onVisibilityChange VisibilityChange

                 -- Listen for key presses
        , Browser.Events.onKeyDown keyDecoder


        -- Listen for orbit-related mouse events
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    let
        (wModel, _) = Widget.init 0 0 "widget"
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , textures = Nothing
      , generatedMeshes = Dict.empty
      , generatedShadows = Dict.empty
      , xRot = 0 
      , yRot = 0 
      , zRot = 0
      , widget = wModel
      }
    , Cmd.batch
        [ Task.perform
            -- The scene gets resized to match the browser window
            (\{ viewport } ->
                Resize
                    (Pixels.int (round viewport.width))
                    (Pixels.int (round viewport.height))
            )
            Browser.Dom.getViewport
        , fetchTextures
        , Task.perform (\_ -> GenerateMeshes myMeshes) (Task.succeed True)
        -- , Task.perform (\_ -> GenerateShadows name myShadowMeshes) (Task.succeed True)
        ]
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            let
                (wModel, wCmd) = Widget.init (toFloat <| unwrapQ width) (toFloat <| unwrapQ height) "widget"
            in
            ( { model | width = width, height = height, widget = wModel }, Cmd.map WidgetMsg wCmd )

        Tick t ->
           let
                tickRate =
                    Duration.milliseconds 1 |> Quantity.per Duration.second

                updatedTime =
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum = Duration.inSeconds updatedTime

            in
                ( { model | time = timeAsNum }, Cmd.none )

        KeyDown key -> 
            case key of
                --rotateX (+)
                "q" -> if (model.xRot < 90) then ({model | xRot = model.xRot + 5}, Cmd.none) else (model,Cmd.none)
                --rotateX (-)
                "w" -> if (model.xRot > -90) then ({model | xRot = model.xRot - 5}, Cmd.none) else (model,Cmd.none)
                --rotateY (+)
                "a" -> if (model.yRot < 90) then ({model | yRot = model.yRot + 5}, Cmd.none) else (model,Cmd.none)
                --rotateY (-)
                "s" -> if (model.yRot > -90) then ({model | yRot = model.yRot - 5}, Cmd.none) else (model,Cmd.none)
                --rotateZ (+)
                "z" -> ({model | zRot = model.zRot + 5}, Cmd.none) 
                --rotateZ (-)
                "x" -> ({model | zRot = model.zRot - 5}, Cmd.none) 

                _ -> (model, Cmd.none)

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
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
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

        GenerateMeshes generatedMeshes ->
            case generatedMeshes of
                [] ->
                    (model, Cmd.none)
                (generatedMesh :: rest) ->
                    let
                        updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.generatedMeshes
                        updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.generatedShadows
                    in
                        ( { model | generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ])

        -- GenerateShadows name meshes ->
        --     case meshes of
        --         [] ->
        --             (model, Cmd.none)
        --         (mesh :: rest) ->
        --             let
        --                 updatedDict = Dict.insert name mesh model.generatedShadows
        --             in
        --                 ( { model | generatedShadows = updatedDict }, Cmd.batch [ Task.perform (\_ -> GenerateShadows name rest) (Task.succeed True) ])

        LoadTexture textures ->
            ( { model | textures = Just textures }, Cmd.none)

        Error _ ->
            ( model, Cmd.none)

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset -> ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30 } , Cmd.none )

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Fetch textures from textureListSkybox
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox2.skybox
fetchTextures : Cmd Msg
fetchTextures =
  textureListSkyBox
    |> List.map Material.load
    -- Load the meterial, [Material.load texture, Material.load texture... ]
    |> Task.sequence -- sequence : List (Task x a) -> Task x (List a)
    -- Transform a list of the tast to a tast
    -- Get the result type as Task WebGL.Texture.Error (List (Texture value))
    |> Task.andThen -- andThen :
    -- concatenate two tasks
         (\textures ->
            case textures of
              [] ->
                Task.fail WebGL.Texture.LoadError
              textList ->
                Task.succeed textList)
              -- If the list is not empty let the tast succeed
    |> Task.attempt -- Attempt to update the task here
       (\result ->
            case result of
                Ok texture -> LoadTexture texture
                Err error -> Error error
        )


type alias GeneratedMesh =
    { name : String
    , mesh : Mesh.Textured WorldCoordinates
    , shadow : Mesh.Shadow WorldCoordinates
    }

{- UNEDITABLE -}
type WorldCoordinates
    = WorldCoordinates

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , textures : Maybe (List (Material.Texture Color))
    , generatedMeshes : Dict String (Mesh.Textured WorldCoordinates)
    , generatedShadows : Dict String (Mesh.Shadow WorldCoordinates)
    , xRot : Float 
    , yRot : Float 
    , zRot : Float
    , widget : Widget.Model
    }

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List GeneratedMesh)
    -- | GenerateShadows String (List (Mesh.Shadow WorldCoordinates))
    | LoadTexture (List (Material.Texture Color))
    | Error WebGL.Texture.Error
    | KeyDown String
    | WidgetMsg Widget.Msg
    | Reset


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


textureListSkyBox : List String
textureListSkyBox =
  [textureBottom, textureTop, textureSide1, textureSide2, textureSide3
    , textureSide4]

view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.centimeters 0 0 300
                , chromaticity = Light.sunlight
                , intensity = LuminousFlux.lumens 10000
                }


        -- Rough approximation of sunlight
        thirdLight =
            Light.directional (Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Light.sunlight
                , intensity = Illuminance.lux 100
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
            (Scene3d.quad (Material.matte floorColour)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)
            ) |> move3D (0,0,-30)

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

        -- If the proper textures aren't loaded for whatever reason, the sky will just be light blue
        texturesList = case model.textures of
            Just textures ->
                textures

            Nothing ->
                List.repeat 6 (Material.constant Color.lightBlue)

        baseEntities =
            [ firstLightBall
        --  , plane
            , Skybox.skybox (List.map Just texturesList) 1000
            ]

    in
    Scene3d.custom
                            { lights = Scene3d.threeLights firstLight thirdLight softLighting
                            , camera = camera
                            , clipDepth = Length.centimeters 10
                            , exposure = Scene3d.exposureValue 6
                            , toneMapping = Scene3d.hableFilmicToneMapping
                            , whiteBalance = Light.fluorescent
                            , antialiasing = Scene3d.multisampling
                            , dimensions = ( model.width, model.height )
                            , background = Scene3d.backgroundColor Color.lightBlue
                            , entities = baseEntities ++ myEntities model
                            }
        |> withOverlay
                [
                    G.text ("azimuth: " ++ String.fromFloat (unwrapQ model.azimuth))
                        |> G.filled G.black
                        |> G.move (toFloat (unwrapQ model.width) / 2 - 200, toFloat (unwrapQ model.height) / 2 - 50)
                ,   G.text ("elevation: " ++ String.fromFloat (unwrapQ model.elevation))
                        |> G.filled G.black
                        |> G.move (toFloat (unwrapQ model.width) / 2 - 200, toFloat (unwrapQ model.height) / 2 - 60)
                ,   G.group [
                                G.roundedRect 60 40 10
                                        |> G.filled G.green
                            ,   G.text "Reset"
                                    |> G.size 16
                                    |> G.centered
                                    |> G.filled G.black
                                    |> G.move(0,-5)
                            ]
                            |> G.move (toFloat (unwrapQ model.width) / 2 - 125, toFloat (unwrapQ model.height) / 2 - 90)
                            |> G.notifyTap Reset
                ]
                model

unwrapQ : Quantity number b -> number
unwrapQ (Quantity.Quantity i) = i

{- EDITABLE -}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with some snowflakes
myEntities model =
    [   
        airplane    
            |> move3D (20,25,0)
            |> rotateZ3D (degrees 180)
            --keyboard control 
            |> rotateX3D (degrees model.xRot)
            |> rotateY3D (degrees model.yRot)
            |> rotateZ3D (degrees model.zRot)

            --any other necessary transformations
            -- |> rotateZ (degrees -90)
            -- |> move (25,0,0)
            
    ] ++ debugs


airplane = Scene3d.group [
    rectangle3D 25 80
      |> metallic Color.red 0.3
      |> move3D (-25, -25, 30)
      ,
      rectangle3D 25 80
            |> metallic Color.red 0.3
            |> move3D (-25, -25, 50)

                           ,
                          box 1 1 20
                             |> plastic Color.blue 0.4
                              |> move3D (-30, -60, 30)
                          ,
                          box 1 1 20
                            |> plastic Color.blue 0.4
                            |> move3D (-20, -60, 30)
                            ,
                             box 1 1 21.5
                            |> plastic Color.blue 0.4
                            |> rotate3D (degrees 25) (degrees 270) 0
                            |> move3D (-20, -60, 30)
                          ,
                          box 1 1 20
                            |> plastic Color.blue 0.4
                            |> move3D (-30, 10, 30)
                          ,
                          box 1 1 20
                            |> plastic Color.blue 0.4
                            |> move3D (-20, 10, 30)
                            ,
                             box 1 1 21.5
                            |> plastic Color.blue 0.4
                            |> rotate3D (degrees 25) (degrees 270) 0
                            |> move3D (-20, 10, 30)


            , cylinder 10 5
                        |> metallic Color.red 0.7
                        |> rotate3D (degrees 90) (degrees 270) 0
                        |> move3D (-35, -25, 36)
                         ,
                                ring 10 1
                              |> metallic Color.lightGray 0.5
                              |> rotate3D (degrees 90) (degrees 90) 0
                              |> move3D (-34, -25, 36)
                             ,
                             ring 10 1
                           |> metallic Color.lightGray 0.5
                           |> rotate3D (degrees 90) (degrees 90) 0
                           |> move3D (-40, -25, 36)
        ,
          cylinder 5 0.75
             |> metallic Color.lightGray 0.7
             |> rotate3D (degrees 90) 0 0
             |> move3D (-25, -8, 20)
             ,
                       cylinder 5 0.75
                          |> metallic Color.lightGray 0.7
                          |> rotate3D (degrees 90) 0 0
                          |> move3D (-25, -42, 20)

                          ,
                                      ring 5 1.5
                                    |> metallic Color.black 0.5
                                    |> rotate3D (degrees 90) 0 0
                                    |> move3D (-25, -8, 20)
                                   ,
                                   ring 5 1.5
                                 |> metallic Color.black 0.5
                                 |> rotate3D (degrees 90) 0 0
                                 |> move3D (-25, -42, 20)
                                 ,
                                         box 1 1 40
                                          |> plastic Color.blue 0.4
                                          |> rotate3D (degrees 90) (degrees 180) 0
                                          |> move3D (-24, -45, 20)
                                            ,
                                                    box 1 1 10
                                                    |> plastic Color.blue 0.4
                                                     |> move3D (-24, -38, 20)
                                                     ,
                                                       box 1 1 10
                                                        |> plastic Color.blue 0.4
                                                         |> move3D (-24, -15, 20)



        ,
          cylinder 7 60
             |> metallic Color.red 0.7
             |> rotate3D (degrees 90) (degrees 270) 0
             |> move3D (25, -25, 35)
             ,
                 rectangle3D 25 40
                   |> metallic Color.red 0.3
                   |> move3D (25, -25, 35)

                     ,
                         rectangle3D 20 10
                           |> metallic Color.red 0.3
                         |> rotate3D (degrees 90) (degrees 180) 0
                           |> move3D (25, -25, 38)


    ]


debugs : List (Entity WorldCoordinates)
debugs =
    [ Scene3d.lineSegment (Material.color Color.darkRed)
        (LineSegment3d.along Axis3d.x (Length.centimeters -999) (Length.centimeters 999))
            |> Scene3d.translateBy (Vector3d.centimeters 0 0 35)
    , Scene3d.lineSegment (Material.color Color.darkGreen)
        (LineSegment3d.along Axis3d.y (Length.centimeters -999) (Length.centimeters 999))
            |> Scene3d.translateBy (Vector3d.centimeters 0 0 35)
    , Scene3d.lineSegment (Material.color Color.darkBlue)
        (LineSegment3d.along Axis3d.z (Length.centimeters -999) (Length.centimeters 999))
            |> Scene3d.translateBy (Vector3d.centimeters 0 0 -30)
    ]



keyDecoder : Decoder Msg
keyDecoder =
    Decode.map KeyDown (Decode.field "key" Decode.string)


























-- Put any custom meshes you need generated in here
myMeshes =
    [ generateEllipsoid 25 10 15

    ]

floorColour = Color.green

{- Here you can specify what images to use to create the skybox -}
textureBottom : String
textureBottom =
    "todo"

textureTop : String
textureTop =
    "todo"

textureSide1 : String
textureSide1 =
    "todo"

textureSide2 : String
textureSide2 =
    "todo"

textureSide3 : String
textureSide3 =
    "todo"

textureSide4 : String
textureSide4 =
    "todo"
