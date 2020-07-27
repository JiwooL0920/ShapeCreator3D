{- Testing file for the 3D slot
 -}

module ShapeCreator exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
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
import Scene3d.Material as Material exposing (Material)
import SolidAngle
import Sphere3d
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture
import Skybox
import Wrapper3D exposing (..)
import Dict exposing (Dict)
import LineSegment3d

import GraphicSVG.Widget as Widget
import GraphicSVG exposing(..)
import GSVGSkybox as GS

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)
import String exposing (..)
import Airplane exposing (airplane)

-- import Main exposing (Model)


{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =
    [ 
        if (List.member model.shape [Cube, Boxx, Cone, Cylinder, Ring]) then (shapeFun model) else (shapeFunTextured model)-- the selection boxes
    ] 

    -- This group is just used for debugging the meshes
    -- , Scene3d.group (List.map (Scene3d.mesh (Material.metal {baseColor = Color.red, roughness = 0.3 })) (Dict.values model.generatedMeshes))
    --   |> move (25,25,50)
    


-- Put any custom meshes you need generated in here. Make sure the values are identical.
myMeshes =
    [ -- This mesh below will be used for the ellipsoid above. Remember to specify its length, width, and height
   
    ]

polygonDict = Dict.fromList [
    (0 , [(-10,-10), (10,10), (10,-10),(-10,-10)])
    , (1 , [(0.5944,35.071),(-8.123,10.897),(-35.86,10.105),(-14.46,-6.142),(-22.39,-33.08),(1.3869,-16.44),(22.390,-31.90),(16.049,-7.727),(36.656,10.105),(10.105,10.105),(0.5944,35.071)])
    , (2, [(-2.575,-42.60),(2.5758,-42.99),(2.5758,-24.37),(23.578,-26.35),(21.597,-19.21),(42.600,-1.783),(37.448,1.3869),(41.015,15.653),(28.334,14.068),(25.560,19.616),(13.275,8.9164),(16.445,32.297),(8.5201,29.126),(0.1981,44.185),(-8.916,30.315),(-16.04,32.693),(-12.48,8.5201),(-24.37,18.823),(-27.54,14.464),(-41.41,15.653),(-37.44,1.7832),(-42.99,-1.386),(-21.20,-19.21),(-22.78,-25.95),(-1.783,-24.37),(-2.575,-42.60)])
  ]

overlay : Model -> List (Shape (Msg Model))
overlay model =
    [
    angleDisplay model 
      |> move (-(relativeP 17 model.width), 0)
    , shapeCreator model |> move (0, (relativeP 24 model.height))
    , navigationButtons model |> scale 0.9 
        |> move ((relativeP 2.3 model.width),(relativeP 2.4 model.height))
    , myList model
    ]
    
myList model = group [
         rect (relativeP 2.75 model.width) (relativeP 9 model.height) 
          |> filled (rgba 255 255 255 0.5) 
          |> move ((relativeP 5.8 model.width),(relativeP 30 model.height))
          |> addOutline (solid 1) lightGrey
        , rect (relativeP 6.4 model.height) (relativeP 27 model.height) |> filled white |> addOutline (solid 1) lightGrey 
          |> move ( (relativeP 4 model.height), (relativeP 12 model.height) )
        , text "Polygon" |> serif |> italic 
          |> size (relativeP 35 model.height) |> filled titleColour 
          |> move ( (relativeP 5 model.height), (relativeP 13.3 model.height) )
         ,"polyPoints = " |> copiable model
           |> scale 1.5
           |> move (0,(relativeP 30 model.height))
         , listCode (getPolygonDic model) |> copiable model
           |> scale 1.5
         ,roundedRect (relativeP 4.8 model.height) (relativeP 27 model.height) (relativeP 120 model.height)
          |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time))) 
          |> addOutline (solid 1) lightGrey 
          |> move ( (relativeP 2.2 model.height), (relativeP 12 model.height) )
         , text "Next Polygon" |> fixedwidth
          |> size (relativeP 35 model.height) |> filled black 
          |> move ( 220, (relativeP 13.3 model.height) )
          |> notifyTap NextPolygon
        ] |> move (-(relativeP 2.4 model.height),-(relativeP 2.3 model.height))
        
shapeCreator model = group [
        --shape creator
        stencils model
          |> scale 0.65
          |> move ( -(relativeP 6.5 model.width), (relativeP 7.1 model.height) )
        , stamps model
          |> scale 0.8
          |> move ( 0, (relativeP 5.8 model.height) )
        , colours model
          |> scale 0.71
          |> move ( (relativeP 3.15 model.height), -(relativeP 46.15 model.height) )
        , transforms model
          |> scale 0.7
          |> move (-(relativeP 3.2 model.height) , (relativeP 24 model.height) )
        , tweaks model
          |> scale 0.7
          |> move (-(relativeP 3.2 model.height) , -(relativeP 24 model.height) )
        , yourCode model
          |> scale 0.8
          |> move ( 0, -(relativeP 10 model.height) )

        ] |> scale 2 
        
      


navigationButtons model = group [
            button1 model
            , button2 model 
          ]

button1 model = group [ circle 40
                |>filled (rgba 255 255 255 0.5) 
                |> addOutline (solid 5) (rgba 255 137 5 0.8)
                ,
                polygon [(0,40),(0,0),(40,0)]
                |> filled (rgba 255 137 5 0.8)
                |> move (-(relativeP 40 model.height),-(relativeP 40 model.height))
            ]     

button2 model = group [
                circle 40
                |> filled (rgba 255 255 255 0.5)
                |> addOutline (solid 5) (rgba 197 125 149 0.8)
                ,

                text "R"
                |> centered
                |> fixedwidth
                |> size (relativeP 10 model.height)
                |> bold
                |> filled (rgba 197 125 149 0.8)
                |> move (0,-(relativeP 40 model.height))

            ] |> move (0,-(relativeP 6 model.height))

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
skyboxType = Skybox.GSVGSkybox False skyboxTop skyboxSides skyBoxBottom  -- Skybox.URLSkybox textureBottom textureTop textureSide1 textureSide2 textureSide3 textureSide4

-- this is 50 by 50
skyboxTop : Shape msg 
skyboxTop =
    group
        [
            square 50 |> filled lightBlue
        ,   circle 10 |> filled yellow
        ]

-- this is 200 by 50
skyboxSides : Shape msg
skyboxSides =
    group
        [
            rect 200 50 |> filled lightBlue |> move (0,25)
        ,   rect 200 50 |> filled green |> move(0,-25)
        ,   triangle 10 |> filled darkGreen |> rotate (degrees -30) |> move (0,5)
        ,   text "abcdefghijklmnopqrstuvwxyz" |> centered |> size 16 |> filled red
        ]

-- this is 50 by 50
skyBoxBottom : Shape msg
skyBoxBottom =
    group
        [ square 50 |> filled green
        ]


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


{-endeditable-}

{-extra-}

type WorldCoordinates
    = WorldCoordinates

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , textures : Dict String (Material.Texture Color.Color)
    , meshStore : MeshStore WorldCoordinates
    , widget : Widget.Model
    , gSkyboxModel : GS.Model
    --shapecreator
    , notify : Notifications
    , shape : Stencil 
    , draw : Draw 
    , style : LineStyle 
    , lineWidth : Float 
    , l : Float
    , w : Float 
    , h : Float 
    , sides : Int 
    , xAngle : Float 
    , yAngle : Float 
    , zAngle : Float 
    , mouth : Float 
    , txt : String 
    , clr : Colour 
    , red : Int 
    , green : Int 
    , blue : Int 
    , hasMove : Bool 
    , hasRotateX : Bool 
    , hasRotateY : Bool 
    , hasRotateZ : Bool 
    , hasScale : Bool 
    , scl : Float 
    , sclx : Float 
    , scly : Float 
    , x : Float 
    , y : Float 
    , z : Float 
    , r : Float 
    , roughness : Int 
    , thickness : Float 
    , currentButton : ButtonDir 
    , buttonDownTime : Float 
    , nextPolygon : Int
    }

type Msg m
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | GenerateMeshes (List (GeneratedMesh WorldCoordinates))
    -- | GenerateShadows String (List (Mesh.Shadow WorldCoordinates))
    | LoadTexture (Dict String (Material.Texture Color.Color))
    | Error WebGL.Texture.Error
    | WidgetMsg Widget.Msg
    | Reset
    | SkyboxMsg GS.Msg
    --shape creator stuff
    | Sten Stencil
    | Draw Draw
    | LStyle
    | CycleText
    | SetColour Colour
    | Toggle Transforms
    | TransM (Model -> Model)
    | Notif Notifications
    | ButtonDown ButtonDir
    | NextPolygon

type ButtonDir
    = RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None


-- the type of stencil selected, these correspond to functions exported by GraphicSVG


type Stencil
    = Cube
    | Square3D
    | Rectangle3D
    | Boxx
    | Sphere
    | Cone
    | Cylinder
    | Ring
    | PolyCone
    | PolyCylinder    
    | Ellipsoid
    | Polygon3D
    | CustomPolygon



-- type of drawing


type Draw
    = Metal
    | Plastic  
    | Matte 


type Colour
    = Black
    | Blue
    | Brown
    | Charcoal
    | DarkBlue
    | DarkBrown
    | DarkCharcoal
    | DarkGray
    | DarkGreen
    | DarkGrey
    | DarkOrange
    | DarkPurple
    | DarkRed
    | DarkYellow
    | Gray
    | Green
    | Grey
    | LightBlue
    | LightBrown
    | LightCharcoal
    | LightGray
    | LightGreen
    | LightGrey
    | LightOrange
    | LightPurple
    | LightRed
    | LightYellow
    | Orange
    | Purple
    | Red
    | White
    | Yellow
    | RGB


type Transforms
    = Move
    | RotateX
    | RotateY 
    | RotateZ 
    | Scale


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type LineStyle
    = Solid
    | Dotted
    | Dashed
    | Longdash
    | Dotdash



-- update helper


cycleTxt s =
    case s of
        "Hello" ->
            "Bonjour"

        "Bonjour" ->
            "Namaste"

        "Namaste" ->
            "Gutten Tag"

        "Gutten Tag" ->
            "Jó napot"

        "Jó napot" ->
            "Dobro utro"

        "Dobro utro" ->
            "Sat Shri Akaal"

        -- "Sat Shri Akaal" ->
        _ ->
            "Hello"




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


view : Model -> Html (Msg Model)
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.centimeters 0 0 100
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
            Scene3d.quad (Material.matte floorColour)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)

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


        baseEntities =
            [  firstLightBall 
            --,plane
            , Skybox.skybox [Dict.get "skyB" model.textures,
                             Dict.get "skyT" model.textures,
                             Dict.get "skyS1" model.textures,
                             Dict.get "skyS2" model.textures,
                             Dict.get "skyS3" model.textures,
                             Dict.get "skyS4" model.textures]
                             1000
            ]

    in
        Html.div []
            [   case skyboxType of
                    Skybox.GSVGSkybox debug sT sS sB ->
                        Html.div [style "position" "absolute", style "left" "0px", style "top" (String.fromInt (unwrapQ model.height) ++ "px")]
                        [
                            Html.h1 [] [Html.text "Skybox Debug"]
                        ,   Html.map SkyboxMsg <| GS.drawSkybox debug model.gSkyboxModel sT sS sB
                        ]
                    _ -> Html.span [] []
            ,   Scene3d.custom
                    { lights = Scene3d.threeLights firstLight thirdLight softLighting
                    , camera = camera
                    , clipDepth = Length.centimeters 10
                    , exposure = Scene3d.exposureValue 6
                    , toneMapping = Scene3d.hableFilmicToneMapping
                    , whiteBalance = Light.fluorescent
                    , antialiasing = Scene3d.multisampling
                    , dimensions = ( model.width, model.height )
                    , background = Scene3d.backgroundColor Color.lightBlue
                    , entities = baseEntities ++ debugs ++ renderEntities (myEntities model)
                    }
                    |> withOverlay (overlay model) model
            ]

angleDisplay : Model -> Shape (Msg m)
angleDisplay model = group
    [
        text ("azimuth: " ++ String.fromInt (round <| unwrapQ model.azimuth * 180 / pi) ++ "º")
                                |> size (relativeP 50 model.height)
                                |> filled black
                                |> move ((relativeP 2.6 model.height), (relativeP 2.4 model.height))
    ,   text ("elevation: " ++ String.fromInt (round <| unwrapQ model.elevation * 180 / pi) ++ "º")
            |> size (relativeP 50 model.height)
            |> filled black
            |> move ((relativeP 2.6 model.height), (relativeP 2.5 model.height))
    ,   group [
                    roundedRect (relativeP 10 model.height) (relativeP 15 model.height) (relativeP 60 model.height)
                            |> filled green
                ,   text "Reset"
                        |> size (relativeP 37.5 model.height)
                        |> centered
                        |> filled black
                        |> move (0, 0)
                ]
                |> move ((relativeP 2.3 model.height), (relativeP 2.8 model.height))
                |> notifyTap Reset
    ]
{-endextra-}


subscriptions : Model -> Sub (Msg m)
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render full screen
          Browser.Events.onResize
            (\widthh heightt ->
                Resize
                    (Pixels.pixels widthh)
                    (Pixels.pixels heightt)
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
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ ->
                Sub.map SkyboxMsg (GS.subscriptions model.gSkyboxModel)
            _ -> Sub.none
        ]

main : Program () Model (Msg Model)
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd (Msg m) )
init _ =
    let
        (wModel, _) = Widget.init 0 0 "widget"
        (gSkyboxModel, gSCmd) = GS.initialModelWithLoad
    in
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , textures = Dict.empty
      , meshStore = { generatedMeshes = Dict.empty, generatedShadows = Dict.empty }
      , widget = wModel
      , gSkyboxModel = gSkyboxModel
      --shape creator
        , notify = NotifyTap
        , shape = Cube
        , draw = Matte
        , style = Solid
        , lineWidth = 1
        , l = 20
        , w = 10
        , h = 15
        , sides = 5
        , xAngle = 30
        , yAngle = 30 
        , zAngle = 30
        , mouth = 0.75
        , txt = "Hello"
        , clr = RGB
        , red = 255
        , green = 0
        , blue = 0
        , hasMove = False
        , hasRotateX = False
        , hasRotateY = False 
        , hasRotateZ = False
        , hasScale = False
        , scl = 2
        , sclx = 2
        , scly = 2
        , x = 0
        , y = 0
        , z = 0
        , r = 10
        , thickness = 3
        , currentButton = None
        , buttonDownTime = 0
        , roughness = 5
        , nextPolygon = 0
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
        , case skyboxType of
            Skybox.URLSkybox top bottom side1 side2 side3 side4 ->
                fetchTextures  [
                                ("skyB", textureBottom),
                                ("skyT", textureTop),
                                ("skyS1", textureSide1),
                                ("skyS2", textureSide2),
                                ("skyS3", textureSide3),
                                ("skyS4", textureSide4)
                               ]
            _ -> Cmd.none
        , Task.perform (\_ -> GenerateMeshes myMeshes) (Task.succeed True)
        , case skyboxType of
            Skybox.GSVGSkybox _ _ _ _ -> Cmd.map SkyboxMsg gSCmd
            _ -> Cmd.none
        -- , Task.perform (\_ -> GenerateShadows name myShadowMeshes) (Task.succeed True)
        ]
    )

update : Msg m -> Model -> ( Model, Cmd (Msg m) )
update message model =
    case message of
        Resize width height ->
            let
                (wModel, wCmd) = Widget.init (Basics.toFloat <| unwrapQ width ) (Basics.toFloat <| unwrapQ height) "widget"
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
                        updatedMeshes = Dict.insert generatedMesh.name generatedMesh.mesh model.meshStore.generatedMeshes
                        updatedShadows = Dict.insert generatedMesh.name generatedMesh.shadow model.meshStore.generatedShadows

                        updatedMeshStore = { generatedMeshes = updatedMeshes, generatedShadows = updatedShadows }
                    in
                        ( { model | meshStore = updatedMeshStore }, Cmd.batch [ Task.perform (\_ -> GenerateMeshes rest) (Task.succeed True) ])

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
            ( { model | textures = textures }, Cmd.none)

        Error err ->
            let
              e = Debug.log "err" err
            in
            ( model, Cmd.none)

        -- This is needed for our widget
        WidgetMsg wMsg ->
            let
                (newWModel, wCmd) = Widget.update wMsg model.widget
            in
            ( { model | widget = newWModel }, Cmd.map WidgetMsg wCmd )

        Reset -> ( { model | azimuth = Angle.degrees 0, elevation = Angle.degrees 30
                            , shape = Cube 
                            , draw = Matte 
                            , l = 20
                            , w = 10
                            , h = 15
                            , sides = 5
                            , xAngle = 30
                            , yAngle = 30
                            , zAngle = 30 
                            , clr = RGB
                            , red = 255
                            , green = 0
                            , blue = 0 
                            , hasMove = False
                            , hasRotateX = False 
                            , hasRotateY = False 
                            , hasRotateZ = False 
                            , hasScale = False 
                            , scl = 2
                            , x = 0
                            , y = 0
                            , z = 0
                            , r = 10
                            , thickness = 3
                            , currentButton = None
                            , buttonDownTime = 0
                            , roughness = 0
                             } , Cmd.none )

        SkyboxMsg sMsg ->
            case sMsg of
                GS.AllLoaded allTextures ->
                    ( model, fetchTextures allTextures )
                _ ->
                    let
                        (gSkyboxModel, gSCmd) = GS.update sMsg model.gSkyboxModel
                    in
                        ( { model | gSkyboxModel = gSkyboxModel } , Cmd.map SkyboxMsg gSCmd)

        --shape creator
        ButtonDown dir ->
            ( { model | currentButton = dir, clr = RGB }, Cmd.none)

        Sten stencil ->
            ({ model | shape = stencil }, Cmd.none)

        Draw draw ->
            ({ model | draw = draw }, Cmd.none)

        LStyle ->
            ({ model
                | style =
                    case model.style of
                        Solid ->
                            Dotted

                        Dotted ->
                            Dashed

                        Dashed ->
                            Longdash

                        Longdash ->
                            Dotdash

                        Dotdash ->
                            Solid
            }, Cmd.none)

        CycleText ->
            ({ model | txt = cycleTxt model.txt }, Cmd.none)

        Toggle Move ->
            ({ model | hasMove = not model.hasMove }, Cmd.none)

        Toggle RotateX ->
            ({ model | hasRotateX = not model.hasRotateX }, Cmd.none)
        
        Toggle RotateY -> 
             ({ model | hasRotateY = not model.hasRotateY }, Cmd.none)

        Toggle RotateZ -> 
             ({ model | hasRotateZ = not model.hasRotateZ }, Cmd.none)

        Toggle Scale ->
            ({ model | hasScale = not model.hasScale }, Cmd.none)

        TransM t ->
           (t model, Cmd.none)
          -- (model, Cmd.none)

        SetColour clr ->
            ({ model | clr = clr }, Cmd.none)

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            ({ model | notify = notif }, Cmd.none)

        NextPolygon -> 
            if ((Dict.get (model.nextPolygon + 1) polygonDict) == Nothing) then
              ({ model | nextPolygon = 0 } , Cmd.none)
            else
              ({ model | nextPolygon = model.nextPolygon + 1 } , Cmd.none)

mouseMoveDecoder : Decoder (Msg m)
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Fetch textures from textureListSkybox
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox.skybox

fetchTextures : List (String, String) -> Cmd (Msg m)
fetchTextures textDist =
  let
    keyList = List.map (\(key, texture) -> key) textDist
    textureList = List.map (\(key, texture) -> texture) textDist
  in
   textureList
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
                Ok textures ->
                  LoadTexture (Dict.fromList (List.map2
                     (\key texture -> (key, texture))
                      keyList
                      textures
                     ))
                Err error -> Error error
        )



debugs : List (Entity WorldCoordinates)
debugs =
    [ Scene3d.lineSegment (Material.color Color.darkRed)
        (LineSegment3d.along Axis3d.x (Length.centimeters 0) (Length.centimeters 5000))
    , Scene3d.lineSegment (Material.color Color.darkGreen)
        (LineSegment3d.along Axis3d.y (Length.centimeters 0) (Length.centimeters 5000))
    , Scene3d.lineSegment (Material.color Color.darkBlue)
        (LineSegment3d.along Axis3d.z (Length.centimeters 0) (Length.centimeters 5000))
    ]

{-html
<!DOCTYPE HTML>
<html>
<head>
    <meta charset="UTF-8">
    <title>Main</title>
    <link rel="icon" href="favicon.ico?v=1" />
    <script src="app.js"></script>
</head>

<body>
<div id="elm"></div>

<div id="canvas"></div>
<script>
{{ elmjs }}

  var app = Elm.{{ modulename }}.init({
    node: document.getElementById('elm')
  });

function triggerDownload (widgetID, imgURI) {
  var evt = new MouseEvent('click', {
    view: window,
    bubbles: false,
    cancelable: true
  });

  var a = document.createElement('a');
  a.setAttribute('download', widgetID + '.png');
  a.setAttribute('href', imgURI);
  a.setAttribute('target', '_blank');

  a.dispatchEvent(evt);
}

app.ports.createPNG.subscribe(function([widgetID, width, height]) {
  var canvas = document.createElement("CANVAS");
  canvas.width=width;
  canvas.height=height;
  var ctx = canvas.getContext('2d');

  var svg = document.getElementById(widgetID);
  var data = (new XMLSerializer()).serializeToString(svg);
  var DOMURL = window.URL || window.webkitURL || window;

  var img = new Image();
  var svgBlob = new Blob([data], {type: 'image/svg+xml;charset=utf-8'});
  var url = DOMURL.createObjectURL(svgBlob);

  img.onload = function () {
    ctx.drawImage(img, 0, 0);
    DOMURL.revokeObjectURL(url);

    var imgURI = canvas
        .toDataURL('image/png')
        .replace('image/png', 'image/octet-stream');

    //triggerDownload(widgetID,imgURI);
    app.ports.receivePNG.send([widgetID, imgURI]);
  };

  img.src = url;
});

  </script>
</body>
</html>

endhtml-}



--graphics svg

-- this case catches every other string and turns it into Hello
-- since there are an infinite number of Strings, we need a catch-all case
-- main view components


stencils model =
  let
    dropDownWords = 
      (Basics.round (relativeP 60 model.height))
  in
    group
        [ rect (relativeP 6 model.width) (relativeP 4.4 model.height) 
          |> filled (rgba 255 255 255 0.5) 
          |> addOutline (solid 1) lightGrey
        , rect (relativeP 15 model.width) (relativeP 50 model.height) |> filled white |> addOutline (solid 1) lightGrey 
          |> move ( 0, (relativeP 10.1 model.height) )
        , text "1. Pick a Stencil!" |> serif |> italic 
          |> size (relativeP 60 model.height) |> filled titleColour 
          |> move ( -(relativeP 38 model.width), (relativeP 10.6 model.height) )
        , group <|
            List.map2
                (\ss y ->
                    stencilString model ss
                        |> text
                        |> fixedwidth
                        |> size (relativeP 60 model.height)
                        |> filled black
                        |> notifyTap (Sten ss)
                        |> move ( -(relativeP 6 model.height), (relativeP 13.3 model.height) )
                        |> time1 model ss (relativeP 6.2 model.width) (relativeP 60 model.height)
    
                        |> move ( 0, y )
                )
                [ Cube, Square3D, Rectangle3D, Boxx, Sphere, Cone, Cylinder, Ring, PolyCone, PolyCylinder, Ellipsoid, Polygon3D ]
                (List.map (\x -> -(relativeP 60 model.height) * Basics.toFloat x) 
                   (List.range 0 (20 * dropDownWords)))
        ]


stamps model =
  let
    dropDownWords = 
      (Basics.round (relativeP 60 model.height))
  in
    group
        [ rect (relativeP 9.1 model.width) (relativeP 12 model.height) |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey 
        , rect (relativeP 13 model.width) (relativeP 50 model.height) |> filled white 
            |> addOutline (solid 1) lightGrey
            |> move (0 , (relativeP 37 model.height))
        , text "2. Choose Material!" |> serif |> italic |> size (relativeP 65 model.height) |> filled titleColour 
            |> move ( -(relativeP 33.5 model.width), (relativeP 45 model.height) )
        , group <|
            List.map2
                (\ss y ->
                    stampString model ss
                        |> text
                        |> fixedwidth
                        |> size (relativeP 60 model.height)
                        |> filled black
                        |> notifyTap (Draw ss)
                        |> move ( -(relativeP 20 model.width), -(relativeP 240 model.height) )
                        |> time2 model ss (relativeP 9.5 model.width) (relativeP 60 model.height)
                        |> move ( 0, y )
                )
                [ Metal, Plastic, Matte ]
                (List.map (\x -> -(relativeP 60 model.height) * Basics.toFloat x) 
                   (List.range 0 (20 * dropDownWords)))
        ]


colours model =
  let
    dropDownWords = 
      (Basics.round (relativeP 60 model.height))
  in
    group
        [ rect (relativeP 8.6 model.width) (relativeP 1.65 model.height) |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey 
         |> move ( 0, (relativeP 35 model.height) )
        , rect (relativeP 16 model.width) (relativeP 50 model.height) |> filled white |> addOutline (solid 1) lightGrey 
         |> move ( 0, (relativeP 3.15 model.height) )
        , text "3. Pick a Colour!" |> serif |> italic |> size (relativeP 60 model.height) |> filled titleColour 
         |> move ( -(relativeP 34.5 model.width), (relativeP 3.2 model.height) )
        , group <|
            List.map2
                (\ss y ->
                    clrString model ss
                        |> text
                        |> fixedwidth
                        |> size (relativeP 60 model.height)
                        |> filled black
                        |> notifyTap (SetColour ss)
                        |> move ( -(relativeP 18.9 model.width), (relativeP 3.5 model.height) )
                        |> time3 model ss (relativeP 9 model.width) 10
                        |> move ( 0, y )
                )
                [ Black
                , White
                , Blue
                , DarkBlue
                , LightBlue
                , Brown
                , DarkBrown
                , LightBrown
                , Charcoal
                , DarkCharcoal
                , LightCharcoal
                , Gray
                , DarkGray
                , LightGray
                , Green
                , DarkGreen
                , LightGreen
                , Orange
                , DarkOrange
                , LightOrange
                , Purple
                , DarkPurple
                , LightPurple
                , Yellow
                , DarkYellow
                , LightYellow
                , Red
                , DarkRed
                , LightRed
                ]
                (List.map (\x -> -(relativeP 60 model.height) * Basics.toFloat x) 
                   (List.range 0 (40 * dropDownWords)))
        , rect (relativeP 9 model.width) (relativeP 60 model.height)
            |> filled (rgba 1 1 1 0)
            |> notifyTap (SetColour RGB)
            |> time3 model RGB (relativeP 9 model.width) (relativeP 60 model.height)
            |> move (0 , -(relativeP 60 model.height))
        , group
            [ triangle (relativeP 75 model.height)
                |> filled (rgb 255 10 10)
                |> rotate (degrees -30)
                |> move ( -(relativeP 6.3 model.height), -(relativeP 60 model.height) )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red =
                                    if m.red < 254 then
                                        m.red + 1

                                    else
                                        255
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown RedUp)
                |> notifyMouseUp (ButtonDown None)
            , triangle (relativeP 75 model.height)
                |> filled (rgb 180 140 140)
                |> rotate (degrees 30)
                |> move ( -(relativeP 7.1 model.height), -(relativeP 66.6 model.height) )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red =
                                    if m.red > 1 then
                                        m.red - 1

                                    else
                                        0
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown RedDown)
                |> notifyMouseUp (ButtonDown None)
            , triangle (relativeP 75 model.height)
                |> filled (rgb 10 255 10)
                |> rotate (degrees -30)
                |> move ( -(relativeP 9.2 model.height), -(relativeP 60 model.height) )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | green =
                                    if m.green < 254 then
                                        m.green + 1

                                    else
                                        255
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown GreenUp)
                |> notifyMouseUp (ButtonDown None)
            , triangle (relativeP 75 model.height)
                |> filled (rgb 140 180 140)
                |> rotate (degrees 30)
                |> move ( -(relativeP 11.1 model.height), -(relativeP 66.6 model.height) )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | green =
                                    if m.green > 1 then
                                        m.green - 1

                                    else
                                        0
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown GreenDown)
                |> notifyMouseUp (ButtonDown None)
            , triangle (relativeP 75 model.height)
                |> filled (rgb 10 10 255)
                |> rotate (degrees -30)
                |> move ( -(relativeP 17.14 model.height), -(relativeP 66.6 model.height) )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | blue =
                                    if m.blue < 254 then
                                        m.blue + 1

                                    else
                                        255
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown BlueUp)
                |> notifyMouseUp (ButtonDown None)
            , triangle (relativeP 75 model.height)
                |> filled (rgb 140 140 180)
                |> rotate (degrees 30)
                |> move ( -(relativeP 25 model.height), -(relativeP 66.6 model.height) )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | blue =
                                    if m.blue > 1 then
                                        m.blue - 1

                                    else
                                        0
                            }
                        )
                    )
                |> notifyMouseDown (ButtonDown BlueDown)
                |> notifyMouseUp (ButtonDown None)
            , "lighter" |> code  model |> move ( -(relativeP 6.6 model.height), -(relativeP 24 model.height) )
            , rect 32 10
                |> filled blank
                |> move ( -75, -27 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red = clamp 0 255 (m.red + 5)
                                , blue = clamp 0 255 (m.blue + 5)
                                , green = clamp 0 255 (m.green + 5)
                            }
                        )
                    )
            , "darker" |> code model |> move ( -(relativeP 13.3 model.height), -(relativeP 24 model.height) )
            , rect 32 10
                |> filled blank
                |> move ( -30, -27 )
                |> notifyTap
                    (TransM
                        (\m ->
                            { m
                                | red = clamp 0 255 (m.red - 5)
                                , blue = clamp 0 255 (m.blue - 5)
                                , green = clamp 0 255 (m.green - 5)
                            }
                        )
                    )
            , "colourful" |> code model|> move ( -(relativeP 6 model.height), -(relativeP 15 model.height) )
            , rect 40 10
                |> filled blank
                |> move ( -80, -47 )
                |> notifyTap
                    (TransM
                        (\m ->
                            case getBrightest model.red model.green model.blue of
                                All ->
                                    m

                                BrRed ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }

                                BrGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount * 2)
                                    }

                                BrBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount * 2)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }

                                RedAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount * 2)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }

                                RedAndBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount * 2)
                                    }

                                BlueAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }
                        )
                    )
            , "colourless" |> code model |> move ( -(relativeP 13.6 model.height), -(relativeP 15 model.height) ) 
            , rect 44 10
                |> filled blank
                |> move ( -23, -47 )
                |> notifyTap
                    (TransM
                        (\m ->
                            case getBrightest model.red model.green model.blue of
                                All ->
                                    m

                                BrRed ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }

                                BrGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount * 2)
                                    }

                                BrBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount * 2)
                                        , green = clamp 0 255 (m.green + colourAmount)
                                    }

                                RedAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue + colourAmount * 2)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }

                                RedAndBlue ->
                                    { m
                                        | red = clamp 0 255 (m.red - colourAmount)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green + colourAmount * 2)
                                    }

                                BlueAndGreen ->
                                    { m
                                        | red = clamp 0 255 (m.red + colourAmount * 2)
                                        , blue = clamp 0 255 (m.blue - colourAmount)
                                        , green = clamp 0 255 (m.green - colourAmount)
                                    }
                        )
                    )
            ]
            |> move ( (relativeP 11.5 model.height), -(relativeP 5.2 model.height) )
        ]


transforms model =
  let
    dropDownWords = 
      (Basics.round (relativeP 60 model.height))
  in
    group
        [ rect (relativeP 4 model.height) (relativeP 8.5 model.height) |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey 
          |> move ( -(relativeP 18.8 model.height), -(relativeP 28.5 model.height) )
        , rect (relativeP 6.3 model.height) (relativeP 50 model.height) |> filled white |> addOutline (solid 1) lightGrey 
          |> move ( -(relativeP 13.3 model.height), (relativeP 42.8 model.height) )
        , text "4. Apply Transforms!" |> serif |> italic |> size (relativeP 60 model.height) |> filled titleColour 
          |> move ( -(relativeP 7 model.height), (relativeP 54.5 model.height) )
        , group <|
            List.map2
                (\ss y ->
                    transformString model ss
                        |> text
                        |> fixedwidth
                        |> size (relativeP 60 model.height)
                        |> filled black
                        |> notifyTap (Toggle ss)
                        |> move ( -(relativeP 8.8 model.height), -(relativeP 240 model.height) )
                        |> time4 model ss (relativeP 4.2 model.height) (relativeP 60 model.height)
                        |> move ( -(relativeP 17.14 model.height), y )
                )
                [ Scale, RotateX, RotateY, RotateZ, Move ]
                (List.map (\x -> -(relativeP 60 model.height) * Basics.toFloat x) 
                   (List.range 0 (20 * dropDownWords)))
        ]


tweaks model =
  let
    dropDownWords = 
      (Basics.round (relativeP 60 model.height))
  in
    group
        [ rect (relativeP 4.2 model.height) (relativeP 3.3 model.height) |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey 
          |> move ( -(relativeP 17.1 model.height), -(relativeP 7.6 model.height) )
        , rect (relativeP 10.9 model.height) (relativeP 50 model.height) |> filled white |> addOutline (solid 1) lightGrey 
          |> move ( -(relativeP 10 model.height), (relativeP 42.8 model.height) )
        , text "5. Tweak it!" |> serif |> italic |> size (relativeP 60 model.height) |> filled titleColour 
          |> move ( -(relativeP 7 model.height), (relativeP 54.5 model.height) )
        , group <|
            List.map2
                (\( str, msg ) ( x, y ) ->
                    str
                        |> text
                        |> fixedwidth
                        |> size (relativeP 60 model.height)
                        |> filled black
                        |> notifyTap msg
                        |> move ( -(relativeP 8.8 model.height) + x, -(relativeP 240 model.height) + y )
                )
                [ ( "move(+,_,_)", TransM (\m -> { m | x = m.x + 10 }) )
                , ( "move(-,_,_)", TransM (\m -> { m | x = m.x - 10 }) )
                , ( "move(_,+,_)", TransM (\m -> { m | y = m.y + 10 }) )
                , ( "move(_,-,_)", TransM (\m -> { m | y = m.y - 10 }) )
                , ( "move(_,_,+)", TransM (\m -> { m | z = m.z + 10 }) )
                , ( "move(_,_,-)", TransM (\m -> { m | z = m.z - 10 }) )



                , ( "x-clockwise", TransM (\m -> { m | xAngle = m.xAngle - 30 }) )
                , ( "x-counter", TransM (\m -> { m | xAngle = m.xAngle + 30 }) )
                , ( "y-clockwise", TransM (\m -> { m | yAngle = m.yAngle - 30 }) )
                , ( "y-counter", TransM (\m -> { m | yAngle = m.yAngle + 30 }) )
                , ( "z-clockwise", TransM (\m -> { m | zAngle = m.zAngle - 30 }) )
                , ( "z-counter", TransM (\m -> { m | zAngle = m.zAngle + 30 }) )




                
            , ( "shinier"
                  , TransM
                        (\m ->
                            { m
                                | roughness =
                                    if m.roughness > 0 then
                                        m.roughness - 1

                                    else
                                        0
                            }
                        )
                  )
              , ( "duller"
                  , TransM
                        (\m ->
                            { m
                                | roughness =
                                    if m.roughness < 10 then
                                        m.roughness + 1

                                    else
                                        10
                            }
                        )
                  )



                , ( "bigger"
                  , TransM
                        (\m ->
                            { m
                                | scl =
                                    if m.scl < 3 then
                                        m.scl + 0.25

                                    else
                                        3
                                , sclx =
                                    if m.sclx < 3 then
                                        m.sclx + 0.25

                                    else
                                        3
                                , scly =
                                    if m.scly < 3 then
                                        m.scly + 0.25

                                    else
                                        3
                            }
                        )
                  )
                , ( "smaller"
                  , TransM
                        (\m ->
                            { m
                                | scl =
                                    if m.scl > -3 then
                                        m.scl - 0.25

                                    else
                                        -3
                                , sclx =
                                    if m.sclx > -3 then
                                        m.sclx - 0.25

                                    else
                                        -3
                                , scly =
                                    if m.scly > -3 then
                                        m.scly - 0.25

                                    else
                                        -3
                            }
                        )
                  )
              

                , ( "x-clockwise", TransM (\m -> { m | xAngle = m.xAngle - 30 }) )
                , ( "x-counter", TransM (\m -> { m | xAngle = m.xAngle + 30 }) )
                , ( "y-clockwise", TransM (\m -> { m | yAngle = m.yAngle - 30 }) )
                , ( "y-counter", TransM (\m -> { m | yAngle = m.yAngle + 30 }) )
                , ( "z-clockwise", TransM (\m -> { m | zAngle = m.zAngle - 30}) )
                , ( "z-counter", TransM (\m -> { m | zAngle = m.zAngle + 30 }) )



                , ( "x-wider"
                  , TransM
                        (\m ->
                            { m
                                | l =
                                    if m.l < 100 then
                                        m.l + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "x-narrower"
                  , TransM
                        (\m ->
                            { m
                                | l =
                                    if m.l > 10 then
                                        m.l - 10

                                    else
                                        10
                            }
                        )
                  )
                , ( "y-wider"
                    , TransM
                        (\m ->
                            { m
                                | w =
                                    if m.w < 100 then
                                        m.w + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "y-narrower"
                  , TransM
                        (\m ->
                            { m
                                | w =
                                    if m.w > 10 then
                                        m.w - 10

                                    else
                                        10
                            }
                        )
                  )
                , ( "z-taller"
                  , TransM
                        (\m ->
                            { m
                                | h =
                                    if m.h < 100 then
                                        m.h + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "z-shorter"
                  , TransM
                        (\m ->
                            { m
                                | h =
                                    if m.h > 10 then
                                        m.h - 10

                                    else
                                        10
                            }
                        )
                  )
                
                , ( "radius(+)"
                  , TransM
                        (\m ->
                            { m
                                | r =
                                    if m.r < 100 then
                                        m.r + 10

                                    else
                                        100
                            }
                        )
                  )
                , ( "radius(-)"
                  , TransM
                        (\m ->
                            { m
                                | r =
                                    if m.r > 10 then
                                        m.r - 10

                                    else
                                        10
                            }
                        )
                  )

                , ( "thicker"
                  , TransM
                        (\m ->
                            { m
                                | thickness =
                                    if m.thickness < 20 then
                                        m.thickness + 3

                                    else
                                        20
                            }
                        )
                  )
                , ( "thinner"
                  , TransM
                        (\m ->
                            { m
                                | thickness =
                                    if m.thickness > 1 then
                                        m.thickness - 3

                                    else
                                        1
                            }
                        )
                  )

               
                -- , ( "red(+)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | red =
                --                     if m.red < 248 then
                --                         m.red + 7

                --                     else
                --                         255
                --             }
                --         )
                --   )
                -- , ( "red(-)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | red =
                --                     if m.red > 8 then
                --                         m.red - 8

                --                     else
                --                         0
                --             }
                --         )
                --   )
                -- , ( "green(+)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | green =
                --                     if m.green < 248 then
                --                         m.green + 7

                --                     else
                --                         255
                --             }
                --         )
                --   )
                -- , ( "green(-)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | green =
                --                     if m.green > 8 then
                --                         m.green - 8

                --                     else
                --                         0
                --             }
                --         )
                --   )
                -- , ( "blue(+)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | blue =
                --                     if m.blue < 248 then
                --                         m.blue + 7

                --                     else
                --                         255
                --             }
                --         )
                --   )
                -- , ( "blue(-)"
                --   , TransM
                --         (\m ->
                --             { m
                --                 | blue =
                --                     if m.blue > 8 then
                --                         m.blue - 8

                --                     else
                --                         0
                --             }
                --         )
                --   )
               

                , ( "sides(+)"
                  , TransM
                        (\m ->
                            { m
                                | sides =
                                    if m.sides > 20 then
                                        3

                                    else
                                        m.sides + 1
                            }
                        )
                  )
                
                ,
                ( "sides(-)"
                  , TransM
                        (\m ->
                            { m
                                | sides =
                                    if m.sides < 3 then
                                        20

                                    else
                                        m.sides - 1
                            }
                        )
                  )
                ]
                (List.concat <| List.map (\idx -> [ ( -(relativeP 20 model.height), -((relativeP 60 model.height) * Basics.toFloat idx) ), ( (relativeP 15 model.height), -((relativeP 60 model.height) * Basics.toFloat idx) ) ]) 
                (List.range 0 (20 * dropDownWords)))
                

        ]


yourCode model =
    group
        [ rect (relativeP 2.1 model.height) (relativeP 7.6 model.height) |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey 
          |> move ( -(relativeP 30 model.height), -(relativeP 24 model.height) )
        , rect (relativeP 7.5 model.height) (relativeP 50 model.height) |> filled white |> addOutline (solid 1) lightGrey 
          |> move ( -(relativeP 10.9 model.height), (relativeP 42.8 model.height) )
        , text "5. Your code!" |> serif |> italic |> size (relativeP 60 model.height) |> filled titleColour 
          |> move ( -(relativeP 7.1 model.height), (relativeP 54.5 model.height) )
        , move ( -(relativeP 4 model.height), 0 ) <|
            group <|
                [ stencilString model model.shape |> copiable model|> move ( -(relativeP 60 model.height), 0 )
                , "  |> "
                    ++ stampString model model.draw 
                    ++ clrString model model.clr
                    ++ (if (model.draw == Metal || model.draw == Plastic) then " 0.5" else "") --TODO: change this to actual roughness variable
                    |> copiable model
                    |> move ( 0, -(relativeP 60 model.height) )

                ]
                    ++ List.map2 (\str y -> str |> copiable model |> move ( 0, y ))
                        (List.concat <|
                            List.map
                                (\( flag, t ) ->
                                    if flag then
                                        [ "  " ++ transformString model t ]

                                    else
                                        []
                                )
                                [ ( model.hasScale, Scale )
                                , ( model.hasRotateX, RotateX )
                                ,  ( model.hasRotateY, RotateY )
                                ,  ( model.hasRotateZ, RotateZ )
                                , ( model.hasMove, Move )
                                ]
                        )
                        [ -(relativeP 30 model.height)
                          , -(relativeP 20 model.height)
                          , -(relativeP 15 model.height)
                          , -(relativeP 12 model.height)
                          , -(relativeP 10 model.height)
                          , -(relativeP 8.5 model.height)
                          , -(relativeP 7.5 model.height) ]
        ]



-- check if the drawn text is the selected function, and if so group a beating rectangle behind it
-- stagger the heartbeats of selected elements to so that they indicate the order of selection


time1 model ss w h shape =
    if ss == model.shape then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time)))
                         |> move (0 , (relativeP 12.5 model.height)), shape ]

    else
        shape


time2 model ss w h shape =
    if ss == model.draw then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 0.5))), shape ]

    else
        shape


time3 model ss w h shape =
    if ss == model.clr then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 1)))
                         |> move ( 0, (relativeP 3.45 model.height) ), shape ]

    else
        shape


time4 model t w h shape =
    if
        case t of
            Move ->
                model.hasMove

            RotateX ->
                model.hasRotateX
        
            RotateY -> 
                model.hasRotateY 

            RotateZ -> 
                model.hasRotateZ

            Scale ->
                model.hasScale

    then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 1.5))), shape ]

    else
        shape



-- view helpers


stencilString m shape =
    case shape of
        Cube ->
            "cube " ++ String.fromFloat m.w

        Square3D ->
            "square3D " ++ String.fromFloat m.w
        
        Rectangle3D ->
            "rectangle3D "  ++ String.fromFloat m.l ++ " " ++ String.fromFloat m.w

        Boxx ->
            "box " ++ String.fromFloat m.l ++ " " ++ String.fromFloat m.w ++ " " ++ String.fromFloat m.h

        Sphere ->
            "sphere " ++ String.fromFloat m.r

        Cone ->
            "cone " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.h

        Cylinder ->
            "cylinder " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.h

        Ring ->
            "ring " ++ String.fromFloat m.r ++ " " ++ String.fromFloat m.thickness 

        PolyCone ->
            -- "polyCone "  ++ "[(-10,-10), (10,10), (10,-10),(-10,-10)] (0,0,25) m.meshStore"

            "polyCone "  ++ "polyPoints (0,0,25) m.meshStore"


        PolyCylinder ->
            "polyCylinder " ++ "polyPoints " ++ String.fromFloat m.h ++ " m.meshStore"
            -- "polyCylinder " ++ "[(-10,-10), (10,10), (10,-10),(-10,-10)] " ++ String.fromFloat m.h ++ " m.meshStore"

        Ellipsoid ->
            "ellipsoid " ++ String.fromFloat m.w ++ " " ++ String.fromFloat m.l ++ " " ++ String.fromFloat m.h ++ " m.meshStore" 

        Polygon3D ->
            "polygon3D "  ++ String.fromInt m.sides ++ " " ++ String.fromFloat m.w

        CustomPolygon ->
            -- "customPolygon "  ++ "[(-10,-10), (10,10), (10,-10),(-10,-10)]"      
            "customPolygon " ++ "LIST"      




stencilFun m =
    case m.shape of
        Cube ->
            cube m.w

        Boxx ->
            box m.l m.w m.h

        Sphere ->
            cube m.r

        Cone ->
            cone m.r m.h

        Cylinder ->
            cylinder m.r m.h

        Ring ->
            ring m.r m.thickness

        _ -> cube m.w --not gonna happen

getPolygonDic model =
  case Dict.get model.nextPolygon polygonDict of
    Just polygon -> polygon
    Nothing -> [(-10,-10), (10,10), (10,-10),(-10,-10)]

stencilFunTextured m =
    case m.shape of 
        Square3D -> 
            square3D m.w
        
        Rectangle3D ->
            rectangle3D m.l m.w

        Sphere -> 
            sphere m.r
        
        Polygon3D ->
            polygon3D m.sides m.w

        PolyCone -> polyCone (getPolygonDic m) (0,0,25) m.meshStore

        PolyCylinder -> polyCylinder (getPolygonDic m) m.h m.meshStore

        Ellipsoid -> ellipsoid m.w m.l m.h m.meshStore     

        CustomPolygon -> customPolygon [(-10,-10), (10,10), (10,-10),(-10,-10)]

        _ -> sphere m.r




stampString m stamp =
    case stamp of
        Metal ->
            "metallic "

        Plastic ->
            "plastic "

        Matte ->
            "matte "



-- shapeFunTextured : Model -> EntityBBox coordinates 
shapeFunTextured m = 
    (
    case m.draw of
        Metal ->
            metallic (colourFun m) (intToFloat m.roughness) (stencilFunTextured m)

        Plastic ->
            plastic (colourFun m) (intToFloat m.roughness)  (stencilFunTextured m)

        Matte -> matte (colourFun m) (stencilFunTextured m)

        
    )
        |> (if m.hasMove then
                move3D ( m.x, m.y, m.z)

            else
                \x -> x
           )
        |> (if m.hasRotateX then
                rotateX3D (degrees m.xAngle) 

            else
                \x -> x
           )
        |> (if m.hasRotateY then
                rotateY3D (degrees m.yAngle) 

            else
                \y -> y
           )   
        |> (if m.hasRotateZ then
                rotateZ3D (degrees m.zAngle) 

            else
                \z -> z
           )           
        |> (if m.hasScale then
                scale3D m.scl

            else
                \x -> x
           )
       

shapeFun : Model -> EntityBBox coordinates
shapeFun m =
    (
    -- let
    --     stencil = if (List.member m.shape [Cube, Boxx, Cone, Cylinder, Ring]) then  stencilFun m else stencilFunTextured m 

    -- in


    case m.draw of
        Metal ->
            metallic (colourFun m) (intToFloat m.roughness) (stencilFun m)

        Plastic ->
            plastic (colourFun m) (intToFloat m.roughness) (stencilFun m)

        Matte -> matte (colourFun m) (stencilFun m)

        
    )
        |> (if m.hasMove then
                move3D ( m.x, m.y, m.z)

            else
                \x -> x
           )
        |> (if m.hasRotateX then
                rotateX3D (degrees m.xAngle) --TODO different angles

            else
                \x -> x
           )
        |> (if m.hasRotateY then
                rotateY3D (degrees m.yAngle) --TODO different angles

            else
                \y -> y
           )   
        |> (if m.hasRotateZ then
                rotateZ3D (degrees m.zAngle) --TODO different angles

            else
                \z -> z
           )           
        |> (if m.hasScale then
                scale3D m.scl

            else
                \x -> x
           )
       

clrString m clr =
    case clr of
        Black ->
            "Color.black"

        Blue ->
            "Color.blue"

        Brown ->
            "Color.brown"

        Charcoal ->
            "Color.charcoal"

        DarkBlue ->
            "Color.darkBlue"

        DarkBrown ->
            "Color.darkBrown"

        DarkCharcoal ->
            "Color.darkCharcoal"

        DarkGray ->
            "Color.darkGray"

        DarkGreen ->
            "Color.darkGreen"

        DarkGrey ->
            "Color.darkGrey"

        DarkOrange ->
            "Color.darkOrange"

        DarkPurple ->
            "Color.darkPurple"

        DarkRed ->
            "Color.darkRed"

        DarkYellow ->
            "Color.darkYellow"

        Gray ->
            "Color.gray"

        Green ->
            "Color.green"

        Grey ->
            "Color.grey"


        LightBlue ->
            "Color.lightBlue"

        LightBrown ->
            "Color.lightBrown"

        LightCharcoal ->
            "Color.lightCharcoal"

        LightGray ->
            "Color.lightGray"

        LightGreen ->
            "Color.lightGreen"

        LightGrey ->
            "Color.lightGrey"

        LightOrange ->
            "Color.lightOrange"

        LightPurple ->
            "Color.lightPurple"

        LightRed ->
            "Color.lightRed"

        LightYellow ->
            "Color.lightYellow"

        Orange ->
            "Color.orange"


        Purple ->
            "Color.purple"

        Red ->
            "Color.red"

        White ->
            "Color.white"

        Yellow ->
            "Color.yellow"

        RGB ->
            "(Color.rgb255 " ++ String.fromInt m.red ++ " " ++ String.fromInt m.green ++ " " ++ String.fromInt m.blue ++ ")"


colourFun m =
    case m.clr of
        Black ->
            Color.black

        Blue ->
            Color.blue

        Brown ->
            Color.brown

        Charcoal ->
            Color.charcoal

        DarkBlue ->
            Color.darkBlue

        DarkBrown ->
            Color.darkBrown

        DarkCharcoal ->
            Color.darkCharcoal

        DarkGray ->
            Color.darkGray

        DarkGreen ->
            Color.darkGreen

        DarkGrey ->
            Color.darkGrey

        DarkOrange ->
            Color.darkOrange

        DarkPurple ->
            Color.darkPurple

        DarkRed ->
            Color.darkRed

        DarkYellow ->
            Color.darkYellow

        Gray ->
            Color.gray

        Green ->
            Color.green

        Grey ->
           Color.grey

        LightBlue ->
            Color.lightBlue

        LightBrown ->
            Color.lightBrown

        LightCharcoal ->
            Color.lightCharcoal

        LightGray ->
            Color.lightGray

        LightGreen ->
            Color.lightGreen

        LightGrey ->
            Color.lightGrey

        LightOrange ->
            Color.lightOrange


        LightPurple ->
            Color.lightPurple

        LightRed ->
            Color.lightRed

        LightYellow ->
            Color.lightYellow

        Orange ->
            Color.orange

        Purple ->
            Color.purple

        Red ->
            Color.red

        White ->
            Color.white

        Yellow ->
            Color.yellow

        RGB ->
            Color.rgb255 m.red m.green m.blue


transformString m t =
    case t of
        Move ->
            "|> move3D (" ++ String.fromFloat m.x ++ "," ++ String.fromFloat m.y ++ "," ++ String.fromFloat m.z ++ ")"

        RotateX ->
            "|> rotateX3D (degrees " ++ String.fromFloat m.xAngle ++ ")"
        
        RotateY ->
            "|> rotateY3D (degrees " ++ String.fromFloat m.yAngle ++ ")"

        RotateZ ->
            "|> rotateZ3D (degrees " ++ String.fromFloat m.zAngle ++ ")"

        Scale ->
            "|> scale3D " ++ String.fromFloat m.scl




--


lineStyleFun m =
    case m.style of
        Solid ->
            solid m.lineWidth

        Dotted ->
            dotted m.lineWidth

        Dashed ->
            dashed m.lineWidth

        Longdash ->
            longdash m.lineWidth

        Dotdash ->
            dotdash m.lineWidth



-- format a string as code


titleColour =
    rgb 255 112 0


copiable model str =
    str |> text |> selectable |> fixedwidth |> size (relativeP 60 model.height) |> filled black


code model str =
    str |> text |> fixedwidth |> size (relativeP 60 model.height) |> filled black


colourAmount =
    3    

intToFloat : Int -> Float 
intToFloat i = case i of 
    0 -> 0.0
    1 -> 0.1 
    2 -> 0.2
    3 -> 0.3 
    4 -> 0.4
    5 -> 0.5
    6 -> 0.6
    7 -> 0.7 
    8 -> 0.8 
    9 -> 0.9 
    _ -> 1.0    