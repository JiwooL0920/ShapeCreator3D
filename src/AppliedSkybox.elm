{- Testing file for the 3D slot
 -}

module AppliedSkybox exposing (main)

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
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import Sphere3d
import Task
import Temperature
import Vector3d exposing (Vector3d)
import Viewpoint3d
import Cone3d
import Point2d
import Arc2d
import Arc3d
import Circle3d
import TriangularMesh
import Cylinder3d
import Triangle3d
import LineSegment3d
import WebGL.Texture
import Skybox
import Parameter1d

{-header<b>3D Slot!</b>: Make things in 3D! -}


{-editable-}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with a basic shape
myEntities model =  
    [ sphere 10
      |> metallic Color.red 0.3
    , box 10 10 20
      |> plastic Color.blue 0.4
      |> move (10, 10, 0)
    , cube 20
      |> customMat Color.orange 0.2 (-0.5*cos (model.time*2) + 0.5)  
      |> move (50*cos model.time, 50*sin model.time, 0)
    , cube 10 
      |> matte Color.black
      |> move(-50,-50,0)
    , cone 10 20
      |> metallic Color.darkGreen 0.7
      |> move (25, -25, 0)
    , cylinder 10 50
      |> metallic Color.yellow 0.7
      |> rotateY (degrees 90)
    , ring 25 5
      |> metallic Color.lightGray 0.5
      |> move (0, 0, 60)
    , rectangle 25 50
      |> metallic Color.red 0.3
      |> move (-25, -25, 40)
    , ellipsoid 25 10 15 model
      |> matte Color.purple
      |> move (50, 50, 50)
    , polyCylinder 
        [(-32,-13.42),(-41.39,5.1468),(-29.98,20.139),(-14.99,10.069),(-15.44,-6.265),(-21.03,-15.44),(-26.18,-17.23),(-32,-13.42)]
        25
        model
      |> matte Color.white
      |> move (-25,60,0)
    , polyCone [(-10,-10), (10,10), (10,-10),(-10,-10)] (0,0,25) model
      |> metallic Color.grey 0.2
      |> move (0,50,0)

    -- This group is just used for debugging the meshes
    -- , Scene3d.group (List.map (Scene3d.mesh (Material.metal {baseColor = Color.red, roughness = 0.3 })) (Dict.values model.generatedMeshes))
    --   |> move (25,25,50)
    ]

-- Put any custom meshes you need generated in here. Make sure the values are identical.
myMeshes = 
    [ -- This mesh below will be used for the ellipsoid above. Remember to specify its length, width, and height
      generateEllipsoid 25 10 15
    , generatePolyCylinder 
        [(-32,-13.42),(-41.39,5.1468),(-29.98,20.139),(-14.99,10.069),(-15.44,-6.265),(-21.03,-15.44),(-26.18,-17.23),(-32,-13.42)]
        25
    , generatePolyCone [(-10,-10), (10,10), (10,-10),(-10,-10)] (0,0,25)
    ]

-- This colour is used to create the floor. If you want custom colours, use Color.hsl or Color.rgb!
floorColour = Color.green

-- Here you can specify what images to use to create the skybox. Just replace "todo" with a link to an image. (Keep the quotes, though!)
textureBottom : String
textureBottom =
    "SkyboxTextures/mac-bottom.jpg"

textureTop : String
textureTop =
    "SkyboxTextures/mac-top.jpg"

textureSide1 : String
textureSide1 =
    "SkyboxTextures/mac-side-1.jpg"

textureSide2 : String
textureSide2 =
    "SkyboxTextures/mac-side-2.jpg"

textureSide3 : String
textureSide3 =
    "SkyboxTextures/mac-side-3.jpg"

textureSide4 : String
textureSide4 =
    "SkyboxTextures/mac-side-4.jpg"

{-endeditable-}

{-extra-}

--clean up type for (x,y,z)
type alias Dimension = (Float,Float,Float)

type alias Mold coordinates a = (Material coordinates { a | normals : () } -> Entity coordinates)

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

--Drawing basic shapes 

-- Materials
-- "Material coordinates { a | normals : () }" allegedly allows us to not have to bother with textured vs. uniform
metallic : Color.Color -> Float -> Mold WorldCoordinates a -> Entity WorldCoordinates 
metallic colour roughness shapeFunc = (Material.metal { baseColor = colour, roughness = roughness }) |> shapeFunc

plastic : Color.Color -> Float -> Mold WorldCoordinates a -> Entity WorldCoordinates
plastic colour roughness shapeFunc = (Material.nonmetal { baseColor = colour, roughness = roughness }) |> shapeFunc

matte : Color.Color -> Mold WorldCoordinates a -> Entity WorldCoordinates
matte colour shapeFunc = (Material.matte colour) |> shapeFunc

customMat : Color.Color -> Float -> Float -> Mold WorldCoordinates a -> Entity WorldCoordinates
customMat colour roughness metallicity shapeFunc = (Material.pbr { baseColor = colour, roughness = roughness, metallic = metallicity }) |> shapeFunc

-- Shapes

cube : Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cube size material = Scene3d.blockWithShadow material <|
        Block3d.from
            (Point3d.centimeters 0 0 0)
            (Point3d.centimeters size size size)

square : Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
square length material = 
    let
        posValue = length / 2
    in
        Scene3d.quadWithShadow material
            (Point3d.centimeters (-posValue) (-posValue) 0)
            (Point3d.centimeters (-posValue) posValue 0)
            (Point3d.centimeters posValue posValue 0)
            (Point3d.centimeters posValue (-posValue) 0)


rectangle : Float -> Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
rectangle length width material = 
    let
        lValue = length / 2
        wValue = width / 2

    in
        Scene3d.quadWithShadow material
            (Point3d.centimeters (-lValue) (-wValue) 0)
            (Point3d.centimeters (-lValue) wValue 0)
            (Point3d.centimeters lValue wValue 0)
            (Point3d.centimeters lValue (-wValue) 0)

box : Float -> Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates 
box length width height material = 
        Scene3d.blockWithShadow material <|
                Block3d.from
                    Point3d.origin
                    (Point3d.centimeters length width height)

sphere : Float -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
sphere r material = 
        (Scene3d.sphereWithShadow material <|
            Sphere3d.withRadius (Length.centimeters r) Point3d.origin)
            |> move (0,0,r)

cone : Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cone r h material = 
    Scene3d.coneWithShadow material <|
        Cone3d.along Axis3d.z
            { base = Length.centimeters 0
            , tip = Length.centimeters h
            , radius = Length.centimeters r
            }

polyCone : List (Float,Float) -> (Float,Float,Float) -> Model -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
polyCone points (xtip,ytip,ztip) model material =
    let 
        meshName = "polyCone" ++ String.fromInt (List.length points) ++ String.fromFloat xtip ++ String.fromFloat ytip ++ String.fromFloat ztip

        mesh = 
            case Dict.get meshName model.generatedMeshes of
                Nothing -> polyConeMesh points (xtip,ytip,ztip)
                Just actualMesh -> actualMesh

        shadow = 
            case Dict.get meshName model.generatedShadows of
                Nothing -> Mesh.shadow (polyConeMesh points (xtip,ytip,ztip))
                Just actualMesh -> actualMesh

    in
        Scene3d.meshWithShadow material mesh shadow

polyCylinder : List (Float,Float) -> Float -> Model -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
polyCylinder points height model material  =
    let 
        meshName = "polyCylinder" ++ String.fromInt (List.length points) ++ String.fromFloat height

        mesh = 
            case Dict.get meshName model.generatedMeshes of
                Nothing -> polyCylinderMesh points height
                Just actualMesh -> actualMesh

        shadow = 
            case Dict.get meshName model.generatedShadows of
                Nothing -> Mesh.shadow (polyCylinderMesh points height)
                Just actualMesh -> actualMesh

    in
        Scene3d.meshWithShadow material mesh shadow

cylinder : Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cylinder r h material =
    Scene3d.cylinderWithShadow material <|
        Cylinder3d.along Axis3d.z
            { start = Length.centimeters 0
            , end = Length.centimeters h
            , radius = Length.centimeters r
            }

ring : Float -> Float -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
ring radius thickness material = Scene3d.mesh material (ringMesh radius thickness)

{-| Create an ellipsoid with a custom length, width, and height.
Requires that you pass in the model as well, in order for it to retrieve the required meshes. -}
ellipsoid : Float -> Float -> Float -> Model -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
ellipsoid length width height model material = 
    let 
        meshName = "ellipsoid" ++ String.fromFloat length ++ String.fromFloat width ++ String.fromFloat height

        mesh = 
            case Dict.get meshName model.generatedMeshes of
                Nothing -> ellipsoidMesh length width height
                Just actualMesh -> actualMesh

        shadow = 
            case Dict.get meshName model.generatedShadows of
                Nothing -> Mesh.shadow (ellipsoidMesh length width height)
                Just actualMesh -> actualMesh

    in
        Scene3d.meshWithShadow material mesh shadow

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

--Translation 
move : Dimension -> Entity coordinates -> Entity coordinates
move (x,y,z) entity = entity |> Scene3d.translateBy (Vector3d.centimeters x y z)      

rotate : Float -> Float -> Float -> Entity coordinates -> Entity coordinates 
rotate pitch yaw roll entity = 
    entity 
        |> Scene3d.rotateAround Axis3d.x (Angle.radians pitch)  
        |> Scene3d.rotateAround Axis3d.y (Angle.radians roll)  
        |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)

--Alternate rotate functions (rotateX means rotate around X axis.. so on)
rotateX : Float -> Entity coordinates -> Entity coordinates
rotateX angle entity = entity |> Scene3d.rotateAround Axis3d.x (Angle.radians angle)

rotateY : Float -> Entity coordinates -> Entity coordinates
rotateY angle entity = entity |> Scene3d.rotateAround Axis3d.y (Angle.radians angle)

rotateZ : Float -> Entity coordinates -> Entity coordinates
rotateZ angle entity = entity |> Scene3d.rotateAround Axis3d.z (Angle.radians angle)


--TODO: Track eneity's position. 
scale : Float -> Entity coordinates -> Entity coordinates 
scale factor entity = entity |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) factor

-- repeat an animation for a given duration
repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
  speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition

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

        -- If the proper textures aren't loaded for whatever reason, the sky will just be light blue
        texturesList = case model.textures of
            Just textures ->
                textures

            Nothing ->
                List.repeat 6 (Material.constant Color.lightBlue)

        baseEntities = 
            [ firstLightBall
            , plane
            , Skybox.skybox (List.map Just texturesList) 10000
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
{-endextra-}


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
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees 0
      , elevation = Angle.degrees 30
      , textures = Nothing 
      , generatedMeshes = Dict.empty
      , generatedShadows = Dict.empty
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
            ( { model | width = width, height = height }, Cmd.none )

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

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Fetch textures from textureListSkybox
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox.skybox
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

-- MESHES ARE STORED HERE

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
                    <| List.range 0 1572
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

-- Modified from "sphere" in elm-3d-scene/src/Scene3d/Primitives.elm so might behave a bit strangely
ellipsoidMesh : Float -> Float -> Float -> Mesh.Textured WorldCoordinates
ellipsoidMesh length width height = 
    let
        n =
            72

        len = Length.centimeters length

        wid = Length.centimeters width

        hei = Length.centimeters height

        m =
            ceiling (toFloat n / 2)

        thetaValues =
            Parameter1d.steps n
                (Quantity.interpolateFrom Quantity.zero (Angle.turns 1))

        phiValues =
            Parameter1d.steps m
                (Quantity.interpolateFrom
                    (Angle.degrees 90)
                    (Angle.degrees -90)
                )

        vertices =
            thetaValues
                |> List.map
                    (\theta ->
                        phiValues
                            |> List.map
                                (\phi ->
                                    { position =
                                        Point3d.xyz
                                            (len |> Quantity.multiplyBy (Angle.cos phi * Angle.cos theta))
                                            (wid |> Quantity.multiplyBy (Angle.cos phi * Angle.sin theta))
                                            (hei |> Quantity.multiplyBy (Angle.sin phi))
                                    , normal =
                                        Direction3d.xyZ theta phi |> Direction3d.toVector
                                    , uv =
                                        ( Quantity.ratio theta (Angle.turns 1)
                                        , Quantity.ratio
                                            (phi |> Quantity.plus (Angle.degrees 90))
                                            (Angle.degrees 180)
                                        )

                                    -- , tangent =
                                    --     Direction3d.xy (theta |> Quantity.plus (Angle.degrees 90))
                                    --         |> Direction3d.toVector
                                    }
                                )
                    )
                |> List.concat
                |> Array.fromList

        thetaStartIndices =
            List.range 0 (n - 1)

        phiStartIndices =
            List.range 0 (m - 1)

        linearIndex i j =
            i * (m + 1) + j

        faces =
            thetaStartIndices
                |> List.map
                    (\i ->
                        phiStartIndices
                            |> List.map
                                (\j ->
                                    let
                                        bottomLeftIndex =
                                            linearIndex i (j + 1)

                                        bottomRightIndex =
                                            linearIndex (i + 1) (j + 1)

                                        topLeftIndex =
                                            linearIndex i j

                                        topRightIndex =
                                            linearIndex (i + 1) j
                                    in
                                    [ ( bottomLeftIndex
                                      , bottomRightIndex
                                      , topRightIndex
                                      )
                                    , ( bottomLeftIndex
                                      , topRightIndex
                                      , topLeftIndex
                                      )
                                    ]
                                )
                            |> List.concat
                    )
                |> List.concat
    in
    Mesh.texturedFaces (TriangularMesh.indexed vertices faces)
        |> Mesh.cullBackFaces

polyCylinderMesh : List (Float,Float) -> Float -> Mesh.Textured WorldCoordinates
polyCylinderMesh points height =
    let
        bottom = List.map 
            ( \ (x,y) -> 
                { position = Point3d.centimeters x y 0
                , uv = (0,0) -- Temporary until we can figure out how UV mapping is actually done
                }
            )
            points
        top = List.map 
            ( \ (x,y) -> 
                { position = Point3d.centimeters x y height
                , uv = (0,0) -- Temporary until we can figure out how UV mapping is actually done
                }
            )
            points
    in
        TriangularMesh.strip top bottom
          |> Mesh.texturedFacets

polyConeMesh : List (Float,Float) -> (Float,Float,Float) -> Mesh.Textured WorldCoordinates
polyConeMesh points (xtip,ytip,ztip) =
    let
        tip =
            { position = Point3d.centimeters xtip ytip ztip
            , uv = (0,0) -- Temporary for now
            }
        apron = List.map 
            ( \ (x,y) -> 
                { position = Point3d.centimeters x y 0
                , uv = (0,0) -- Temp
                }
            )
            points
    in
        TriangularMesh.fan tip apron
          |> Mesh.texturedFacets

-- These probably aren't needed anymore
-- defaultEllipsoid : Mesh.Textured WorldCoordinates
-- defaultEllipsoid = ellipsoidMesh 2 1 1

-- defaultEllipsoidShadow : Mesh.Shadow WorldCoordinates
-- defaultEllipsoidShadow = Mesh.shadow defaultEllipsoid

type alias GeneratedMesh = 
    { name : String
    , mesh : Mesh.Textured WorldCoordinates
    , shadow : Mesh.Shadow WorldCoordinates
    }

generateEllipsoid : Float -> Float -> Float -> GeneratedMesh
generateEllipsoid length width height = 
    { name = "ellipsoid" ++ String.fromFloat length ++ String.fromFloat width ++ String.fromFloat height
    , mesh = ellipsoidMesh length width height
    , shadow = Mesh.shadow (ellipsoidMesh length width height)
    }

generatePolyCylinder : List (Float,Float) -> Float -> GeneratedMesh
generatePolyCylinder points height = 
    { name = "polyCylinder" ++ String.fromInt (List.length points) ++ String.fromFloat height
    , mesh = polyCylinderMesh points height
    , shadow = Mesh.shadow (polyCylinderMesh points height)
    }

generatePolyCone : List (Float,Float) -> (Float,Float,Float) -> GeneratedMesh
generatePolyCone points (xtip,ytip,ztip) = 
    { name = "polyCone" ++ String.fromInt (List.length points) ++ String.fromFloat xtip ++ String.fromFloat ytip ++ String.fromFloat ztip
    , mesh = polyConeMesh points (xtip,ytip,ztip)
    , shadow = Mesh.shadow (polyConeMesh points (xtip,ytip,ztip))
    }