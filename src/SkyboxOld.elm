module SkyboxOld exposing (main)

{-| This example illustrates creating a fully textured physically-based object,
with textures controlling color, roughness and metallicness. It also includes
some logic for rotating an object in the scene (as opposed to orbiting the
camera).
-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Array
import Block3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Length exposing (Meters)
import LuminousFlux
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture
import Duration exposing (Duration)


type WorldCoordinates
    = WorldCoordinates

type Model
    = Loading
        { colorTexture : Maybe (Material.Texture Color)
        , roughnessTexture : Maybe (Material.Texture Float)
        , metallicTexture : Maybe (Material.Texture Float)
        }
    | Loaded
        { colorTexture : Material.Texture Color
        , roughnessTexture : Material.Texture Float
        , metallicTexture : Material.Texture Float
        , time : Float 
        , azimuth : Angle
        , elevation : Angle
        , orbiting : Bool
        }
    | Errored String


type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | GotMetallicTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | MouseDown
    | MouseUp
    | MouseMove Float Float
    | Tick Duration

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

makeAxis : Float -> Float -> Float -> Axis3d.Axis3d Meters coords
makeAxis x y z = Axis3d.through Point3d.origin (getDirection x y z)

-- Rotate an entity around an axis. I recommend using makeAxis above to create the axis of rotation.
rotate : Axis3d.Axis3d Meters coords -> Float -> Entity.Entity coords -> Entity.Entity coords
rotate axis angle entity = Entity.rotateAround axis (Angle.radians angle) entity


skybox : Material.Textured WorldCoordinates -> Float -> Scene3d.Entity WorldCoordinates
skybox material size =
    let
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.centimeters -size

        positive =
            Length.centimeters size

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces with different colors
        bottom =
            Scene3d.quad material p1 p2 p3 p4

        top =
            Scene3d.quad material p5 p6 p7 p8

        front =
            Scene3d.quad material p2 p3 p7 p6

        back =
            Scene3d.quad material p1 p4 p8 p5

        left =
            Scene3d.quad material p1 p2 p6 p5

        right =
            Scene3d.quad material p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]

init : ( Model, Cmd Msg )
init =
    ( Loading
        { colorTexture = Nothing
        , roughnessTexture = Nothing
        , metallicTexture = Nothing
        }
    , Cmd.batch
        [ Material.load "skybox.jpg"
            |> Task.attempt GotColorTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_rgh.jpg"
            |> Task.attempt GotRoughnessTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_met.jpg"
            |> Task.attempt GotMetallicTexture
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        updatedModel =
            case model of
                Loading textures ->
                    case message of
                        GotColorTexture (Ok colorTexture) ->
                            checkIfLoaded { textures | colorTexture = Just colorTexture }

                        GotRoughnessTexture (Ok roughnessTexture) ->
                            checkIfLoaded { textures | roughnessTexture = Just roughnessTexture }

                        GotMetallicTexture (Ok metallicTexture) ->
                            checkIfLoaded { textures | metallicTexture = Just metallicTexture }

                        GotColorTexture (Err _) ->
                            Errored "Error loading color texture"

                        GotRoughnessTexture (Err _) ->
                            Errored "Error loading roughness texture"

                        GotMetallicTexture (Err _) ->
                            Errored "Error loading metallic texture"

                        MouseDown ->
                            model

                        MouseUp ->
                            model

                        MouseMove _ _ ->
                            model

                        Tick _ ->
                            model

                Loaded loadedModel ->
                    case message of
                        Tick t -> 
                            let
                                tickRate = 
                                    Duration.milliseconds 1 |> Quantity.per Duration.second
                                
                                updatedTime = 
                                    Duration.seconds loadedModel.time |> Quantity.plus (tickRate |> Quantity.for t)
                            in
                                Loaded { loadedModel | time = Duration.inSeconds updatedTime }

                        GotColorTexture _ ->
                            model

                        GotRoughnessTexture _ ->
                            model

                        GotMetallicTexture _ ->
                            model

                        MouseDown ->
                            Loaded { loadedModel | orbiting = True }

                        MouseUp ->
                            Loaded { loadedModel | orbiting = False }

                        MouseMove dx dy ->
                            if loadedModel.orbiting then
                                let
                                    -- rotationRate =
                                    --     Angle.degrees 1 |> Quantity.per Pixels.pixel

                                    -- -- Here we figure out what axis to rotate the sphere around,
                                    -- -- based on the drag direction. For example, if we drag
                                    -- -- vertically, then we want to rotate around a horizontal axis,
                                    -- -- and if we drag horizontally then we want to rotate around a
                                    -- -- vertical axis. 'Horizontal' and 'vertical' here should be
                                    -- -- with respect to the view plane, not the global coordinate
                                    -- -- system, so we use the X and Y directions of the viewpoint.
                                    -- rotationVector =
                                    --     Vector3d.withLength (dx |> Quantity.at rotationRate)
                                    --         (Viewpoint3d.yDirection viewpoint)
                                    --         |> Vector3d.plus
                                    --             (Vector3d.withLength (dy |> Quantity.at rotationRate)
                                    --                 (Viewpoint3d.xDirection viewpoint)
                                    --             )
                                    rotation numPixels =
                                        Angle.degrees (0.25 * numPixels)

                                    newAzimuth =
                                        loadedModel.azimuth |> Quantity.minus (rotation dx)

                                    newElevation =
                                        loadedModel.elevation
                                            |> Quantity.plus (rotation dy)
                                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                                in
                                Loaded { loadedModel | azimuth = newAzimuth, elevation = newElevation }

                            else
                                model

                Errored _ ->
                    model
    in
    ( updatedModel, Cmd.none )


{-| Every time a texture gets returned from an HTTP request, one of the Maybe
fields in the record below gets set to 'Just texture'. Every time that happens,
we use this function to check if _all_ of the textures have been loaded. If so,
we can transition into the Loaded state; otherwise we stay in the Loading state
and wait for the remaining textures to load.
-}
checkIfLoaded :
    { colorTexture : Maybe (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , metallicTexture : Maybe (Material.Texture Float)
    }
    -> Model
checkIfLoaded textures =
    case ( textures.colorTexture, textures.roughnessTexture, textures.metallicTexture ) of
        ( Just colorTexture, Just roughnessTexture, Just metallicTexture ) ->
            Loaded
                { colorTexture = colorTexture
                , roughnessTexture = roughnessTexture
                , metallicTexture = metallicTexture
                , orbiting = False
                , time = 0
                , azimuth = Angle.degrees 45
                , elevation = Angle.degrees 40

                -- -- Start with the sphere coordinate system aligned with the
                -- -- world coordinate system
                -- , sphereFrame = Frame3d.atOrigin
                }

        _ ->
            Loading textures

sunlight : Light WorldCoordinates Bool
sunlight =
    Light.directional (Light.castsShadows False)
        { chromaticity = Light.sunlight
        , intensity = Illuminance.lux 20000
        , direction = Direction3d.yz (Angle.degrees -120)
        }


sky : Light WorldCoordinates Never
sky =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , intensity = Illuminance.lux 7000
        , chromaticity = Light.skylight
        }


environment : Light WorldCoordinates Never
environment =
    Light.overhead
        { upDirection = Direction3d.negativeZ
        , intensity = Illuminance.lux 5000
        , chromaticity = Light.daylight
        }


camera : Model -> Camera3d Meters WorldCoordinates
camera model =
    -- Create a perspective camera based on the current azimuth/elevation from
    -- the model
    case model of
        Loaded loadedModel -> 
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 -20
                        , azimuth = loadedModel.azimuth
                        , elevation = loadedModel.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        _ ->
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 -20
                        , azimuth = (Angle.degrees 0)
                        , elevation = (Angle.degrees 0)
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }


-- {-| Define the sphere in its own coordinate system
-- -}
-- sphere : Sphere3d Meters SphereCoordinates
-- sphere =
--     Sphere3d.withRadius (Length.centimeters 5) Point3d.origin


view : Model -> Html Msg
view model =
    case model of
        Loaded loadedModel ->
            let
                -- Create a fully textured PBR material from the three loaded
                -- textures. Note that you can also use Material.constant if you
                -- want to use a constant value for one or two of the parameters
                -- instead of an actual texture.
                material =
                    Material.texturedPbr
                        { baseColor = loadedModel.colorTexture
                        , roughness = Material.constant 0.0
                        , metallic = Material.constant 0.0
                        }
            in
            Html.div []
                [ Html.div [Html.Events.onMouseDown MouseDown] 
                    [ Scene3d.custom
                        { camera = camera (Loaded loadedModel)
                        , clipDepth = Length.meters 0.1
                        , dimensions = ( Pixels.pixels 1024, Pixels.pixels 768 )
                        , antialiasing = Scene3d.multisampling
                        , lights = Scene3d.threeLights sunlight sky environment
                        , exposure = Scene3d.exposureValue 11
                        , toneMapping = Scene3d.noToneMapping
                        , whiteBalance = Light.daylight
                        , background = Scene3d.transparentBackground
                        , entities =
                            [ floor
                            , skybox material 512
                            ]
                        } ] ]

        Loading _ ->
            Html.text "Loading..."

        Errored message ->
            Html.text message


{-| Decode mouse movement just like in OrbitingCamera example
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading _ ->
            Sub.none

        Errored _ ->
            Sub.none

        Loaded { orbiting } ->
            if orbiting then
                Sub.batch
                    [ Browser.Events.onMouseMove decodeMouseMove
                    , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                    , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)
                    ]

            else
                Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
