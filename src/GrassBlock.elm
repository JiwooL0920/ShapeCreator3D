module GrassBlock exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d
import Color
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels exposing (Pixels)
import Point3d
import Block3d
import Vector3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Triangle3d
import Viewpoint3d
import WebGL.Texture
import Color exposing (Color)
import Task

import Scene3d.Light as Light

import LuminousFlux
import Illuminance
import Direction3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    , mesh1 : Scene3d.Entity WorldCoordinates -- Saved Mesh values for rendering
    , mesh2 : Mesh.Plain WorldCoordinates
    , textures : CubeMaterial
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | GotTextures (Result WebGL.Texture.Error (Material.Texture Color, Material.Texture Color, Material.Texture Color))

cube = (Scene3d.block
            (Material.matte Color.white) <|
            Block3d.with {
                x1 = Length.meters 0
            ,   x2 = Length.meters 1
            ,   y1 = Length.meters 0
            ,   y2 = Length.meters 1
            ,   z1 = Length.meters 0
            ,   z2 = Length.meters 1
            })
            |> Scene3d.translateBy (Vector3d.meters 0 0 -0.5)


init : () -> ( Model, Cmd Msg )
init () =
    -- Create a couple of Mesh values containing a single triangle each and
    -- store them in the model
    let
        mesh1 =
            cube

        mesh2 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)
                ]
    in
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , mesh1 = cube
      , mesh2 = mesh2
      , textures = CubeMaterial
                        (Material.matte Color.white)
                        (Material.matte Color.white)
                        (Material.matte Color.white)
                        (Material.matte Color.white)
                        (Material.matte Color.white)
                        (Material.matte Color.white)
      }
    , Task.attempt GotTextures <| Task.map3 (\a b c -> (a,b,c))
                                        (Material.load "https://cschank.github.io/img/top.png")
                                        (Material.load  "https://cschank.github.io/img/bottom.jpg")
                                        (Material.load  "https://cschank.github.io/img/side.png")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        GotTextures result ->
            case result of
                Ok (top, bottom, side) ->
                    ({ model | textures = { top = Material.texturedColor top
                                          , bottom = Material.texturedColor bottom
                                          , left = Material.texturedColor side
                                          , right = Material.texturedColor side
                                          , front = Material.texturedColor side
                                          , back = Material.texturedColor side
                                          } }, Cmd.none)
                error -> (Debug.log (Debug.toString error) model, Cmd.none)


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)

type alias CubeMaterial =
    {
        top: Material.Textured WorldCoordinates
    ,   bottom: Material.Textured WorldCoordinates
    ,   left: Material.Textured WorldCoordinates
    ,   right: Material.Textured WorldCoordinates
    ,   front: Material.Textured WorldCoordinates
    ,   back: Material.Textured WorldCoordinates
    }

minecraftBlock : CubeMaterial -> Float -> Scene3d.Entity WorldCoordinates
minecraftBlock material size =
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
            Scene3d.quad material.bottom p1 p2 p3 p4

        top =
            Scene3d.quad material.top p5 p6 p7 p8

        front =
            Scene3d.quad material.front p2 p3 p7 p6

        back =
            Scene3d.quad material.back p1 p4 p8 p5

        left =
            Scene3d.quad material.left p1 p2 p6 p5

        right =
            Scene3d.quad material.right p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]
        |> Scene3d.translateBy (Vector3d.centimeters size size 0)


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "OrbitingCamera"
    , body =
        let
            lightBulb =
                Light.point (Light.castsShadows True)
                    { position = Point3d.centimeters 50 -40 50
                    , chromaticity = Light.incandescent -- color of the light
                    , intensity = LuminousFlux.lumens 400 -- total light 'power'
                    }

            -- Create some soft lighting to fill in shadowed areas
            softLighting =
                Light.overhead
                    { upDirection = Direction3d.z
                    , chromaticity = Light.incandescent
                    , intensity = Illuminance.lux 50
                    }
        in
        [
            Scene3d.custom
                { entities =
                      -- Draw the two single-triangle meshes
                      [-- model.mesh1
                        minecraftBlock model.textures 50
                      ]
                , camera = camera
                , background = Scene3d.transparentBackground
                , clipDepth = Length.centimeters 1
                , dimensions = ( Pixels.pixels 400, Pixels.pixels 300 )

                -- Define the lights to use in the scene. elm-3d-scene only supports up
                -- to eight total lights, so there are different functions for different
                -- numbers of lights instead of a single function taking a list.
                , lights = Scene3d.twoLights lightBulb softLighting

                -- This is a reasonably typical exposure value for an interior home
                -- scene; see https://en.wikipedia.org/wiki/Exposure_value#Tabulated_exposure_values
                -- for some representative values for different types of scenes
                , exposure = Scene3d.exposureValue 5

                -- White balance specifies what color shows up as white in the rendered
                -- scene; this should usually be set to the dominant light color
                , whiteBalance = Light.incandescent

                -- When using Scene3d.custom, we have to explicitly specify what kind of
                -- antialiasing (if any) to use
                , antialiasing = Scene3d.multisampling

                -- Similarly, we have to specify what kind of tone mapping (if any) to
                -- use; see the ExposureAndToneMapping example for details
                , toneMapping = Scene3d.noToneMapping
                }

        --Scene3d.unlit
        --    { camera = camera
        --    , clipDepth = Length.meters 0.1
        --    , dimensions = ( Pixels.pixels 400, Pixels.pixels 300 )
        --    , background = Scene3d.transparentBackground
        --    , entities =
        --        -- Draw the two single-triangle meshes
        --        [ model.mesh1
        --        , Scene3d.mesh (Material.color Color.blue) model.mesh2
        --        ]
        --    }
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
