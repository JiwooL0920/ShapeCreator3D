module ControlCharacter exposing (main)
{- Goal
    - Create a character that can move 
    around by pressing keyboard key 'a', 's', 'd' and 'w'.
    - The camera should always focus on the character and its
    location should associate with the character while it is moving 
-}
import Angle as Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Dom
import Browser.Events as BrowserE
import Character
import Camera3d
import Color
import Char
import Direction3d
import Frame3d
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LineSegment3d
import LuminousFlux
import Illuminance
import Quantity exposing (Quantity)
import Pixels exposing (Pixels)
import Point3d
import Task
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import String
import Vector3d
import Viewpoint3d

type WorldCoordinates
    = WorldCoordinates

type alias Model = {
    width : Quantity Float Pixels -- Width of the browser window
    , height : Quantity Float Pixels -- Height of the browser window
    , cPosition : Vector3d.Vector3d Meters WorldCoordinates -- Current position of the character
    , cAngle : Angle -- Current angle that the character is facing
    , lAngel : Angle -- Angle of the leg of the character
    , lMove : Bool -- Control the leg of the character
  }

type Msg 
  = Resize (Quantity Float Pixels) (Quantity Float Pixels) 
  | CharacterKey String
  | ControlKey String

-- Listen to two events. 1. key press. 2. key up
-- Use an additional  string "Press"/"Release" to distinguish the event
keyDecoderP : Decode.Decoder Msg
keyDecoderP =
  Decode.map (tokey "Press") (Decode.field "key" Decode.string)

keyDecoderU : Decode.Decoder Msg
keyDecoderU = 
  Decode.map (tokey "Release") (Decode.field "key" Decode.string)

tokey : String -> String -> Msg
tokey state keyValue =
    case String.uncons(keyValue) of 
       Just ( char, "" ) ->
            CharacterKey ((String.fromChar (Char.toLower char)) ++ state)
       _ ->
            ControlKey keyValue

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch
  [
    BrowserE.onKeyPress keyDecoderP
    , BrowserE.onKeyUp keyDecoderU
  ]


init : () -> ( Model, Cmd Msg )
init () =
  -- create the initial point of the character 
  (   
      {
        width = Quantity.zero
      , height = Quantity.zero
      , cPosition = Vector3d.zero -- initial characer position 0 , 0, 0
      , lAngel = Angle.degrees 0 
      , cAngle = Angle.degrees 0 -- initial facing yaxis
      , lMove = True
      }
      , Task.perform
        (\{ viewport } ->
            Resize
                (Pixels.pixels viewport.width)
                (Pixels.pixels viewport.height)
        )
        Browser.Dom.getViewport
  )
main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

update :  Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

    CharacterKey "dPress" ->
      (
        -- move +yaxais, leg rotate toward x direction
        { model 
           | cPosition = Vector3d.plus (Vector3d.meters 0.0 0.2 0) model.cPosition
           , cAngle = Angle.degrees 0
           , lAngel = Angle.degrees 15
           , lMove = not (model.lMove)
        }
        , Cmd.none
      )
    CharacterKey "dRelease" ->
      (
        {model | lAngel = Angle.degrees 0}
       , Cmd.none
      )
    CharacterKey "sPress" ->
      (
        -- move +xaxais, leg rotate toward y direction
        { model 
           | cPosition = Vector3d.plus (Vector3d.meters 0.2 0.0 0) model.cPosition
           , cAngle = Angle.degrees -90
           , lAngel = Angle.degrees 15
           , lMove = not (model.lMove)
        }
        , Cmd.none
      )
    CharacterKey "sRelease" ->
      (
        {model | lAngel = Angle.degrees 0}
       , Cmd.none
      )
    CharacterKey "aPress" ->
      (
        -- move -yaxais, leg rotate toward x direction
        { model 
           | cPosition = Vector3d.plus (Vector3d.meters 0.0 -0.2 0) model.cPosition
           , cAngle = Angle.degrees -180
           , lAngel = Angle.degrees 15
           , lMove = not (model.lMove)
        }
        , Cmd.none
      )
    CharacterKey "aRelease" ->
      (
        {model | lAngel = Angle.degrees 0}
       , Cmd.none
      )
    CharacterKey "wPress" ->
      (
        -- move -xaxais, leg rotate toward y direction
        { model 
           | cPosition = Vector3d.plus (Vector3d.meters -0.2 -0.0 0) model.cPosition
           , cAngle = Angle.degrees -270
           , lAngel = Angle.degrees 15
           , lMove = not (model.lMove)
        }
        , Cmd.none
      )
    CharacterKey "wRelease" ->
      (
        {model | lAngel = Angle.degrees 0}
       , Cmd.none
      )
    _ ->
      (model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    let
 -- Locate the character's current position and the direction it faced
        cPoint3d = Point3d.xyz (Vector3d.xComponent  model.cPosition) (Vector3d.yComponent  model.cPosition) (Vector3d.zComponent  model.cPosition)
        cFace = Axis3d.through cPoint3d Direction3d.z

  -- Create a camera using perspective projection
  -- The eye of the camera is relative to the position of the character
  -- The eye of the camera will always be "(Frame3d.atPoint (Point3d.meters -4 -2 -2))"
  -- away from the character
        eyePFromC = Point3d.relativeTo (Frame3d.atPoint (Point3d.meters -4 -2 -2)) cPoint3d
          
        camera =
            Camera3d.perspective
                { -- Camera is at the local point (4, 2, 2), looking at the point
                  -- cPoint3d where the character is currently located, 
                  -- oriented so that positive Z appears up
                  viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = cPoint3d
                        , eyePoint = eyePFromC -- Point3d.meters 4 2 2 -- x y z
                        , upDirection = Direction3d.positiveZ
                        }
                -- The image on the screen will have a total rendered 'height'
                -- of 30 degrees; small angles make the camera act more like a
                -- telescope and large numbers make it act more like a fisheye
                -- lens
                , verticalFieldOfView = Angle.degrees 30
                }
        -- Create a point light representing an incandescent (tungsten) light
        -- bulb, and specify that it should cast shadows. Only up to four lights
        -- in a given scene can cast shadows, and casting shadows is relatively
        -- expensive, so try to limit the number of shadows and shadow-casting
        -- lights in a given scene.
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
        rotateCharacter = Axis3d.through (Point3d.meters 0 0 0) Direction3d.z
    in
    { title = "ControlCharacter"
    , body =[ Scene3d.custom
       {
           entities = [
                Character.character {lMove = model.lMove, lAngel = model.lAngel}
                  |> Scene3d.rotateAround rotateCharacter (Angle.degrees 90)
                  |> Scene3d.translateBy model.cPosition
                  |> Scene3d.rotateAround cFace model.cAngle

       {--      charactor model
                |> Scene3d.translateBy model.cPosition
                |> Scene3d.rotateAround cFace model.cAngle --}
            ,flower 2 0 0 flowerMaterial1
            ,flower -2 0 0 flowerMaterial2
            ,flower 0 2 0 flowerMaterial3
            ,flower 0 -2 0 flowerMaterial4
          ]
           , camera = camera
           , clipDepth = Length.meters 1
           , background = Scene3d.transparentBackground
           , dimensions = ( model.width, model.height )
           , lights = Scene3d.twoLights lightBulb softLighting
           , exposure = Scene3d.exposureValue 5
           , whiteBalance = Light.incandescent
           , antialiasing = Scene3d.multisampling
           , toneMapping = Scene3d.noToneMapping
       }
      ]
    }

flowerMaterial1 = Material.color Color.lightBlue
flowerMaterial2 = Material.color Color.yellow
flowerMaterial3 = Material.color Color.brown
flowerMaterial4 = Material.color Color.lightPurple
flower a b c metrial = Scene3d.sphere metrial 
           <|Sphere3d.withRadius (Length.centimeters 20) (Point3d.meters a b c)