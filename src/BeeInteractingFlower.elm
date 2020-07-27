module BeeInteractingFlower exposing (main)

{--
The goal of collision:
 The goal of collision is to achieve the collision detection between objects.
 It is now doing the collision detection by using the function, intersects,
 in the library BoundingBox3d.
 It is a broad and rough way the do the detection by using the bounding box
 because there might be lots of spacing around the object.
 A way to increase the accuracy is using more than one bounding boxes for 
 an object. Ex : https://www.oreilly.com/library/view/learning-xna-40/9781449397210/httpatomoreillycomsourceoreillyimages737413.png.jpg
--}
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Browser
import Browser.Dom
import Browser.Events
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d
import Color exposing (Color)
import Character
import Cylinder3d
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
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
import SolidAngle
import Skybox
import Sphere3d
import Shape exposing (Dimension,
   Axis, cube, prism, sphere, cone, cylinder
   ,ringMesh, forest, tree, flower, garden
   ,bee
   ,myMat, myTexturedMat
   , cloudGroup)
import Task
import Temperature
import Vector3d exposing (Vector3d)
import Viewpoint3d
import Point2d
import TriangularMesh
import LineSegment3d
import WebGL.Texture

-- testing
import Block3d
type WorldCoordinates
    = WorldCoordinates

type Direction
    = Up
    | Down
    | Left
    | Right
    | Forward
    | Backward
    | FlyAround
    | None

speed : Float
speed = 1

maxVelocity : Float
maxVelocity = speed * 2

minVelocity : Float
minVelocity = (-speed) * 2

rotSpeed : Float
rotSpeed = 2

hitSignDisappear : Length.Length
hitSignDisappear = Length.centimeters 100

flyStop : Int
flyStop = 2

-- The type record the id of every object
--  Should id be type or String?
type ObjectsId
  = Tree
  | Flower
  | Cube
  | Sphere

type alias Model =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , beePos : Point3d Meters WorldCoordinates
    , prePos : Point3d Meters WorldCoordinates
    , camPos : Point3d Meters WorldCoordinates
    , beeRot : Angle -- Can probably just be replaced with azimuth
    , velocity : Vector3d Meters WorldCoordinates
    , rotVelocity : Float
    , dirLR : Direction
    , dirFB : Direction
    , dirUD : Direction
    , textures : Dict String (Material.Texture Color)
    --- MapObjects and bee now are the objects that contain shape and boundingBox
    , mapObjects : {shape : Entity WorldCoordinates
                  , boundingBox : List (BoundingBox3d Meters WorldCoordinates)
                  , id : List String
                  }
    , bee : Float -> {shape : Entity WorldCoordinates
                  , boundingBox : BoundingBox3d Meters WorldCoordinates}
    --- Hitsign
    , hit : { showHitSign : Bool
            , hitBBPos : Point3d Meters WorldCoordinates
            }
    , flyAround : Bool
    , flyCountDown : Int
    }


type Msg
    = Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | KeyDown String
    | KeyUp String
    | LoadTexture (Dict String (Material.Texture Color))
    | Error WebGL.Texture.Error

debugs : List (Entity WorldCoordinates)
debugs =
    [ Scene3d.lineSegment (Material.color Color.darkRed)
        (LineSegment3d.along Axis3d.x (Length.centimeters 0) (Length.centimeters 5000))
    , Scene3d.lineSegment (Material.color Color.darkGreen)
        (LineSegment3d.along Axis3d.y (Length.centimeters 0) (Length.centimeters 5000))
    , Scene3d.lineSegment (Material.color Color.darkBlue)
        (LineSegment3d.along Axis3d.z (Length.centimeters 0) (Length.centimeters 5000))
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
      , azimuth = Angle.degrees -90
      , elevation = Angle.degrees 30
      , beePos = Point3d.origin
      , prePos = Point3d.origin
      , camPos = Point3d.origin
      , beeRot = Angle.degrees 0
      , velocity = Vector3d.zero
      , rotVelocity = 0
      , dirFB = None
      , dirLR = None
      , dirUD = None
      , textures = Dict.empty 
      , mapObjects = -- Downside of doing this is that animating the trees moving becomes somewhat more difficult (Right now they just stay still)
            let
                t = pi



                cubeShape = Shape.cubeSAB Color.red 50 
                         |> Shape.translateBy (Vector3d.centimeters 0 1000 50)
                sphereShape = Shape.sphereSAB Color.blue 40
                         |> Shape.translateBy (Vector3d.centimeters 550 -1000 40)
                -- Character
                forestShape = Shape.forestSAB (2*sin t + 20)
                gardenShape = Shape.gardenSAB
            in
                { shape = Scene3d.group [cubeShape.shape, sphereShape.shape
                    , forestShape.shape, gardenShape.shape]

                  ,boundingBox = [cubeShape.boundingBox, sphereShape.boundingBox]
                      ++ gardenShape.boundingBox 
                      ++ forestShape.boundingBox
                  ,id = ["Cube", "Sphere"]
                      ++ (buildId "Flower" gardenShape.boundingBox)
                      ++ (buildId "Tree" forestShape.boundingBox)
                }
        , bee = Shape.beeSAB 
        , hit = { showHitSign = False 
                , hitBBPos = Point3d.origin
                } 
        , flyAround = False
        , flyCountDown = 0
      }
      , Cmd.batch 
        [
            Task.perform
            (\{ viewport } ->
                Resize
                    (Pixels.pixels viewport.width)
                    (Pixels.pixels viewport.height)
            )
            Browser.Dom.getViewport
        ,   fetchTextures
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

                -- Bee moves by adding different vectors to its velocity depending on which keys are being pressed
                -- This allows for movement in more than 1 direction at a time
                -- We calculate the new position of the bee and check will the
                -- bee collide with other objects. Only update the new position
                -- of the bee, if the bee will not collide with other objects.
                tryToUpdateVelocity = 
                    model.velocity 
                      |> Vector3d.plus (if not (model.dirFB == None) then Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirFB) else Vector3d.zero)
                      |> Vector3d.plus (if not (model.dirLR == None) then Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirLR) else Vector3d.zero)
                      |> Vector3d.plus (if not (model.dirUD == None) then
                          if model.dirUD == Down && hitGround then 
                              Vector3d.zero 
                          else Vector3d.withLength (Length.centimeters speed) (directionConverter model model.dirUD)
                        else Vector3d.zero)
                      |> Vector3d.minus (Vector3d.scaleBy 0.1 model.velocity)
                
                collisionVelocity =
                  Vector3d.zero
                    |> Vector3d.minus (if not (model.dirFB == None) then 
                       (Vector3d.scaleBy 3
                          (Vector3d.withLength 
                             (Length.centimeters speed) 
                             (directionConverter model model.dirFB)))
                      else Vector3d.zero)
                    |> Vector3d.minus (if not (model.dirLR == None) then
                      (Vector3d.scaleBy 3
                        (Vector3d.withLength 
                          (Length.centimeters speed) 
                          (directionConverter model model.dirLR)))
                      else Vector3d.zero)
                    |> Vector3d.minus (if not (model.dirUD == None) then
                          if model.dirUD == Down && hitGround then 
                              Vector3d.zero 
                          else
                          (Vector3d.scaleBy 3
                            (Vector3d.withLength 
                              (Length.centimeters speed) 
                              (directionConverter model model.dirUD)))
                      else Vector3d.zero) 
                    |> Vector3d.minus (Vector3d.scaleBy 0.1 model.velocity)
                    
                tryUpdateBeePos = 
                    if goingDown && hitGround then 
                        let 
                            x = Vector3d.xComponent model.velocity

                            y = Vector3d.yComponent model.velocity
                            
                            velocityNoZ = Vector3d.xyz x y (Length.centimeters 0) 
                        in
                            model.beePos
                              |> Point3d.translateBy velocityNoZ
                
                    else 
                        model.beePos 
                          |> Point3d.translateBy model.velocity

                
                tryUpdateBeeBB = 
                  model.bee 0
                    |> Shape.rotateAround Axis3d.z newBeeRot
                    |> Shape.translateBy (Vector3d.from Point3d.origin tryUpdateBeePos)
                    |> Shape.translateBy (Vector3d.withLength (Length.centimeters (5*sin (timeAsNum))) Direction3d.z)

                --Collision detection
                --ifCollided == True when the collision happens
                ifCollided = 
                  List.any (\elm -> elm == True) 
                    (
                      List.map (BoundingBox3d.intersects tryUpdateBeeBB.boundingBox)
                         model.mapObjects.boundingBox
                    )
                        

                newBeePos = 
                    if ifCollided then model.prePos
                   else tryUpdateBeePos
                     
                newVelocity = 
                   if ifCollided then collisionVelocity
                   else tryToUpdateVelocity
                
                newBeeRot = model.beeRot |> Quantity.plus (Angle.degrees model.rotVelocity)

                goingDown = not (Quantity.greaterThanOrEqualTo (Length.centimeters 0) (Vector3d.zComponent model.velocity))

                hitGround = not (Quantity.greaterThanOrEqualTo (Length.centimeters 0) (Point3d.zCoordinate model.beePos))

                --- Decide whether or not show the hit sign
                getIdifCollided =
                  List.filter (\(id , box, tf) -> 
                     if (tf == True) then
                        True
                     else False
                     )
                    <| List.map3 (\id box tf -> (id , box, tf)) 
                        model.mapObjects.id
                        model.mapObjects.boundingBox
                          <| List.map (BoundingBox3d.intersects tryUpdateBeeBB.boundingBox)
                                model.mapObjects.boundingBox

                ifHitFlower = 
                  if ifCollided then 
                    List.foldr 
                      (\ (id , box, tf) hitRecord -> 
                        if (id == "Flower") then {
                          showHitSign = True
                          , hitBBPos = BoundingBox3d.centerPoint box
                         }
                          else { 
                          showHitSign = False
                          , hitBBPos = Point3d.origin
                         })
                          model.hit getIdifCollided
                    
                  else 
                    -- Decide whether nor not the bee is too far from the
                    -- flower. If the bee is flying away from the flower
                    -- the the hitsign disappear.
                    if ( 
                         (Point3d.distanceFrom newBeePos model.hit.hitBBPos)
                         --(Length.centimeters 10)
                         |> Quantity.greaterThan hitSignDisappear ) 
                         then
                         {
                          showHitSign = False
                          , hitBBPos = Point3d.origin
                         }
                    else
                       {
                        showHitSign = model.hit.showHitSign
                        , hitBBPos = model.hit.hitBBPos
                       }
                newFlyCountDown = model.flyCountDown + 1
            in
                (   { model 
                    | time = timeAsNum
                    , camPos =  newBeePos |> Point3d.translateIn Direction3d.z (Length.centimeters 30)
                    , velocity = newVelocity
                    , prePos = model.beePos
                    , beePos = newBeePos
                    , beeRot = newBeeRot
                    , hit = {showHitSign = ifHitFlower.showHitSign
                             , hitBBPos = ifHitFlower.hitBBPos }
                    , flyCountDown = newFlyCountDown
                    }
                , Cmd.none )
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
                            |> Quantity.clamp (Angle.degrees -5) (Angle.degrees 85)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , beeRot = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        KeyDown key ->
            let
                dir = toDirection key
            in
                case dir of

                    Left ->
                        ( { model | dirLR = Left
                        ,flyAround = False }, Cmd.none )

                    Right ->
                        ( { model | dirLR = Right
                        ,flyAround = False }, Cmd.none )

                    Forward ->
                        ( { model | dirFB = Forward 
                        ,flyAround = False}, Cmd.none )

                    Backward ->
                        ( { model | dirFB = Backward 
                        ,flyAround = False}, Cmd.none )

                    Up ->
                        ( { model | dirUD = Up 
                        ,flyAround = False}, Cmd.none )

                    Down ->
                            ( { model | dirUD = Down 
                            ,flyAround = False}, Cmd.none )
                    
                    FlyAround -> 
                      if model.hit.showHitSign && 
                        (not (model.flyAround)) then
                        ( {model | 
                           flyAround = True
                           , hit = {
                               showHitSign = False
                               , hitBBPos = model.hit.hitBBPos
                           }
                           , flyCountDown = 0
                           }, Cmd.none)
                      else ( {model | flyAround = False}, Cmd.none)


                    _ ->
                        ( model, Cmd.none )

        KeyUp key ->
            let
                dir = toDirection key

            in
                case dir of

                    Left ->
                        ( { model | dirLR = None }, Cmd.none )

                    Right ->
                        ( { model | dirLR = None }, Cmd.none )

                    Forward ->
                        ( { model | dirFB = None }, Cmd.none )

                    Backward ->
                        ( { model | dirFB = None }, Cmd.none )

                    Up ->
                        ( { model | dirUD = None }, Cmd.none )

                    Down ->
                        ( { model | dirUD = None }, Cmd.none )
                     
                    FlyAround -> if (model.flyAround == False) then
                            ( {model | flyAround = False}, Cmd.none)
                        else if 
                          (model.flyAround 
                            && (model.flyCountDown > flyStop) ) then
                            ( {model | flyAround = False}, Cmd.none)
                        else
                            ( {model | flyAround = model.flyAround}, Cmd.none)
                    _ ->
                        ( model, Cmd.none )

        LoadTexture textures -> 
            ( { model | textures = textures }, Cmd.none)

        Error err -> 
            ( model, Cmd.none)


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

        -- Listen for key presses
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder

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
    

        -- Rough approximation of sunlight near sunset
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
                (Point3d.centimeters -3000 -3000 0)
                (Point3d.centimeters 3000 -3000 0)
                (Point3d.centimeters 3000 3000 0)
                (Point3d.centimeters -3000 3000 0)



        -- Define camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = model.camPos
                        , azimuth = model.beeRot |> Quantity.plus (Angle.degrees 90)
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        texturesList = [Dict.get "skyB" model.textures,
                             Dict.get "skyT" model.textures,
                             Dict.get "skyS1" model.textures,
                             Dict.get "skyS2" model.textures,
                             Dict.get "skyS3" model.textures,
                             Dict.get "skyS4" model.textures]
        beeShape = if model.flyAround then
            model.bee model.time
              |> Shape.rotateAround Axis3d.z model.beeRot
              --|> Shape.translateBy (Vector3d.from Point3d.origin model.beePos)
              |> Shape.translateBy (Vector3d.withLength (Length.centimeters (5*sin (model.time))) Direction3d.z)
              |> Shape.translateBy (Vector3d.xyz  
                      (Point3d.xCoordinate model.hit.hitBBPos)
                      (Point3d.yCoordinate model.hit.hitBBPos)
                      (Point3d.zCoordinate model.hit.hitBBPos)
                    )
              |> Shape.translateBy (Vector3d.from Point3d.origin
                (Point3d.centimeters (40*sin (degrees (90*model.time))) (40*cos (degrees (90*model.time))) 0))
          else
            model.bee model.time
              |> Shape.rotateAround Axis3d.z model.beeRot
              |> Shape.translateBy (Vector3d.from Point3d.origin model.beePos)
              |> Shape.translateBy (Vector3d.withLength (Length.centimeters (5*sin (model.time))) Direction3d.z)

        hitsign = Shape.hitsign (Dict.get "cButton" model.textures)
                    |> Scene3d.rotateAround Axis3d.z model.beeRot
                    |> Scene3d.translateBy (Vector3d.from Point3d.origin model.beePos)
                    |> Scene3d.translateBy (Vector3d.withLength (Length.centimeters (5*sin (model.time))) Direction3d.z)
                   
                   

       -- Uncommand the following code to show the bounding  box of the bee
       {-- bbbox =  Scene3d.blockWithShadow (Material.color Color.lightBlue) <|
          Block3d.with
            {
              x1 = BoundingBox3d.minX beeShape.boundingBox 
            , y1 = BoundingBox3d.minY beeShape.boundingBox 
            , z1 = BoundingBox3d.minZ beeShape.boundingBox 
            , x2 = BoundingBox3d.maxX beeShape.boundingBox 
            , y2 = BoundingBox3d.maxY beeShape.boundingBox 
            , z2 = BoundingBox3d.maxZ beeShape.boundingBox 
            } --}

    in
    Scene3d.custom
        { lights = Scene3d.threeLights firstLight thirdLight softLighting
        , camera = camera
        , clipDepth = Length.centimeters 10
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( Pixels.int (round (Pixels.toFloat model.width)), Pixels.int (round (Pixels.toFloat model.height)) )
        , background = Scene3d.backgroundColor (Color.lightBlue)
        , entities =  
              
                     [   plane
                        , firstLightBall
                        , beeShape.shape
                        , model.mapObjects.shape
                        , Skybox.skybox texturesList 10000
                        , if (model.hit.showHitSign) then hitsign
                          else Scene3d.nothing 
       -- Uncommand the following code to show the bounding  box of the bee
                        -- , bbbox
                        , cloudGroup |> Scene3d.translateBy (Vector3d.centimeters  0 (2000*sin (0.01*model.time)) 0)
                      ]
                      ++ debugs
        }

keyDecoder : Decoder Msg
keyDecoder =
    Decode.map KeyDown (Decode.field "key" Decode.string)

keyUpDecoder : Decoder Msg
keyUpDecoder =
    Decode.map KeyUp (Decode.field "key" Decode.string)

-- Converts a key "code" to a Direction
toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
      Left

    "a" ->
      Left

    "A" ->
      Left

    "ArrowRight" ->
      Right

    "d" ->
      Right

    "D" ->
      Right

    "ArrowUp" ->
      Forward

    "w" ->
      Forward

    "W" ->
      Forward

    "ArrowDown" ->
      Backward

    "s" ->
      Backward

    "S" ->
      Backward

    "c" ->
      FlyAround

    "C" ->
      FlyAround

    " " ->
      Up 

    "Shift" ->
      Down

    _ ->
      None

-- Converts a Direction to a Direction3d that is relative to the bee
directionConverter : Model -> Direction -> Direction3d coords
directionConverter model dir = 
    let
        forwardDir = Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 270)) (Angle.degrees 0)

        backwardDir = Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 90)) (Angle.degrees 0)

        rightDir = Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 180)) (Angle.degrees 0)

        leftDir = Direction3d.xyZ model.beeRot (Angle.degrees 0)

        upDir = Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 270)) (Angle.degrees 90)

        downDir = Direction3d.xyZ (model.beeRot |> Quantity.plus (Angle.degrees 270)) (Angle.degrees (-90))
    in
        case dir of
            Forward -> forwardDir

            Backward -> backwardDir

            Left -> leftDir

            Right -> rightDir

            Up -> upDir

            Down -> downDir

            _ -> downDir

buildId : String 
  -> List (BoundingBox3d Meters WorldCoordinates)
  -> List (String)
buildId idName objectList =
  List.map
    -- The function for creating the idName can be more
    -- custermize is the future as needed.
    (\object -> idName)
    objectList

textureBottom =
    "SkyboxTextures/gradientSkyBottom.png"
textureTop =
    "SkyboxTextures/gradientSkyTop.png"
textureSide1 =
    "SkyboxTextures/gradientSkySide.png"
textureSide2 =
    "SkyboxTextures/gradientSkySide.png"
textureSide3 =
    "SkyboxTextures/gradientSkySide.png"
textureSide4 =
    "SkyboxTextures/gradientSkySide.png"
textureHitButton = 
    "OtherTextures/cButton.png"

textDist : List (String , String)
textDist = [
   ("skyB", textureBottom),
   ("skyT", textureTop),
   ("skyS1", textureSide1),
   ("skyS2", textureSide2),
   ("skyS3", textureSide3),
   ("skyS4", textureSide4),
   ("cButton", textureHitButton)
  ]

-- Fetch the texture from the List, textureListSkyBox.
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox.skybox

fetchTextures : Cmd Msg
fetchTextures = 
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