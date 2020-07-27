module HousePlusSkybox exposing (main)

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
import Scene3d.Material as Material
import Cylinder3d
import Triangle3d
import LineSegment3d
import WebGL.Texture
import Skybox



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
      -- Add an attribute for the texture
    }


type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | LoadTexture (List (Material.Texture Color))
    | Error WebGL.Texture.Error


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
      , textures = Nothing
      }
    , Cmd.batch 
       [
           Task.perform
            (\{ viewport } ->
               Resize
                (Pixels.pixels (round viewport.width))
                (Pixels.pixels (round viewport.height))
            )
            Browser.Dom.getViewport
        , fetchTextures
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
                            --|> Quantity.clamp (Angle.degrees 5) (Angle.degrees 85)
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
        LoadTexture textures -> 
            ( { model | textures = Just textures }, Cmd.none)

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
                    (Pixels.pixels width)
                    (Pixels.pixels height)
            )

        -- Subscribe to animation frames to animate the cubes
        , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)

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
        sphere =
            Sphere3d.atPoint properties.position (Length.millimeters 100)

        -- Calculate the luminance of the sphere surface by dividing the given
        -- total luminous flux of the light by the surface area of the sphere
        -- and by the solid angle of a hemisphere (assuming that each point on
        -- the surface of the bulb emits light equally in all directions)...I
        -- am not 100% sure this is exactly correct =)
        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea sphere)

        -- Create an emissive (glowing) material for the sphere
        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance
    in
    ( Light.point (Light.castsShadows True) properties
    , Scene3d.sphere sphereMaterial sphere
    )


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

leafMeshOld : Float -> Mesh.Uniform WorldCoordinates
leafMeshOld y = 
    let
        frontLeft =
            Point3d.centimeters 40 0 0

        frontRight =
            Point3d.centimeters 40 40 0

        backLeft =
            Point3d.centimeters 0 0 0

        backRight =
            Point3d.centimeters 0 40 0

        tip =
            Point3d.centimeters 20 y 70

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
    -- Create a elm-3d-scene Mesh value from the TriangularMesh; we use
    -- Mesh.indexedFacets so that normal vectors will be generated for each face
        Mesh.indexedFacets triangularMesh

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

parasolMesh : Mesh.Uniform WorldCoordinates
parasolMesh = 
    let
        frontLeft =
            Point3d.centimeters 1 0 0

        frontRight =
            Point3d.centimeters 1 40 0 

        backLeft =
            Point3d.centimeters 0 0 0 

        backRight =
            Point3d.centimeters 0 40 0

        tip =
            Point3d.centimeters 0.5 20 30



        -- Create a TriangularMesh value from an array of vertices and list
        -- of index triples defining faces (see https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/TriangularMesh#indexed)
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
    -- Create a elm-3d-scene Mesh value from the TriangularMesh; we use
    -- Mesh.indexedFacets so that normal vectors will be generated for each face
        Mesh.indexedFacets triangularMesh



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

        house = Scene3d.group [pyramidEntity, houseBody, door, windows, chimney]
            |> Scene3d.translateBy (Vector3d.centimeters -40 30 0)

      
        pyramidEntity =
            (Scene3d.mesh (Material.matte Color.darkRed) pyramidMesh)
            |> Scene3d.translateBy 
                (Vector3d.centimeters 0 0 30)

        houseBody =
            (Scene3d.block (Material.matte Color.white) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters 40
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters 60
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters 30
                    }  )
            |> Scene3d.translateBy (Vector3d.centimeters -20 -30 0) 

        door = Scene3d.group [doorframe, doornob]

        doorframe = 
            Scene3d.quad (Material.matte Color.darkBrown)
                (Point3d.centimeters 0 0 0)
                (Point3d.centimeters 0 0 20)
                (Point3d.centimeters 15 0 20)
                (Point3d.centimeters 15 0 0)
            |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
            |> Scene3d.translateBy (Vector3d.centimeters 20.3 -20 0)
        
        doornob = 
            (Scene3d.sphere (Material.metal { baseColor = Color.yellow, roughness = 0.1 }) <|
                Sphere3d.withRadius (Length.centimeters 1.5) Point3d.origin)
            |> Scene3d.translateBy 
                                    -- x  y  z
                (Vector3d.centimeters 20 -10 10)

        windows = Scene3d.group [window]

        window = 
            Scene3d.quad (Material.matte Color.darkGrey)
                (Point3d.centimeters 0 0 0)
                (Point3d.centimeters 0 0 18)
                (Point3d.centimeters 13 0 18)
                (Point3d.centimeters 13 0 0)
            |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
            |> Scene3d.rotateAround Axis3d.x (Angle.degrees 90)
            |> Scene3d.translateBy (Vector3d.centimeters 20.3 23 10)

        chimney =  (Scene3d.block (Material.matte Color.darkBrown) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters 10
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters 10
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters 30
                    }  )
            |> Scene3d.translateBy (Vector3d.centimeters 5 19.9 23) 


    
        clouds = Scene3d.group [
                                 cloud 
                                 |> Scene3d.translateBy (Vector3d.centimeters 0 -50 80)
                                 ,
                                 cloud 
                                 |> Scene3d.translateBy (Vector3d.centimeters 30 -10 70)
                                 ,
                                 cloud 
                                 |> Scene3d.translateBy (Vector3d.centimeters -35 20 75)
                               ]

        cloud = Scene3d.group [cloudPart
                                |> Scene3d.translateBy (Vector3d.centimeters 0 0 10)
                             , cloudPart
                                |> Scene3d.translateBy (Vector3d.centimeters 0 6 10)
                             , cloudPart
                                |> Scene3d.translateBy (Vector3d.centimeters 0 12 10)  
                             , cloudPart 
                                |> Scene3d.translateBy (Vector3d.centimeters 0 3 14)
                             , cloudPart 
                                |> Scene3d.translateBy (Vector3d.centimeters 0 9 14)
                             ]

        cloudPart = (Scene3d.sphere (Material.matte Color.white) <|
                Sphere3d.withRadius (Length.centimeters 5) Point3d.origin)
                    
            -- Scale
          --  |> Scene3d.scaleAbout (Point3d.centimeters 50 0 0) 2

        fences = Scene3d.group [backFence -70
                                    |> Scene3d.translateBy (Vector3d.centimeters -140 0 0)
                                , frontFence -70
                                , sideFence -70
                                , rightFence -70
                                , leftFence
                                , frontFenceHorizontal
                                , frontFenceHorizontal
                                    |> Scene3d.translateBy (Vector3d.centimeters 0 100 0)
                                , backFenceHorizontal
                                , sidefenceHorizontal
                                , sidefenceHorizontal
                                    |> Scene3d.translateBy (Vector3d.centimeters 0 150 0)
                                ]

        frontFence y = if y == 70 then Scene3d.group []
                       else if (-30 <= y && y <= 20) then Scene3d.group [frontFence (y+10)]
                       else Scene3d.group [fence y, frontFence (y+10)]


        backFence y = (if y == 70 then Scene3d.group []
                    else Scene3d.group [fence y, backFence (y+10)])

        rightFence y = if y == 70 then Scene3d.group []
                       else Scene3d.group [sideFence y, rightFence (y+10)]

        leftFence = rightFence -70 
                    |> Scene3d.translateBy (Vector3d.centimeters 0 -150 0)
            
        sideFence y = fence y 
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)

        fence y =  (Scene3d.block (Material.matte Color.darkBrown) <|
                Block3d.from
                    (Point3d.centimeters 0 0 0)
                    (Point3d.centimeters 1.5 3 13))
            |> Scene3d.translateBy (Vector3d.centimeters 70 y 0) 
        
        frontFenceHorizontal = (Scene3d.block (Material.matte Color.darkBrown) <|
                Block3d.from
                    (Point3d.centimeters 0 0 0)
                    (Point3d.centimeters 1.5 50 3))
            |> Scene3d.translateBy (Vector3d.centimeters 70 -80 5)
    
        backFenceHorizontal = (Scene3d.block (Material.matte Color.darkBrown) <|
                Block3d.from
                    (Point3d.centimeters 0 0 0)
                    (Point3d.centimeters 1.5 150 3))
            |> Scene3d.translateBy (Vector3d.centimeters -70 -78 5)

        sidefenceHorizontal = backFenceHorizontal 
                            |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
                            |> Scene3d.translateBy (Vector3d.centimeters 0 -10 0)

        tree y = Scene3d.group [  root
                              , leaves y 15 40
                              ]

        root =  (Scene3d.cylinder (Material.matte Color.brown) <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 80
                    , radius = Length.centimeters 5
                    } )
                |> Scene3d.translateBy (Vector3d.centimeters -30 -50 0)

        leaves y z col = 
            Scene3d.group 
                ( List.map
                    ( \ h -> Scene3d.mesh (Material.matte (Color.rgb 0 h 0)) (leafMesh y h)
                            |> Scene3d.translateBy (Vector3d.centimeters -50 -70 h)
                    )
                    [15,25,35]
                )

        leavesOld y z col = if (z == 45) then Scene3d.group []
                    else Scene3d.group [ leaf y z col, leavesOld y (z+10) (col+30)]
        leaf y z col =        
            (Scene3d.mesh (Material.matte (Color.rgb 0 z 0)) (leafMeshOld y))
            |> Scene3d.translateBy (Vector3d.centimeters -50 -70 z)

        -- flower n = if (n == 0) then Scene3d.group []
        --            else Scene3d.group [
        --                 myTriangle

        --            ]
                   
        -- myTriangle = Scene3d.mesh (Material.color Color.red) (Mesh.triangles [
        --                     Triangle3d.fromVertices
        --                         ( Point3d.centimeters 0 0 0
        --                         , Point3d.centimeters 0 10 0
        --                         , Point3d.centimeters 5 5 20
        --                         )
        --                ])
        --                                         |> Scene3d.rotateAround Axis3d.z (Angle.degrees 2)

        --                |> Scene3d.translateBy (Vector3d.centimeters 50 0 0)

        basketBallCourt = Scene3d.group [ basketBallGround
                                        , basketBallPole
                                        , backBoard
                                        , aim
                                    --    , rim
                                        , exampleCircle
                                        , exampleCircle1
                                        , basketBallRect
                                        , ring |> Scene3d.translateBy (Vector3d.centimeters 35 -65 44)
                                        ] 

        ring = Scene3d.mesh (Material.metal {baseColor=Color.rgb 10 10 10, roughness=0.5})
                <| ringMesh 6 0.5

        basketBallGround =  (Scene3d.block (Material.matte Color.lightGrey) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters 60
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters 80
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters 1
                    }  )
            |> Scene3d.translateBy (Vector3d.centimeters 5 -75 0) 

        basketBallPole = (Scene3d.cylinder (Material.matte Color.darkGrey) <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 60
                    , radius = Length.centimeters 1
                    } )
                |> Scene3d.translateBy (Vector3d.centimeters 35 -73 0)

        backBoard = (Scene3d.block (Material.matte Color.lightGrey) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters 30
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters 1
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters 20
                    }  )
            |> Scene3d.translateBy (Vector3d.centimeters 20 -73 40) 

        ball =  (Scene3d.sphere (Material.matte Color.orange) <|
                Sphere3d.withRadius (Length.centimeters 4) Point3d.origin)
            |> Scene3d.translateBy (Vector3d.centimeters 20 -50 5) 

        aim = Scene3d.group [ horizontalLine
                            , horizontalLine 
                                |> Scene3d.translateBy (Vector3d.centimeters -13 0 0)
                            , verticalLine
                            , verticalLine  
                                |> Scene3d.translateBy (Vector3d.centimeters 0 0 8)

                        ]

        horizontalLine = Scene3d.quad (Material.matte Color.red)
                (Point3d.centimeters 0 0 0)
                (Point3d.centimeters 0 0 10)
                (Point3d.centimeters 2 0 10)
                (Point3d.centimeters 2 0 0)
            |> Scene3d.translateBy (Vector3d.centimeters 40 -71.9 43)

        verticalLine = Scene3d.quad (Material.matte Color.red)
                (Point3d.centimeters 0 0 0)
                (Point3d.centimeters 0 0 2)
                (Point3d.centimeters 12 0 2)
                (Point3d.centimeters 12 0 0)
            |> Scene3d.translateBy (Vector3d.centimeters 28 -71.9 43)

        rim = (Scene3d.cylinder (Material.matte Color.orange) <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 2
                    , radius = Length.centimeters 7
                    } )
            |> Scene3d.translateBy (Vector3d.centimeters 34 -66 43) 

        

        basketBallRect = Scene3d.quad (Material.matte Color.red)
                (Point3d.centimeters 0 0 0) -- back left corner
                (Point3d.centimeters 0 44 0) --back right corner
                (Point3d.centimeters 30 44 0) -- front right corner
                (Point3d.centimeters 30 0 0) -- front left corner
            |> Scene3d.translateBy (Vector3d.centimeters 20 -72 1.2) 

        exampleCircle =
            (Scene3d.cylinder (Material.matte Color.black) <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 0.1
                    , radius = Length.centimeters 15
                    } )
            |> Scene3d.translateBy (Vector3d.centimeters 35 -30 1) 

        exampleCircle1 =
                (Scene3d.cylinder (Material.matte Color.lightGrey) <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 0.1
                    , radius = Length.centimeters 14
                    } )
            |> Scene3d.translateBy (Vector3d.centimeters 35 -30 1.001) 

        table = Scene3d.group [  tableLeg
                               , tableLeg 
                                    |> Scene3d.translateBy (Vector3d.centimeters -20 0 0)
                               , tableLeg 
                                    |> Scene3d.translateBy (Vector3d.centimeters 0 -10 0) 
                               , tableLeg 
                                    |> Scene3d.translateBy (Vector3d.centimeters -20 -10 0)
                               , tableBoard
                               , cup Color.purple
                               , cup Color.yellow
                                    |> Scene3d.translateBy (Vector3d.centimeters -5 0 0)
                               , cup Color.blue 
                               |> Scene3d.translateBy (Vector3d.centimeters -2.5 -2 0)
                                , magazines
                                    |> Scene3d.translateBy (Vector3d.centimeters 1 1 0)


                              ]

        cup c = (Scene3d.cylinder (Material.metal { baseColor = c, roughness = 0.3 }) <|
                (Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 4.5
                    , radius = Length.centimeters 1.5
                    } ))
            |> Scene3d.translateBy (Vector3d.centimeters 40 60 7) 

        tableBoard = (Scene3d.block (Material.matte Color.lightGrey) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters 30
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters 15
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters 1
                    }  )
            |> Scene3d.translateBy (Vector3d.centimeters 15 47 6) 

        tableLeg = (Scene3d.cylinder (Material.matte Color.white) <|
                (Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 7
                    , radius = Length.centimeters 1
                    } ))
            |> Scene3d.translateBy (Vector3d.centimeters 40 60 0) 

        magazine c = (Scene3d.block (Material.matte c) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters 7
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters 10
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters 0.3
                    }  )
            |> Scene3d.translateBy (Vector3d.centimeters 16 50 7) 

        magazines = Scene3d.group [  magazine Color.red
                               , magazine Color.green 
                                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees 30)
                                    |> Scene3d.translateBy (Vector3d.centimeters 30 -1 0.5)
                               , magazine Color.purple 
                                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees -20)
                                    |> Scene3d.translateBy (Vector3d.centimeters -19 9 1)]

        mailbox = Scene3d.group [ 
            
                    (Scene3d.block (Material.metal { baseColor = Color.red, roughness = 0.3 }) <|
                        Block3d.with
                            { x1 = Length.centimeters 0
                            , x2 = Length.centimeters 10
                            , y1 = Length.centimeters 0
                            , y2 = Length.centimeters 6
                            , z1 = Length.centimeters 0
                            , z2 = Length.centimeters 5
                            }  )
                    |> Scene3d.translateBy (Vector3d.centimeters 70 20 6)

                    ,

                    (Scene3d.cylinder (Material.metal { baseColor = Color.red, roughness = 0.3 }) <|
                        (Cylinder3d.along Axis3d.z
                            { start = Length.centimeters 0
                            , end = Length.centimeters 7
                            , radius = Length.centimeters 1
                            } ))
                    |> Scene3d.translateBy (Vector3d.centimeters 75 23 0)
                ]

        chair = Scene3d.group [
                    chairBody
                    ,
                    chairBody 
                        |> Scene3d.rotateAround Axis3d.x (Angle.degrees -45)
                        |> Scene3d.translateBy (Vector3d.centimeters 0 0 45)

                    , chairLeg
                    , chairLeg 
                        |> Scene3d.translateBy (Vector3d.centimeters 0 10 0) 


            ] 

        chairBody = (Scene3d.block (Material.metal { baseColor = Color.lightBlue, roughness = 0.3 }) <|
                        Block3d.with
                            { x1 = Length.centimeters 0
                            , x2 = Length.centimeters 10
                            , y1 = Length.centimeters 0
                            , y2 = Length.centimeters 2
                            , z1 = Length.centimeters 0
                            , z2 = Length.centimeters 15
                            }  )
                    |> Scene3d.rotateAround Axis3d.x (Angle.degrees -45)
                    |> Scene3d.translateBy (Vector3d.centimeters 50 60 6)
        
        chairLeg = (Scene3d.block (Material.metal { baseColor = Color.lightBlue, roughness = 0.3 }) <|
                        Block3d.with
                            { x1 = Length.centimeters 0
                            , x2 = Length.centimeters 8
                            , y1 = Length.centimeters 0
                            , y2 = Length.centimeters 1.5
                            , z1 = Length.centimeters 0
                            , z2 = Length.centimeters 6
                            }  )
                    |> Scene3d.translateBy (Vector3d.centimeters 51 49 0)

        parasol = Scene3d.group [ 
                                  parasolPole 50
                                , parasolPole 20
                                    |> Scene3d.rotateAround Axis3d.x (Angle.degrees -45)
                                    |> Scene3d.translateBy (Vector3d.centimeters 0 20 70)
                                , parasolPole 35
                                    |> Scene3d.rotateAround Axis3d.x (Angle.degrees 45)
                                    |> Scene3d.translateBy (Vector3d.centimeters 0 34 -11.5)
                                , parasolTip 
                                , parasolPart
                                , parasolPart
                                    |> Scene3d.rotateAround Axis3d.y (Angle.degrees 90)
                                    |> Scene3d.translateBy (Vector3d.centimeters  -21 0 101)
                                , parasolPart 
                                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
                                    |> Scene3d.translateBy (Vector3d.centimeters  97 16.7 0)
                                , parasolPart 
                                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
                                    |> Scene3d.translateBy (Vector3d.centimeters  -17.2 97.4 0.7)



                                ]

        parasolPole n = (Scene3d.cylinder (Material.metal { baseColor = Color.darkGrey, roughness = 0.3 }) <|
                (Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters n
                    , radius = Length.centimeters 1
                    } ))
            |> Scene3d.translateBy (Vector3d.centimeters 40 67 0) 

        parasolTip = (Scene3d.sphere (Material.metal { baseColor = Color.grey, roughness = 0.1 }) <|
                        Sphere3d.withRadius (Length.centimeters 2) Point3d.origin)
                        |> Scene3d.translateBy (Vector3d.centimeters 40 57 61) 
        
        parasolPart =  (Scene3d.mesh (Material.matte Color.blue) parasolMesh)
            |> Scene3d.rotateAround Axis3d.y (Angle.degrees -45)
            |> Scene3d.translateBy (Vector3d.centimeters 60 37 40)
            
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
                , verticalFieldOfView = Angle.degrees 60
                }
    in
     case model.textures of
      Just texturesList  ->
        Scene3d.custom
          { lights = Scene3d.threeLights firstLight thirdLight softLighting
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
                        , house
                        , clouds
                          |> Scene3d.translateBy (Vector3d.withLength (Length.centimeters (20*sin (100* model.time))) Direction3d.y)
                        , fences
                        , tree (3*sin (1000* model.time) + 20)
                            |> Scene3d.translateBy (Vector3d.centimeters -10 -10 0)
                        , tree  (3*sin (1000* model.time) + 20)
                            |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) 0.7
                            |> Scene3d.translateBy (Vector3d.centimeters -40 0 0)

                        -- , flower  50
                        , basketBallCourt
                        , ball 
                            |> Scene3d.translateBy (Vector3d.withLength (Length.centimeters (abs <| 30*sin (3000* model.time))) Direction3d.z)

                        , table
                        , mailbox
                        , chair
                        , chair 
                            |> Scene3d.translateBy (Vector3d.centimeters -50 0 0)
                        , parasol
                        , Skybox.skybox (List.map Just texturesList) 1000
                      ]
          }
      Nothing ->
        Html.text "Loading..."
     -- Nothing means the load texture fail.

-- The textures for the skybox
-- textureBottom =
--     "./texture/sky-box-Bottom.jpg"
-- textureTop =
--     "./texture/sky-box-Top.jpg"
-- textureSide1 =
--     "./texture/sky-box-Side1.jpg"
-- textureSide2 =
--     "./texture/sky-box-Side2.jpg"
-- textureSide3 =
--     "./texture/sky-box-Side3.jpg"
-- textureSide4 =
--     "./texture/sky-box-Side4.jpg"

textureBottom =
    "https://i.imgur.com/hxZnxTb.png" -- The source images for these were jpgs so it's not the greatest quality
textureTop =
    "https://i.imgur.com/ABrYIGq.png"
textureSide1 =
    "https://i.imgur.com/YMbHdAt.png"
textureSide2 =
    "https://i.imgur.com/b9tOTQE.png"
textureSide3 =
    "https://i.imgur.com/4iIoO5F.png"
textureSide4 =
    "https://i.imgur.com/7atbfPN.png"
textureListSkyBox = 
  [textureBottom, textureTop, textureSide1, textureSide2, textureSide3
    , textureSide4]

-- Fetch the texture from the List, textureListSkyBox.
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