module Character exposing (character)
{--
   -- Design a character
--}

import Angle as Angle exposing (Angle)
import Axis3d
import Array
import Block3d
import Camera3d
import Cylinder3d
import Color
import Direction3d
import Html exposing (Html)
import Illuminance
import Length
import LuminousFlux
import Pixels
import Point3d
import Sphere3d
import Scene3d exposing (Entity)
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import TriangularMesh
import Viewpoint3d
import Quantity


main : Html msg
main =
    let
        -- Create a camera using perspective projection
        camera =
            Camera3d.perspective
                { -- Camera is at the point (4, 2, 2), looking at the point
                  -- (0, 0, 0), oriented so that positive Z appears up
                  viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 4 2 2
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
    in
    -- Render a scene that doesn't involve any lighting (no lighting is needed
    -- here since we provided a material that will result in a constant color
    -- no matter what lighting is used)
    Scene3d.custom
        { 
          entities = [ character {
            lMove = True , lAngel = (Angle.degrees -10)}
          ]
        , camera = camera
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , dimensions = ( Pixels.pixels 600, Pixels.pixels 600 )
        , lights = Scene3d.twoLights lightBulb softLighting
        , exposure = Scene3d.exposureValue 5
        , whiteBalance = Light.incandescent
        , antialiasing = Scene3d.multisampling
        , toneMapping = Scene3d.noToneMapping
        }
character : { lMove: Bool, lAngel: Angle } 
   -> Entity coordinates
character { lMove , lAngel} = 
  let 
   rotateHand = Axis3d.through (Point3d.meters 0 0 0) Direction3d.x
   rotateEye = Axis3d.through (Point3d.meters 0 0 0) Direction3d.z
  in
   Scene3d.group [head,neck,body
            ,eye
              |>Scene3d.scaleAbout Point3d.origin 0.8
              |>Scene3d.translateIn Direction3d.z (Length.centimeters 8)
              |>Scene3d.translateIn Direction3d.y (Length.centimeters 2)
              |>Scene3d.rotateAround rotateEye (Angle.degrees -90)
            ,hand
              |>Scene3d.rotateAround rotateHand (Angle.degrees 15)
              |>Scene3d.scaleAbout Point3d.origin 0.8
              |>Scene3d.translateIn Direction3d.z (Length.centimeters -10)
              |>Scene3d.translateIn Direction3d.y (Length.centimeters 16)
            ,hand
              |>Scene3d.rotateAround rotateHand (Angle.degrees -15)
              |>Scene3d.scaleAbout Point3d.origin 0.8
              |>Scene3d.translateIn Direction3d.z (Length.centimeters -10)
              |>Scene3d.translateIn Direction3d.y (Length.centimeters -16)
            , legGroup {
               lMove =  lMove 
               ,lAngel = lAngel}
    ]
legGroup : { lMove: Bool, lAngel: Angle } 
  -> Entity coordinates
legGroup { lMove , lAngel} = 
    let
      direction = Axis3d.through (Point3d.meters 0 0 0) Direction3d.y
      rotateLeg = Scene3d.rotateAround direction lAngel
      singleLeg = leg
                   |>Scene3d.scaleAbout Point3d.origin 0.8
                   |>Scene3d.translateIn Direction3d.z (Length.centimeters -45)
    in
      if(lMove) then 
       Scene3d.group [
            singleLeg
              |>Scene3d.translateIn Direction3d.y (Length.centimeters 8)
              |>rotateLeg
            ,singleLeg
              |>Scene3d.translateIn Direction3d.y (Length.centimeters -8)
          ]
      else
         Scene3d.group [
            singleLeg
              |>Scene3d.translateIn Direction3d.y (Length.centimeters 8)
            ,singleLeg
              |>Scene3d.translateIn Direction3d.y (Length.centimeters -8)
              |>rotateLeg
          ]
head = 
  let
    material = Material.matte Color.yellow
    cylinder = Cylinder3d.centeredOn (Point3d.centimeters 0 0 12) Direction3d.positiveZ
            { 
              radius = Length.centimeters 15
              , length = Length.centimeters 18
            }
   in
     Scene3d.cylinder material cylinder

eye =
  let
    eyeMaterial = Material.matte Color.black
    singleEye = Scene3d.sphere eyeMaterial 
           <|Sphere3d.withRadius (Length.centimeters 5) (Point3d.meters 0 0 0)
  in
    Scene3d.group [
      singleEye
        |> Scene3d.translateIn Direction3d.y (Length.centimeters 10)
        |> Scene3d.translateIn Direction3d.z (Length.centimeters 8)
        |> Scene3d.translateIn Direction3d.x (Length.centimeters -8)
      , singleEye
        |> Scene3d.translateIn Direction3d.y (Length.centimeters 10)
        |> Scene3d.translateIn Direction3d.z (Length.centimeters 8)
        |> Scene3d.translateIn Direction3d.x (Length.centimeters 8)
    ]

neck = 
  let
    material = Material.matte Color.yellow
    cylinder = Cylinder3d.centeredOn Point3d.origin Direction3d.positiveZ
            { 
              radius = Length.centimeters 10
              , length = Length.centimeters 10
            }
   in
     Scene3d.cylinder material cylinder

body = 
  let 
    material = Material.matte Color.yellow
    
    p1f = Point3d.centimeters 10 11 0
    p2f = Point3d.centimeters 10 -11 0
    p3f = Point3d.centimeters 10 14 -21
    p4f = Point3d.centimeters 10  -14 -21
    p1b = Point3d.centimeters -10 11 0
    p2b = Point3d.centimeters -10 -11 0
    p3b = Point3d.centimeters -10 14 -21
    p4b = Point3d.centimeters -10  -14 -21
    triangularMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ p1f -- 0 
                    , p2f -- 1
                    , p3f -- 2
                    , p4f -- 3
                    , p1b -- 4
                    , p2b -- 5
                    , p3b -- 6
                    , p4b -- 7
                    ]
                )
                [ ( 0, 3, 2 ), ( 0, 1, 3 ) -- trapezoid front
                , ( 4, 7, 6 ), ( 4, 7, 5 ) -- trapezoid back
                , ( 0, 4, 1 ), ( 0, 5, 1 ) -- trapezoid top
                , ( 0, 4, 6 ), ( 0, 6, 2 ) -- trapezoid side left
                , ( 5, 1, 3 ), ( 5, 3, 4 ) -- trapezoid side right
                ]
    bodyMesh = Mesh.indexedFacets triangularMesh
  in
    Scene3d.mesh material bodyMesh
      |> Scene3d.scaleAbout Point3d.origin 1.2
      |> Scene3d.translateIn Direction3d.z (Length.centimeters -3)

leg = 
  let
    material = Material.matte Color.yellow
    cylinder = Cylinder3d.centeredOn (Point3d.centimeters 0 0 15) Direction3d.positiveY
            { 
              radius = Length.centimeters 14
              , length = Length.centimeters 18
            }
    blockleg1 = Block3d.from (Point3d.centimeters -10 -10 -20) (Point3d.centimeters 8 9 5)
    blockfeet = Block3d.from (Point3d.centimeters -10 -10 -2) (Point3d.centimeters 16 9 10)
    thigh = Scene3d.cylinder material cylinder
    leg1 = Scene3d.block material blockleg1
    feet = Scene3d.block material blockfeet
  in
   Scene3d.group [thigh, leg1
     , feet|>Scene3d.translateIn Direction3d.z (Length.centimeters -18)]

hand = 
  let
    material = Material.matte Color.yellow
    cylinder1 = Cylinder3d.centeredOn (Point3d.origin) Direction3d.positiveZ
            { 
              radius = Length.centimeters 9
              , length = Length.centimeters 25
            }
    joint = Scene3d.sphere material 
           <|Sphere3d.withRadius (Length.centimeters 10) (Point3d.origin)
    handUp = Scene3d.cylinder material cylinder1
  in
    Scene3d.group[joint
      , handUp|>Scene3d.translateIn Direction3d.z (Length.centimeters -16)
      , joint 
         |> Scene3d.scaleAbout Point3d.origin 0.9
         |>Scene3d.translateIn Direction3d.z (Length.centimeters -30)
      , palm
         |>Scene3d.translateIn Direction3d.z (Length.centimeters -43)
    ]

palm = 
  let
    material = Material.matte Color.yellow
    pointListBig a = List.map
                 ( \elm ->
                     let 
                       y = cos (degrees (toFloat elm))
                       z = sin (degrees (toFloat elm))
                     in
                       Point3d.centimeters a y z 
                 ) <| List.range 0 180
    pointList1 = pointListBig 1
    pointList2 = pointListBig -1
    pointListSmall extrnalP = List.map (\list ->
                              let
                                newy = Quantity.multiplyBy 0.8 (Point3d.yCoordinate list)
                                newz = Quantity.multiplyBy 0.8 (Point3d.zCoordinate list)
                              in
                                Point3d.xyz (Point3d.xCoordinate list) newy newz
                          ) extrnalP
    pointList3 = pointListSmall pointList1
    pointList4 = pointListSmall pointList2
    triangularMesh = 
        TriangularMesh.indexed
           (Array.fromList (List.append pointList1 
                             (List.append pointList2 
                               (List.append pointList3 pointList4))))
           (List.concatMap ( \elm ->  [(elm,elm+181,elm+1)
                                      ,(elm+181,elm+1,elm+182)
                                      ,(elm+362,elm+543,elm+363)
                                      ,(elm+543,elm+363,elm+544)
                                      ,(elm,elm+362,elm+1)
                                      ,(elm+362,elm+1,elm+363)
                                      ,(elm+181,elm+543,elm+182)
                                      ,(elm+543,elm+182,elm+544)]
                            ) <| List.range 0 179)
    parabola = Mesh.indexedFacets triangularMesh
 in
    Scene3d.mesh material parabola
     |> Scene3d.scaleAbout Point3d.origin 10
