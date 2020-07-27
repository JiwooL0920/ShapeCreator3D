module Wrapper3D exposing 
    ( metallic, plastic, matte, customMat, textured
    , cube, square3D, rectangle3D, box
    , sphere, cone, cylinder
    , polyCone, polyCylinder
    , ring, ellipsoid, polygon3D, customPolygon
    , move3D, scale3D, rotate3D
    , rotateX3D, rotateY3D, rotateZ3D
    , group3D, nameObject
    , repeatDuration, repeatDistance
    , generateEllipsoid, generatePolyCone, generatePolyCylinder
    , Dimension, Mold, GeneratedMesh, MeshStore, withOverlay, unwrapQ
    , EntityBBox(..),BoxMsg(..)
    , renderEntities, renderCollider
    , boxOutline
    , getTexture, loadTexture
    , relativeP
    )

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
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
import Parameter1d

import GraphicSVG.Widget as Widget
import GraphicSVG as G
import Html.Attributes as HA

type EntityBBox coordinates = 
    ObjectGroup 
       (List (EntityBBox coordinates)) 
       (BoxMsg coordinates)
       String
    | Object {shape : Entity coordinates
              , boundingBox : BoxMsg coordinates
              , name : String }    

type BoxMsg coordinates = NoBox | Box (BoundingBox3d Meters coordinates)

type alias Dimension = (Float,Float,Float)

type alias MeshStore coordinates = 
    { generatedMeshes : Dict String (Mesh.Textured coordinates)
    , generatedShadows : Dict String (Mesh.Shadow coordinates)
    }

type alias Mold coordinates a = (Material coordinates { a | normals : () } -> EntityBBox coordinates)

type alias TexturedMold coordinates a = (Material coordinates { a | normals : (), uvs : () } -> EntityBBox coordinates)

-- Materials
-- "Material coordinates { a | normals : () }" allegedly allows us to not have to bother with textured vs. uniform

{-| Makes a shape look metallic. Takes a colour and roughness value. -}
metallic : Color.Color -> Float -> Mold coordinates a -> EntityBBox coordinates
metallic colour roughness shapeFunc = (Material.metal { baseColor = colour, roughness = roughness }) |> shapeFunc

{-| Makes a shape look like plastic, wood, or other similar materials. Takes a colour and roughness value. -}

plastic : Color.Color -> Float -> Mold coordinates a -> EntityBBox coordinates
plastic colour roughness shapeFunc = (Material.nonmetal { baseColor = colour, roughness = roughness }) |> shapeFunc

{-| Makes a shape nonreflective. Takes only a colour. -}

matte : Color.Color -> Mold coordinates a -> EntityBBox coordinates
matte colour shapeFunc = (Material.matte colour) |> shapeFunc

{-| Makes a shape look as metallic as you want. Takes colour, roughness, and metallicity values. -}

customMat : Color.Color -> Float -> Float -> Mold coordinates a -> EntityBBox coordinates
customMat colour roughness metallicity shapeFunc = (Material.pbr { baseColor = colour, roughness = roughness, metallic = metallicity }) |> shapeFunc

{-| Makes a shape look as metallic as you want, and applies a texture to it. Takes a texture, as well as roughness and metallicity values. -}
textured : Material.Texture Color.Color -> Float -> Float -> TexturedMold coordinates a -> EntityBBox coordinates
textured texture roughness metallicity shapeFunc = 
    (Material.texturedPbr { baseColor = texture, roughness = Material.constant roughness, metallic = Material.constant metallicity }) |> shapeFunc

--- Shapes

cube : Float -> Material.Uniform coordinates -> EntityBBox coordinates
cube size material = 
  let 
    cubeShape = Block3d.from
                    (Point3d.centimeters 0 0 0)
                    (Point3d.centimeters size size size)
   in Object {
    shape = Scene3d.blockWithShadow material cubeShape
    , boundingBox = Block3d.boundingBox cubeShape |> Box
    , name = "cube"
  }

square3D : Float -> Material.Textured coordinates -> EntityBBox coordinates
square3D length material = 
    let
        posValue = length / 2
        quadShape = Scene3d.quadWithShadow material
            (Point3d.centimeters (-posValue) (-posValue) 0)
            (Point3d.centimeters (-posValue) posValue 0)
            (Point3d.centimeters posValue posValue 0)
            (Point3d.centimeters posValue (-posValue) 0)
        quadBBox = BoundingBox3d.from
                    (Point3d.centimeters (-posValue) (-posValue) 0)
                    (Point3d.centimeters posValue posValue 0)
    in
      Object {
       shape = quadShape
      , boundingBox = Box quadBBox
      , name = "square"
      }


rectangle3D : Float -> Float -> Material.Textured coordinates -> EntityBBox coordinates
rectangle3D length width material = 
    let
        lValue = length / 2
        wValue = width / 2
        rectangleShape = Scene3d.quadWithShadow material
            (Point3d.centimeters (-lValue) (-wValue) 0)
            (Point3d.centimeters (-lValue) wValue 0)
            (Point3d.centimeters lValue wValue 0)
            (Point3d.centimeters lValue (-wValue) 0)
        rectangleBBox = BoundingBox3d.from
                    (Point3d.centimeters (-lValue) (-wValue) 0)
                    (Point3d.centimeters lValue wValue 0)
    in
      Object {
       shape = rectangleShape
      , boundingBox = Box rectangleBBox
      , name = "rectangle"
      }
box : Float -> Float -> Float -> Material.Uniform coordinates -> EntityBBox coordinates
box length width height material = 
    let
      boxShape = Block3d.from
                    Point3d.origin
                    (Point3d.centimeters length width height)
    in Object {
    shape = Scene3d.blockWithShadow material boxShape
    , boundingBox = Block3d.boundingBox boxShape |> Box
    , name = "box"
    }

sphere : Float -> Material.Textured coordinates -> EntityBBox coordinates
sphere r material = 
    let 
      sphereShape = Sphere3d.withRadius (Length.centimeters r) Point3d.origin
    in Object {
    shape = Scene3d.sphereWithShadow material sphereShape
    , boundingBox = Sphere3d.boundingBox sphereShape |> Box
    , name = "sphere"
    } |> move3D (0,0,r)


cone : Float -> Float -> Material.Uniform coordinates -> EntityBBox coordinates
cone r h material = 
    let
      coneShape = Cone3d.along Axis3d.z
            { base = Length.centimeters 0
            , tip = Length.centimeters h
            , radius = Length.centimeters r
            }
    in Object {
    shape = Scene3d.coneWithShadow material coneShape
    , boundingBox = Cone3d.boundingBox coneShape |> Box
    , name = "cone"
    }

polyCone : List (Float,Float) -> (Float,Float,Float) -> MeshStore coordinates -> Material.Textured coordinates -> EntityBBox coordinates
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
                
        bottom = customPolygon points material

        minX = List.minimum (List.map (\(x, y) -> x) points)
        maxX = List.maximum (List.map (\(x, y) -> x) points)
        minY = List.minimum (List.map (\(x, y) -> y) points)
        maxY = List.maximum (List.map (\(x, y) -> y) points)

    in
      case bottom of
        Object bottom_obj ->
           case ( minX, maxX ) of
             ( Just minx, Just maxx ) ->
                case (minY, maxY) of
                  (Just miny, Just maxy) ->
                    Object {
                     shape = Scene3d.group [ Scene3d.meshWithShadow material mesh shadow, bottom_obj.shape ]
                     , boundingBox = BoundingBox3d.from
                         (Point3d.centimeters (maxx + xtip) (maxy + ytip) ztip)
                         (Point3d.centimeters minx miny 0)
                         |> Box
                     , name = "polyCone"
                     }
                  ( _ , _ ) ->
                     Object {
                        shape = Scene3d.group [ Scene3d.meshWithShadow material mesh shadow, bottom_obj.shape ]
                        , boundingBox = NoBox
                        , name = "polyCone"
                     }
             ( _ , _ )  ->
               Object {
               shape = Scene3d.group [ Scene3d.meshWithShadow material mesh shadow, bottom_obj.shape ]
               , boundingBox = NoBox
               , name = "polyCone"
               }
        _ ->
            Object {
                  shape = Scene3d.nothing 
                  , boundingBox = NoBox
                  , name = "error-polyCone"
                }
             

polyCylinder : List (Float,Float) -> Float -> MeshStore coordinates -> Material.Textured coordinates -> EntityBBox coordinates
polyCylinder points height model material  =
    let 
        meshName = "polyCylinder" ++ String.fromInt (List.length points) ++ String.fromFloat height

        bottom = customPolygon points material

        top = customPolygon points material |> move3D (0,0,height)

        mesh = 
            case Dict.get meshName model.generatedMeshes of
                Nothing -> polyCylinderMesh points height
                Just actualMesh -> actualMesh

        shadow = 
            case Dict.get meshName model.generatedShadows of
                Nothing -> Mesh.shadow (polyCylinderMesh points height)
                Just actualMesh -> actualMesh

        minX = List.minimum (List.map (\(x, y) -> x) points)
        maxX = List.maximum (List.map (\(x, y) -> x) points)
        minY = List.minimum (List.map (\(x, y) -> y) points)
        maxY = List.maximum (List.map (\(x, y) -> y) points)

    in
      case (bottom, top) of 
        (Object bottom_obj, Object top_obj) -> 
            case ( minX, maxX ) of
              ( Just minx, Just maxx ) ->
                 case (minY, maxY) of
                   (Just miny, Just maxy) ->
                     Object {
                      shape = Scene3d.group [ Scene3d.meshWithShadow material mesh shadow, bottom_obj.shape, top_obj.shape ]
                      , boundingBox = BoundingBox3d.from
                          (Point3d.centimeters maxx maxy height)
                          (Point3d.centimeters minx miny 0)
                          |> Box
                      , name = "polyCylinder"
                      }
                   ( _ , _ ) ->
                      Object {
                         shape = Scene3d.group [ Scene3d.meshWithShadow material mesh shadow, bottom_obj.shape, top_obj.shape ]
                         , boundingBox = NoBox
                         , name = "polyCylinder"
                      }
      
              ( _ , _ ) ->
                Object {
                  shape = Scene3d.group [ Scene3d.meshWithShadow material mesh shadow, bottom_obj.shape, top_obj.shape ]
                  , boundingBox = NoBox
                  , name = "polyCylinder"
                }
        (_, _) ->
            Object {
                  shape = Scene3d.nothing 
                  , boundingBox = NoBox
                  , name = "error-polyCylinder"
                }
      

cylinder : Float -> Float -> Material.Uniform coordinates -> EntityBBox coordinates
cylinder r h material =
    let
      cylinderShape = Cylinder3d.along Axis3d.z
            { start = Length.centimeters 0
            , end = Length.centimeters h
            , radius = Length.centimeters r
            }
    in Object {shape = Scene3d.cylinderWithShadow material cylinderShape
    , boundingBox = Cylinder3d.boundingBox cylinderShape |> Box
    , name = "cylinder"
    }


ring : Float -> Float -> Material.Uniform coordinates -> EntityBBox coordinates
ring radius thickness material = 
    let
      ringBBox = BoundingBox3d.fromExtrema
        { minX = Length.centimeters (0 - radius)
        , maxX = Length.centimeters (0 + radius)
        , minY = Length.centimeters (0 - radius)
        , maxY = Length.centimeters (0 + radius)
        , minZ = Length.centimeters (0 - thickness)
        , maxZ = Length.centimeters (0 + thickness)
        }
    in Object {shape = Scene3d.mesh material (ringMesh radius thickness)
    , boundingBox = Box ringBBox
    , name = "ring"
    }

{-| Create an ellipsoid with a custom length, width, and height.
Requires that you pass in the model as well, in order for it to retrieve the required meshes. -}
ellipsoid : Float -> Float -> Float -> MeshStore coordinates -> Material.Textured coordinates -> EntityBBox coordinates
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

        ellipsoidBBox = BoundingBox3d.fromExtrema
            { minX = Length.centimeters (0 - length)
            , maxX = Length.centimeters (0 + length)
            , minY = Length.centimeters (0 - width)
            , maxY = Length.centimeters (0 + width)
            , minZ = Length.centimeters (0 - height)
            , maxZ = Length.centimeters (0 + height)
            }

    in Object {shape = Scene3d.meshWithShadow material mesh shadow
    , boundingBox = Box ellipsoidBBox
    , name = "ellipsoid"
    }


{-| Create a polygon with a custom number of sides (min. 3) and size, in centimeters. -}
polygon3D : Int -> Float -> Material.Textured coordinates -> EntityBBox coordinates
polygon3D sides size material = 
    let
        mesh = polygonMesh sides

        shadow = Mesh.shadow (polygonMesh sides)
        polygonBBox = BoundingBox3d.fromExtrema 
            { minX = Length.centimeters (0 - 1)
            , maxX = Length.centimeters (0 + 1)
            , minY = Length.centimeters (0 - 1)
            , maxY = Length.centimeters (0 + 1)
            , minZ = Length.centimeters (0)
            , maxZ = Length.centimeters (0)
            }
    in  Object {shape = Scene3d.meshWithShadow material mesh shadow
    , boundingBox = Box polygonBBox
    , name = "polygon"
     } |> scale3D size

{-| Create a custom polygon from a list of points -}
customPolygon : List (Float,Float) -> Material.Textured coordinates -> EntityBBox coordinates
customPolygon points material = 
    let
        mesh = customPolyMesh points

        shadow = Mesh.shadow (customPolyMesh points)
        minX = List.minimum (List.map (\(x, y) -> x) points)
        maxX = List.maximum (List.map (\(x, y) -> x) points)
        minY = List.minimum (List.map (\(x, y) -> y) points)
        maxY = List.maximum (List.map (\(x, y) -> y) points)

    in
      case ( minX, maxX ) of
        ( Just minx, Just maxx ) ->
           case (minY, maxY) of
             (Just miny, Just maxy) ->
               Object {
                shape = Scene3d.meshWithShadow material mesh shadow
                , boundingBox = BoundingBox3d.from
                    (Point3d.centimeters maxx maxy 0)
                    (Point3d.centimeters minx miny 0)
                    |> Box
                , name = "customPolygon"
                }
             ( _ , _ ) ->
                Object {
                   shape = Scene3d.meshWithShadow material mesh shadow
                   , boundingBox = NoBox
                   , name = "customPolygon"
                }

        ( _ , _ ) ->
          Object {
            shape = Scene3d.meshWithShadow material mesh shadow
            , boundingBox = NoBox
            , name = "customPolygon"
          }        
        
--Create a box outline for showing the bounding box 
boxOutline : Material.Plain coordinates -> BoundingBox3d Meters coordinates -> Entity coordinates
boxOutline material boundingBox =
  let
    toBlock3d = 
        Block3d.with
          {
            x1 = BoundingBox3d.minX boundingBox
          , y1 = BoundingBox3d.minY boundingBox 
          , z1 = BoundingBox3d.minZ boundingBox
          , x2 = BoundingBox3d.maxX boundingBox 
          , y2 = BoundingBox3d.maxY boundingBox
          , z2 = BoundingBox3d.maxZ boundingBox
          }
    edges = Block3d.edges toBlock3d
    
  in Scene3d.group (List.map 
    (\line -> 
    Scene3d.lineSegment material line
    ) edges)

-- Transformations 
move3D : Dimension -> EntityBBox coordinates -> EntityBBox coordinates
move3D (x,y,z) entityBBox = 
    case entityBBox of 
      ObjectGroup listObj bBox name ->
        ObjectGroup 
          (List.map (\object -> move3D (x,y,z) object) listObj)
          (case bBox of
            Box boundingbox -> boundingbox
              |> BoundingBox3d.translateBy (Vector3d.centimeters x y z)
              |> Box
            NoBox -> NoBox)
          name
      Object object ->
        case object.boundingBox of
          Box boundingBox -> 
             Object {shape = object.shape
                     |> Scene3d.translateBy (Vector3d.centimeters x y z)  
             , boundingBox = boundingBox
                |> BoundingBox3d.translateBy (Vector3d.centimeters x y z)
                |> Box
             , name = object.name
             }
          NoBox ->
            Object {shape = object.shape
                     |> Scene3d.translateBy (Vector3d.centimeters x y z)  
             , boundingBox = NoBox
             , name = object.name
            }


rotate3D : Float -> Float -> Float -> EntityBBox coordinates -> EntityBBox coordinates
rotate3D pitch yaw roll entityBBox = 
    let
      toBlock3d boundingBox = 
        Block3d.with
          {
            x1 = BoundingBox3d.minX boundingBox
          , y1 = BoundingBox3d.minY boundingBox
          , z1 = BoundingBox3d.minZ boundingBox 
          , x2 = BoundingBox3d.maxX boundingBox
          , y2 = BoundingBox3d.maxY boundingBox 
          , z2 = BoundingBox3d.maxZ boundingBox
          }
    in 
    case entityBBox of 
      ObjectGroup listObj bBox name ->
        ObjectGroup 
          (List.map (\object -> rotate3D pitch yaw roll object) listObj)
          (case bBox of
            Box boundingbox -> toBlock3d boundingbox
              |> Block3d.rotateAround Axis3d.x (Angle.radians pitch)  
              |> Block3d.rotateAround Axis3d.y (Angle.radians roll)  
              |> Block3d.rotateAround Axis3d.z (Angle.radians yaw)
              |> Block3d.boundingBox
              |> Box
            NoBox -> NoBox)
          name
      Object object ->
        case object.boundingBox of
          Box boundingBox -> 
             Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.x (Angle.radians pitch)  
                     |> Scene3d.rotateAround Axis3d.y (Angle.radians roll)  
                     |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)
             , boundingBox = toBlock3d boundingBox
                     |> Block3d.rotateAround Axis3d.x (Angle.radians pitch)  
                     |> Block3d.rotateAround Axis3d.y (Angle.radians roll)  
                     |> Block3d.rotateAround Axis3d.z (Angle.radians yaw)
                     |> Block3d.boundingBox
                     |> Box
             , name = object.name
             }
          NoBox ->
            Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.x (Angle.radians pitch)  
                     |> Scene3d.rotateAround Axis3d.y (Angle.radians roll)  
                     |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)
             , boundingBox = NoBox
             , name = object.name
            }

-- Alternate rotate functions (rotateX means rotate around X axis.. so on)
rotateX3D : Float -> EntityBBox coordinates -> EntityBBox coordinates
rotateX3D angle entityBBox = 
    let
      toBlock3d boundingBox = 
        Block3d.with
          {
            x1 = BoundingBox3d.minX boundingBox
          , y1 = BoundingBox3d.minY boundingBox
          , z1 = BoundingBox3d.minZ boundingBox 
          , x2 = BoundingBox3d.maxX boundingBox
          , y2 = BoundingBox3d.maxY boundingBox 
          , z2 = BoundingBox3d.maxZ boundingBox
          }
    in 
    case entityBBox of 
      ObjectGroup listObj bBox name ->
        ObjectGroup 
          (List.map (\object -> rotateX3D angle object) listObj)
          (case bBox of
            Box boundingbox -> toBlock3d boundingbox
              |> Block3d.rotateAround Axis3d.x (Angle.radians angle) 
              |> Block3d.boundingBox
              |> Box
            NoBox -> NoBox)
          name
      Object object ->
        case object.boundingBox of
          Box boundingBox -> 
             Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.x (Angle.radians angle)
             , boundingBox = toBlock3d boundingBox
                     |> Block3d.rotateAround Axis3d.x (Angle.radians angle)  
                     |> Block3d.boundingBox
                     |> Box
             , name = object.name
             }
          NoBox ->
            Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.x (Angle.radians angle)
             , boundingBox = NoBox
             , name = object.name
            }

rotateY3D : Float -> EntityBBox coordinates -> EntityBBox coordinates
rotateY3D angle entityBBox = 
    let
      toBlock3d boundingBox = 
        Block3d.with
          {
            x1 = BoundingBox3d.minX boundingBox
          , y1 = BoundingBox3d.minY boundingBox
          , z1 = BoundingBox3d.minZ boundingBox
          , x2 = BoundingBox3d.maxX boundingBox 
          , y2 = BoundingBox3d.maxY boundingBox
          , z2 = BoundingBox3d.maxZ boundingBox
          }
    in 
    case entityBBox of 
      ObjectGroup listObj bBox name ->
        ObjectGroup 
          (List.map (\object -> rotateY3D angle object) listObj)
          (case bBox of
            Box boundingbox -> toBlock3d boundingbox
              |> Block3d.rotateAround Axis3d.y (Angle.radians angle)
              |> Block3d.boundingBox
              |> Box
            NoBox -> NoBox)
          name
      Object object ->
        case object.boundingBox of
          Box boundingBox -> 
             Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.y (Angle.radians angle)
             , boundingBox = toBlock3d boundingBox
                     |> Block3d.rotateAround Axis3d.y (Angle.radians angle)  
                     |> Block3d.boundingBox
                     |> Box
             , name = object.name
             }
          NoBox ->
            Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.y (Angle.radians angle)
             , boundingBox = NoBox
             , name = object.name
            }

rotateZ3D : Float -> EntityBBox coordinates -> EntityBBox coordinates
rotateZ3D angle entityBBox = 
    let
      toBlock3d boundingBox = 
        Block3d.with
          {
            x1 = BoundingBox3d.minX boundingBox
          , y1 = BoundingBox3d.minY boundingBox
          , z1 = BoundingBox3d.minZ boundingBox
          , x2 = BoundingBox3d.maxX boundingBox 
          , y2 = BoundingBox3d.maxY boundingBox
          , z2 = BoundingBox3d.maxZ boundingBox
          }
    in 
    case entityBBox of 
      ObjectGroup listObj bBox name ->
        ObjectGroup 
          (List.map (\object -> rotateZ3D angle object) listObj)
          (case bBox of
            Box boundingbox -> toBlock3d boundingbox
              |> Block3d.rotateAround Axis3d.z (Angle.radians angle)
              |> Block3d.boundingBox
              |> Box
            NoBox -> NoBox)
          name
      Object object ->
        case object.boundingBox of
          Box boundingBox -> 
             Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.z (Angle.radians angle)
             , boundingBox = toBlock3d boundingBox
                     |> Block3d.rotateAround Axis3d.z (Angle.radians angle)  
                     |> Block3d.boundingBox
                     |> Box
             , name = object.name
             }
          NoBox ->
             Object {shape = object.shape
                     |> Scene3d.rotateAround Axis3d.z (Angle.radians angle)
             , boundingBox = NoBox
             , name = object.name
            }

-- Scale an entity by a given amount
scale3D : Float -> EntityBBox coordinates -> EntityBBox coordinates
scale3D factor entityBBox = 
    case entityBBox of 
      ObjectGroup listObj bBox name ->
        ObjectGroup 
          (List.map (\object -> scale3D factor object) listObj)
          (case bBox of
            Box boundingbox -> boundingbox
              |> BoundingBox3d.scaleAbout (Point3d.centimeters 0 0 0) factor
              |> Box
            NoBox -> NoBox)
          name
      Object object ->
        case object.boundingBox of
          Box boundingBox -> 
             Object {shape = object.shape
                     |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) factor
             , boundingBox = boundingBox
                |> BoundingBox3d.scaleAbout (Point3d.centimeters 0 0 0) factor
                |> Box
             , name = object.name
             }
          NoBox ->
             Object {shape = object.shape
                     |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) factor
             , boundingBox = object.boundingBox
             , name = object.name
            }

-- Group EntityBBox coordinates
group3D : List (EntityBBox coordinates) -> EntityBBox coordinates
group3D entitybBoxList  =
    let
        boxList = List.foldr 
            (\entity otherEntity -> 
                case entity of
                ObjectGroup objlist bBox name -> 
                    case bBox of
                        Box boundingbox -> boundingbox :: otherEntity
                        _ -> otherEntity
                Object object ->
                    case object.boundingBox of 
                        Box boundingbox -> boundingbox :: otherEntity
                        _ -> otherEntity)
            []
            entitybBoxList
        objListHelper entityList = List.foldr
            (\entity otherEntity -> 
                case entity of
                    ObjectGroup obj_list bBox name -> (objListHelper obj_list) ++ otherEntity
                    Object object -> [object] ++ otherEntity
            )
            []
            entityList
        objList = objListHelper entitybBoxList
    in
        case List.head boxList of 
            Just boxList_head ->
                case List.tail boxList of
                    Just boxList_tail ->
                        ObjectGroup
                            entitybBoxList
                            (Box (BoundingBox3d.aggregate boxList_head boxList_tail))
                            "group"
                    Nothing -> 
                        case List.head objList of 
                            Just objList_head -> Object objList_head
                            Nothing -> Object { shape = Scene3d.nothing
                                        , boundingBox = NoBox
                                        , name = "empty"
                                        }
            Nothing -> Object { shape = Scene3d.nothing
                                , boundingBox = NoBox
                                , name = "empty"
                                }

{-| Give a name to an `Object`, which can be useful with collision detection -}
nameObject : String -> EntityBBox coordinates -> EntityBBox coordinates
nameObject newName object = 
    case object of
        ObjectGroup subObjects boundingBox _ ->
            ObjectGroup subObjects boundingBox newName
        Object attributes ->
            Object { attributes | name = newName }

-- repeat an animation for a given duration
repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
  speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition

repeatDistance : Float -> Float -> Float -> Float -> Float 
repeatDistance speed distance startPosition time =
    repeatDuration speed (round <| distance / speed) startPosition time 

-- MESHES ARE STORED HERE

ringMesh : Float -> Float -> Mesh.Uniform coordinates
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
ellipsoidMesh : Float -> Float -> Float -> Mesh.Textured coordinates
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

polyCylinderMesh : List (Float,Float) -> Float -> Mesh.Textured coordinates
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

polyConeMesh : List (Float,Float) -> (Float,Float,Float) -> Mesh.Textured coordinates
polyConeMesh points (xtip,ytip,ztip) =
    let
        tip =
            { position = Point3d.centimeters xtip ytip ztip
            --Point3d.centimeters xtip ytip ztip
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

{-| Creates a 3D mesh representing a 2D polygon given the number of sides.
Number of sides is automatically adjusted to be at least 3. 
Since this mesh is quite simple, dynamically generating it should be fine. -}

polygonMesh : Int -> Mesh.Textured coordinates
polygonMesh sides = 
    let
        -- Can't really have a polygon with less than 3 sides
        actualSides = 
            if sides < 3 then
                3
            else
                sides

        angleDegrees = 360 / toFloat actualSides

        getPosition n = Point3d.translateBy (Vector3d.withLength (Length.centimeters 1) (Direction3d.xy (Angle.degrees (angleDegrees * n)))) Point3d.origin

        getRange values = (Maybe.withDefault 0 (List.maximum values), Maybe.withDefault 0 (List.minimum values))

        xCoords = Debug.log "xCoords" (List.map ( \ point -> Length.inMeters (Point3d.xCoordinate point) ) (List.map (\n -> getPosition (toFloat n)) (List.range 0 (actualSides-1))))

        yCoords = List.map ( \ point -> Length.inMeters (Point3d.yCoordinate point) ) (List.map (\n -> getPosition (toFloat n)) (List.range 0 (actualSides-1)))

        uWidth = Tuple.first (getRange xCoords) - Tuple.second (getRange xCoords)

        vWidth = Tuple.first (getRange yCoords) - Tuple.second (getRange yCoords)

        points = List.map
            ( \ n -> 
                { position = getPosition n
                , uv = ((Length.inMeters (Point3d.xCoordinate (getPosition n)) - Tuple.second (getRange xCoords)) / uWidth, (Length.inMeters (Point3d.yCoordinate (getPosition n)) - Tuple.second (getRange yCoords)) / vWidth)
                }
            )
            (List.map toFloat (List.range 0 actualSides))

        polyOrigin = 
            let 
                default = { position = Point3d.origin, uv = (0.5,0.5) }
            in
                Maybe.withDefault default (List.head points)
    in
        TriangularMesh.fan polyOrigin points
        |> Mesh.texturedFacets

customPolyMesh : List (Float,Float) -> Mesh.Textured coordinates
customPolyMesh points =
    let
        firstPoint = Maybe.withDefault (0,0) (List.head points)
        tip =
            { position = Point3d.origin
            --Point3d.origin --Point3d.centimeters (Tuple.first firstPoint) (Tuple.second firstPoint) 0
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

type alias GeneratedMesh coordinates = 
    { name : String
    , mesh : Mesh.Textured coordinates
    , shadow : Mesh.Shadow coordinates
    }

generateEllipsoid : Float -> Float -> Float -> GeneratedMesh coordinates
generateEllipsoid length width height = 
    { name = "ellipsoid" ++ String.fromFloat length ++ String.fromFloat width ++ String.fromFloat height
    , mesh = ellipsoidMesh length width height
    , shadow = Mesh.shadow (ellipsoidMesh length width height)
    }

generatePolyCylinder : List (Float,Float) -> Float -> GeneratedMesh coordinates
generatePolyCylinder points height = 
    { name = "polyCylinder" ++ String.fromInt (List.length points) ++ String.fromFloat height
    , mesh = polyCylinderMesh points height
    , shadow = Mesh.shadow (polyCylinderMesh points height)
    }

generatePolyCone : List (Float,Float) -> (Float,Float,Float) -> GeneratedMesh coordinates
generatePolyCone points (xtip,ytip,ztip) = 
    { name = "polyCone" ++ String.fromInt (List.length points) ++ String.fromFloat xtip ++ String.fromFloat ytip ++ String.fromFloat ztip
    , mesh = polyConeMesh points (xtip,ytip,ztip)
    , shadow = Mesh.shadow (polyConeMesh points (xtip,ytip,ztip))
    }

withOverlay :
    List (G.Shape msg) ->
    { a | widget : Widget.Model, height : Quantity Int Pixels, width : Quantity Int Pixels, time : Float } ->
    Html msg ->
    Html msg
withOverlay shapes model html =
    Html.div []
        [
            Html.div [HA.style "position" "absolute",HA.style "top" "0px",HA.style "left" "0px"]
                [
                    html
                ]
        ,    Html.div [HA.style "position" "absolute",HA.style "top" "0px",HA.style "left" "0px", HA.style "width" (String.fromInt (unwrapQ model.width) ++ "px"), HA.style "height" (String.fromInt (unwrapQ model.height) ++ "px")]
             [
                Widget.view model.widget shapes
             ]
        ]

unwrapQ : Quantity number b -> number
unwrapQ (Quantity.Quantity q) = q

renderEntities : List (EntityBBox coordinates) -> List (Entity coordinates)
renderEntities objects = 
    let
        extract =
            \ obj ->
                case obj of
                    ObjectGroup moreObjects _ _ ->
                        Scene3d.group (renderEntities moreObjects)
                    Object objRecord ->
                        objRecord.shape
    in
        List.map extract objects
    
{-| Signifies that the collider of an object should be rendered as well as the object itself.
Colliders will start out as red, and change colour with as group depth increases. -}
renderCollider : Int -> EntityBBox coordinates -> EntityBBox coordinates
renderCollider maxDepth object =
    renderColliderHelper maxDepth 0 object

{-| Helper function for `renderCollider` -}
renderColliderHelper : Int -> Int -> EntityBBox coordinates -> EntityBBox coordinates
renderColliderHelper maxDepth n object =
    let
        hue = clamp 0.0 1.0 (toFloat n * 0.1)

        colour = Color.hsl hue 1 0.5

        collider boundingBox = boxOutline (Material.color colour) boundingBox

        colliderObj bBox = Object { shape = collider bBox, boundingBox = NoBox, name = "renderedCollider" }

    in
        if n >= maxDepth then
            object
        else
            case object of
                ObjectGroup objects bBox name ->
                    case bBox of
                        Box boundingBox ->
                            ObjectGroup ((colliderObj boundingBox) :: (List.map (renderColliderHelper maxDepth (n + 1)) objects)) bBox name
                        NoBox ->
                            object
                Object obj ->
                    case obj.boundingBox of
                        Box boundingBox ->
                            Object { obj | shape = Scene3d.group [ obj.shape, collider boundingBox ] }
                        NoBox ->
                            object

{-| Given a dictionary of textures and an index string, return the texture associated with that index.
If that texture doesn't exist, a constant black texture is returned -}
getTexture : Dict String (Material.Texture Color.Color) -> String -> Material.Texture Color.Color
getTexture textures index = 
    let
        default = Material.constant Color.black

        lookupResult = Dict.get index textures

    in
        Maybe.withDefault default lookupResult

{-| Function for abstracting (name,url) tuples in texture lists -}
loadTexture : String -> String -> (String, String)
loadTexture name url = (name, url)

relativeP : Float -> Quantity Int Pixels -> Float
relativeP blockAnount windowSize = 
  (toFloat (unwrapQ windowSize) / blockAnount)

