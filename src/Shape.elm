module Shape exposing 
    ( Dimension
      ,Axis, axisX, axisY, axisZ
      ,cube, prism, sphere, cone, cylinder
      ,ringMesh, forest, tree, flower, garden
      ,bee, hitsign
      ,cubeSAB, prismSAB, sphereSAB, coneSAB, cylinderSAB
      ,ringSAB, forestSAB, treeSAB, flowerSAB, gardenSAB
      ,beeSAB
      ,translateBy, translateIn, rotateAround, scaleAbout
      ,myMat, myTexturedMat
      ,cloudGroup
    )


import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Angle exposing (Angle)
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
import Cone3d
import Color exposing (Color)
import Cylinder3d
import Direction3d exposing (Direction3d)
import TriangularMesh
import Scene3d exposing (Entity)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import Sphere3d
import Length exposing (Meters, Length)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import Quantity exposing (Unitless)

-- Testing
import Pixels
import Viewpoint3d
import Camera3d
import Html exposing (Html)
import Angle
import Direction3d

type alias ShapeSAB coordinates = 
        {shape : Entity coordinates
        , boundingBox : BoundingBox3d Meters coordinates
        }

--clean up type for (x,y,z)
type alias Dimension = (Float,Float,Float)

--custom type for axis
type Axis = X | Y | Z 

type alias Entity coordinates =
    Scene3d.Entity coordinates

type alias MaterialUniform coordinates =
    Material.Uniform coordinates

type alias MaterialTextured coordinates =
    Material.Textured coordinates

type alias MeshUniform coordinates =
    Mesh.Uniform coordinates


axisX : Axis 
axisX = X
axisY : Axis 
axisY = Y
axisZ : Axis 
axisZ = Z

cube : Color.Color -> Float -> Entity coordinates
cube colour size = Scene3d.blockWithShadow (myMat colour) <|
        Block3d.from
            (Point3d.centimeters -size -size -size)
            (Point3d.centimeters size size size)


prism : Color.Color -> Dimension -> Entity coordinates
prism colour (x,y,z) = 
        Scene3d.blockWithShadow (myMat colour) <|
                Block3d.with
                    { x1 = Length.centimeters -x
                    , x2 = Length.centimeters x
                    , y1 = Length.centimeters -y
                    , y2 = Length.centimeters y
                    , z1 = Length.centimeters -z
                    , z2 = Length.centimeters z
                    }


sphere : Color.Color -> Float -> Entity coordinates
sphere colour radius = 
        Scene3d.sphereWithShadow (myTexturedMat colour) <|
            Sphere3d.withRadius (Length.centimeters radius) Point3d.origin
 

cone : Color.Color -> Axis -> Dimension -> Entity coordinates
cone colour axis (b,t,r) = 
        let 
            along = 
                case axis of 
                    X -> Axis3d.x 
                    Y -> Axis3d.y 
                    Z -> Axis3d.z     
        in 
            Scene3d.coneWithShadow (myMat colour) <|
                Cone3d.along along
                    { base = Length.centimeters b
                    , tip = Length.centimeters t
                    , radius = Length.centimeters r
                    }

        
cylinder : Color.Color -> Axis -> Dimension -> Entity coordinates
cylinder colour axis (s,e,r) =
        let 
            along = 
                case axis of 
                    X -> Axis3d.x 
                    Y -> Axis3d.y 
                    _ -> Axis3d.z    

        in 
            Scene3d.cylinderWithShadow (myMat colour) <|
                Cylinder3d.along along
                    { start = Length.centimeters s
                    , end = Length.centimeters e 
                    , radius = Length.centimeters r
                    }

ringMesh : Float -> Float -> MeshUniform coordinates
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
                    <| List.range 0 400
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


{-- Build a tree and a forest --}

treeLeafSize : Int
treeLeafSize = 40

leafMesh : Float -> Float -> MeshUniform coordinates
leafMesh tipY rotOffset =
    let
        tip =
            Point3d.centimeters 20 tipY 70
        apron = List.map 
            ( \ t -> 
                let 
                    radius = 5 * (4+abs(sin(0.1*pi*t + rotOffset)))
                    angle = (2/(toFloat treeLeafSize))*pi*t
                in
                Point3d.centimeters (20 + radius * cos angle)
                                    (20 + radius * sin angle)
                                    0
            )
            (List.map toFloat <| List.range 0 treeLeafSize)
    in
        TriangularMesh.fan tip apron
          |> Mesh.indexedFacets

tree : Float -> Entity coordinates
tree tipY = 
    let 
        root =  (Scene3d.cylinder (Material.matte Color.brown) <|
                Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 80
                    , radius = Length.centimeters 5
                    } )
                |> Scene3d.translateBy (Vector3d.centimeters -30 -50 0)

        leaves tipH = 
            Scene3d.group 
                ( List.map
                    ( \ h -> Scene3d.mesh (Material.matte (Color.rgb 0 h 0)) (leafMesh tipH h)
                            |> Scene3d.translateBy (Vector3d.centimeters -50 -70 h)
                    )
                    [15,25,35]
                ) 
    in
        Scene3d.group [  root
                      , leaves tipY
                      ]

--scalable, relocatable trees 
forest : Float -> Entity coordinates
forest t = Scene3d.group 
    ( List.map 
            -- scale, (x,y) coordinate
            ( \(s,(x,y))-> tree (4*sin t + 20) 
                                |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) s
                                |> Scene3d.translateBy (Vector3d.centimeters x y 0)
            )
            [ (3.5 ,(100,-100))
            , (5, (-500,234))
            , (2, (-234,234))
            , (3, (2222,3434))
            , (1, (-939, 231))
            ]
    )

{-- Build a flower and a garden --}

middle : Entity coordinates
middle = sphere Color.yellow 17
                |> Scene3d.translateBy (Vector3d.centimeters 0 0 55)

stem :Entity coordinates  
stem = cylinder (Color.hsl 0.35 1 0.3) Z (0,50,7)
    |> Scene3d.translateBy (Vector3d.centimeters 0 0 0)    


outerPetal : Float -> Entity coordinates 
outerPetal h = cone (Color.hsl h 1 0.3) Y (0, 50, 6.5)
        |> Scene3d.rotateAround Axis3d.x (Angle.degrees -50)  
        |> Scene3d.translateBy (Vector3d.centimeters 0 -50 70)
        

innerPetal : Float -> Entity coordinates
innerPetal h = cone (Color.hsl h 1 0.6) Y (0, 35,4)
        |> Scene3d.rotateAround Axis3d.x (Angle.degrees -50)  
        |> Scene3d.translateBy (Vector3d.centimeters 0 -36 70)

--passes shape itself as input so that outerPetal/innerpetal doesnt get computed over and over --> performance??
flower : (Float, Entity coordinates , Entity coordinates) ->  Entity coordinates
flower (angle,outp,inp) = 
        if angle == 720 then 
            Scene3d.group [ middle, stem ]
        --outer petal 
        else if (0 <= angle && angle < 360) then 
            Scene3d.group [ outp |> Scene3d.rotateAround Axis3d.z (Angle.degrees (angle+10))
                          , flower ((angle + 15),outp,inp)
                          ]
        --inner petal 
        else
            Scene3d.group [inp |> Scene3d.rotateAround Axis3d.z (Angle.degrees (angle+10))
                         , flower ((angle + 15),outp,inp)
                         ]

--scalable, relocatable flowers, can change colours too 
garden :  Entity coordinates
garden = Scene3d.group 
    ( List.map 
            -- colour, scale, (x,y) coordinate
            ( \(colour, scale,(x,y))-> flower (0,(outerPetal colour),(innerPetal colour))
                    |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) scale
                    |> Scene3d.translateBy (Vector3d.centimeters x y 0)
            )
            [ (0.7, 0.8 ,(100,-100))
            -- , (0.85, 0.7, (-500,234))
            -- , (0.6, 1.2, (-234,-234))
            , (0.2, 0.5, (1400,90))
            , (1, 1, (-939, 231))
            , (0.5, 0.6, (-100,0))
            -- , (0.1, 0.1, (432,666))
            -- , (0.2, 0.2, (20,-330))
            -- , (0.3, 0.3, (100,100))
            -- , (0.4, 0.4, (55,-234))
            -- , (0.5, 0.5, (2000,0))
            -- , (0.6, 0.6, (1000,1000))
            -- , (0.7, 0.7, (1000,-1000))
            -- , (0.8, 0.8, (-1000,-1000))
            ]
    )    

{-- Build a bee --}

bee : Float -> Entity coordinates
bee time = Scene3d.group [ 
    black 
       |> Scene3d.translateBy (Vector3d.centimeters 0 -8 0)
    ,
    yellow   
       |> Scene3d.translateBy (Vector3d.centimeters 0 -8 0)
    , black
    , yellow
    , black 
       |> Scene3d.translateBy (Vector3d.centimeters 0 8 0)
    , bum
       |> Scene3d.translateBy (Vector3d.centimeters 0 -4 0)
    , face
    , sting
       |> Scene3d.translateBy (Vector3d.centimeters 0 20 30)
    , eye
       |> Scene3d.translateBy (Vector3d.centimeters -4 -15 33)
    , eye
       |> Scene3d.translateBy (Vector3d.centimeters 4 -15 33)
    , legGroup 
       |> Scene3d.rotateAround Axis3d.y (Angle.degrees 60)  
       |> Scene3d.translateBy (Vector3d.centimeters 5 0 21)
    , legGroup 
       |> Scene3d.rotateAround Axis3d.y (Angle.degrees -60)  
       |> Scene3d.translateBy (Vector3d.centimeters -8 0 15)
    , antenna
    , antenna 
       |> Scene3d.translateBy (Vector3d.centimeters 10 0 0)
    , mouth
       |> Scene3d.rotateAround Axis3d.x (Angle.degrees -90)  
       |> Scene3d.rotateAround Axis3d.y (Angle.degrees 45) 
       |> Scene3d.translateBy (Vector3d.centimeters 0 -16.3 30)
    , wing
       |> Scene3d.translateBy (Vector3d.centimeters 20 0 0)
       |> Scene3d.rotateAround Axis3d.y (Angle.degrees (-15*sin (10*time)))
       |> Scene3d.translateBy (Vector3d.centimeters 0 0 30)
    , wing
       |> Scene3d.translateBy (Vector3d.centimeters (-20) 0 0)
       |> Scene3d.rotateAround Axis3d.y (Angle.degrees (15*sin (10*time)))
       |> Scene3d.translateBy (Vector3d.centimeters 0 0 30)
   ]
       |> Scene3d.translateBy (Vector3d.centimeters 0 0 20)

black = prism Color.black (10,2,10)
    |> Scene3d.translateBy (Vector3d.centimeters 0 5 30)

yellow = prism Color.yellow (10,2,10)
    |> Scene3d.translateBy (Vector3d.centimeters 0 9 30)
    
bum = cube Color.black 8
    |> Scene3d.translateBy (Vector3d.centimeters 0 18 30)

face = cube Color.yellow 9
    |> Scene3d.translateBy (Vector3d.centimeters 0 -7 30)

wing = cylinder (Color.rgba 0.9 0.9 0.9 0.5) Z (0,1,15)

sting = cone Color.darkGrey Y (2,10,2)

eye = cube Color.black 2

leg = cylinder Color.black X (0,7,0.7)

antenna = Scene3d.group [
    (Scene3d.mesh (Material.nonmetal {baseColor=Color.black, roughness=0.5})
        <| ringMesh 7 0.7)
        |> Scene3d.rotateAround Axis3d.y (Angle.degrees -90)  
        |> Scene3d.translateBy (Vector3d.centimeters -5 -18 37)
    ,
    sphere Color.black 2
        |> Scene3d.translateBy (Vector3d.centimeters -5 -18 45)
    ]

mouth = (Scene3d.mesh (Material.nonmetal {baseColor=Color.black, roughness=0.5})
          <| ringMesh 5 0.7)

legGroup = Scene3d.group [
       leg 
       |> Scene3d.translateBy (Vector3d.centimeters 0 -2 0)
    ,
       leg
       |> Scene3d.translateBy (Vector3d.centimeters 0 5 0)
    ,
       leg 
       |> Scene3d.translateBy (Vector3d.centimeters 0 12 0)
    ] 


{-- Flower hit sign --}

{-- boundingBox --}
 
hitsign textureList = 
  case textureList of
    Just texture ->
      Scene3d.quad (Material.texturedColor (texture))
                   ( Point3d.xyz (Length.centimeters 5) (Length.centimeters 0) (Length.centimeters 75))
                   ( Point3d.xyz (Length.centimeters -5) (Length.centimeters 0) (Length.centimeters 75))
                   ( Point3d.xyz (Length.centimeters -5) (Length.centimeters 0) (Length.centimeters 85))
                   ( Point3d.xyz (Length.centimeters 5) (Length.centimeters 0) (Length.centimeters 85))
    _ ->
      Scene3d.quad (Material.matte Color.blue)
                   ( Point3d.xyz (Length.centimeters 10) (Length.centimeters 0) (Length.centimeters 75))
                   ( Point3d.xyz (Length.centimeters -10) (Length.centimeters 0) (Length.centimeters 75))
                   ( Point3d.xyz (Length.centimeters -10) (Length.centimeters 0) (Length.centimeters 95))
                   ( Point3d.xyz (Length.centimeters 10) (Length.centimeters 0) (Length.centimeters 95))


-- boundingBox for ring
ringBoundingBox : Float -> Float -> BoundingBox3d Meters coordinates
ringBoundingBox radius thickness = 
    let
        cx = 0
        cy = 0
        cz = 0
    in
    BoundingBox3d.fromExtrema
        { minX = Length.centimeters (cx - radius)
        , maxX = Length.centimeters (cx + radius)
        , minY = Length.centimeters (cy - radius)
        , maxY = Length.centimeters (cy + radius)
        , minZ = Length.centimeters (cz - thickness)
        , maxZ = Length.centimeters (cz + thickness)
        }

-- BoundingBox for tree
leafBoundingBox : BoundingBox3d Meters coordinates
leafBoundingBox  = 
  let
    leafApproximate = Cylinder3d.along Axis3d.z
                        { start = Length.centimeters 0
                        , end = Length.centimeters 50
                        , radius = Length.centimeters 35
                        } 
                        |> Cylinder3d.translateBy (Vector3d.centimeters -50 -70 15)
  in
    Cylinder3d.boundingBox leafApproximate
rootBoundingBox : BoundingBox3d Meters coordinates 
rootBoundingBox = 
  let 
    root = Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 80
                    , radius = Length.centimeters 5
                    } 
              |> Cylinder3d.translateBy (Vector3d.centimeters -30 -50 0)
  in Cylinder3d.boundingBox root

-- BoundingBox for flower
stemBoundingBox : BoundingBox3d Meters coordinates 
stemBoundingBox =
  let
     stemShape = Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 50 
                    , radius = Length.centimeters 7
                    }
  in Cylinder3d.boundingBox stemShape

flowerMiddleBounedBox : BoundingBox3d Meters coordinates 
flowerMiddleBounedBox = 
  let
      middelShape = 
        Sphere3d.withRadius (Length.centimeters 17) Point3d.origin
                |> Sphere3d.translateBy (Vector3d.centimeters 0 0 55)
  in Sphere3d.boundingBox middelShape


-- BoundingBox for bee
beeBoundingBox : BoundingBox3d Meters coordinates 
beeBoundingBox = 
  let 
    beeBodyShaprBase = 
       Block3d.with
          { x1 = Length.centimeters -2
          , x2 = Length.centimeters 2
          , y1 = Length.centimeters -2
          , y2 = Length.centimeters 2
          , z1 = Length.centimeters 4
          , z2 = Length.centimeters -4
          }
    bbsb1 = beeBodyShaprBase 
              |> Block3d.translateBy (Vector3d.centimeters 0 5 50)
    wingShape = Cylinder3d.along Axis3d.z
                    { start = Length.centimeters 0
                    , end = Length.centimeters 0.5 
                    , radius = Length.centimeters 10
                    }
    wing1 = wingShape 
              |> Cylinder3d.translateBy (Vector3d.centimeters 15 0 40)
    wing2 = wingShape 
              |> Cylinder3d.translateBy (Vector3d.centimeters -15 0 40) 
  in
    BoundingBox3d.aggregate 
      (Block3d.boundingBox bbsb1) 
          [ Cylinder3d.boundingBox wing1
           ,Cylinder3d.boundingBox wing2
          ]

{-- Shape bouned with bounded box --}
-- SAB stands for shape and bounded box
cubeSAB : Color.Color -> Float
           -> {shape : Entity coordinates
              , boundingBox : BoundingBox3d Meters coordinates
              }
cubeSAB colour size = 
   let 
     cubeShape = Block3d.from
                  (Point3d.centimeters -size -size -size)
                  (Point3d.centimeters size size size)
   in {
    shape = Scene3d.blockWithShadow (myMat colour) cubeShape
    , boundingBox = Block3d.boundingBox cubeShape
  }

prismSAB : Color.Color -> Dimension
            -> {shape : Entity coordinates
              , boundingBox : BoundingBox3d Meters coordinates
              }
prismSAB colour (x,y,z) = 
  let
    primShape = Block3d.with
                    { x1 = Length.centimeters -x
                    , x2 = Length.centimeters x
                    , y1 = Length.centimeters -y
                    , y2 = Length.centimeters y
                    , z1 = Length.centimeters -z
                    , z2 = Length.centimeters z
                    }
  in {
      shape = Scene3d.blockWithShadow (myMat colour) primShape
      , boundingBox = Block3d.boundingBox primShape
  }

sphereSAB : Color.Color -> Float
            -> {shape : Entity coordinates
              , boundingBox : BoundingBox3d Meters coordinates
              }
sphereSAB colour radius = 
  let
    sphereShape = Sphere3d.withRadius (Length.centimeters radius) Point3d.origin
  in {shape = Scene3d.sphereWithShadow (myTexturedMat colour) sphereShape
     , boundingBox = Sphere3d.boundingBox sphereShape
  }

coneSAB : Color.Color -> Axis -> Dimension
          -> {shape : Entity coordinates
              , boundingBox : BoundingBox3d Meters coordinates
              }
coneSAB colour axis (b,t,r) = 
  let
    along = 
       case axis of 
          X -> Axis3d.x 
          Y -> Axis3d.y 
          Z -> Axis3d.z 
    coneShape = Cone3d.along along
                    { base = Length.centimeters b
                    , tip = Length.centimeters t
                    , radius = Length.centimeters r
                    }
  in {shape = Scene3d.coneWithShadow (myMat colour) coneShape
    , boundingBox = Cone3d.boundingBox coneShape
     }

cylinderSAB : Color.Color -> Axis -> Dimension
           -> {shape : Entity coordinates
              , boundingBox : BoundingBox3d Meters coordinates
              }
cylinderSAB colour axis (s,e,r) =
  let
     along = 
       case axis of 
          X -> Axis3d.x 
          Y -> Axis3d.y 
          Z -> Axis3d.z  
     cylinderShape = Cylinder3d.along along
                    { start = Length.centimeters s
                    , end = Length.centimeters e 
                    , radius = Length.centimeters r
                    }
  in {shape = Scene3d.cylinderWithShadow  (myMat colour) cylinderShape
    , boundingBox = Cylinder3d.boundingBox cylinderShape
     }

{-- Not sure about the type for the meterial
ringSAB : Material coordinates attributes
        -> Float -> Float
        -> {shape : Entity coordinates
            , boundingBox : BoundingBox3d Meters coordinates
            }
--}
ringSAB material radius thickness =
  let
    ringShape = ringMesh radius thickness
  in {shape = Scene3d.mesh material ringShape
    , boundingBox = ringBoundingBox radius thickness
     }

treeSAB : Float
      -> {shape : Entity coordinates
        , boundingBox : BoundingBox3d Meters coordinates
        }
treeSAB tipY = 
  { shape = tree tipY
   , boundingBox = rootBoundingBox
      {--  BoundingBox3d.union 
            (leafBoundingBox) rootBoundingBox --}
  }

forestSAB : Float 
        -> {shape : Entity coordinates
        , boundingBox : List (BoundingBox3d Meters coordinates)
        }
forestSAB t =
  let
    treeShape =  --A list
       ( List.map 
            -- scale, (x,y) coordinate
            ( \(s,(x,y))-> treeSAB (4*sin t + 20) 
                                |> scaleAbout (Point3d.centimeters 0 0 0) s
                                |> translateBy (Vector3d.centimeters x y 0)
            )
            [ (3.5 ,(100,-100))
              , (5, (-500,234))
              , (2, (-234,234))
              , (3, (2222,3434))
              , (1, (-939, 231))
              , (5, (110,110))
            ]
       )
    treeShapeGroup = Scene3d.group 
      (List.map
         (\treeRecord -> treeRecord.shape)
         treeShape
      )
    treeBoundingBoxList = 
      (List.map
         (\treeRecord -> treeRecord.boundingBox)
         treeShape
      )
  in {shape = treeShapeGroup
    , boundingBox = treeBoundingBoxList
     }

flowerSAB : (Float, Entity coordinates , Entity coordinates)
    -> {shape : Entity coordinates
        , boundingBox : BoundingBox3d Meters coordinates
        }
flowerSAB (angle,outp,inp) = 
  { shape = flower (angle,outp,inp)
  , boundingBox = BoundingBox3d.union
                   stemBoundingBox flowerMiddleBounedBox
  }

gardenSAB : {shape : Entity coordinates
           , boundingBox : List (BoundingBox3d Meters coordinates)
            }
gardenSAB = 
  let
    flowerShape = 
       List.map 
            -- colour, scale, (x,y) coordinate
            ( \(colour, scale,(x,y))-> flowerSAB (0,(outerPetal colour),(innerPetal colour))
                    |> scaleAbout (Point3d.centimeters 0 0 0) scale
                    |> translateBy (Vector3d.centimeters x y 0)
            )
            [ (0.7, 0.8 ,(100,-100))
            , (0.2, 0.7, (1400,90))
            , (1, 1, (-939, 231))
            , (0.5, 0.6, (-100,0))
            ]  
    flowerShapeGroup = Scene3d.group 
      (List.map
         (\flowerRecord -> flowerRecord.shape)
         flowerShape
      )
    flowerBoundingBoxList = 
      (List.map
         (\flowerRecord -> flowerRecord.boundingBox)
         flowerShape
      )
  in {shape = flowerShapeGroup
    , boundingBox = flowerBoundingBoxList
     }

beeSAB : Float
      -> {shape : Entity coordinates
        , boundingBox : BoundingBox3d Meters coordinates
        }
beeSAB time = {shape = bee time
   , boundingBox = beeBoundingBox
   }

--No collision detection for cloud 
cloudGroup : Entity coordinates 
cloudGroup = Scene3d.group (
            List.map 
                (\(x,y,z) -> cloud (x,y,z))
                [(0,0,700)
                ,(500,100,500)
                ]
            )

cloud : (Float,Float,Float) -> Entity coordinates 
cloud (x,y,z)= Scene3d.group [cloudPart
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
                      |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) 5
                      |> Scene3d.translateBy (Vector3d.centimeters x y z)
--TODO: change opacity when hsla/rgba library changes 
cloudPart : Entity coordinates 
cloudPart = sphere Color.white 5

{-Material-}
--non-metal material
myMat : Color.Color -> MaterialUniform coordinates
myMat colour = Material.nonmetal { baseColor = colour, roughness = 0.2 }

--textured non-metal material (some shapes require textured material? )
myTexturedMat : Color.Color -> MaterialTextured coordinates
myTexturedMat colour = Material.matte colour

{-Transformation for the shaped which bouned with bounded box-}

-- scaleAbout, translateBy, translateIn

scaleAbout : Point3d Meters coordinates -> Float 
   -> ShapeSAB coordinates 
   -> ShapeSAB coordinates
scaleAbout centerPoint scale shapeSAB = 
  let 
    newShape = shapeSAB.shape 
                 |> Scene3d.scaleAbout centerPoint scale
    newBox = shapeSAB.boundingBox 
                 |> BoundingBox3d.scaleAbout centerPoint scale
  in { shape = newShape
    , boundingBox = newBox
     }
translateBy : Vector3d Meters coordinates
   -> ShapeSAB coordinates 
   -> ShapeSAB coordinates
translateBy displacement shapeSAB = 
  let 
    newShape = shapeSAB.shape 
                 |> Scene3d.translateBy displacement
    newBox = shapeSAB.boundingBox 
                 |> BoundingBox3d.translateBy displacement
  in { shape = newShape
    , boundingBox = newBox
     }

translateIn : Direction3d coordinates -> Length
   -> ShapeSAB coordinates 
   -> ShapeSAB coordinates
translateIn direction distance shapeSAB =
  let 
    newShape = shapeSAB.shape 
                 |> Scene3d.translateIn direction distance
    newBox = shapeSAB.boundingBox 
                 |> BoundingBox3d.translateIn direction distance
  in { shape = newShape
    , boundingBox = newBox
     }

rotateAround : Axis3d Meters coordinates -> Angle
   -> ShapeSAB coordinates 
   -> ShapeSAB coordinates
rotateAround axis angle shapeSAB =
  let 
    newShape = shapeSAB.shape 
                 |> Scene3d.rotateAround axis angle
    bbToBlock = 
      Block3d.with
        {
          x1 = BoundingBox3d.minX shapeSAB.boundingBox 
        , y1 = BoundingBox3d.minY shapeSAB.boundingBox 
        , z1 = BoundingBox3d.minZ shapeSAB.boundingBox 
        , x2 = BoundingBox3d.maxX shapeSAB.boundingBox 
        , y2 = BoundingBox3d.maxY shapeSAB.boundingBox 
        , z2 = BoundingBox3d.maxZ shapeSAB.boundingBox 
        }
    rotateBlock = bbToBlock
                    |> Block3d.rotateAround axis angle
      
  in { shape = newShape
    , boundingBox = Block3d.boundingBox rotateBlock
     }

--- Testing the shape
main : Html msg
main =
    let
        -- Create a single rectangle from its color and four vertices
        -- (Scene3d.quad can be used to create any flat four-sided shape)
        square =
            Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)

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
        material = Material.matte Color.yellow
    in
    -- Render a scene that doesn't involve any lighting (no lighting is needed
    -- here since we provided a material that will result in a constant color
    -- no matter what lighting is used)
    Scene3d.unlit
        { -- Our scene has a single 'entity' in it
          entities = [ square
           ,  cylinder (Color.rgba 0.9 0.9 0.9 0.5) Z (0,1,15) ]

        -- Provide the camera to be used when rendering the scene
        , camera = camera

        -- Anything closer than 1 meter to the camera will be clipped away
        -- (this is necessary because of the internals of how WebGL works)
        , clipDepth = Length.meters 1

        -- Using a transparent background means that the HTML underneath the
        -- scene will show through
        , background = Scene3d.transparentBackground

        -- Size in pixels of the generated HTML element
        , dimensions = ( Pixels.int 400, Pixels.int 300 )
        }
