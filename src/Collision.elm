module Collision exposing ( noCollide
  , changeBBoxName
  , getCollisions, getNonCollidingObjects
  , isColliding
  , isCollidingByName, getCollisionsByName
  )

import BoundingBox3d exposing (BoundingBox3d)
import Length exposing (Meters)
import Wrapper3D exposing (..)
import Point3d
import WebGL exposing (entity)

{-| Removes the bounding box of an object, so it won't collide with anything. -}
noCollide : EntityBBox coordinates -> EntityBBox coordinates
noCollide entityBBox = 
  case entityBBox of
    ObjectGroup listObj bBox name ->
      ObjectGroup listObj NoBox name
    Object object -> Object {shape = object.shape
                         , boundingBox = NoBox
                         , name = object.name }

{-- Return a EntityBBox that contains a new name --}
changeBBoxName : EntityBBox coordinates -> String -> EntityBBox coordinates
changeBBoxName entityBBox newName = 
  case entityBBox of
    ObjectGroup listObj bBox name ->
      ObjectGroup listObj bBox newName
    Object object -> Object {shape = object.shape
                         , boundingBox = object.boundingBox
                         , name = newName }


{-| Returns a list of all objects in `entity_list` that collide with `src` -}
getCollisions : EntityBBox coordinates
           -> List (EntityBBox coordinates)
           -> List (EntityBBox coordinates)
getCollisions src entity_list = 
  case src of 
    ObjectGroup listObj bBox name ->
      case bBox of
        Box _ -> 
          getCollisionsHelper src entity_list
        NoBox -> []
    Object object ->
      case object.boundingBox of 
        Box _ ->
          getCollisionsHelper src entity_list
        NoBox -> []

getCollisionsHelper : EntityBBox coordinates
           -> List (EntityBBox coordinates)
           -> List (EntityBBox coordinates)

getCollisionsHelper src entity_list = 
  let
    srcBbox = 
      case src of
        ObjectGroup _ boundingBox _ ->
          case boundingBox of
            Box bBox ->
              bBox
            -- This case shouldn't be reached here since the main function handles this case already
            NoBox ->
              BoundingBox3d.singleton Point3d.origin
        Object attributes ->
          case attributes.boundingBox of
            Box bBox ->
              bBox
            -- This case shouldn't be reached here since the main function handles this case already
            NoBox ->
              BoundingBox3d.singleton Point3d.origin
  in
    case src of
      ObjectGroup objects bBox _ ->
        getCollisionsBetweenObjects objects entity_list
      Object _ ->
        List.foldr (\entity otherEntity-> 
          case entity of
            ObjectGroup listObj bBox name ->
              case bBox of 
                Box entity_bbox -> 
                  -- Check if two boxs are collided
                  if (BoundingBox3d.intersects srcBbox entity_bbox) then
                    getCollisionsHelper src listObj
                  else otherEntity
                NoBox -> otherEntity
            Object object ->
              case object.boundingBox of 
                Box entity_bbox ->
                  -- Check if two boxs are collided
                  if (BoundingBox3d.intersects srcBbox entity_bbox) then
                    [Object object] ++ otherEntity
                  else otherEntity
                NoBox -> otherEntity
            ) [] entity_list

{-| Given two lists of objects, returns only the objects from `possibleCollisions` that are colliding with objects from `sourceObjects` -}
getCollisionsBetweenObjects : List (EntityBBox coordinates) -> List (EntityBBox coordinates) -> List (EntityBBox coordinates)
getCollisionsBetweenObjects sourceObjects possibleCollisions = 
    case sourceObjects of
        [] ->
            []
        (object :: rest) ->
            getCollisions object possibleCollisions ++ getCollisionsBetweenObjects rest possibleCollisions


{-| Returns a list of all objects in `entity_list` that do not collide with `src` -}
getNonCollidingObjects : EntityBBox coordinates
           -> List (EntityBBox coordinates)
           -> List (EntityBBox coordinates)
getNonCollidingObjects src entity_list = 
  case src of 
    ObjectGroup listObj bBox name ->
      case bBox of
        Box entity_bbox -> 
          getNonCollidingObjectsHelper entity_bbox entity_list
        NoBox -> entity_list
    Object object ->
      case object.boundingBox of 
        Box entity_bbox ->
          getNonCollidingObjectsHelper entity_bbox entity_list
        NoBox -> entity_list

getNonCollidingObjectsHelper : BoundingBox3d Meters coordinates 
           -> List (EntityBBox coordinates)
           -> List (EntityBBox coordinates)
getNonCollidingObjectsHelper srcBbox entity_list = 
  List.foldr (\entity otherEntity-> 
     case entity of
       ObjectGroup listObj bBox name ->
         case bBox of 
           Box entity_bbox -> 
             -- Check if two boxs are collided
             if (BoundingBox3d.intersects srcBbox entity_bbox) then
               getNonCollidingObjectsHelper srcBbox listObj
             else listObj
           NoBox -> otherEntity
       Object object ->
         case object.boundingBox of 
           Box entity_bbox ->
             -- Check if two boxs are collided
             if (BoundingBox3d.intersects srcBbox entity_bbox) then
               otherEntity
             else [Object object] ++ otherEntity
           NoBox -> otherEntity
       ) [] entity_list

{-| Checks if any objects in `entity_list` collide with `src` and returns the result, as a `Bool` -}
isColliding : EntityBBox coordinates
           -> List (EntityBBox coordinates)
           -> Bool
isColliding src entity_list = 
  (List.length (getCollisions src entity_list)) >= 1 

{-| Checks if any objects in `entity_list` called `targetName` collide with `src` and returns the result, as a `Bool` -}
isCollidingByName : EntityBBox coordinates
           -> List (EntityBBox coordinates)
           -> String
           -> Bool
isCollidingByName src entity_list targetName = 
  List.any (\collided_obj -> 
     case collided_obj of
       ObjectGroup listObj bBox name -> name == targetName
       Object object -> object.name == targetName
     )
    <| getCollisions src entity_list

{-| Returns a list of all objects named `targetName` in `entity_list` that collide with `src -}
getCollisionsByName : EntityBBox coordinates
           -> List (EntityBBox coordinates)
           -> String
           -> List (EntityBBox coordinates)
getCollisionsByName src entity_list targetName = 
  List.filter (\collided_obj -> 
     case collided_obj of
       ObjectGroup listObj bBox name -> name == targetName
       Object object -> object.name == targetName
     ) 
    <| getCollisions src entity_list
