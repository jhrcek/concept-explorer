module Context exposing
    ( CellCoord
    , ColIdx(..)
    , Context
    , RowIdx(..)
    , Swap(..)
    , addColumn
    , addRow
    , applySwap
    , attributeCount
    , getAttributeName
    , getObjectName
    , inRelation
    , init
    , objectCount
    , removeColumn
    , removeRow
    , setAttributeName
    , setObjectName
    , toggleCell
    )

import Array exposing (Array)
import Set exposing (Set)


{-| What can be done with a context?

  - Add an object (+ say which attributes it has)
  - Add an attribute (+ say which objects have it)
  - Ask: what attributes does object have?
  - Ask: which objects have this attribute?
  - Ask: which concepts are in the context?
  - Ask: is given object / attribute part of given concept?
  - Ask: is concept A subconcept of B?

---

Unrelated to context, but related to visual representation:

  - reorder objects
  - toggle object / attribute pair

-}
type Context
    = Context
        { relation : Set ( Int, Int )
        , objects : Array String
        , attributes : Array String
        }


setAttributeName : ColIdx -> String -> Context -> Context
setAttributeName (ColIdx colIdx) newName (Context c) =
    Context { c | attributes = Array.set colIdx newName c.attributes }


setObjectName : RowIdx -> String -> Context -> Context
setObjectName (RowIdx rowIdx) newName (Context c) =
    Context { c | objects = Array.set rowIdx newName c.objects }


getAttributeName : ColIdx -> Context -> Maybe String
getAttributeName (ColIdx colIdx) (Context c) =
    Array.get colIdx c.attributes


getObjectName : RowIdx -> Context -> Maybe String
getObjectName (RowIdx rowIdx) (Context c) =
    Array.get rowIdx c.objects


inRelation : RowIdx -> ColIdx -> Context -> Bool
inRelation (RowIdx rowIdx) (ColIdx colIdx) (Context c) =
    Set.member ( rowIdx, colIdx ) c.relation


objectCount : Context -> Int
objectCount (Context c) =
    Array.length c.objects


attributeCount : Context -> Int
attributeCount (Context c) =
    Array.length c.attributes


addRow : Context -> Context
addRow (Context c) =
    let
        objectName =
            "Object " ++ String.fromInt (Array.length c.objects)
    in
    Context { c | objects = Array.push objectName c.objects }


addColumn : Context -> Context
addColumn (Context c) =
    let
        attributeName =
            "Attribute " ++ String.fromInt (Array.length c.attributes)
    in
    Context { c | attributes = Array.push attributeName c.attributes }


removeRow : Context -> Context
removeRow (Context c) =
    let
        newHeight =
            Array.length c.objects - 1
    in
    Context
        { c
            | objects = Array.slice 0 newHeight c.objects
            , relation = Set.filter (\( x, _ ) -> x < newHeight) c.relation
        }


removeColumn : Context -> Context
removeColumn (Context c) =
    let
        newWidth =
            Array.length c.attributes - 1
    in
    Context
        { c
            | attributes = Array.slice 0 newWidth c.attributes
            , relation = Set.filter (\( _, y ) -> y < newWidth) c.relation
        }


toggleCell : CellCoord -> Context -> Context
toggleCell ( RowIdx row, ColIdx col ) (Context ctx) =
    let
        newRelation =
            if Set.member ( row, col ) ctx.relation then
                Set.remove ( row, col ) ctx.relation

            else
                Set.insert ( row, col ) ctx.relation
    in
    Context { ctx | relation = newRelation }


applySwap : Swap -> Context -> Context
applySwap swap (Context c) =
    case swap of
        NoSwap ->
            Context c

        SwapRows a b ->
            Context
                { c
                    | relation = Set.map (\( x, y ) -> ( swapInts a b x, y )) c.relation
                    , objects = swapArray a b c.objects
                }

        SwapColumns a b ->
            Context
                { c
                    | relation = Set.map (\( x, y ) -> ( x, swapInts a b y )) c.relation
                    , attributes = swapArray a b c.attributes
                }


{-| Put `from` at the place of `to` and shift everything between by one to fill in the empty place
-}
swapInts : Int -> Int -> Int -> Int
swapInts from to x =
    if x == from then
        to

    else if from < x && x <= to then
        x - 1

    else if to <= x && x < from then
        x + 1

    else
        x


swapArray : Int -> Int -> Array a -> Array a
swapArray from to array =
    Array.indexedMap
        (\i a -> Maybe.withDefault a <| Array.get (swapInts to from i) array)
        array


type alias CellCoord =
    ( RowIdx, ColIdx )


type ColIdx
    = ColIdx Int


type RowIdx
    = RowIdx Int


{-| Represents exchange of columns / rows to be performed
-}
type Swap
    = NoSwap
    | SwapRows Int Int
    | SwapColumns Int Int


init : Context
init =
    Context
        { relation = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 1 ), ( 1, 2 ), ( 2, 2 ), ( 3, 0 ), ( 3, 3 ) ]
        , objects = Array.fromList [ "Object 0", "Object 1", "Object 2", "Object 3" ]
        , attributes = Array.fromList [ "Attribute 0", "Attribute 1", "Attribute 2", "Attribute 3" ]
        }
