module Context exposing
    ( Context(..)
    , addColumn
    , addRow
    , removeColumn
    , removeRow
    )

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
        , rows : Int
        , cols : Int
        }


addRow : Context -> Context
addRow (Context c) =
    Context { c | rows = c.rows + 1 }


addColumn : Context -> Context
addColumn (Context c) =
    Context { c | cols = c.cols + 1 }


removeRow : Context -> Context
removeRow (Context c) =
    Context
        { c
            | rows = Basics.max 0 (c.rows - 1)
            , relation = Set.filter (\( x, _ ) -> x < c.rows) c.relation
        }


removeColumn : Context -> Context
removeColumn (Context c) =
    Context
        { c
            | cols = Basics.max 0 (c.cols - 1)
            , relation = Set.filter (\( _, y ) -> y < c.cols) c.relation
        }
