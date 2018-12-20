module Context exposing (Concept(..), Context(..))

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
type Context o a
    = Context
        { objects : Set a
        , attributes : Set a
        , relation : Set ( o, a )
        }


type Concept o a
    = Concept
        { extent : Set o
        , intent : Set a
        }
