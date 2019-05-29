module Main exposing (main)

import Browser
import Context exposing (Context(..))
import Draggable
import Draggable.Events
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Set


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { context : Context
    , dragState : DragState
    , drag : Draggable.State CellCoord
    }


type DragState
    = NotDragging
      -- It's not yet clear if user wants to drag column horizontally
      -- or row vertically, so just accumulate delta until it's big enough to decide
    | DirectionUndecided CellCoord Draggable.Delta
    | Horizontal ColIdx Float
    | Vertical RowIdx Float


type alias CellCoord =
    ( RowIdx, ColIdx )


type ColIdx
    = ColIdx Int


type RowIdx
    = RowIdx Int


type Msg
    = OnDragStart CellCoord
    | OnDragBy Draggable.Delta
    | CellClicked CellCoord
    | DragMsg (Draggable.Msg CellCoord)
    | DragEnd


testContext : Context
testContext =
    Context
        { relation = Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 1 ), ( 1, 2 ), ( 2, 2 ), ( 3, 0 ), ( 3, 3 ) ]
        , rows = 3
        , cols = 3
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { context = testContext
      , dragState = NotDragging
      , drag = Draggable.init
      }
    , Cmd.none
    )


dragConfig : Draggable.Config CellCoord Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragStart OnDragStart
        , Draggable.Events.onDragBy OnDragBy
        , Draggable.Events.onDragEnd DragEnd
        , Draggable.Events.onClick CellClicked
        ]


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


updateDragState : Draggable.Delta -> DragState -> DragState
updateDragState ( dx, dy ) dragState =
    case dragState of
        NotDragging ->
            NotDragging

        DirectionUndecided ( rowIdx, colIdx ) ( dx0, dy0 ) ->
            let
                ( dx1, dy1 ) =
                    ( dx0 + dx, dy0 + dy )
            in
            if abs dx1 < 5 && abs dy1 < 5 then
                DirectionUndecided ( rowIdx, colIdx ) ( dx1, dy1 )

            else if abs dx1 < abs dy1 then
                Vertical rowIdx dy1

            else
                Horizontal colIdx dx1

        Horizontal colIdx dx0 ->
            Horizontal colIdx (dx0 + dx)

        Vertical rowIdx dy0 ->
            Vertical rowIdx (dy0 + dy)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDragStart cellCoord ->
            ( { model | dragState = DirectionUndecided cellCoord ( 0, 0 ) }
            , Cmd.none
            )

        OnDragBy delta ->
            ( { model | dragState = updateDragState delta model.dragState }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        CellClicked cellCoord ->
            ( { model | context = toggleCell cellCoord model.context }
            , Cmd.none
            )

        DragEnd ->
            let
                swap =
                    determineSwap model.dragState
            in
            ( { model
                | dragState = NotDragging
                , context = applySwap swap model.context
              }
            , Cmd.none
            )


view : Model -> Html Msg
view { context, dragState } =
    Element.layout [] (grid context dragState)


grid : Context -> DragState -> Element Msg
grid (Context c) dragState =
    List.range 0 c.rows
        |> List.map (\rowIdx -> gridRow (Context c) dragState rowIdx)
        |> Element.column [ Border.width 1 ]


gridRow : Context -> DragState -> Int -> Element Msg
gridRow (Context c) dragState rowIdx =
    let
        verticalOffset =
            case dragState of
                NotDragging ->
                    0

                DirectionUndecided _ _ ->
                    0

                Horizontal _ _ ->
                    0

                Vertical (RowIdx draggedRowIdx) offset ->
                    calculateOffset rowIdx draggedRowIdx offset
    in
    List.range 0 c.cols
        |> List.map (\colIdx -> gridCell (Context c) dragState rowIdx colIdx)
        |> Element.row [ Element.moveDown verticalOffset ]


gridCell : Context -> DragState -> Int -> Int -> Element Msg
gridCell (Context c) dragState rowIdx colIdx =
    let
        horizontalOffset =
            case dragState of
                NotDragging ->
                    0

                DirectionUndecided _ _ ->
                    0

                Vertical _ _ ->
                    0

                Horizontal (ColIdx draggedColIdx) offset ->
                    calculateOffset colIdx draggedColIdx offset
    in
    Element.el
        [ Border.width 1
        , Element.width (Element.px cellSize)
        , Element.height (Element.px cellSize)
        , Element.centerX
        , Element.alignTop
        , Element.htmlAttribute <| Draggable.mouseTrigger ( RowIdx rowIdx, ColIdx colIdx ) DragMsg
        , Element.moveRight horizontalOffset
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            , Font.color (Element.rgb 0 0.5 0)
            , Font.size 40
            ]
        <|
            Element.text <|
                if Set.member ( rowIdx, colIdx ) c.relation then
                    "✓"

                else
                    ""


calculateOffset : Int -> Int -> Float -> Float
calculateOffset index draggedIndex offset =
    if draggedIndex == index then
        offset
        -- dragging up (left) - rows (cols) before the dragged one will move down (right) to make room for it

    else if index < draggedIndex && offset < 0 && draggedIndex + getIndexOffset offset <= index then
        cellSizeFloat
        -- dragging down (right)- rows (cols) after the dragged one will move up (left) to make room for it

    else if draggedIndex < index && 0 < offset && index <= draggedIndex + getIndexOffset offset then
        -cellSizeFloat

    else
        0


{-| Convert offset in pixels, to offset expressed in number of cells.
E.g. if I dragged -260 pixels and cell size is 100, the offset is -3 cells.
-}
getIndexOffset : Float -> Int
getIndexOffset offset =
    round (offset + signum offset * cellSizeFloat / 2) // cellSize


signum : Float -> Float
signum x =
    case compare x 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


cellSize : Int
cellSize =
    60


cellSizeFloat : Float
cellSizeFloat =
    toFloat cellSize


{-| Represents exchange of columns / rows to be performed
-}
type Swap
    = NoSwap
    | SwapRows Int Int
    | SwapColumns Int Int


determineSwap : DragState -> Swap
determineSwap dragState =
    case dragState of
        NotDragging ->
            NoSwap

        DirectionUndecided _ _ ->
            NoSwap

        Horizontal (ColIdx draggedIdx) offset ->
            case getIndexOffset offset of
                0 ->
                    NoSwap

                nonzeroIndexOffset ->
                    SwapColumns draggedIdx (draggedIdx + nonzeroIndexOffset)

        Vertical (RowIdx draggedIdx) offset ->
            case getIndexOffset offset of
                0 ->
                    NoSwap

                nonzeroIndexOffset ->
                    SwapRows draggedIdx (draggedIdx + nonzeroIndexOffset)


applySwap : Swap -> Context -> Context
applySwap swap (Context c) =
    case swap of
        NoSwap ->
            Context c

        SwapRows a b ->
            Context
                { c | relation = Set.map (\( x, y ) -> ( swapInts a b x, y )) c.relation }

        SwapColumns a b ->
            Context
                { c | relation = Set.map (\( x, y ) -> ( x, swapInts a b y )) c.relation }


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
