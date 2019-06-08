module Main exposing (main)

import Browser
import Context exposing (CellCoord, ColIdx(..), Context, RowIdx(..), Swap(..))
import Draggable
import Draggable.Events
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


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


type Msg
    = OnDragStart CellCoord
    | OnDragBy Draggable.Delta
    | CellClicked CellCoord
    | DragMsg (Draggable.Msg CellCoord)
    | DragEnd
    | AddRow
    | AddColumn
    | RemoveRow
    | RemoveColumn


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { context = Context.init
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
            ( { model | context = Context.toggleCell cellCoord model.context }
            , Cmd.none
            )

        DragEnd ->
            let
                swap =
                    determineSwap model.dragState
            in
            ( { model
                | dragState = NotDragging
                , context = Context.applySwap swap model.context
              }
            , Cmd.none
            )

        AddRow ->
            ( { model | context = Context.addRow model.context }
            , Cmd.none
            )

        AddColumn ->
            ( { model | context = Context.addColumn model.context }
            , Cmd.none
            )

        RemoveRow ->
            ( { model | context = Context.removeRow model.context }
            , Cmd.none
            )

        RemoveColumn ->
            ( { model | context = Context.removeColumn model.context }
            , Cmd.none
            )


view : Model -> Html Msg
view { context, dragState } =
    Element.layout [] <|
        Element.column []
            [ gridControls
            , grid context dragState
            ]


gridControls : Element Msg
gridControls =
    let
        button : Msg -> String -> Element Msg
        button msg label =
            Input.button
                [ Element.padding 4
                , Border.solid
                , Border.width 2
                , Border.color (Element.rgb255 0 0 0)
                , Background.color (Element.rgb255 0 140 186)
                , Border.rounded 5
                , Font.color (Element.rgb 1 1 1)
                , Font.size 13
                ]
                { onPress = Just msg
                , label = Element.text label
                }
    in
    Element.row
        [ Element.spacing 10
        , Element.padding 10
        ]
        [ button RemoveRow "- Row"
        , button AddRow "+ Row"
        , button RemoveColumn "- Column"
        , button AddColumn "+ Column"
        ]


grid : Context -> DragState -> Element Msg
grid c dragState =
    List.range 0 (Context.objectCount c)
        |> List.map (\rowIdx -> gridRow c dragState rowIdx)
        |> Element.column [ Border.width 1 ]


gridRow : Context -> DragState -> Int -> Element Msg
gridRow c dragState rowIdx =
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
    List.range 0 (Context.attributeCount c)
        |> List.map (\colIdx -> gridCell c dragState rowIdx colIdx)
        |> Element.row [ Element.moveDown verticalOffset ]


gridCell : Context -> DragState -> Int -> Int -> Element Msg
gridCell context dragState rowIdx colIdx =
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
                if Context.inRelation (RowIdx rowIdx) (ColIdx colIdx) context then
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
