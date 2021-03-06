module Main exposing (main)

import Array
import Browser
import Context exposing (CellCoord, ColIdx(..), Context, RowIdx(..), Swap(..))
import Draggable
import Draggable.Events
import Element exposing (Element, px)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra


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
    , drag : Draggable.State CellCoord
    , dragState : DragState
    , nameEditState : NameEditState
    }


type DragState
    = NotDragging
      -- It's not yet clear if user wants to drag column horizontally
      -- or row vertically, so just accumulate delta until it's big enough to decide
    | DirectionUndecided CellCoord Draggable.Delta
    | Horizontal ColIdx Float
    | Vertical RowIdx Float


type NameEditState
    = NotEditing
    | EditingRow RowIdx String
    | EditingColumn ColIdx String


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
    | NameEditStateChanged NameEditState
    | ObjectNameEditConfirmed RowIdx String
    | AttributeNameEditConfirmed ColIdx String


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { context = Context.init
      , drag = Draggable.init
      , dragState = NotDragging
      , nameEditState = NotEditing
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
                    determineSwap model.dragState (Context.objectCount model.context) (Context.attributeCount model.context)
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

        NameEditStateChanged newNameEditState ->
            ( { model | nameEditState = newNameEditState }
            , Cmd.none
            )

        ObjectNameEditConfirmed rowIdx newName ->
            ( { model
                | context = Context.setObjectName rowIdx newName model.context
                , nameEditState = NotEditing
              }
            , Cmd.none
            )

        AttributeNameEditConfirmed colIdx newName ->
            ( { model
                | context = Context.setAttributeName colIdx newName model.context
                , nameEditState = NotEditing
              }
            , Cmd.none
            )


view : Model -> Html Msg
view { context, dragState, nameEditState } =
    Element.layout [] <|
        Element.column []
            [ gridControls
            , grid context dragState nameEditState
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
                , Border.rounded 4
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


grid : Context -> DragState -> NameEditState -> Element Msg
grid context dragState nameEditState =
    Element.column []
        [ attributeNamesRow nameEditState dragState context
        , Element.row []
            [ objectNamesColumn nameEditState dragState context
            , Element.column [ Border.width 1 ] <|
                List.map (\rowIdx -> gridRow context dragState rowIdx) <|
                    List.range 0 (Context.objectCount context - 1)
            ]
        ]


gridRow : Context -> DragState -> Int -> Element Msg
gridRow context dragState rowIdx =
    List.range 0 (Context.attributeCount context - 1)
        |> List.map (\colIdx -> gridCell context dragState rowIdx colIdx)
        |> Element.row [ Element.moveDown <| verticalOffset dragState (RowIdx rowIdx) ]


attributeNamesRow : NameEditState -> DragState -> Context -> Element Msg
attributeNamesRow nameEditState dragState context =
    Context.attributeNames context
        |> Array.indexedMap (\rowIdx objectName -> attributeNameCell nameEditState dragState rowIdx objectName)
        |> Array.toList
        |> Element.column
            [ Border.width 1
            , Element.rotate (3 * pi / 2)
            , Element.htmlAttribute <| Html.Attributes.style "transform-origin" "bottom right"
            , Element.moveUp <| toFloat nameCellWidth
            , Element.moveRight <| 2 + cellSizeFloat * toFloat (Context.attributeCount context)
            ]


objectNamesColumn : NameEditState -> DragState -> Context -> Element Msg
objectNamesColumn nameEditState dragState context =
    Context.objectNames context
        |> Array.indexedMap (\rowIdx objectName -> objectNameCell nameEditState dragState rowIdx objectName)
        |> Array.toList
        |> Element.column [ Border.width 1 ]


objectNameCell : NameEditState -> DragState -> Int -> String -> Element Msg
objectNameCell nameEditState dragState rowIdx objectName =
    let
        width =
            Element.width <| px nameCellWidth

        height =
            Element.height <| px cellSize

        cellNotEdited =
            Element.el
                [ width
                , height
                , Border.width 1
                , Events.onDoubleClick <| NameEditStateChanged <| EditingRow (RowIdx rowIdx) objectName
                , Element.moveDown <| verticalOffset dragState (RowIdx rowIdx)
                ]
            <|
                Element.el [ Element.centerY, Element.centerX ] <|
                    Element.text objectName
    in
    case nameEditState of
        EditingRow (RowIdx editedRowIdx) currentText ->
            if rowIdx == editedRowIdx then
                Input.text
                    [ width
                    , height
                    , Element.moveDown <| verticalOffset dragState (RowIdx rowIdx)
                    , Element.htmlAttribute <| Html.Events.Extra.onEnter <| ObjectNameEditConfirmed (RowIdx editedRowIdx) currentText
                    , Background.color (Element.rgb255 220 220 220)
                    ]
                    { onChange = NameEditStateChanged << EditingRow (RowIdx editedRowIdx)
                    , text = currentText
                    , placeholder = Nothing
                    , label = Input.labelHidden "Object name"
                    }

            else
                cellNotEdited

        _ ->
            cellNotEdited


attributeNameCell : NameEditState -> DragState -> Int -> String -> Element Msg
attributeNameCell nameEditState dragState colIdx objectName =
    let
        width =
            Element.width <| px nameCellWidth

        height =
            Element.height <| px cellSize

        cellNotEdited =
            Element.el
                [ width
                , height
                , Border.width 1
                , Events.onDoubleClick <| NameEditStateChanged <| EditingColumn (ColIdx colIdx) objectName
                , Element.moveDown <| horizontalOffset dragState (ColIdx colIdx)
                ]
            <|
                Element.el [ Element.centerY, Element.centerX ] <|
                    Element.text objectName
    in
    case nameEditState of
        EditingColumn (ColIdx editedColIdx) currentText ->
            if colIdx == editedColIdx then
                Input.text
                    [ width
                    , height
                    , Element.moveDown <| horizontalOffset dragState (ColIdx colIdx)
                    , Element.htmlAttribute <| Html.Events.Extra.onEnter <| AttributeNameEditConfirmed (ColIdx editedColIdx) currentText
                    , Background.color (Element.rgb255 220 220 220)
                    ]
                    { onChange = NameEditStateChanged << EditingColumn (ColIdx editedColIdx)
                    , text = currentText
                    , placeholder = Nothing
                    , label = Input.labelHidden "Attribute name"
                    }

            else
                cellNotEdited

        _ ->
            cellNotEdited


gridCell : Context -> DragState -> Int -> Int -> Element Msg
gridCell context dragState rowIdx colIdx =
    Element.el
        [ Border.width 1
        , Element.width (px cellSize)
        , Element.height (px cellSize)
        , Element.centerX
        , Element.alignTop
        , Element.htmlAttribute <| Draggable.mouseTrigger ( RowIdx rowIdx, ColIdx colIdx ) DragMsg
        , Element.moveRight <| horizontalOffset dragState (ColIdx colIdx)
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


horizontalOffset : DragState -> ColIdx -> Float
horizontalOffset dragState (ColIdx colIdx) =
    case dragState of
        Horizontal (ColIdx draggedColIdx) offset ->
            calculateOffset colIdx draggedColIdx offset

        _ ->
            0


verticalOffset : DragState -> RowIdx -> Float
verticalOffset dragState (RowIdx rowIdx) =
    case dragState of
        Vertical (RowIdx draggedRowIdx) offset ->
            calculateOffset rowIdx draggedRowIdx offset

        _ ->
            0


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


nameCellWidth : Int
nameCellWidth =
    120


cellSizeFloat : Float
cellSizeFloat =
    toFloat cellSize


determineSwap : DragState -> Int -> Int -> Swap
determineSwap dragState maxRows maxCols =
    case dragState of
        NotDragging ->
            NoSwap

        DirectionUndecided _ _ ->
            NoSwap

        Horizontal (ColIdx draggedIdx) offset ->
            case getIndexOffset offset |> clamp -draggedIdx (maxCols - draggedIdx - 1) of
                0 ->
                    NoSwap

                nonzeroIndexOffset ->
                    SwapColumns draggedIdx (draggedIdx + nonzeroIndexOffset)

        Vertical (RowIdx draggedIdx) offset ->
            case getIndexOffset offset |> clamp -draggedIdx (maxRows - draggedIdx - 1) of
                0 ->
                    NoSwap

                nonzeroIndexOffset ->
                    SwapRows draggedIdx (draggedIdx + nonzeroIndexOffset)
