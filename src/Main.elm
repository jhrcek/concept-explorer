module Main exposing (main)

import Browser
import Draggable
import Draggable.Events
import Element as E exposing (Element, el, px, text)
import Element.Border as Border
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
    { dragState : DragState
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
    | DragMsg (Draggable.Msg CellCoord)
    | DragEnd


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { dragState = NotDragging
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
        ]


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


updateDragState : Draggable.Delta -> DragState -> DragState
updateDragState ( dx, dy ) dragState =
    case dragState of
        NotDragging ->
            Debug.todo "Calling this method in NotDragging state means my expectation that OnDragBy would never come before OnDragStart is broken"

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
update msg ({ dragState } as model) =
    case Debug.log "msg" msg of
        OnDragStart cellCoord ->
            ( { model | dragState = DirectionUndecided cellCoord ( 0, 0 ) }, Cmd.none )

        OnDragBy delta ->
            ( { model | dragState = updateDragState delta dragState }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        DragEnd ->
            ( { model | dragState = NotDragging }, Cmd.none )


view : Model -> Html Msg
view { dragState } =
    E.layout [] (grid dragState)


grid : DragState -> Element Msg
grid dragState =
    List.range 1 10
        |> List.map (\rowIdx -> gridRow dragState rowIdx)
        |> E.column [ Border.width 1 ]


gridRow : DragState -> Int -> Element Msg
gridRow dragState rowIdx =
    let
        verticalOffset =
            case dragState of
                NotDragging ->
                    0

                DirectionUndecided _ _ ->
                    0

                Horizontal _ _ ->
                    0

                Vertical (RowIdx idx) offset ->
                    if idx == rowIdx then
                        offset

                    else
                        0
    in
    List.range 1 10
        |> List.map (\colIdx -> gridCell dragState rowIdx colIdx)
        |> E.row [ E.moveDown verticalOffset ]


gridCell : DragState -> Int -> Int -> Element Msg
gridCell dragState rowIdx colIdx =
    let
        horizontalOffset =
            case dragState of
                NotDragging ->
                    0

                DirectionUndecided _ _ ->
                    0

                Horizontal (ColIdx idx) offset ->
                    if idx == colIdx then
                        offset

                    else
                        0

                Vertical _ _ ->
                    0
    in
    el
        [ Border.width 1
        , E.width (px 60)
        , E.height (px 60)
        , E.centerX
        , E.alignTop
        , E.htmlAttribute <| Draggable.mouseTrigger ( RowIdx rowIdx, ColIdx colIdx ) DragMsg
        , E.moveRight horizontalOffset
        ]
        (el [ E.centerX, E.centerY ] <| text <| "(" ++ String.fromInt rowIdx ++ "," ++ String.fromInt colIdx ++ ")")
