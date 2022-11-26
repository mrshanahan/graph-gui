module ChromaticNumber exposing (..)

import Graph exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Debug exposing (log,toString)

import Json.Decode as Decode

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

type Selection
  = NoneSelected
  | NodeSelected Int
  | EdgeSelected (Int,Int)

type alias Model =
  { graph : Graph
  , nextId : Int
  , selection : Selection
  , dragging : Maybe Int
  }

type alias MouseLocationData =
  { clientX : Int
  , clientY : Int
  }

mouseLocationDecoder : Decode.Decoder MouseLocationData
mouseLocationDecoder =
  Decode.map2 MouseLocationData
    (Decode.field "clientX" Decode.int)
    (Decode.field "clientY" Decode.int)

type Msg
  = ClickEmpty MouseLocationData
  | SelectNode Int
  | SelectEdge (Int,Int)
  | StartDrag Int
  | DragNode MouseLocationData
  | EndDrag MouseLocationData

init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Graph [] []) 0 NoneSelected Nothing
  , Cmd.none
  )

handleMsg : Msg -> Model -> Model
handleMsg msg model =
  let
    moveGraphNode (id,x,y) g =
      let
        moveNode n = if n.id == id then { n | x = x, y = y } else n
      in
      { g | nodes = List.map moveNode g.nodes }
  in
  case msg of
    ClickEmpty data ->
      case model.selection of
        NoneSelected ->
          { model
          | graph = addNode (Node model.nextId data.clientX data.clientY) model.graph
          , nextId = model.nextId+1
          }
        _ ->
          { model | selection = NoneSelected }
    SelectNode n ->
      case model.selection of
        NodeSelected id ->
          if id == n then
            { model | graph = removeNode id model.graph, selection = NoneSelected }
          else
            { model | graph = addEdge (id,n) model.graph, selection = NoneSelected }
        _ ->
          { model | selection = NodeSelected n }
    SelectEdge (n1,n2) ->
      case model.selection of
        EdgeSelected (m1,m2) ->
          if (m1,m2) == (n1,n2) then
            { model | graph = removeEdge (n1,n2) model.graph, selection = NoneSelected }
          else
            { model | selection = EdgeSelected (m1,m2) }
        _ ->
          { model | selection = EdgeSelected (n1,n2) }
    StartDrag n ->
      { model | dragging = Just n }
    DragNode data ->
      case model.dragging of
        Just n ->
          { model
          | graph = moveGraphNode (n,data.clientX,data.clientY) model.graph
          , selection = NoneSelected
          }
        _ ->
          model
    EndDrag data ->
      case model.dragging of
        Just n ->
          { model | dragging = Nothing }
        _ ->
          model

update : Msg -> Model -> ( Model, Cmd Msg )
--update msg model = ( handleMsg msg model |> log "update", Cmd.none )
update msg model = ( handleMsg msg model, Cmd.none )

drawNode : (Bool, Bool) -> Node -> Svg Msg
drawNode (isSelected,isDragging) { id, x, y } =
  let color =
        if isSelected then "yellow"
        else if isDragging then "pink"
        else "red"
  in
  circle
  [ fill color -- "red"
  , stroke color -- "red"
  , cx <| String.fromInt x
  , cy <| String.fromInt y
  , r "10"
  , Svg.Events.onClick (SelectNode id)
  , Svg.Events.onMouseDown (StartDrag id)
  , Svg.Events.on "mouseup" (Decode.map EndDrag mouseLocationDecoder)
  , Svg.Events.on "mousemove" (Decode.map DragNode mouseLocationDecoder)
  , Svg.Events.on "mouseleave" (Decode.map DragNode mouseLocationDecoder)
  ]
  []
  -- [ Svg.text <| String.fromInt id ]

drawViewNode : (Selection, Maybe Int) -> Node -> Svg Msg
drawViewNode (selection, dragging) node =
  let
      isSelected =
        case selection of
          NodeSelected id -> id == node.id
          _               -> False
      isDragging =
        case dragging of
          Just id -> id == node.id
          _       -> False
  in
  drawNode (isSelected, isDragging) node

find : (a -> Bool) -> List a -> Maybe a
find f xs =
  case xs of
    []     -> Nothing
    x::xs2 -> if f x then Just x else find f xs2

drawEdge : Bool -> List Node -> (Int, Int) -> List (Svg Msg)
drawEdge isSelected nodes (id1,id2) =
  let
    mn1 = find (\n -> n.id == id1) nodes
    mn2 = find (\n -> n.id == id2) nodes
    color = if isSelected then "yellow" else "black"
    width = if isSelected then "5" else "2"
  in
  case (mn1,mn2) of
    (_,Nothing) -> []
    (Nothing,_) -> []
    (Just n1, Just n2) ->
      [ line
        [ stroke color
        , strokeWidth width
        , x1 <| String.fromInt n1.x
        , y1 <| String.fromInt n1.y
        , x2 <| String.fromInt n2.x
        , y2 <| String.fromInt n2.y
        ]
        []
      , line
        [ stroke "transparent" -- "blue"
        , strokeWidth "20"
        , strokeOpacity "0.0"
        , x1 <| String.fromInt n1.x
        , y1 <| String.fromInt n1.y
        , x2 <| String.fromInt n2.x
        , y2 <| String.fromInt n2.y
        , Svg.Events.onClick (SelectEdge (n1.id,n2.id))
        ]
        []
      ]

drawViewEdge : Selection -> List Node -> (Int, Int) -> List (Svg Msg)
drawViewEdge selection nodes (id1,id2) =
  case selection of
    EdgeSelected (n1,n2) -> if (n1,n2) == (id1,id2) then drawEdge True nodes (id1,id2)
                                            else drawEdge False nodes (id1,id2)
    _                    -> drawEdge False nodes (id1,id2)

drawGraph : (Selection, Maybe Int) -> Graph -> List (Svg Msg)
drawGraph (selection, dragging) { nodes, edges } =
  (rect
  [ fill "transparent"
  , fillOpacity "0.0"
  , stroke "black"
  , width "400"
  , height "400"
  , Svg.Events.on "click" (Decode.map ClickEmpty mouseLocationDecoder)
  , Svg.Events.on "mouseup" (Decode.map EndDrag mouseLocationDecoder)
  ] []) ::
    (List.append
      (List.concat <| (List.map (drawViewEdge selection nodes) edges))
      (List.map (drawViewNode (selection, dragging)) nodes)
    )

view : Model -> Html Msg
view model =
  div []
    [ svg
      [ width "400"
      , height "400"
      ]
      (drawGraph (model.selection,model.dragging) model.graph)
    , div [] [ Html.text <| "V: " ++ (toString model.graph.nodes) ]
    , div [] [ Html.text <| "|V|: " ++ (String.fromInt <| List.length model.graph.nodes) ]
    , div [] [ Html.text <| "E: " ++ (toString model.graph.edges) ]
    , div [] [ Html.text <| "|E|: " ++ (String.fromInt <| List.length model.graph.edges) ]
    , div [] [ Html.text <| "CHI(G;3): " ++ (String.fromInt <| chromatic 3 model.graph) ]
    ]
      
