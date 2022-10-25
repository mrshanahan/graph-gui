module ChromaticNumber exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Json.Decode as Decode

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

type alias Model =
  { graph : Graph
  , display : String
  , selectedNode : Maybe Int
  , selectedEdge : Maybe (Int,Int)
  , nextId : Int
  }

type alias Node =
  { id : Int
  , x : Int
  , y : Int
  }

type alias Graph =
  { nodes : List Node
  , edges : List (Int, Int)
  }

type alias OnClickData =
  { clientX : Int
  , clientY : Int
  }

clickDecoder : Decode.Decoder OnClickData
clickDecoder =
  Decode.map2 OnClickData
    (Decode.field "clientX" Decode.int)
    (Decode.field "clientY" Decode.int)

type Msg
  = ClickEmpty OnClickData
  | SelectNode Int
  | SelectEdge (Int,Int)

init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Graph [] []) "" Nothing Nothing 0
  , Cmd.none
  )

handleMsg : Msg -> Model -> Model
handleMsg msg model =
  let
    addGraphNode n g = { g | nodes = List.append g.nodes [ n ] }
    removeGraphNode id g =
      { g
      | nodes = List.filter (\n -> n.id /= id) g.nodes
      , edges = List.filter (\(n1,n2) -> n1 /= id && n2 /= id) g.edges
      }
    removeGraphEdge (n1,n2) g =
      { g | edges = List.filter (\(m1,m2) -> (m1,m2) /= (n1,n2)) g.edges }

    -- TODO: Make searching/updating not Omega(n)
    addGraphEdge (n1,n2) g =
      let
        edge = if n1 < n2 then (n1,n2) else (n2,n1)
        hasNode id = List.any (\n -> n.id == id) << .nodes
      in
      if hasNode n1 g && hasNode n2 g then
        { g | edges = List.sort <| edge :: g.edges }
      else
        g
  in
  case msg of
    ClickEmpty data ->
      case (model.selectedNode,model.selectedEdge) of
        (Nothing,Nothing) ->
          { model
          | graph = addGraphNode (Node model.nextId data.clientX data.clientY) model.graph
          , nextId = model.nextId+1
          }
        (_,_)             ->
          { model | selectedNode = Nothing, selectedEdge = Nothing }
    SelectNode n ->
      case model.selectedNode of
        Nothing ->
          { model
          | selectedNode = Just n
          , selectedEdge = Nothing
          }
        Just id ->
          if id == n then
            { model
            | graph = removeGraphNode id model.graph
            , selectedNode = Nothing
            , selectedEdge = Nothing
            }
          else
            { model
            | graph = addGraphEdge (id,n) model.graph
            , selectedNode = Nothing
            , selectedEdge = Nothing
            }
    SelectEdge (n1,n2) ->
      case model.selectedEdge of
        Nothing ->
          { model
          | selectedEdge = Just (n1,n2)
          , selectedNode = Nothing }
        Just (m1,m2)  ->
          if (m1,m2) == (n1,n2) then
            { model
            | graph = removeGraphEdge (n1,n2) model.graph
            , selectedEdge = Nothing
            , selectedNode = Nothing
            }
          else
            { model
            | selectedEdge = Just (m1,m2)
            , selectedNode = Nothing
            }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( handleMsg msg model, Cmd.none )

drawNode : Bool -> Node -> Svg Msg
drawNode isSelected { id, x, y } =
  let
    color = if isSelected then "yellow" else "red"
  in
  circle
  [ fill color -- "red"
  , stroke color -- "red"
  , cx <| String.fromInt x
  , cy <| String.fromInt y
  , r "10"
  , Svg.Events.onClick (SelectNode id)
  ]
  []
  -- [ Svg.text <| String.fromInt id ]

drawViewNode : Maybe Int -> Node -> Svg Msg
drawViewNode selected node =
  case selected of
    Nothing -> drawNode False node
    Just id -> if id == node.id then drawNode True node
                                else drawNode False node

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

drawViewEdge : Maybe (Int,Int) -> List Node -> (Int, Int) -> List (Svg Msg)
drawViewEdge selectedEdge nodes (id1,id2) =
  case selectedEdge of
    Nothing ->
      drawEdge False nodes (id1,id2)
    Just (n1,n2) ->
      if (n1,n2) == (id1,id2) then
        drawEdge True nodes (id1,id2)
      else
        drawEdge False nodes (id1,id2)

drawGraph : Maybe Int -> Maybe (Int, Int) -> Graph -> List (Svg Msg)
drawGraph selectedNode selectedEdge { nodes, edges } =
  (rect
  [ fill "transparent"
  , fillOpacity "0.0"
  , stroke "black"
  , width "400"
  , height "400"
  , Svg.Events.on "click" (Decode.map ClickEmpty clickDecoder)
  ] []) ::
    (List.append
      (List.concat <| (List.map (drawViewEdge selectedEdge nodes) edges))
      (List.map (drawViewNode selectedNode) nodes)
    )

view : Model -> Html Msg
view model =
  div []
    [ svg
      [ width "400"
      , height "400"
      ]
      (drawGraph model.selectedNode model.selectedEdge model.graph)
    , Html.text <| String.fromInt <| List.length model.graph.nodes
    ]
      
