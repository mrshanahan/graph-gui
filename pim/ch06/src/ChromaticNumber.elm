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
  = AddNode OnClickData
  | AddEdge (Int, Int)
  | SelectNode Int
  | DeleteNode

init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Graph [] []) "" Nothing 0
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
    AddNode data ->
      case model.selectedNode of
        Nothing -> 
          { model
          | graph = addGraphNode (Node model.nextId data.clientX data.clientY) model.graph
          , nextId = model.nextId+1
          }
        Just _  -> { model | selectedNode = Nothing }
    AddEdge (n1,n2) ->
      model
    SelectNode n ->
      case model.selectedNode of
        Nothing -> { model | selectedNode = Just n }
        Just id ->
          if id == n then
            model
          else
            { model
            | graph = addGraphEdge (id,n) model.graph
            , selectedNode = Nothing
            }
    DeleteNode ->
      case model.selectedNode of
        Nothing -> model
        Just id ->
          { model
          | graph = removeGraphNode id model.graph
          , selectedNode = Nothing
          }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( handleMsg msg model, Cmd.none )

drawNode : Bool -> Node -> Svg Msg
drawNode isSelected { id, x, y } =
  let
    color = if isSelected then "yellow" else "red"
    handler = if isSelected then DeleteNode else SelectNode id
  in
  circle
  [ fill color -- "red"
  , stroke color -- "red"
  , cx <| String.fromInt x
  , cy <| String.fromInt y
  , r "10"
  , Svg.Events.onClick handler
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

drawEdge : List Node -> (Int, Int) -> List (Svg Msg)
drawEdge nodes (id1,id2) =
  let
    mn1 = find (\n -> n.id == id1) nodes
    mn2 = find (\n -> n.id == id2) nodes
  in
  case (mn1,mn2) of
    (_,Nothing) -> []
    (Nothing,_) -> []
    (Just n1, Just n2) ->
      [ line
        [ stroke "black"
        , strokeWidth "2"
        , x1 <| String.fromInt n1.x
        , y1 <| String.fromInt n1.y
        , x2 <| String.fromInt n2.x
        , y2 <| String.fromInt n2.y
        ]
        []
      ]

drawGraph : Maybe Int -> Graph -> List (Svg Msg)
drawGraph selectedNode { nodes, edges } =
  (rect
  [ fill "transparent"
  , fillOpacity "0.0"
  , stroke "black"
  , width "400"
  , height "400"
  , Svg.Events.on "click" (Decode.map AddNode clickDecoder)
  ] []) ::
    (List.append
      (List.concat <| (List.map (drawEdge nodes) edges))
      (List.map (drawViewNode selectedNode) nodes)
    )

view : Model -> Html Msg
view model =
  div []
    [ svg
      [ width "400"
      , height "400"
      ]
      (drawGraph model.selectedNode model.graph)
    , Html.text <| String.fromInt <| List.length model.graph.nodes
    ]
      
