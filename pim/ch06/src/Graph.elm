module Graph exposing (..)

type alias Node =
  { id : Int
  , x : Int
  , y : Int
  }

type alias Graph =
  { nodes : List Node
  , edges : List (Int, Int)
  }

chromatic : Int -> Graph -> Int
chromatic k g = 0

addNode : Node -> Graph -> Graph
addNode n g = { g | nodes = List.append g.nodes [ n ] }

removeNode : Int -> Graph -> Graph
removeNode id g =
  { g
  | nodes = List.filter (\n -> n.id /= id) g.nodes
  , edges = List.filter (\(n1,n2) -> n1 /= id && n2 /= id) g.edges
  }

-- TODO: Make searching/updating not Omega(n)
addEdge : (Int, Int) -> Graph -> Graph
addEdge (n1,n2) g =
  let
    edge = if n1 < n2 then (n1,n2) else (n2,n1)
    hasNode id = List.any (\n -> n.id == id) << .nodes
  in
  if hasNode n1 g && hasNode n2 g then
    { g | edges = List.sort <| edge :: g.edges }
  else
    g

removeEdge : (Int, Int) -> Graph -> Graph
removeEdge (n1,n2) g =
  { g | edges = List.filter (\(m1,m2) -> (m1,m2) /= (n1,n2)) g.edges }
