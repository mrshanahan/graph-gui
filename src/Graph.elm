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
chromatic k g =
  case (g.nodes,g.edges) of
    ([],_)    -> 0
    (ns,[])   -> k ^ (List.length ns)
    (_,e::es) -> (chromatic k <| removeEdge e g) - (chromatic k <| contractEdge e g)

contractEdge : (Int, Int) -> Graph -> Graph
contractEdge (id1,id2) g =
  let
    find : (a -> Bool) -> List a -> Maybe a
    find f xs =
      case xs of
        []     -> Nothing
        x::xs2 -> if f x then Just x else find f xs2

    orderedPair : (Int, Int) -> (Int, Int)
    orderedPair (x,y) = if x < y then (x,y) else (y,x)

    remapEdge : Int -> Int -> (Int, Int) -> Maybe (Int, Int)
    remapEdge from to (m,n) =
      if m == from && to /= n      then Just <| orderedPair (to,n)
      else if n == from && to /= m then Just <| orderedPair (m,to)
      else                              Nothing

    touchesNode : Int -> (Int, Int) -> Bool
    touchesNode id (m,n) = m == id || n == id

    uniq : List a -> List a
    uniq xs =
      let
        inner (prev,ys) =
          case (prev,ys) of
            (Just y, y2 :: ys2)  -> if y == y2 then inner (Just y, ys2)
                                    else            y :: inner (Just y2, ys2)
            (Nothing, y2 :: ys2) -> inner (Just y2, ys2)
            (Just y, [])         -> [y]
            (Nothing, [])        -> []
      in
      inner (Nothing, xs)

    mn1 = find (\n -> n.id == id1) g.nodes
    mn2 = find (\n -> n.id == id2) g.nodes
    me = find (\(m,n) -> m == id1 && n == id2 || m == id2 && n == id1) g.edges
  in
  case (mn1,mn2,me) of
    (Just _, Just _, Just _) ->
      { g
      | nodes = List.filter (\n -> n.id /= id2) g.nodes
      , edges =
          uniq
          <| List.sort
          <| List.append (List.filter (not << touchesNode id2) g.edges)
                         (List.filterMap (remapEdge id2 id1) g.edges)
      }
    _ ->
      g

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
