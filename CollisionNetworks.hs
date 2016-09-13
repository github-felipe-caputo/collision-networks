module CollisionNetworks where

import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe
import Data.Char

-- A graph will be represented by a adjacency list
type Graph = Map Node [Node]

-- For the sake of clarity, we will rename Ints as Nodes
type Node = Int 

-- Here a list of values that represent collision is added to the graph
readLinesIntoGraph :: [String]     -- First argument: list of collision, Ex. [("1 2"),("3 4")]
                   -> Graph        -- Second argument: the graph where we will add the collisions
                   -> Maybe Graph  -- Return: final graph with the new collisions, returns Nothing if there was an error reading the file
readLinesIntoGraph [] graph = Just graph
readLinesIntoGraph (x:xs) graph
    | (length $ words x) /= 2                    = Nothing -- if there isnt exactly 2 words per line, error
    | not (all isDigit a) || not (all isDigit b) = Nothing -- if the two words arent ints, error
    | otherwise                                  = (readLinesIntoGraph xs (addEdgeToGraph (read a) (read b) graph))
    where a = (words x) !! 0
          b = (words x) !! 1

-- adding edges is simple with a adjacency list, with log complexity using maps
addEdgeToGraph :: Node   -- First argument: one of the nodes
               -> Node   -- Second argument: the other node
               -> Graph  -- Third argument: the graph where we will add the collision
               -> Graph  -- Return: final graph with the new collisions
addEdgeToGraph a b graph = Map.insertWith (concat') b [a] (Map.insertWith (concat') a [b] graph)

-- special concat function for our graph, we only add a collision if it doesnt exist
concat' :: [Node] -- First argument: a list of nodes (in this case it will always be a list with one value)
        -> [Node] -- Second argument: the previous list of nodes
        -> [Node] -- Return: either the list with the new value, or just the old list
concat' [a] b
    | res == True   = b
    | otherwise     = a:b
    where res = a `elem` b
concat' _ _ = [] -- this will never be called given the way the function is used, but it's here for the sake of completion

-- check collision will be based on a depth first search
checkCollision :: Node   -- First argument: one of the nodes
               -> Node   -- Second argument: the other node
               -> Graph  -- Third argument: the graph where we will check for the collision
               -> Bool   -- Return: True or False, if the collision is there or not
checkCollision a b graph
    | graph == Map.empty                 = False -- if graph is empty
    | isNothing looka || isNothing lookb = False -- if either of the values aren't in the graph
    | otherwise                          = Set.member b (dfs a b graph Set.empty)
    where looka = Map.lookup a graph
          lookb = Map.lookup b graph

-- Depth First Search
dfs :: Node      -- First argument: source node
    -> Node      -- Second argument: goal node
    -> Graph     -- Third argument: the graph we will search
    -> Set Node  -- Fourth argument: list of visited nodes (a set for more efficient searches)
    -> Set Node  -- Return: the list of visited nodes which will include 'goal' if it was reached
dfs source goal graph visitedNodes
    | Set.member goal visitedNodes   = visitedNodes
    | Set.member source visitedNodes = visitedNodes
    | otherwise                      = Prelude.foldl (\acc i -> dfs i goal graph acc) (Set.insert source visitedNodes) x
    where x = fromJust $ Map.lookup source graph -- neighbors

-- functions to print the graph 
printGraph :: Graph  -- First argument: the graph we will print
           -> String -- Return: the printed graph
printGraph graph
    | graph == Map.empty = "Graph is empty.\n"
    | otherwise          = printList (Map.toList graph)

-- how we actually print the Map (the Graph)
printList :: [(Node, [Node])] -- First argument: the Map as a list, a Node plus its adjacent nodes
          -> String           -- Return: the printed list
printList [] = ""
printList (x:xs) = show (fst x) ++ " | " ++ show (snd x) ++ "\n" ++ printList xs