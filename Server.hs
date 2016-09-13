{-# LANGUAGE OverloadedStrings #-}

module Main where

-- other modules
import CollisionNetworks

-- imports
import Network.Wai.Middleware.RequestLogger
import Control.Monad.Trans
import Control.Monad
import System.Directory

import Data.String
import Data.Maybe
import Data.Char
import Data.Map as Map

-- web framework API
import Web.Scotty

-- 'mutable' variable
import Data.IORef

main :: IO ()
main = do
    graph <- newIORef Map.empty -- initializes a IORef, mutable variable for the graph

    let startServer = scotty 3000 $ do
        -- Prints a log of the server
        middleware logStdoutDev

        -- Server is up
        get "/" $ do
            html "Collision Network Server"

        -- Prints the whole graph, represented by an adjacency list
        get "/printGraph" $ do
            aux <- liftIO $ readIORef graph
            text $ fromString $ printGraph aux

        -- check if the collision between x and y exists (if they are on the same collision network)
        get "/checkCollision/:x/:y" $ do
            a <- param "x"
            b <- param "y"

            -- Check if the parameters are actually Ints before moving on
            unless (all isDigit a) $ raise " First value is not an integer.\n"
            unless (all isDigit b) $ raise " Second value is not an integer.\n"

            -- Reads our graph from memory and checks the collision
            aux <- liftIO $ readIORef graph
            text $ fromString $ show (checkCollision (read a) (read b) aux)

        -- Posting data, going to read a file of collisions
        post "/readFile/:fileName" $ do
            filePointer <- param "fileName"

            -- Check if the file actually exists before moving on to reading it
            fileExists <- liftIO $ doesFileExist filePointer
            unless (fileExists) $ raise " File does not exist.\n"

            -- Read file contents
            contents <- liftIO $ readFile filePointer
            let edges = lines contents

            aux <- liftIO $ readIORef graph -- reads our graph from memory
            newGraph <- return $ readLinesIntoGraph edges aux -- add the collisions from the file

            if isNothing newGraph then
                raise " File not formatted correctly.\n" -- if readLinesIntoGraph returns nothing
            else
                liftIO $ writeIORef graph (fromJust newGraph) -- saves graph back in memory

            -- success message
            text $ fromString $ "File read '" ++ filePointer ++ "'.\n"

        -- Posting data, adding new collision to the graph
        post "/addCollision/:x/:y" $ do
            a <- param "x"
            b <- param "y"

            -- Check if the parameters are actually Ints before moving on
            unless (all isDigit a) $ raise " First value is not an integer.\n"
            unless (all isDigit b) $ raise " Second value is not an integer.\n"

            aux <- liftIO $ readIORef graph -- reads our graph from memory
            newGraph <- return $ addEdgeToGraph (read a) (read b) aux -- new graph with the new collision
            liftIO $ writeIORef graph newGraph -- saves graph back in memory

            -- success message
            text $ fromString $ "Values inserted " ++ a ++ " " ++ b ++ ".\n" 

    -- startserver
    startServer