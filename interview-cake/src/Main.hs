{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad   (join)
import           Data.Bool
import           Data.Foldable   (find)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord        (comparing)
import           Data.Text       (Text)

main :: IO ()
main = print $ minSpan4 "Jayden" "Adam" network

type User = Text
type Path = [User]

type Network = M.Map Text [Text]

firstJusts :: [Maybe a] -> Maybe a
firstJusts = join . find isJust

-- First note, didn't account for infinite loops
-- second note, using join to reduce monad layers
minSpan1 :: User -> User -> Network -> Maybe Path
minSpan1 from to net =
  bfs to [] $ net M.!? from
    where bfs :: User -> [User] -> Maybe Path -> Maybe Path
          bfs to path mNeighbors = do
            neighbors <- mNeighbors
            bool
              (firstJusts $ map (\n -> bfs to (n:path) (net M.!? n)) neighbors)
              (Just $ to:path)
              (to `elem` neighbors)

-- First note, didn't account for infinite loops
-- Second note - maybes are hard to work with
-- Third note - it was difficult to figure out where to reduce the [Path] to Path
-- Fourth note - runtime should be O(N+M), is this solution slower?
minSpan2 :: User -> User -> Network -> Maybe Path
minSpan2 from to net = reverse <$> bfs to from [from]
    where
      bfs :: User -> User -> Path -> Maybe Path
      bfs to next path = do
          neighbors <- net M.!? next
          bool
            (listToMaybe $ sortOn length $ mapMaybe
                (\n -> bfs to n (n:path))
                (filter (not . (`elem` path)) neighbors))
            (Just $ to:path)
            (to `elem` neighbors)

-- minSpan3 :: User -> User -> Network -> Maybe Path
-- minSpan3 from to net = (listToMaybe . sortOn length) <$> bfs to from [from]
--     where
--       bfs :: User -> User -> Path -> Maybe [Path]
--       bfs to next path = do
--           neighbors <- net M.!? next
--           if (to `elem` neighbors)
--             then
--             Just [to:path]
--             else
--             (Just $ concat $ mapMaybe
--                 (\n -> bfs to n (n:path))
--                 (filter (not . (`elem` path)) neighbors))
-- src/Main.hs:56:24: error:
--     * Couldn't match type `Maybe [User]' with `[User]'
--       Expected type: Maybe Path
--         Actual type: Maybe (Maybe [User])
--     * In the expression:
--         (listToMaybe . (sortOn length)) <$> bfs to from [from]
--       In an equation for `minSpan3':
--           minSpan3 from to net
--             = (listToMaybe . (sortOn length)) <$> bfs to from [from]
--             where
--                 bfs :: User -> User -> Path -> Maybe [Path]
--                 bfs to next path
--                   = do neighbors <- net M.!? next
--                        ....
--    |
-- 56 | minSpan3 from to net = (listToMaybe . (sortOn length)) <$> bfs to from [from]


-- Avoid Nested Maybes using binds!
minSpan4 :: User -> User -> Network -> Maybe Path
minSpan4 from to net = (sortOn length) <$> bfs to from [from] >>= listToMaybe
    where
      bfs :: User -> User -> Path -> Maybe [Path]
      bfs to next path = do
          neighbors <- net M.!? next
          if (to `elem` neighbors)
            then
            Just [to:path]
            else
            (Just $ concat $ mapMaybe
                (\n -> bfs to n (n:path))
                (filter (not . (`elem` path)) neighbors))

network :: Network
network = M.fromList [
    ("Min",     ["William", "Jayden", "Omar"]),
    ("William", ["Min", "Noam"]),
    ("Jayden",  ["Min", "Amelia", "Ren", "Noam"]),
    ("Ren",     ["Jayden", "Omar"]),
    ("Amelia",  ["Jayden", "Adam", "Miguel"]),
    ("Adam",    ["Amelia", "Miguel", "Sofia", "Lucas"]),
    ("Miguel",  ["Amelia", "Adam", "Liam", "Nathan"]),
    ("Noam",    ["Nathan", "Jayden", "William"]),
    ("Omar",    ["Ren", "Min", "Scott"])]
