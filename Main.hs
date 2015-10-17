{-# LANGUAGE UnicodeSyntax, MultiWayIf, ScopedTypeVariables #-}

module Main(main) where

import Prelude.Unicode

import           Control.Monad.Random
import           Control.Monad.Random.Class
import           Data.Maybe
import           Data.Foldable
import           Data.Monoid
import           Data.Vector                ((!?))
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VMut

type Vec = V.Vector


-- Parameters ------------------------------------------------------------------

board_width  = 16 ∷ Int
board_height = 16 ∷ Int
mines        = 40 ∷ Int
cellSize     = 20 ∷ Int


-- States and State Transitions ------------------------------------------------

data CellStatus   = Closed | Open
data CellContents = Neighbors Int | Mine
data GameStatus   = Playing | Lost | StartingGame
data UserClick    = UserClick (Int,Int) | RestartGame (Int,Int)

type CellState = (CellStatus, CellContents)
type GameBoard = Vec (Vec CellState)
type GameState = (GameStatus, GameBoard, StdGen)


-- Initialization --------------------------------------------------------------

initialReset = RestartGame(0,0)

dummyState = (Closed, Neighbors (-1))

initializeState ∷ (Int,Int) → StdGen → GameState
initializeState (w,h) gen = ( StartingGame
                            , V.replicate w (V.replicate h dummyState)
                            , gen
                            )

-- Basic Operations ------------------------------------------------------------

getBoardWidth board = V.length board

getBoardHeight board = fromMaybe 0
                     $ V.length <$> board !? 0

totalBoardWidth board = (cellSize+2)*(getBoardWidth board)

totalBoardHeight board = (cellSize+2)*(getBoardHeight board)

getCell ∷ (Int,Int) → GameBoard → CellState
getCell (x,y) board = fromMaybe dummyState $ (board !? x) >>= (!? y)

numberOfMines ∷ (Int,Int) → GameBoard → Int
numberOfMines pos board =
    case getCell pos board of
        (_, Mine) → 1
        (_, _)    → 0

revealAll = V.map (V.map revealCell)

revealCell (status, contents) = (Open, contents)

neighbors ∷ (Int,Int) → [(Int,Int)]
neighbors (x,y) = [ (x-1, y-1), (x, y-1), (x+1, y-1)
                  , (x-1, y),             (x+1, y)
                  , (x-1, y+1), (x, y+1), (x+1, y+1)
                  ]


-- Game Logic ------------------------------------------------------------------

updateState ∷ UserClick → GameState → GameState
updateState click oldState = case (click, oldState) of
    (RestartGame (w,h), (status, board, gen)) →
        initializeState (w,h) gen
    (UserClick (x,y), (status, board, gen)) →
        case status of
            StartingGame →
                (Playing, revealNeighbors [(x,y)] newRandomBoard, gen')
                  where (newRandomBoard, gen') = runRand (fillRandomBoard (x,y) mines board) gen
            Playing →
                case getCell (x,y) board of
                    (Open, _)             → (Playing, board, gen)
                    (Closed, Mine)        → (Lost,    revealAll board, gen)
                    (Closed, Neighbors _) → (Playing, revealNeighbors [(x,y)] board, gen)
            _ →
                (status, board, gen)

revealNeighbors ∷ [(Int,Int)] → GameBoard → GameBoard
revealNeighbors []           board = board
revealNeighbors ((x,y):rest) board =
    case getCell (x,y) board of
        (Open, _)             → revealNeighbors rest board
        (Closed, Mine)        → revealNeighbors rest board
        (Closed, Neighbors 0) → revealNeighbors (neighbors (x,y) <> rest)
                              $ modifyCell (x,y) (Open, Neighbors 0) board
        (Closed, Neighbors n) → revealNeighbors rest
                              $ modifyCell (x,y) (Open, Neighbors n) board


fillNeighbors ∷ GameBoard → GameBoard
fillNeighbors board = V.imap fillNeighborsInColumn board
  where
    nMines ∷ (Int,Int) → Int
    nMines pos = numberOfMines pos board

    fillNeighborsInColumn x column = V.imap (\y → computeNeighbors (x,y)) column

    sumNeighbors ∷ (Int,Int) -> Int
    sumNeighbors = foldl' (+) 0 . fmap nMines . neighbors

    computeNeighbors pos (status, contents) =
        case contents of Mine → (status, Mine)
                         _    → (status, Neighbors (sumNeighbors pos))


fillRandomBoard ∷ MonadRandom m ⇒ (Int,Int) → Int → GameBoard → m GameBoard
fillRandomBoard (x,y) n board = do
    mineX ∷ Int ← getRandomR (0, getBoardWidth board-1)
    mineY ∷ Int ← getRandomR (0, getBoardHeight board-1)

    let (cellStatus, cellContents) = getCell (mineX,mineY) board

    if | n≡0                → return (fillNeighbors board)
       | mineX≡x || mineY≡y → fillRandomBoard (x,y) n board
       | otherwise          →
            case cellContents of
                Mine → fillRandomBoard (x,y) n board
                _    → fillRandomBoard (x,y) (n-1)
                          (modifyCell (mineX,mineY) (cellStatus, Mine) board)

modifyCell ∷ (Int,Int) → CellState → GameBoard → GameBoard
modifyCell (x,y) cellState board =
    fromMaybe board $ do
        col ← board !? x
        let col' = V.modify (\c -> VMut.write c y cellState) col
        return $ V.modify (\b -> VMut.write b x col') board


-- FRP Stuff -------------------------------------------------------------------

main = return ()

-- clickChannel ∷ Signal.Channel UserClick
-- clickChannel = Signal.channel (RestartGame(0,0))

-- clickSignal ∷ Signal UserClick
-- clickSignal = Signal.subscribe clickChannel

-- main ∷ Signal Element
-- main = clickSignal
--   |> Signal.foldp updateState (initializeState boardW boardH (mkStdGen 12345))
--   |> Signal.map drawScene

-- drawScene ∷ GameState → Element
-- drawScene (status, board, _) = flow down [spacer 10 10, flow right [spacer 10 10, drawBoard board, spacer 30 30,
--   case status of
--     Lost →
--       clickable (Signal.send clickChannel (RestartGame board_width board_height))
--         <| color grey
--         <| centered <| fromString "You lost, click this to clear game"
--     StartingGame → centered <| fromString "Click to begin"
--     Playing →
--       clickable (Signal.send clickChannel (RestartGame board_width board_height))
--         <| color grey
--         <| centered <| fromString "Restart game"
--   ]
--  ]


-- Rendering -------------------------------------------------------------------

-- drawBoard ∷ GameBoard → Element
-- drawBoard board = color black
--                 $ container (totalBoardWidth board + 2) (totalBoardHeight board + 2) middle
--                 $ flow right
--                 $ List.map drawColumn
--                 $ toIndexedList board

-- drawColumn ∷ (Int, Array CellState) → Element
-- drawColumn (x, column) = flow down
--                        $ List.map (drawCell' x)
--                        $ toIndexedList column

-- drawCell' x (y, c) = color black
--                    $ container (cellSize+2) (cellSize+2) middle
--                    $ clickable (Signal.send clickChannel (UserClick (x,y)))
--                    $ drawCell c

-- drawCell ∷ CellState → Element
-- drawCell (status, contents) =
--     case status of Open → cellElement
--                    _    → color grey $ spacer cellSize cellSize
--   where
--     cellElement = case contents of
--         Mine        → color red
--                     $ container cellSize cellSize middle
--                     $ centered
--                     $ fromString "!"
--         Neighbors 0 → color white
--                     $ spacer (cellSize+2) (cellSize+2)
--         Neighbors n → color white
--                     $ container cellSize cellSize middle
--                     $ asText n
