--use the Store comonad to simulate Conway's game of life (inefficiently)

import Control.Comonad
import Control.Comonad.Trans.Store
import Data.List

-- These imports define the following:

-- class Functor w => Comonad w where
--   extract :: w a -> a
--   duplicate :: w a -> w (w a)
--   duplicate = extend id
--   extend :: (w a -> b) -> w a -> w b
--   extend f = fmap f . duplicate

-- instance Comonad (Store s) where
--   extract (Store f s) = f s
--   duplicate (Store f s) = Store (Store f) s

-- store :: (s -> a) -> s -> Store s a
-- runStore :: Store s a -> (s -> a, s)

--seeks to a new index
-- seek :: s -> StoreT s w a -> StoreT s w a
--takes a function to seek to a new index
-- seeks :: (s -> s) -> StoreT s w a -> StoreT s w a

type Cell = Bool
type Pos = (Int,Int)

initPos :: Pos
initPos = (0,0)

--set some initial pattern (in this case, a solid square)
initFun :: Int -> Int -> Cell
initFun x y = abs(x) < 5 && abs(y) < 5
--put initial pattern in the Store data structure
-- store takes a function that assigns values (Cell) to indices (Pos)
-- as well as an initial "focus" Pos
initStore :: Store Pos Cell
initStore = store (\x -> initFun (fst x) (snd x)) initPos

--list of relative neighbor positions
neighborPos :: [Pos]
neighborPos = [(x,y) | x <- [-1..1], y <- [-1..1], not ((x ==0) && (y ==0))]

--translates position x by dx
shiftPos :: Pos -> Pos -> Pos
shiftPos dx = \x -> (fst x + fst dx, snd x + snd dx)

--compute number of alive neighbors
--by seeking to each neighbor, extracting their value, then
--fromEnum converts False -> 0, True -> 1
--sum to get number alive
nAlive :: Store Pos Cell -> Int
nAlive xStore = sum $ map (\dp -> fromEnum $ extract $ seeks (shiftPos dp) xStore) neighborPos

--update rule for Conway's game of life
--lives or dies depending on current state and number of living neighbors
updateRules :: Cell -> Int -> Cell
updateRules True  n = (n == 2) || (n == 3)
updateRules False n = (n == 3)

--apply update rule to "focused" cell of xStore
updateCell xStore = updateRules (extract xStore) (nAlive xStore)

--update entire grid by extend-ing cell update
update = extend updateCell

--generator for successive states of cellular automaton
automaton = iterate update initStore

--extract grid (list of lists) of cells from xStore, specified by sGrid
getRange :: [[s]] -> Store s Cell -> [[Cell]]
getRange sGrid xStore = map (\sList -> map (\p -> extract $ seek p xStore) sList) sGrid

--grid of positions
posGrid :: [[Pos]]
posGrid = [[(x,y) | x <- [-10 .. 10]] | y <- [-10.. 10]]
 
--display row of cells
showRow = intercalate " " . map showCell

--display cell
showCell :: Cell -> String
showCell True = "o"
showCell False = "_"

--display cell grid
showGrid xStore = mapM_ (putStrLn . showRow) $ getRange posGrid xStore

--display the first few states
main = do
	putStrLn <- automaton !! 0
	putStrLn <- automaton !! 1
	putStrLn <- automaton !! 2