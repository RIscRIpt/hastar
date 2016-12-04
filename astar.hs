import Data.Char
import Data.List
import Data.Function
import Data.Matrix
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

data Cell  = Empty | Barrier | Path | Start | Finish deriving (Eq, Show)
type Map   = Matrix Cell
data Coord = Coord
    { coordRow    :: Int
    , coordColumn :: Int
    } deriving (Ord, Eq, Show)

infinity = maxBound :: Int
-- infinity = read "Infinity" :: Float

instance (Num a, Num b) => Num (a, b) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) * (c, d) = (a * c, b * d)
    (a, b) - (c, d) = (a - c, b - d)
    abs      (a, b) = (abs a, abs b)
    signum   (a, b) = (signum a, signum b)
    fromInteger i   = (fromInteger i, fromInteger i)

str2map :: String -> Map
str2map str = fromLists [[char2cell c | c <- s] | s <- lines str]

char2cell :: Char -> Cell
char2cell cc
    | c `elem` ['x', '|', '-', '+'] = Barrier
    | c == 's'                      = Start
    | c == 'f'                      = Finish
    | otherwise                     = Empty
    where c = toLower cc

map2str :: Map -> String
map2str m = unlines [[cell2char c | c <- vector] | vector <- toLists m]

cell2char :: Cell -> Char
cell2char c = case c of
    Path    -> 'o'
    Barrier -> 'X'
    Start   -> 'S'
    Finish  -> 'F'
    _       -> ' '

distance :: Coord -> Coord -> Int
distance (Coord x1 y1) (Coord x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)
-- distance :: Coord -> Coord -> Float
-- distance (Coord x1 y1) (Coord x2 y2) = sqrt $ fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

between :: Int -> (Int, Int) -> Bool
between value (low, high) = value >= low && value <= high

pair2Coord :: (Int, Int) -> Coord
pair2Coord (r, c) = Coord r c

cellCoord :: Cell -> Map -> Maybe Coord
cellCoord cell m = case flattenIndex of
    Just index -> Just $ pair2Coord $ 1 + index `divMod` ncols m
    _          -> Nothing
    where flattenIndex = elemIndex cell $ toList m

getCell :: Coord -> Map -> Cell
getCell (Coord r c) = unsafeGet r c

adjMapCoords :: Map -> Coord -> [Coord]
adjMapCoords m = validateCoords' . secureCoords' . adjCoords'
    where adjCoords' (Coord r c)  = [ Coord r (c-1)
                                    , Coord r (c+1)
                                    , Coord (r-1) c
                                    , Coord (r+1) c
                                    ]
          secureCoords'           = filter isInMap'
          validateCoords'         = filter notBarrier'
          isInMap' (Coord r c)    = r `between` (1, nrows m) && c `between` (1, ncols m)
          notBarrier' c           = getCell c m /= Barrier

pavePath :: [Coord] -> Map -> Map
pavePath = flip $ foldr paver
    where paver coord@(Coord c r) world =
            if cell /= Start && cell /= Finish then
                unsafeSet Path (c, r) world
            else
                world
            where cell = getCell coord world

-- TODO: remove from totalEst neighbours which are in opened set

findPath :: Coord -> Coord -> Map -> [Coord]
findPath start finish m = makePath finish $ findPath' beginOpened beginClosed beginPassed beginTotalEst beginCameFrom
    where beginOpened   = S.fromList [start]
          beginClosed   = S.fromList []
          beginPassed   = M.fromList [(start, 0)]
          beginTotalEst = M.fromList [(distance start finish, [start])]
          beginCameFrom = M.empty

          findPath' opened closed passed totalEst cameFrom =
              let passedFromStart c          = M.findWithDefault infinity c passed
                  neighbourHeuristic n       = start2neighbour + distance n finish
                  shouldBeExplored c         = c `S.notMember` closed && start2neighbour < passedFromStart c
                  start2neighbour            = 1 + passedFromStart current
                  Just (minScore, current:_) = M.lookupGT (-infinity) totalEst
                  neighbours                 = filter shouldBeExplored $ adjMapCoords m current
                  newOpened                  = foldr S.insert (S.delete current opened) neighbours
                  newClosed                  = S.insert current closed
                  newCameFrom                = foldr (flip M.insert current) cameFrom neighbours
                  newPassed                  = foldr (flip M.insert start2neighbour) passed neighbours
                  newTotalEst                = let adjTotalEst = M.adjust tail minScore totalEst
                                                   ttvTotalEst = if M.lookup minScore adjTotalEst == Just [] then
                                                                     M.delete minScore adjTotalEst
                                                                 else
                                                                     adjTotalEst
                                               in  foldr (\ n -> M.insertWith ((:) . head) (neighbourHeuristic n) [n]) ttvTotalEst neighbours
              in  if current /= finish then
                      findPath' newOpened newClosed newPassed newTotalEst newCameFrom
                  else
                      cameFrom

          makePath coord dict = makePath' [coord] where
              makePath' path@(ph:_) =
                  case M.lookup ph dict of
                      Just v  -> makePath' $ v : path
                      Nothing -> path

getStrmap :: IO String
getStrmap = getContents

main = do
    strmap <- getStrmap
    let world = str2map strmap

    let startCoord  = fromJust $ cellCoord Start world
    let finishCoord = fromJust $ cellCoord Finish world

    let path          = findPath startCoord finishCoord world
    let worldWithPath = pavePath path world

    putStrLn $ map2str worldWithPath
    print $ length path

