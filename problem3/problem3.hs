import qualified Data.Map as M

type HouseMap = M.Map (Int, Int) Bool
type Position = (Int, Int)
type Accumulator = (HouseMap, (Position, Position), Bool)

movPos :: Position -> Char -> Position
movPos (x, y) c
  | c == '>'  = (x+1, y)
  | c == '<'  = (x-1, y)
  | c == '^'  = (x, y+1)
  | c == 'v'  = (x, y-1)
  | otherwise = (x, y)

emptyMap :: HouseMap
emptyMap = M.fromList [((0,0), True)]

emptyPos :: Position
emptyPos = (0,0)

emptyAccum :: Accumulator
emptyAccum = (emptyMap, (emptyPos, emptyPos), False)

insertAndMove2 :: Accumulator -> Char -> Accumulator
insertAndMove2 (m, (p1,p2), s) c =
  case s of
    True  -> (\x -> (M.insert x True m, (p1, x), False)) $ movPos p2 c
    False -> (\x -> (M.insert x True m, (x, p2), True )) $ movPos p1 c

insertAndMove  :: Accumulator -> Char -> Accumulator
insertAndMove (m, (p1,p2), _) c = (\x -> (M.insert x True m, (x, p2), False)) $ movPos p1 c

main :: IO ()
main = do
  file <- readFile "problem3.input"
  let eval  = foldl insertAndMove  emptyAccum file
  let eval2 = foldl insertAndMove2 emptyAccum file
  print $ M.size . getFirst $ eval
  print $ M.size . getFirst $ eval2
  where getFirst (x, y, z) = x
