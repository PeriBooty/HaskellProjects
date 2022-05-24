import Data.Char
import Data.Function
import Data.List
import System.IO

clear = putStr "\ESC[2J" >> hFlush stdout

main = clear >> mainLoop (replicate 9 ' ')

swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = map (map snd) . groupBy ((==) `on` ((`div`n) . fst)) . zip [0..]

parseInput :: String -> Maybe (Char, Int)
parseInput (sym : ' ' : idx : _) | sym `elem` "XO" && isDigit idx && idx /= '9' = Just (sym, digitToInt idx)
parseInput _ = Nothing

prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

getPositionAndPlayer :: IO (Char, Int)
getPositionAndPlayer = prompt "X or O, Index: " >>= maybe getPositionAndPlayer return . parseInput

modifyBoard :: (Char, Int) -> [Char] -> [Char]
modifyBoard (new_elem, index) lst
    | isSpace $ lst !! index =
        take index lst ++ [new_elem] ++ drop (index+1) lst
    | otherwise = lst

printBoard :: [Char] -> IO ()
printBoard board = putStrLn (intercalate 
    "\n---+---+---\n"
    (map ((" " ++) . intercalate " | " . map pure) (splitEvery 3 board)))

mainLoop :: [Char] -> IO b
mainLoop board = 
    printBoard board >>
    (mainLoop =<< (modifyBoard <$> getPositionAndPlayer <*> pure board))