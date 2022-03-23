{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Create.Box  where -- only export moveBox  module Create.Box(moveBox) where
-- only export moveBox
import Control.Concurrent (threadDelay)


hyphens :: Int -> String
hyphens m = replicate (m + 2) '-'

spaces :: Int -> Maybe Int -> String
spaces n Nothing = ['|'] ++ replicate n ' ' ++ ['|']
spaces n (Just b1) = "|" ++ replicate b1 ' ' ++ "O" ++ replicate (n - b1 - 1) ' ' ++ "|"

makeBoxList :: (Int, Int) -> (Int, Int) -> [String]
makeBoxList (columns, rows) (_column, _row)
    | _column < 0 || _row < 0 || _column > columns - 1 || _row > rows - 1 = error "The ball is outside of the box!"
    | otherwise =
                [hyphens columns] ++ -- create top border
                replicate _row (spaces columns Nothing) ++ -- create list of rows with spaces up to _row 
                [spaces columns (Just _column)] ++ -- create 1 row with O at position _column
                replicate (rows - _row - 1) (spaces columns Nothing) ++ -- create remaining list of rows with spaces
                [hyphens columns] -- create bottom border

printBox :: (Int, Int) -> (Int, Int) -> Int -> IO ()
printBox (columns, rows) (_column, _row) time =
    threadDelay time >>
    clean >>
    print time >>
    print (_column, _row) >>
    putStrLn (unlines $ makeBoxList (columns,rows) (_column,_row))

clean :: IO ()
clean = putStr "\ESC[2J"

data Vector
    = GoUp
    | GoDown
    | GoLeft
    | GoRight
    | Direction Vector Vector
    deriving (Show)

data Velocity = Velocity {
                initialVel :: Int,
                finalVel :: Int,
                acceleration :: Int
}

type CurrentVel = Int

moveBox' :: (Int, Int) -> (Int, Int) -> Vector -> CurrentVel -> Velocity -> IO b
moveBox' (columns, rows) (_column, _row) vector time vel =
    case vector of
        (Direction GoDown GoDown)   | _row == rows-1                                            ->  action (columns, rows) (_column, _row -1) (Direction GoUp GoUp) deltaTime vel
                                    | otherwise                                                 ->  action (columns,rows) (_column, _row+1) (Direction GoDown GoDown) time vel

        (Direction GoUp GoUp)       | _row == 0                                                 ->  action (columns, rows) (_column,_row +1) (Direction GoDown GoDown) deltaTime vel
                                    | otherwise                                                 ->  action (columns, rows) (_column,_row -1) (Direction GoUp GoUp) time vel

        (Direction GoDown GoRight)  | _column == columns-1 && _row /= 0 && _row /= rows-1       -> action (columns, rows) (_column-1,_row +1) (Direction GoDown GoLeft) deltaTime vel
                                    | _column /= 0 && _column /= columns-1 && _row == rows-1    -> action (columns, rows) (_column+1,_row -1) (Direction GoUp GoRight) deltaTime vel
                                    | _column == columns-1 && _row == rows-1                    -> action (columns, rows) (_column-1,_row -1) (Direction GoUp GoLeft) deltaTime vel
                                    | otherwise                                                 -> action (columns, rows) (_column+1,_row +1) (Direction GoDown GoRight) time vel

        (Direction GoDown GoLeft)   | _column == 0 && _row /= 0 && _row /= rows-1               -> action (columns, rows) (_column+1,_row +1) (Direction GoDown GoRight) deltaTime vel
                                    | _column /= 0 && _row == rows-1                            -> action (columns, rows) (_column-1,_row -1) (Direction GoUp GoLeft) deltaTime vel
                                    | _column == 0 && _row /= 0                                 -> action (columns, rows) (_column+1,_row -1) (Direction GoUp GoRight) deltaTime vel
                                    | otherwise                                                 -> action (columns, rows) (_column-1,_row +1) (Direction GoDown GoLeft) time vel

        (Direction GoUp GoRight)    | _column == columns-1 && _row /= 0                         -> action (columns, rows) (_column-1,_row -1) (Direction GoUp GoLeft) deltaTime vel
                                    | _column /= 0 && _column /= columns-1 && _row == 0         -> action (columns, rows) (_column+1,_row +1) (Direction GoDown GoRight) deltaTime vel
                                    | _column == columns-1 && _row == 0                         -> action (columns, rows) (_column-1,_row +1) (Direction GoDown GoLeft) deltaTime vel
                                    | otherwise                                                 -> action (columns, rows) (_column+1,_row -1) (Direction GoUp GoRight) time vel

        (Direction GoUp GoLeft)     | _column == 0 && _row /= 0                                 -> action (columns, rows) (_column+1,_row -1) (Direction GoUp GoRight) deltaTime vel
                                    | _column /= 0 && _row == 0                                 -> action (columns, rows) (_column-1,_row +1) (Direction GoDown GoLeft) deltaTime vel
                                    | _column == 0 && _row == 0                                 -> action (columns, rows) (_column+1,_row +1) (Direction GoDown GoRight) deltaTime vel
                                    | otherwise                                                 -> action (columns, rows) (_column-1,_row -1) (Direction GoUp GoLeft) time vel
     where
        action (columns, rows) (newColumn, newRow) direction time vel = printBox (columns,rows) (newColumn,newRow) time >>
                                                                        moveBox' (columns,rows) (newColumn, newRow) direction time vel
        deltaTime = if time == finalVel vel then
                        time
                    else
                        time - acceleration vel