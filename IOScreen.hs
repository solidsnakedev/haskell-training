
import System.IO (hFlush, stdin,stdout, hSetBuffering, BufferMode (NoBuffering))
import Control.Concurrent ( threadDelay )

data Screen = Screen {
    columns :: [Int],
    rows :: [Int],
    limit :: Int,
    fillPointer :: Char,
    emptyPointer :: Char
}
    deriving Show

screen :: Screen
screen = Screen {
    columns = [0 .. limit screen],
    rows = [0 .. limit screen],
    limit = 60,
    fillPointer = 'X',
    emptyPointer = '.'
    }

type Position = (Int, Int)
data Pixel = Pixel {
        positionX :: Int,
        positionY :: Int,
        pointer :: Char
}
    deriving Show

newtype Pixels = Pixels [Pixel]


createPixels pixels position screen =
    map (\rows -> map (\x -> if positionX x == fst position && positionY x == snd position then Pixel (positionX x)  (positionY x) (fillPointer screen) else x ) rows ) pixels

--initDraw :: Screen -> IO ()
initDraw screen =  pixels
     where
        pixels = map (\row -> zipWith3 Pixel (replicate (limit screen) row) (columns screen) (replicate (limit screen) (emptyPointer screen))) (rows screen)

drawPixels pixels = do
    pause
    clean
    mapM_ (\rows -> mapM_ (\x -> putChar (pointer x)) rows >> putChar '\n') pixels
    putChar '\n'

drawPixelsDelay [] _ = return ()
drawPixelsDelay (pixels:pixelsxs) time = do
    threadDelay time
    clean
    mapM_ (\rows -> mapM_ (\x -> putChar (pointer x)) rows >> putChar '\n') pixels
    putChar '\n'
    putStrLn . show $ (time+300000)
    drawPixelsDelay pixelsxs (time+300000)


printAll pixels [] screen = return ()
printAll pixels ((x,y):poxs) screen = do
     drawPixels n
     printAll n poxs screen
    where
        n = createPixels pixels (x,y) screen


pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 1000000

clean :: IO ()
clean = putStr "\ESC[2J"

primes = filterPrime [2.. 100]
  where 
      filterPrime [] = []
      filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


main :: IO ()
main = do
    --hSetBuffering stdin  NoBuffering
    --hSetBuffering stdout NoBuffering
    let baseScreen = initDraw screen
    drawPixels baseScreen
    putChar '\n'

    let seriesY =[
                map ( ^ 2) $ take 10 [1 ..],
                map fib [0 .. 20],
                map (\x -> abs (x -1)) [-30 .. 30],
                map (\z -> 10 + round  (10 * cos z) ) [1 .. 50]
                ]
    let seriesXY = map (\y -> zip [0 .. length y] y) seriesY ++
                   [map (\(x,y) -> (x+20,y+20))[(1,2),(1,3),(1,6),(1,7),(2,1),(2,4),(2,5),(2,8),(3,2),(3,7),(4,3),(4,6),(5,4),(5,5)]]
    mapM_ (\xy -> printAll baseScreen xy screen) seriesXY
    
    let y = [0 .. 9]
    let xy = zip [0 .. length y +1] y
    let newScreen = map (\xy -> createPixels baseScreen xy screen) xy
     --mapM_ (\nScreen -> drawPixels nScreen ) newScreen
    drawPixelsDelay newScreen 1000000