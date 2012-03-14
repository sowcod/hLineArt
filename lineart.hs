import System.IO
import Control.Concurrent

left = 3
top  = 3
right = 50
bottom = 30

cls :: IO ()
cls = do putStr "\ESC[2J"

type Pos = (Int, Int)
type PosF = (Float, Float)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

lineboardx :: PosF -> PosF -> Float -> [PosF]
lineboardx (x1, y1) (x2, y2) xn | x1 > x2   = lineboardx (x2, y2) (x1, y1) x2
                                | xn >= x2  = [(x2, y2)]
                                | otherwise = (xa, ya) : lineboardx (x1, y1) (x2, y2) (xa + 1)
                                 where (xa, ya) = (xn, yp)
                                       yp = d * (xn - x1) + y1
                                       d = (y2 - y1) / (x2 - x1)

lineboard :: PosF -> PosF -> [PosF]
lineboard (x1, y1) (x2, y2) | dx > dy   = lineboardx (x1, y1) (x2, y2) x1
                            | otherwise = [(y, x) | (x, y) <- lineboardx (y1, x1) (y2, x2) y1]
                             where dx = abs(x2 - x1)
                                   dy = abs(y2 - y1)

toPos :: PosF -> Pos
toPos (x, y) = (round(x), round(y))

showcellsf :: [PosF] -> IO ()
showcellsf b = seqn [writeat (toPos p) "*" | p <- b]

type MovingPoint = (PosF, PosF)

move :: MovingPoint -> MovingPoint
move ((px, py), (mx, my)) = ((px + mx, py + my), (mx, my))

reflect ((px, py), (mx, my)) | px > right  = reflect ((right  + (right  - px), py), (-mx, my))
                             | px < left   = reflect ((left   + left    - px , py), (-mx, my))
                             | py > bottom = reflect ((px, bottom + (bottom - py)), (mx, -my))
                             | py < top    = reflect ((px, top    + top     - py ), (mx, -my))
                             | otherwise   = ((px, py), (mx, my))

mp2posF :: MovingPoint -> PosF
mp2posF (p,m) = p

linesboard :: [PosF] -> [PosF]
linesboard (p:ps) | null ps   = []
                  | otherwise = lineboard p (head ps) ++ linesboard ps

polyboard :: [PosF] -> [PosF]
polyboard (p:ps) = lineboard p (last ps) ++ linesboard (p:ps)

frameboard :: [PosF]
frameboard = polyboard [(left-1, top-1), (right+1, top-1), (right+1, bottom+1), (left-1, bottom+1)]

animateArr :: [MovingPoint] -> IO ()
animateArr mps = do cls
                    showcellsf frameboard
                    showcellsf (polyboard [mp2posF mp | mp <- mpns])
                    goto (0,0)
                    threadDelay 50000
                    animateArr mpns
                 where mpns = [reflect (move mpn) | mpn <- mps]

animate_test = animateArr [((25,15), (-0.4,-1)),((30,10), (1,0.8)),((20,05), (-1.4,0.8)), ((22,08), (1.4,1.2))]

main = animate_test
