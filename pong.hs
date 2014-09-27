{-# LANGUAGE RecursiveDo #-}
import FRP.Helm
import FRP.Elerea.Simple
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window


data Player = Player { mx :: Double, my :: Double, speed :: Double}
data Ball   = Ball   { ballX :: Double, ballY :: Double, dx :: Double, dy :: Double}

type Pos = (Double, Double)

width :: Double
width = 800
height :: Double
height = 600

playerPos :: Player -> Pos
playerPos p = (mx p, my p)

inside :: (Ord a, Fractional a) => a -> a -> Bool
inside point wall = -wall / 2 <= point && point <= wall / 2

ballSignal :: SignalGen (Signal Pos)
ballSignal = mdo
    ballP <- (\b -> (ballX b, ballY b)) <~ do
        aiPos <- aiSignal ballP'
        plPos <- playerSignal
        transfer2 initBallState updater aiPos plPos
    ballP' <- delay (0, 0)  ballP
    return ballP
        where
            initBallState    = Ball { ballX = 0, ballY = 0, dx = 3, dy = 2}
            updater :: Pos -> Pos -> Ball -> Ball
            updater (x1, y1) (x2, y2) myball@(Ball{ballX = x, ballY = y, dx = dx', dy = dy'}) =
                myball{ ballX = newx, ballY = newy,
                        dx = if not (inside newx width) || colliding then
                            (-1) * (dx' + (signum dx') * 0.5)
                        else dx',
                        dy = if not (inside newy height) then
                            (-1) * dy'
                        else dy'
                        }
                where
                    colliding = colliding' x1 y1 || colliding' x2 y2
                    colliding' x' y' = x' - 10 <= newx && newx <= x' + 10 && y' - 50 <= newy && newy <= y' + 50
                    newx = x + dx'
                    newy = y + dy'

aiSignal :: Signal Pos ->  SignalGen (Signal Pos)
aiSignal ballP = playerPos <~ transfer initial newState ballP
        where
            initial = Player { mx =  375, my = 0, speed = 2.5}
            newState :: Pos -> Player -> Player
            newState (_, by) ai' = ai'{my = min (max newPos (-height / 2 + 50))
                                                (height / 2 - 50)
                                                }
                  where newPos = (my ai') + signum (by - my ai') * speed ai'

playerSignal :: SignalGen (Signal Pos)
playerSignal = playerPos <~ (foldp newState initState Keyboard.arrows)
    where
        initState = Player { mx = -375, my = 0, speed = 2.5}
        newState :: (Int, Int) -> Player -> Player
        newState (_, dir) me = me {
                my = min (max newPos (-height / 2 + 50)) (height / 2 - 50)
            }
            where newPos = my me + (realToFrac dir) * speed me

render :: (Int, Int) -> Pos -> Pos -> Pos -> Element
render (w, h) (playerX, playerY) (aiX, aiY) (x, y) =
    centeredCollage w h [move (playerX, playerY) $ filled white $ rect 20 100,
                         move (aiX, aiY)         $ filled white $ rect 20 100,
                         move (x, y)             $ filled white $ rect 10 10
                        ]

main :: IO ()
main = do
    engine <- startup defaultConfig {windowDimensions = (round width, round height)}
    run engine $ render <~ (Window.dimensions engine)
        ~~ playerSignal ~~ (ballSignal >>= aiSignal) ~~ ballSignal
