{-# LANGUAGE RecursiveDo #-}
import FRP.Helm
import FRP.Elerea.Simple
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window


data Player = Player { mx, my, speed , paddleWidth, paddleHeight :: Double}
data Ball   = Ball   { ballX , ballY , dx , dy, rad :: Double}

type Pos = (Double, Double)

width :: Double
width = 800
height :: Double
height = 600

playerPos :: Player -> Pos
playerPos p = (mx p, my p)

inside :: (Ord a, Fractional a) => a -> a -> Bool
inside point wall = -wall / 2 <= point && point <= wall / 2

initBallState :: Ball
initBallState    = Ball { ballX = 0, ballY = 0, dx = 3, dy = 2, rad = 5}

ballSignal :: SignalGen (Signal Ball)
ballSignal = mdo
    ballP <- do
        aiPos <- aiSignal ballP'
        plPos <- playerSignal
        transfer2 initBallState updater aiPos plPos
    ballP' <- delay initBallState  ballP
    return ballP
        where
            updater :: Player -> Player -> Ball -> Ball
            updater Player{mx = x1, my = y1} Player{mx = x2, my = y2} myball@(Ball{ballX = x, ballY = y, dx = dx', dy = dy'}) =
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

aiSignal :: Signal Ball ->  SignalGen (Signal Player)
aiSignal ballP =  transfer initial newState ballP
        where
            initial = Player { mx =  375, my = 0, speed = 2.5, paddleWidth = 20, paddleHeight = 100}
            newState :: Ball -> Player -> Player
            newState Ball{ballY =  by} ai' = ai'{my = min (max newPos (-height / 2 + 50))
                                                (height / 2 - 50)
                                                }
                  where newPos = (my ai') + signum (by - my ai') * speed ai'

playerSignal :: SignalGen (Signal Player)
playerSignal = foldp newState initState Keyboard.arrows
    where
        initState = Player { mx = -375, my = 0, speed = 2.5, paddleWidth = 20, paddleHeight = 100}
        newState :: (Int, Int) -> Player -> Player
        newState (_, dir) me = me {
                my = min (max newPos (-height / 2 + 50)) (height / 2 - 50)
            }
            where newPos = my me + (realToFrac dir) * speed me

render :: (Int, Int) -> Player -> Player -> Ball -> Element
render (w, h) player ai ball  =
    centeredCollage w h [move (mx player, my player)   $ filled white $ rect (paddleWidth player) (paddleHeight player),
                         move (mx ai, my ai)           $ filled white $ rect (paddleWidth ai) (paddleHeight ai),
                         move (ballX ball, ballY ball) $ filled white $ circle (rad ball)
                        ]

main :: IO ()
main = do
    engine <- startup defaultConfig {windowDimensions = (round width, round height)}
    run engine $ render <~ (Window.dimensions engine)
        ~~ playerSignal ~~ (ballSignal >>= aiSignal) ~~ ballSignal
