import FRP.Helm
import FRP.Elerea.Simple
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data Player = Player { mx :: Double, my :: Double, speed :: Double}
data Ball   = Ball   { ballX :: Double, ballY :: Double, dx :: Double, dy :: Double}
data State  = State  { ball :: Ball, player :: Player, ai :: Player}

type Pos = (Double, Double)

width :: Double
width = 800
height :: Double
height = 600

playerPos :: Player -> Pos
playerPos p = (mx p, my p)

ballSignal :: SignalGen (Signal Pos)
ballSignal = (\b -> (ballX b, ballY b)) <~ (foldp moveBall initialState ((,) <~ playerSignal ~~ aiSignal))
    where
        initialState = Ball { ballX = 0, ballY = 0, dx = 3, dy = 2}
        moveBall :: (Pos, Pos) -> Ball -> Ball
        moveBall ((x1, y1), (x2, y2)) myball@(Ball{ballX = x, ballY = y, dx = dx', dy = dy'}) =
            myball{ ballX = newx, ballY = newy,
                    dx = if newx > width  / 2 || newx < (-width / 2) || colliding then (-1) * dx' else dx',
                    dy = if newy > height / 2 || newy < (-height/ 2) then (-1) * dy' else dy'
                    }
            where
                colliding = colliding' x1 y1 || colliding' x2 y2
                colliding' x' y' = x' - 10 <= newx && newx <= x' + 10 && y' - 50 <= newy && newy <= y' + 50
                newx = x + dx'
                newy = y + dy'

playerSignal :: SignalGen (Signal Pos)
playerSignal = playerPos <~ (foldp newState initState Keyboard.wasd)
    where
        initState = Player { mx = -375, my = 0, speed = 2.5}
        newState :: (Int, Int) -> Player -> Player
        newState (_, dir) me = me {
                my = min (max newPos (-height / 2 + 50)) (height / 2 - 50)
            }
            where newPos = my me + (realToFrac dir) * speed me

aiSignal :: SignalGen (Signal Pos)
aiSignal = playerPos <~ (foldp newState initial Keyboard.arrows)
    where initial = Player { mx =  375, my = 0, speed = 2.5}
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
    run engine $ render <~ (Window.dimensions engine) ~~ playerSignal
                                        ~~ aiSignal ~~ ballSignal
