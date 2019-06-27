module Main where
import qualified Game
import qualified Model


main :: IO ()
main =
    Game.loop (welcomeMessage, Model.init)



welcomeMessage :: String
welcomeMessage =
    "Welcome to chess! Make a move by typing \
    \\"((startX, startY),(endX,endY))\""

