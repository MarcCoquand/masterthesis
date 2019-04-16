module Main where
import qualified Game
import qualified Model


main :: IO ()
main =
    start


start :: IO ()
start =
    Game.loop (welcomeMessage, Model.init)


welcomeMessage :: String
welcomeMessage =
    "Welcome to chess! Make a move by typing \
    \\"((startX, startY),(endX,endY))\""

