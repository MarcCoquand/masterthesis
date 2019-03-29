module Main where
import qualified Game

main :: IO ()
main = Game.loop >> putStrLn "Game exit"
