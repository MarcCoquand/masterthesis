module Main where
import qualified Game

main :: IO ()
main = Game.run >> putStrLn "Game exit"
