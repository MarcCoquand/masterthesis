{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game where


import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer.Lazy (MonadWriter)
import qualified Control.Monad.Writer.Lazy as Writer
import Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (guard)
import qualified Board
import Board (Board)
import qualified Piece
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.Maybe (fromJust)



-- TYPES


data Player 
    = Black
    | White
    deriving (Eq)


data Piece 
    = Pawn Bool -- Has moved?
    | Knight


data Square 
    = Blank
    | HasPiece Player Piece


type HasGame m = (MonadState (Player, Board Square) m, Piece.HasCheck m)


data Error 
    = InvalidStartCoord
    | InvalidDestinationCoord



-- INSTANCES


collisionCheck :: Square -> Bool
collisionCheck Blank =
    False
collisionCheck (HasPiece _ _ ) =
    True


attackCheck :: Player -> Square -> Bool
attackCheck player Blank =
    True
attackCheck player (HasPiece pieceOwner _) =
    player == pieceOwner


instance Show Piece where
    show (Pawn _) = "P"
    show Knight = "K"


instance Show Square where
    show Blank = "  "
    show (HasPiece White piece) =
        "w" ++ show piece
    show (HasPiece Black piece) =
        "b" ++ show piece


instance (Monad m, MonadState (Player, Board Square) m) => Piece.HasCheck m where
    isCollision coord =
        do  (player, board) <- State.get
            let maybeCoord =
                    Board.makeCoord board coord
            case maybeCoord of 
                Just c ->
                    let 
                        square =
                            Board.get board c
                    in 
                        return . collisionCheck $ square
                Nothing ->
                    return False

    isIndomitable coord =
        do  (player, board) <- State.get
            let maybeCoord =
                    Board.makeCoord board coord
            case maybeCoord of 
                Just c ->
                    let 
                        square =
                            Board.get board c
                    in 
                        return . attackCheck player $ square
                Nothing ->
                    return True



-- MOVE

                
getPiece :: Square -> Maybe (Player,Piece)
getPiece square = 
    case square of
        Blank -> 
            Nothing

        HasPiece player piece -> 
            Just (player,piece)


getPieceFromBoard :: (HasGame m) => Board.Coord -> m (Maybe (Player,Piece))
getPieceFromBoard pos = 
    do  (_, board) <- State.get
        return . getPiece $ Board.get board pos


makeMoveSet :: (HasGame m) => Board.Coord -> Piece -> m (Set Board.Coord)
makeMoveSet pos Knight =
    do  (_, board) <- State.get
        moves <- Piece.knightMoves (Board.extractCoord pos)
        return . Board.coordSet board $ moves
makeMoveSet pos (Pawn hasMoved) =
    do  (player, board) <- State.get
        case player of 
            Black ->
                do  moves <- 
                        Piece.pawnMoves 
                            hasMoved 
                            Piece.South 
                            (Board.extractCoord pos)
                    return . Board.coordSet board $ moves

            White -> 
                do  moves <- 
                        Piece.pawnMoves 
                            hasMoved 
                            Piece.North 
                            (Board.extractCoord pos)
                    return . Board.coordSet board $ moves
            

updatePieceInfo :: Piece -> Piece
updatePieceInfo Knight = Knight
updatePieceInfo (Pawn _) = Pawn True


movePieceIfAllowed :: 
    Set Board.Coord 
    -> Player 
    -> Piece
    -> Board.Coord 
    -> Board.Coord 
    -> [(Board.Coord, Square)]
movePieceIfAllowed moveSet player piece start destination 
    | (Set.member destination moveSet) =
            [ (start, Blank)
            , (destination, (HasPiece player (updatePieceInfo piece)))
            ]
    | otherwise = []


nextBoard :: HasGame m 
    => Board.Coord 
    -> Board.Coord 
    -> m [(Board.Coord, Square)]
nextBoard start destination =
    do  (player, board) <- State.get
        maybePiece <- getPieceFromBoard start 
        
        case maybePiece of 
            Just (pieceOwner, piece) ->
                if (pieceOwner /= player) then
                    return []

                else 
                    do  moveSet <- makeMoveSet start piece
                        return $ 
                            movePieceIfAllowed 
                                moveSet 
                                player 
                                piece 
                                start 
                                destination

            Nothing ->
                return []
        

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black


move :: (HasGame m) 
    => (Int,Int) 
    -> (Int,Int) 
    -> m (Either Error (Player, Board Square))
move start destination = 
    do  (player, board) <- State.get

        -- Validate coords
        let maybeDestinationCoord =
                Board.makeCoord board destination

        let maybeStartCoord =
                Board.makeCoord board destination

        case (maybeStartCoord, maybeDestinationCoord) of 
            (Just startCoord, Just destinationCoord) ->
                do  result <- nextBoard startCoord destinationCoord
                    let newBoard =
                            Board.update board result  
                    return . Right $ (nextPlayer player, newBoard)
            (Nothing,_) -> 
                return . Left $ InvalidStartCoord
            (_,Nothing) -> 
                return . Left $ InvalidDestinationCoord


-- GAME


parseInput :: String -> Maybe ((Int, Int),(Int,Int))
parseInput string =
    readMaybe string


turn :: (HasGame m, MonadWriter String m, MonadReader String m) => m ()
turn =
    do  input <- Reader.ask
        let maybeMove =
                parseInput input

        case maybeMove of 
            Just (start, destination) -> 
                do  maybeUpdated <- move start destination
                    case maybeUpdated of 
                        Right updated -> 
                            do  Writer.tell "Move successful"
                                State.put updated

                        Left InvalidStartCoord ->
                            Writer.tell 
                                "Invalid start square."

                        Left InvalidDestinationCoord ->
                            Writer.tell 
                                "Invalid destination square."
                
            Nothing -> 
                Writer.tell "Invalid move, format must be ((X,Y),(X,Y))"
        
        
initialize :: Board Square
initialize = 
    let 
        wP = HasPiece White (Pawn False)
        bP = HasPiece Black (Pawn False)
        wK = HasPiece White Knight
        bK = HasPiece Black Knight
        bl = Blank
    in
        fromJust $ Board.construct 
            [ wP,wP,wK,wP,wP,wK,wP,wP
            , wP,wP,wP,wP,wP,wP,wP,wP
            , bl,bl,bl,bl,bl,bl,bl,bl
            , bl,bl,bl,bl,bl,bl,bl,bl
            , bl,bl,bl,bl,bl,bl,bl,bl
            , bl,bl,bl,bl,bl,bl,bl,bl
            , bP,bP,bP,bP,bP,bP,bP,bP
            , bP,bP,bK,bP,bP,bK,bP,bP
            ]


        
