{-
 - Game logic, turns e.t.c.
 - Future developers should replace String Data.Text to gain extra performance.
 -}
module Game where


import           Board               (Board)
import qualified Board
import           Control.Monad.Extra (iterateM)
import qualified Coord
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromJust, maybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Text.Read           (readMaybe)



-- * PLAYER


data Player
    = Black
    | White
    deriving (Eq)


nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black


instance Show Player where
    show Black = "Black"
    show White = "White"



-- * PIECE


data Piece
    = Pawn Bool -- ^ Has the pawn moved or not
    | Knight


instance Show Piece where
    show (Pawn _) = "P"
    show Knight   = "K"


updatePieceInfo :: Piece -> Piece
updatePieceInfo Knight =
    Knight
updatePieceInfo (Pawn _) =
    -- Pawn has moved once
    Pawn True



-- * SQUARE


data Square
    = Blank
    | IsPiece Player Piece


instance Show Square where
    show Blank = "  "
    show (IsPiece White piece) =
        "w" ++ show piece
    show (IsPiece Black piece) =
        "b" ++ show piece


collisionCheck :: Square -> Bool
collisionCheck Blank =
    False
collisionCheck (IsPiece _ _) =
    True


attackCheck :: Player -> Square -> Bool
attackCheck player Blank =
    True
attackCheck player (IsPiece pieceOwner _) =
    player == pieceOwner


checkSquare :: (Square -> Bool) -> Board Square -> Board.Coord -> Bool
checkSquare check fromBoard =
    check . Board.get fromBoard


checkCoordinate :: Board Square -> (Square -> Bool) -> (Int, Int) -> Bool
checkCoordinate board withCheck coord =
  let
      maybeCoord =
          Board.makeCoord board coord
  in
      maybe True (checkSquare withCheck board) maybeCoord


getPiece :: Square -> Maybe (Player, Piece)
getPiece square =
    case square of
        Blank ->
            Nothing

        IsPiece player piece ->
            Just (player, piece)


movePieceIfAllowed
  :: Set Board.Coord
  -> Player
  -> Piece
  -> Board.Coord
  -> Board.Coord
  -> [(Board.Coord, Square)]
movePieceIfAllowed moveSet player piece start destination =
  if Set.member destination moveSet
    then
      [(start, Blank), (destination, IsPiece player (updatePieceInfo piece))]
    else []


initialBoard :: Board Square
initialBoard =
  let wP = IsPiece White (Pawn False)
      bP = IsPiece Black (Pawn False)
      wK = IsPiece White Knight
      bK = IsPiece Black Knight
      bl = Blank
  in
      -- fromJust is ok here as we want the program to crash if the board is
      -- not constructed correctly.
      fromJust . Board.construct $
      [ wP, wP, wK, wP, wP, wK, wP, wP
      , wP, wP, wP, wP, wP, wP, wP, wP
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bl, bl, bl, bl, bl, bl, bl, bl
      , bP, bP, bP, bP, bP, bP, bP, bP
      , bP, bP, bK, bP, bP, bK, bP, bP
      ]



-- * ENVIRONMENT


data Environment = UnsafeMakeEnvironment
    { currentPlayer :: Player
    , currentBoard  :: Board Square
    , stateHandler  :: Coord.Handle
    }


makeEnvironment :: Player -> Board Square -> Environment
makeEnvironment player board =
    UnsafeMakeEnvironment
        { currentPlayer = player
        , currentBoard = board
        , stateHandler = effectfulHandle player board
        }


initialEnvironment :: Environment
initialEnvironment =
    makeEnvironment White initialBoard


effectfulHandle :: Player -> Board Square -> Coord.Handle
effectfulHandle player board =
        Coord.MakeHandle
            { Coord.isCollision = \coord ->
                -- move as much logic out as possible from effectful
                -- computations for simple unit tests
                checkCoordinate board collisionCheck coord

            , Coord.isIndomitable = \coord ->
                checkCoordinate board (attackCheck player) coord
            }



-- * GAME


data Error
    = InvalidStartCoord
    | InvalidDestinationCoord
    | ParseError
    deriving (Show)



type Move = ((Int,Int),(Int,Int))


type State = (String, Environment)


getPieceFromBoard
    :: Board Square
    -> Board.Coord
    -> Maybe (Player, Piece)
getPieceFromBoard board pos =
    getPiece $ Board.get board pos


makeMoveSet
    :: Environment
    -> Board.Coord
    -> Piece
    -> Set Board.Coord
makeMoveSet env pos Knight =
    let
        moves =
            Coord.knightMoves (stateHandler env) (Board.extractCoord pos)
    in
        Board.coordSet (currentBoard env) $ moves
makeMoveSet env pos (Pawn hasMoved) =
        case (currentPlayer env) of
            Black ->
                let
                    moves =
                        Coord.pawnMoves
                            (stateHandler env)
                            hasMoved
                            Coord.South
                            (Board.extractCoord pos)
                in
                    Board.coordSet (currentBoard env) $ moves

            White ->
                let
                    moves =
                        Coord.pawnMoves
                            (stateHandler env)
                            hasMoved
                            Coord.North
                            (Board.extractCoord pos)
                in
                    Board.coordSet (currentBoard env) $ moves


nextBoard
  :: Environment
  -> Board.Coord
  -> Board.Coord
  -> [(Board.Coord, Square)]
nextBoard env start destination =
    let
        maybePiece =
            getPieceFromBoard (currentBoard env) start
    in
        case maybePiece of
          Just (pieceOwner, piece) ->
            if pieceOwner /= (currentPlayer env)
                then []
            else
                let
                    moveSet =
                        makeMoveSet env start piece
                in
                    movePieceIfAllowed
                        moveSet
                        (currentPlayer env)
                        piece
                        start
                        destination

          Nothing -> []


move
  :: Environment
  -> Move
  -> Either Error (Player, Board Square)
move env (start,destination) = do

  -- Validate coords
    let maybeDestinationCoord =
            Board.makeCoord (currentBoard env) destination

    let maybeStartCoord =
            Board.makeCoord (currentBoard env) destination

    case (maybeStartCoord, maybeDestinationCoord) of
        (Just startCoord, Just destinationCoord) ->
            let
                result = nextBoard env startCoord destinationCoord
                newBoard = Board.update (currentBoard env) result
            in
                Right (nextPlayer (currentPlayer env), newBoard)

        (Nothing, _) ->
            Left InvalidStartCoord

        (_ , Nothing) ->
            Left InvalidDestinationCoord



parseInput :: String -> Either Error Move
parseInput str =
    let
        input =
            readMaybe str
    in
        case input of
            Just input ->
                Right input
            Nothing ->
                Left ParseError


attemptMove :: Environment -> String -> Either Error (Player, Board Square)
attemptMove env str =
    do  moves <- parseInput str
        move env moves


runMove :: State -> State
runMove (str,env) =
    let updated =
            attemptMove env str

    in
        case updated of
            Right (updatedPlayer, updatedBoard) ->
                ("Move successful", makeEnvironment updatedPlayer updatedBoard)

            Left InvalidStartCoord ->
                ("Invalid start square.", env)

            Left InvalidDestinationCoord ->
                ("Invalid destination square.", env)

            Left ParseError ->
                ("Parse error.", env)


step :: State -> IO State
step (str, env) =
    do  putStrLn str
        putStrLn (show (currentBoard env))
        putStrLn ("Current player is: " ++ (show . currentPlayer $ env))
        next <- getLine
        return . runMove $ (next, env)


loop :: IO [State]
loop =
    iterateM step ("Welcome to game", initialEnvironment)
