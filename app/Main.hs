module Main where

import Control.Monad.State.Lazy (
  MonadIO (liftIO),
  MonadState (get),
  StateT,
  evalStateT,
  gets,
  modify,
 )
import Data.Char (chr, ord)
import Data.List (uncons)
import Data.Vector.Unboxed (Vector, replicate, (!), (//))
import Data.Word (Word8)
import System.Environment (getArgs)

data VM = VM {mem :: Vector Word8, ptr :: Int, input :: String} deriving (Show)
type VMState = StateT VM IO

data Instruction
  = Add
  | Sub
  | MoveLeft
  | MoveRight
  | Output
  | Input
  | Loop [Instruction]
  deriving (Eq, Show)

data OptimizedInstruction
  = OptimizedAdd Word8
  | OptimizedSub Word8
  | OptimizedMoveLeft Int
  | OptimizedMoveRight Int
  | OptimizedClear
  | OptimizedLoop [OptimizedInstruction]
  | OptimizedOutput
  | OptimizedInput
  deriving (Show)

parse :: String -> [Instruction]
parse [] = []
parse (x : xs) = case x of
  '+' -> Add : parse xs
  '-' -> Sub : parse xs
  '<' -> MoveLeft : parse xs
  '>' -> MoveRight : parse xs
  '.' -> Output : parse xs
  ',' -> Input : parse xs
  '[' ->
    let (body, xs') = consumeLoopBody xs 1
     in (Loop $ parse body) : parse xs'
  _ -> parse xs
 where
  consumeLoopBody :: String -> Int -> (String, String)
  consumeLoopBody [] _ = ([], [])
  consumeLoopBody (x' : xs') level
    | x' == ']' && level == 1 = ([], xs')
    | x' == '[' = consumeChar (level + 1)
    | x' == ']' = consumeChar (level - 1)
    | otherwise = consumeChar level
   where
    consumeChar level' =
      let (body, xs'') = consumeLoopBody xs' level'
       in (x' : body, xs'')

optimize :: [Instruction] -> [OptimizedInstruction]
optimize [] = []
optimize (x : xs) = case x of
  Loop body -> optimizeLoop body : optimize xs
  Output -> OptimizedOutput : optimize xs
  Input -> OptimizedInput : optimize xs
  _ -> optimizeOne xs
 where
  optimizeOne [] = [optVersion 1]
  optimizeOne xs' =
    let count = length $ takeWhile (== x) xs'
     in optVersion (1 + count) : optimize (dropWhile (== x) xs')

  optVersion :: Int -> OptimizedInstruction
  optVersion c = case x of
    Add -> OptimizedAdd $ fromIntegral c
    Sub -> OptimizedSub $ fromIntegral c
    MoveLeft -> OptimizedMoveLeft $ fromIntegral c
    MoveRight -> OptimizedMoveRight $ fromIntegral c
    _ -> error "optimize: invalid instruction"

  optimizeLoop [] = error "optimizeLoop: empty loop body"
  optimizeLoop [Sub] = OptimizedClear
  optimizeLoop [Add] = OptimizedClear
  optimizeLoop body = OptimizedLoop (optimize body)

main :: IO ()
main = do
  args <- getArgs
  (file, memSize) <- case args of
    [file, memSize] -> do
      memSize' <- readIO memSize
      return (file, memSize')
    [file] -> do
      return (file, 1024)
    _ -> error "Usage: ./brainfuck <file>"

  contents <- readFile file
  let initialState = VM{mem = Data.Vector.Unboxed.replicate memSize 0, ptr = 0, input = ""}
      instructions = optimize $ parse contents
  evalStateT (interpret instructions) initialState

interpret :: [OptimizedInstruction] -> VMState ()
interpret [] = return ()
interpret (x : xs) = do
  interpretInstruction x
  interpret xs

interpretInstruction :: OptimizedInstruction -> VMState ()
interpretInstruction (OptimizedAdd c) = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', (mem' ! ptr') + c)]}
interpretInstruction (OptimizedSub c) = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', (mem' ! ptr') - c)]}
interpretInstruction (OptimizedMoveLeft c) = do
  ptr' <- gets ptr
  modify $ \vm -> vm{ptr = (ptr' - c)}
interpretInstruction (OptimizedMoveRight c) = do
  ptr' <- gets ptr
  modify $ \vm -> vm{ptr = (ptr' + c)}
interpretInstruction OptimizedOutput = do
  mem' <- gets mem
  ptr' <- gets ptr
  liftIO $ putChar $ chr' (mem' ! ptr')
 where
  chr' :: Word8 -> Char
  chr' = chr . fromEnum
interpretInstruction OptimizedInput = do
  vm <- get
  let input' = input vm
      mem' = mem vm
      ptr' = ptr vm
      (charValue, newInput) = maybe (0, "") (\(x, xs) -> (ord' x, xs)) $ uncons input'
  modify $ \vm' -> vm'{mem = mem' // [(ptr', charValue)], input = newInput}
 where
  ord' :: Char -> Word8
  ord' = toEnum . ord
interpretInstruction OptimizedClear = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', 0)]}
interpretInstruction (OptimizedLoop body) = do
  ptr' <- gets ptr
  mem' <- gets mem
  if mem' ! ptr' == 0
    then return ()
    else do
      interpret body
      interpretInstruction (OptimizedLoop body)
