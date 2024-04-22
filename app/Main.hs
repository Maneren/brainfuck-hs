module Main where

import Control.Monad.State.Lazy (
  MonadIO (liftIO),
  MonadState (get),
  StateT,
  evalStateT,
  gets,
  modify,
 )
import Data.Array.Unboxed (UArray, listArray, (!), (//))
import Data.Char (chr, ord)
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import System.Environment (getArgs)

-- Memory, Pointer, and Output
data VM = VM {mem :: UArray Int Word8, ptr :: Int, input :: String} deriving (Show)
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
    let body = parse xs
     in (Loop body) : parse (drop (length body + 1) xs)
  ']' -> []
  _ -> parse xs

optimize :: [Instruction] -> [OptimizedInstruction]
optimize [] = []
optimize (x : xs) = case x of
  Loop body -> OptimizedLoop (optimize body) : optimize xs
  Output -> OptimizedOutput : optimize xs
  Input -> OptimizedInput : optimize xs
  _ -> optimizeOne xs
 where
  optimizeOne [] = [optVersion (1 :: Int)]
  optimizeOne xs' =
    let count = length $ takeWhile (== x) xs'
     in optVersion (1 + count) : optimize (dropWhile (== x) xs')

  optVersion c = case x of
    Add -> OptimizedAdd $ fromIntegral c
    Sub -> OptimizedSub $ fromIntegral c
    MoveLeft -> OptimizedMoveLeft $ fromIntegral c
    MoveRight -> OptimizedMoveRight $ fromIntegral c
    _ -> error "optimize: invalid instruction"

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
  let initialState = VM{mem = listArray (0, memSize - 1) (repeat 0), ptr = 0, input = ""}
      instructions = optimize $ parse contents
  evalStateT (interpret instructions) initialState

interpret :: [OptimizedInstruction] -> VMState ()
interpret [] = return ()
interpret (x : xs) = do
  current <- interpretInstruction x
  case current of
    Just str -> do
      liftIO $ putChar str
      interpret xs
    Nothing -> interpret xs

interpretInstruction :: OptimizedInstruction -> VMState (Maybe Char)
interpretInstruction (OptimizedAdd c) = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', (mem' ! ptr') + c)]}
  return Nothing
interpretInstruction (OptimizedSub c) = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', (mem' ! ptr') - c)]}
  return Nothing
interpretInstruction (OptimizedMoveLeft c) = do
  ptr' <- gets ptr
  modify $ \vm -> vm{ptr = (ptr' - c)}
  return Nothing
interpretInstruction (OptimizedMoveRight c) = do
  ptr' <- gets ptr
  modify $ \vm -> vm{ptr = (ptr' + c)}
  return Nothing
interpretInstruction OptimizedOutput = do
  mem' <- gets mem
  ptr' <- gets ptr
  return $ Just $ chr' (mem' ! ptr')
 where
  chr' = chr . fromEnum
interpretInstruction OptimizedInput = do
  vm <- get
  let input' = input vm
      mem' = mem vm
      ptr' = ptr vm
      (charValue, newInput) = fromMaybe (0, "") $ uncons input' <&> \(x, xs) -> (ord' x, xs)
  modify $ \vm' -> vm'{mem = mem' // [(ptr', charValue)], input = newInput}
  return Nothing
 where
  ord' :: Char -> Word8
  ord' = toEnum . ord
interpretInstruction (OptimizedLoop xs) = do
  vm <- get
  if mem vm ! (ptr vm) == 0
    then return Nothing
    else do
      interpret xs
      interpretInstruction (OptimizedLoop xs)
