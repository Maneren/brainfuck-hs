module Main where

import Control.Monad.State.Lazy (MonadIO (liftIO), MonadState (get), StateT, evalStateT, gets, modify)

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

-- >>> parse "-[+>+<]"
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
      instructions = parse contents
  evalStateT (interpret instructions) initialState

interpret :: [Instruction] -> VMState ()
interpret [] = return ()
interpret (x : xs) = do
  current <- interpretInstruction x
  case current of
    Just str -> do
      liftIO $ putChar str
      interpret xs
    Nothing -> interpret xs

interpretInstruction :: Instruction -> VMState (Maybe Char)
interpretInstruction Add = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', (mem' ! ptr') + 1)]}
  return Nothing
interpretInstruction Sub = do
  mem' <- gets mem
  ptr' <- gets ptr
  modify $ \vm -> vm{mem = mem' // [(ptr', (mem' ! ptr') - 1)]}
  return Nothing
interpretInstruction MoveLeft = do
  ptr' <- gets ptr
  modify $ \vm -> vm{ptr = (ptr' - 1)}
  return Nothing
interpretInstruction MoveRight = do
  ptr' <- gets ptr
  modify $ \vm -> vm{ptr = (ptr' + 1)}
  return Nothing
interpretInstruction Output = do
  mem' <- gets mem
  ptr' <- gets ptr
  return $ Just $ chr' (mem' ! ptr')
 where
  chr' = chr . fromEnum
interpretInstruction Input = do
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
interpretInstruction (Loop xs) = do
  vm <- get
  if mem vm ! (ptr vm) == 0
    then return Nothing
    else do
      interpret xs
      interpretInstruction (Loop xs)
