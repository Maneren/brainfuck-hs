module Main where

import Control.Monad.State.Lazy (
  MonadIO (liftIO),
  StateT,
  evalStateT,
  gets,
  modify,
 )
import Data.Char (chr, ord)
import Data.List (uncons)
import Data.Vector.Unboxed (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)

data VM = VM
  { mem :: MVector MV.RealWorld Word8
  , ptr :: Int
  , input :: String
  }

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
  hSetBuffering stdout NoBuffering

  args <- getArgs
  (file, memSize) <- case args of
    [file, memSize] -> do
      memSize' <- readIO memSize
      return (file, memSize')
    [file] -> do
      return (file, 256)
    _ -> error "Usage: ./brainfuck <file>"

  contents <- readFile file
  initialMemory <- MV.new memSize :: IO (MVector MV.RealWorld Word8)

  let initialState = VM{mem = initialMemory, ptr = 0, input = ""}
      instructions = optimize $ parse contents

  evalStateT (interpret instructions) initialState
interpret :: [OptimizedInstruction] -> VMState ()
interpret [] = return ()
interpret (x : xs) = do
  interpretOne x
  interpret xs
 where
  interpretOne :: OptimizedInstruction -> VMState ()
  interpretOne (OptimizedAdd c) = interpretAdd c
  interpretOne (OptimizedSub c) = interpretSub c
  interpretOne (OptimizedMoveLeft c) = interpretMoveLeft c
  interpretOne (OptimizedMoveRight c) = interpretMoveRight c
  interpretOne OptimizedOutput = interpretOutput
  interpretOne OptimizedInput = interpretInput
  interpretOne OptimizedClear = interpretClear
  interpretOne (OptimizedLoop body) = interpretLoop body

  interpretAdd :: Word8 -> VMState ()
  interpretAdd c = do
    mem' <- gets mem
    ptr' <- gets ptr
    value <- MV.read mem' ptr'

    MV.write mem' ptr' (value + c)

  interpretSub :: Word8 -> VMState ()
  interpretSub c = do
    mem' <- gets mem
    ptr' <- gets ptr
    value <- MV.read mem' ptr'

    MV.write mem' ptr' (value - c)

  interpretMoveLeft :: Int -> VMState ()
  interpretMoveLeft c = modify $ \vm -> vm{ptr = (ptr vm - c)}

  interpretMoveRight :: Int -> VMState ()
  interpretMoveRight c = modify $ \vm -> vm{ptr = (ptr vm + c)}

  interpretOutput = do
    mem' <- gets mem
    ptr' <- gets ptr
    value <- MV.read mem' ptr'

    liftIO $ putChar $ chr' value
   where
    chr' :: Word8 -> Char
    chr' = chr . fromEnum

  interpretInput = do
    input' <- gets input
    mem' <- gets mem
    ptr' <- gets ptr

    let (charValue, newInput) = maybe (0, "") (\(ch, rest) -> (ord' ch, rest)) $ uncons input'

    MV.write mem' ptr' charValue

    modify $ \vm' -> vm'{input = newInput}
   where
    ord' :: Char -> Word8
    ord' = fromIntegral . ord

  interpretClear = do
    mem' <- gets mem
    ptr' <- gets ptr
    MV.write mem' ptr' 0

  interpretLoop :: [OptimizedInstruction] -> VMState ()
  interpretLoop body = do
    mem' <- gets mem
    ptr' <- gets ptr

    cond <- MV.read mem' ptr'

    if cond == 0
      then return ()
      else do
        interpret body
        interpretLoop body
