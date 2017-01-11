{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Criterion.Main
import Data.IORef
import Data.Foldable (for_)

import Debug.Trace

main :: IO ()
main = do
  mv <- newMVar (0 :: Int)
  tv <- newTMVarIO (0 :: Int)
  ir <- newIORef (0 :: Int)

  let numThreads = 100

  let modifyGroup addIterations =
        bgroup ("modify " ++ show addIterations)
          [ bench "MVar" $ nfIO (f numThreads addIterations (modifyMVarPure_ mv))
          -- , bench "TMVar" $ nfIO (f numThreads addIterations (modifyTMVarPure_ tv)) -- bad because it doesn't force anything to be done, just modifies thunk
          , bench "IORef" $ nfIO (f numThreads addIterations (atomicModifyIORef_ ir))
          ]

  defaultMain
    [ bgroup "read"
      [ bench "MVar" $ nfIO (readMVar mv)
      , bench "TMVar" $ nfIO (atomically $ readTMVar tv)
      , bench "IORef" $ nfIO (readIORef ir)
      ]
    , bgroup "overwrite 1"
      [ bench "MVar" $ nfIO (takeMVar mv >> putMVar mv 0)
      , bench "TMVar" $ nfIO (atomically $ takeTMVar tv >> putTMVar tv 0)
      , bench "IORef" $ nfIO (atomicWriteIORef ir 0)
      ]
    , bgroup "overwrite 2"
      [ bench "MVar" $ nfIO (swapMVar mv 0) -- slower than take+put probably because it uses masking
      , bench "TMVar" $ nfIO (atomically $ swapTMVar tv 0)
      , bench "IORef" $ nfIO (atomicWriteIORef ir 0)
      ]
    , modifyGroup 1
    , modifyGroup 1000
    , modifyGroup 10000
    -- , modifyGroup 100000
    -- , modifyGroup 1000000
    ]

  -- The numbers here can be different because criterion decides how many times
  -- to run our actions.
  print =<< readMVar mv
  print =<< (atomically $ readTMVar tv)
  print =<< readIORef ir

modifyMVarPure_ :: MVar a -> (a -> a) -> IO ()
modifyMVarPure_ var f = do
  modifyMVar_ var (\old -> evaluate old >> return (f old))

modifyTMVarPure_ :: TMVar a -> (a -> a) -> IO ()
modifyTMVarPure_ var f = atomically $ takeTMVar var >>= putTMVar var . f

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\x -> (f x, ()))

_ITERATIONS_PER_THREAD :: Int
_ITERATIONS_PER_THREAD = 100

f :: Int -> Int -> ((Int -> Int) -> IO ()) -> IO ()
f numThreads addIterations modifyVar = do
  forConcurrently_ [1..numThreads] $ \_ -> do
    for_ [1.._ITERATIONS_PER_THREAD] $ \_ -> do
      modifyVar (manyAdds addIterations)

manyAdds :: Int -> (Int -> Int)
manyAdds  0  x = x + 1
manyAdds !i !x = manyAdds (i - 1) (x + i)
