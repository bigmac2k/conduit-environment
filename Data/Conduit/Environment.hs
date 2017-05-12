{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}
module Data.Conduit.Environment where

import Control.Monad
import Data.Foldable
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Sequence as S
import qualified Data.ByteString as BS
import Data.Sequence (Seq, (|>), (<|))
import Data.Monoid
import Data.List
import Control.Exception

environments :: forall a m. ({-Show a, -}MonadResource m, Monoid a) => (Int, Int) -> a -> (a -> Bool) -> Conduit a m a
environments (before, after) join pred = assert (before >= 0 && after >= 0) $ go (initBuf before) []
  where
  initBuf :: Int -> Seq a
  initBuf n = S.replicate n mempty
  addBuf :: Seq a -> a -> Seq a
  addBuf seq a = S.drop 1 seq |> a
  addOuts :: [(Int, Seq a)] -> a -> [(Int, Seq a)]
  addOuts outs val = map (\(ctr, s) -> (ctr - 1, s |> join |> val)) outs
  go :: Seq a -> [(Int, Seq a)] -> Conduit a m a
  go buf outs = do
    let (finished, unfinished) = partition (\(ctr, _) -> ctr == 0) outs
    {-liftIO $ putStrLn $ "finished: " ++ show finished-}
    forM_ finished $ yield . fold . snd
    chunk <- await
    case chunk of
      Nothing -> forM_ unfinished $ \(_, s) -> yield (fold s)
      Just val -> do
        let outs' = (unfinished ++ [(after + 1, sIntersperse join buf) | pred val])
        {-liftIO (putStrLn $ "input: " ++ show val ++ " buf: " ++ show buf ++ " unfinished: " ++ show unfinished ++ " outs': " ++ show outs')-}
        go (addBuf buf val) (addOuts outs' val)

sIntersperse :: a -> Seq a -> Seq a
sIntersperse a sa | S.null sa = sa
                  | otherwise = let res = foldr (\e s -> e <| a <| s) S.empty sa in S.take (length res - 1) res

lineEnvironments :: MonadResource m => (Int, Int) -> (BS.ByteString -> Bool) -> Conduit BS.ByteString m BS.ByteString
lineEnvironments env pred = environments env "\n" pred
