{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.ChainSync
  ( Client (..)
  , syncChain
  ) where

import           Control.Monad.Class.MonadSTM (MonadSTM (..), TVar)

import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Chain (Chain (..), Point (..), ChainUpdate(..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainProducerState (ChainProducerState, ReaderId)
import qualified Ouroboros.Network.ChainProducerState as ChainProducerState
import           Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient (..), ClientStIdle (..), ClientStNext (..), ClientStIntersect (..))

data Client header m t = Client
  { rollbackward :: Point header -> Point header -> m (Either t (Client header m t))
  , rollforward  :: header -> m (Either t (Client header m t))
  , points       :: [Point header] -> m (Client header m t)
  }



-- | An instance of the client side of the chain sync protocol that
-- consumes into a 'Chain' stored in a 'TVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
syncChain
    :: forall header m a. (HasHeader header, MonadSTM m)
    => TVar m (Chain header)
    -> Client header m a
    -> ChainSyncClient header (Point header) m a
syncChain chainvar client = ChainSyncClient $
    initialise <$> getChainPoints
  where
    initialise :: ([Point header], Client header m a) -> ClientStIdle header (Point header) m a
    initialise (points, client') =
      SendMsgFindIntersect points $
      -- In this consumer example, we do not care about whether the server
      -- found an intersection or not. If not, we'll just sync from genesis.
      --
      -- Alternative policies here include:
      --  iteratively finding the best intersection
      --  rejecting the server if there is no intersection in the last K blocks
      --
      ClientStIntersect {
        recvMsgIntersectImproved  = \_ _ -> ChainSyncClient (return (requestNext client')),
        recvMsgIntersectUnchanged = \  _ -> ChainSyncClient (return (requestNext client'))
      }

    requestNext :: Client header m a -> ClientStIdle header (Point header) m a
    requestNext client' =
      SendMsgRequestNext
        (handleNext client')
        -- We received a wait message, and we have the opportunity to do
        -- something. In this example we don't take up that opportunity.
        (return (handleNext client'))

    handleNext :: Client header m a -> ClientStNext header (Point header) m a
    handleNext client' =
      ClientStNext {
        recvMsgRollForward  = \header _pHead -> ChainSyncClient $ do
          addBlock header
          choice <- rollforward client' header
          pure $ case choice of
            Left a -> SendMsgDone a
            Right client'' -> requestNext client''

      , recvMsgRollBackward = \pIntersect pHead -> ChainSyncClient $ do
          rollback pIntersect
          choice <- rollbackward client' pIntersect pHead
          pure $ case choice of
            Left a -> SendMsgDone a
            Right client'' -> requestNext client''
      }

    getChainPoints :: m ([Point header], Client header m a)
    getChainPoints = do
      pts <- Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)
      client' <- points client pts
      pure (pts, client')

    addBlock :: header -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollback :: Point header -> m ()
    rollback p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let (Just !chain') = Chain.rollback p chain
        writeTVar chainvar chain'

-- | Offsets from the head of the chain to select points on the consumer's
-- chain to send to the producer. The specific choice here is fibonacci up
-- to 2160.
--
recentOffsets :: [Int]
recentOffsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]
