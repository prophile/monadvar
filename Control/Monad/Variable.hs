module Control.Monad.Variable(Variable, get, set, newVar, liftVar, MonadVariable) where

import qualified Control.Concurrent.STM as STM
import Data.IORef
import Control.Monad.Trans
import Control.Monad(liftM)

import Data.Monoid(Monoid)

import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.RWS.Lazy as RWSLazy
import qualified Control.Monad.Trans.RWS.Strict as RWSStrict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error

data Variable m a = Variable { get :: m a,
                               set :: a -> m () }

class (Monad m) => MonadVariable m where
  newVar :: a -> m (Variable m a)

liftVar :: (MonadTrans t, MonadVariable m) => a -> t m (Variable (t m) a)
liftVar = lift . liftM liftV . newVar
  where liftV (Variable g s) = Variable (lift g) (lift . s)

-- base instances
instance MonadVariable STM.STM where
  newVar x = do
    var <- STM.newTVar x
    return Variable { get = STM.readTVar var,
                      set = STM.writeTVar var }

instance MonadVariable IO where
  newVar x = do
    var <- newIORef x
    return Variable { get = readIORef var,
                      set = writeIORef var }

-- TODO: write one for ST

instance (MonadVariable m) => MonadVariable (ReaderT r m) where
  newVar = liftVar

instance (Monoid w, MonadVariable m) => MonadVariable (WriterStrict.WriterT w m) where
  newVar = liftVar

instance (Monoid w, MonadVariable m) => MonadVariable (WriterLazy.WriterT w m) where
  newVar = liftVar

instance (MonadVariable m) => MonadVariable (StateLazy.StateT s m) where
  newVar = liftVar

instance (MonadVariable m) => MonadVariable (StateStrict.StateT s m) where
  newVar = liftVar

instance (Error e, MonadVariable m) => MonadVariable (ErrorT e m) where
  newVar = liftVar

instance (Monoid w, MonadVariable m) => MonadVariable (RWSLazy.RWST r w s m) where
  newVar = liftVar

instance (Monoid w, MonadVariable m) => MonadVariable (RWSStrict.RWST r w s m) where
  newVar = liftVar

instance (MonadVariable m) => MonadVariable (MaybeT m) where
  newVar = liftVar

instance (MonadVariable m) => MonadVariable (ContT r m) where
  newVar = liftVar
