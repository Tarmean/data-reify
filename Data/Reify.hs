{-# LANGUAGE CPP, TypeFamilies, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Data.Reify (
        module Data.Reify,
        module Data.Reify.Graph,
        ) where

import qualified Data.IntMap as IM
import qualified Data.Set as S

import System.Mem.StableName ( StableName, hashStableName, makeStableName )
import Control.Monad.State
    ( modify,
      StateT(..),
      gets,
      evalStateT )

import Unsafe.Coerce ( unsafeCoerce )
import Data.Kind (Type)
import Control.Monad.Writer ( MonadTrans(..), unless, MonadIO(..), WriterT )
import Control.Monad.Reader ( ReaderT )

import Data.Reify.Graph

-- Data.Reify with a different interface.
--
-- - Call `findNodes` on the values you want to reify, returning keys
-- - Run the monad stack
--
-- You need to manually implement the MonadGlobals type-class, which is responsible for storing your var:=>val bindings.
--
-- Usually an instance looks like this:
--
-- @
--
--     data MyGlobals = MyGlobals
--       { recExprs :: M.IntMap Expr
--       , recStatements :: M.IntMap Statement
--       }
--     instance Monad m => MonadGlobals (StateT MyGlobals m) Expr where
--         tellGlobal k v = modify $ \s -> s { recExprs = M.insert k v (recExprs s) }
--     instance Monad m => MonadGlobals (StateT MyGlobals m) Statement where
--         tellGlobal k v = modify $ \s -> s { recStatements = M.insert k v (recStatements s) }
--
--    type Expr = Expr' Int
--    type RecExpr = RecExpr (Expr' RecExpr)
--    instance Applicative f => MuRef RecExpr f where
--        type DeRef RecExpr = Expr
--        type Key RecExpr = Int
--        mapDeRef f (RecExpr a) = case a of
--           PlusF a b -> Plus <$> f a <*> f b
--           LambdaF func -> do
--               var <- stableName func
--               body <- f (func (Var var))
--               pure (Lamba var body)
--           ...
-- @
--
-- 

-- | Core function, traverses the term recursively.
-- - Substitute all sub-terms with keys
-- - Add the flattened term to the environment
findNodes :: forall s m. (MuRef s m, MonadRef m) => s -> m (Key s)
findNodes !val = do
    name <- stableName val
    let k = makeKey @s @m  val name
    visitOnce name $ do
        normalized <- mapDeRef findNodes val
        tellGlobal @s @m k normalized
    pure k


-- | A monad transformer that keeps track of the name=>node pairs.
class Monad m => MonadGlobals o m where
    tellGlobal :: Key o -> DeRef o -> m ()

-- | 'MuRef' is a relative to traverse: We turn pointers with types `a,b,c` into Int-like references `Key a,Key b,Key c`.
-- This turns our top-level type `s` into `DeRef s`.
-- 
-- Usually, the `f` can remain polymorphic. If you want to use your own monad transformer stack, you need to specify the `f` type.
-- The bottom will always be the `Stable` monad which is responsible for the actual reification.
class (MonadGlobals a f) => MuRef a f where
  -- | A flattened representation of the type. In DeRef, pointers to `a` are replaced by `Key a`.
  type DeRef a :: Type
  -- | Key represents a reference, usually `Int`
  type Key a :: Type
  type Key a = Int
  makeKey :: a -> Unique -> Key a
  default makeKey :: (Key a ~ Unique) => a -> Unique -> Key a
  makeKey _ a = a
  mapDeRef :: (forall b. (MuRef b f) => b -> f (Key b)) -> a -> f (DeRef a)

runStable :: Stable o -> IO o
runStable (Stable m) = evalStateT m emptyEnv
  where emptyEnv = RefEnv { nameMap = StableMap IM.empty, uniqueGen = 0, seen = S.empty }

stableKey :: forall m a. (MuRef a m, MonadRef m) => a -> m (Key a)
stableKey a = makeKey @_ @m a <$> stableName a
-- | Internal monad class for name generation and tracking
-- It's a class so users can lift them through their own monad transformers
class Monad m => MonadRef m where
    stableName :: a -> m Unique
    default stableName :: (MonadTrans t, MonadRef m', m ~ t m') => a -> m Unique
    stableName = lift . stableName
    wasVisited :: Unique -> m Bool
    default wasVisited :: (MonadTrans t, MonadRef m', m ~ t m') => Unique -> m Bool
    wasVisited = lift . wasVisited
    markVisited :: Unique -> m ()
    default markVisited :: (MonadTrans t, MonadRef m', m ~ t m') => Unique -> m ()
    markVisited = lift . markVisited
    freshName :: m Unique
    default freshName :: (MonadTrans t, MonadRef m', m ~ t m') => m Unique
    freshName = lift freshName


instance MonadRef m => MonadRef (StateT s m)
instance (Monoid s, MonadRef m) => MonadRef (WriterT s m)
instance (MonadRef m) => MonadRef (ReaderT s m)

newtype Stable a = Stable { unStable :: StateT RefEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO)
data RefEnv = RefEnv { 
        nameMap ::  StableMap Unique,
        uniqueGen :: Unique,
        seen :: S.Set Unique
    }
instance MonadRef Stable where
    freshName = Stable $ do
        modify $ \s -> s { uniqueGen = uniqueGen s + 1 }
        gets uniqueGen
    stableName a = do
        name <- Stable (makeDynStableName a)
        mvar <- Stable $ gets (lookupName name . nameMap)
        case mvar of
          Just var -> pure var
          Nothing -> do
            var <- freshName
            Stable $ modify $ \s -> s { nameMap = insertStableName  name var (nameMap s) }
            pure var
    wasVisited u = Stable $ gets (S.member u . seen)
    markVisited u = Stable $ modify $ \s -> s { seen = S.insert u (seen s) }
visitOnce :: MonadRef m => Unique -> m () -> m ()
visitOnce u m = do
    visited <- wasVisited u
    unless visited $ do
        markVisited u
        m

-- todo: use a hashmap with builtin probing
newtype StableMap v = StableMap { unStableMap :: IM.IntMap [(DynStableName,Int)]}
lookupName :: DynStableName -> StableMap v -> Maybe Int
lookupName sn (StableMap m) = 
  case IM.lookup (hashStableName (unDynName sn)) m of
    Nothing -> Nothing
    Just xs -> lookup sn xs
insertStableName :: DynStableName -> Int -> StableMap v -> StableMap v
insertStableName sn i (StableMap m) = 
  StableMap $ IM.insertWith (++) (hashStableName (unDynName sn)) [(sn,i)] m


-- Stable names that not use phantom types.
-- As suggested by Ganesh Sittampalam.
newtype DynStableName = DynStableName { unDynName :: StableName ()}

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
    (DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: MonadIO m => a -> m DynStableName
makeDynStableName a = do
    st <- liftIO (makeStableName a)
    return $ DynStableName (unsafeCoerce st)


newtype ReifyGraphT o m a = ReifyGraphT { unReifyGraphT :: StateT (IM.IntMap o) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
instance MonadRef m => MonadRef (ReifyGraphT o m)
  
reifyGraph :: (Key a ~ Int, MuRef a (ReifyGraphT (DeRef a) Stable)) => a -> IO (Graph (DeRef a))
reifyGraph s = fmap toGraph $ runStable $ runStateT (unReifyGraphT (findNodes s)) IM.empty
  where
    toGraph (root, m) = Graph (IM.toList m) root
instance (Key a ~ Int, o ~ DeRef a, Monad m) => MonadGlobals a (ReifyGraphT o m) where
    tellGlobal k v = ReifyGraphT $ modify (IM.insert k v)
