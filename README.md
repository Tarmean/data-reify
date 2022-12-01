# data-reify [![Hackage version](https://img.shields.io/hackage/v/data-reify.svg?style=flat)](http://hackage.haskell.org/package/data-reify) [![Build Status](https://github.com/ku-fpg/data-reify/workflows/Haskell-CI/badge.svg)](https://github.com/ku-fpg/data-reify/actions?query=workflow%3AHaskell-CI)

Data-reify lets you reify sharing. That is, turn cyclic data with pointers into flat graphs with named references. It uses GHC's Sys.Mem.StableName api to track pointer-identities.

This is a fork which dramatically changes the interface. The core is the MonadRef class:

```Haskell
type Unique = Int
class Monad m => MonadRef m where
    -- | Calling with the same value returns the same Unique
    stableName :: a -> m Unique
    freshName :: m Unique
    wasVisited :: Unique -> m Bool
    markVisited :: Unique -> m ()
```

The library does a traversal through your type and uses `stableName` to replace pointers with ints.

We must keep track of the key-value pairs generated.
In the new interface users must implement a typeclass to store key-value pairs. This makes it much easier to transform mutually recursive types, just implement it so each type to inserted into a separate map.

```Haskell
class Monad m => MonadGlobals o m where
    tellGlobal :: Key o -> DeRef o -> m ()

-- Example instance:
type GlobalEnv = IM.IntMap FlatExpr
instance Monad m => MonadGlobals Expr (StateT GlobalEnv m) where
    tellGlobal k v = modify (IM.insert k v)
```

MuRef performs the traversal, replacing recursive positions with flat keys. 

```Haskell
class (MonadGlobals a m) => MuRef a m where
    mapDeRef :: (forall b. (MuRef m b) => b -> m (Key b)) -> a -> m (DeRef a)
    type DeRef a

    type Key a = Unique
    makeKey :: a -> Unique -> Key a
    makeKey _ uniq = uniq
```

Usage example:

```Haskell
data Expr = Lambda (Expr -> Expr) | ...
data FlatExpr = Lambda' Int (Key Expr) | Plus' (Key Expr) (Key Expr) | Var Int | ...
instance (MonadRef m) => MuRef Expr m where
    type DeRef Expr = FlatExpr
    mapDeRef visitChildren (Lambda fun) = do
       uniq <- stableName fun
       let var = Var uniq
       body <- visitChildren (fun var)
       pure (Lambda' var body)
    mapDeRef visitChildren (Plus l r) = Plus' <$> visitChildren l <*> visitChildren r
    ...


flattenExpr :: Expr -> IO (Int, IM.IntMap FlatExpr)
flattenExpr e = runStable (runStateT (findNodes e) mempty)
```


`data-reify` provided the ability to turn recursive structures into explicit graphs. Many (implicitly or explicitly) recursive data structure can be given this ability, via a type class instance. This gives an alternative to using `Ref` for observable sharing.

Observable sharing in general is unsafe, so we use the IO monad to bound this effect, but can be used safely even with `unsafePerformIO` if some simple conditions are met. Typically this package will be used to tie the knot with DSLs that depend of observable sharing, like Lava.

Providing an instance for `MuRef` is the mechanism for allowing a structure to be reified into a graph, and several examples of this are provided.

History: Version 0.1 used unsafe pointer compares. Version 0.2 of `data-reify` used StableNames, and was much faster. Version 0.3 provided two versions of `MuRef`, the mono-typed version, for trees of a single type, and the dynamic-typed version, for trees of different types. Version 0.4 used `Int` as a synonym for `Unique` rather than `Data.Unique` for node ids, by popular demand. Version 0.5 merged the mono-typed and dynamic version again, by using `DynStableName`, an unphantomized version of `StableName`.
