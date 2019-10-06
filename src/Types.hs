{-# LANGUAGE FlexibleInstances #-}
module Types where
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set, difference, empty, singleton, union)

each :: (a->b) -> (c->d) -> (a,c) -> (b,d)
each f f2 (a,b) = (f a, f2 b)

type Id = Int

data Mono = Var Id |
  App Mono [Mono] |
  Bound String deriving (Show, Eq, Ord)

data Poly = Poly (Set Id) Mono deriving (Show, Eq, Ord)

type Gamma = (Int, Map Expr Poly)

type Inference = Either String (Gamma, Mono, Map Id Mono)

data Symbol = Symbol String deriving (Eq, Ord, Show)

data Expr =
  Lam Symbol Expr
  | Ref Symbol
  | Let Symbol Expr Expr
  | Call Expr Expr deriving (Eq, Ord, Show)

type Unify = Maybe (Map Id Mono)

class Subst a where
  subst :: Map Id Mono -> a -> a

class Type a where
  occurs :: Id -> a -> Bool
  ftv :: a -> Set Id

class Vars a where
  vars :: a -> Set Id

instance Subst Poly where
  subst m (Poly ids var) = Poly ids (subst m var)

subst_ :: Id -> Mono -> Mono -> Mono
subst_ i t t2@(Var i')  = if i == i' then t else t2
subst_ _ _ t2@(Bound _) = t2
subst_ i t (App t2 ts)  = App (subst_ i t t2) (fmap (subst_ i t) ts)

instance Subst Mono where
  subst m var =
    foldl
    (\acc (k,v) -> subst_ k v acc)
    var
    $
    M.assocs m

instance Vars Mono where
  vars (Bound _)  = empty
  vars (Var v)    = singleton v
  vars (App a bs) = union (vars a) $ foldl union empty $ fmap vars bs

instance Vars (Map Id Mono) where
  vars g = foldl union empty $ fmap (uncurry union . (each singleton vars)) $ M.assocs g

instance Type Mono where
  occurs a t = occurs a (Poly empty t)
  ftv (Var x) = singleton x
  ftv (App t1 t2) = union (ftv $ Poly empty t1) (foldl union empty $ fmap (ftv . (Poly empty)) t2)
  ftv (Bound _) = empty

instance Type Poly where
  occurs a t = elem a (ftv t)
  ftv (Poly ids t) = difference (ftv $ Poly empty t) ids


