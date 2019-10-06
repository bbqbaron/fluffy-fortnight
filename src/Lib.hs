module Lib
    ( infer, Expr(..), Mono(..), Poly(..), Symbol(..), Inference
    , pbound, pvar, fntype
    ) where
import           Data.Either (Either (..))
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Set    (Set, empty, member, singleton, union)
import           Types

fresh :: Int -> Int
fresh = (+) 1

inst :: Int -> Poly -> (Int, Mono)
inst g (Poly ids t) = case t of
  Var var -> if member var ids
    then let g' = fresh g
         in (g', Var g')
    else (g, Var var)
  App t1 ts ->
    let (g', t1' : ts') = foldl
          (\(gacc, ts'') t' ->
             let (g'', t'') = inst gacc (Poly empty t')
             in (g'', t'' : ts''))
          (g, [])
          (t1 : ts)
    in (g', App t1' ts')
  _ -> (g, t)

pvar = Poly empty . Var
pbound = Poly empty . Bound
fntype xs = App (Bound "->") xs

infer :: Gamma -> Expr -> Inference
infer ctx (Ref s)      = tvar ctx s
infer ctx (Lam s e)    = tabs ctx s e
infer ctx (Let s e e2) = tlet ctx s e e2
infer ctx (Call e e2)  = tapp ctx e e2

tabs :: Gamma -> Symbol -> Expr -> Inference
tabs (maxid, ctx) arg body =
  let t' = fresh maxid
  in
    fmap
    (\((m'', ctx'), t'', sub) ->
        ((m'', ctx'), fntype [Var t', t''], sub))
    (infer (t', M.insert (Ref arg) (pvar t') ctx) body)

mtry :: Maybe a -> Either String a
mtry (Just x) = Right x
mtry Nothing  = Left "Noooo"

tapp :: Gamma -> Expr -> Expr -> Inference
tapp ctx e1 e2 = do
  (ctx', t1, s1) <- infer ctx e1
  (ctx'', t2, s2) <- infer ctx' e2
  t3 <- pure $ fresh $ fst ctx''
  mgu <- mtry $ unify M.empty (subst s2 t1, fntype [t2, Var t3])
  return ((t3, snd ctx''), subst mgu $ Var t3, M.unions [s1, s2, mgu])

tlet :: Gamma -> Symbol -> Expr -> Expr -> Inference
tlet g name e e2 = do
  ((mid', ctx'), t, s) <- infer g e
  (ctx'', t2, s2) <- infer (mid', (M.insert (Ref name) (gen (subst s t)) $
                                               M.fromList $
                                               map
                                               (each id (subst s))
                                               $ M.assocs ctx'
                                              )) e2
  return (ctx'', t2, M.unions [s, s2])

tvar :: Gamma -> Symbol -> Inference
tvar (mid, ctx) n =
  case inst mid <$> M.lookup (Ref n) ctx of
    Nothing        -> Left $ ("No var" ++ show n)
    Just (mid', t) -> Right ((mid', ctx), t, M.empty)

extract :: Poly -> (Set Id, Mono)
extract (Poly ids t) = (ids, t)

gen :: Mono -> Poly
gen (Var var) =
    Poly (singleton var) (Var var)
gen (App t t2) =
  let (ida, t') = extract $ gen t
      (idb, t'') = unzip $ fmap (extract . gen) t2
  in
    Poly
    (union ida $ foldl union empty idb)
    $ App t' t''
gen x@(Bound _) = Poly empty $ x

doUnify :: Map Id Mono -> (Mono, Mono) -> Unify
doUnify g (v, v2) | v == v2 = Just g
doUnify _ (Bound _, Bound _) = Nothing
doUnify g (b@(Bound _), Var s) = Just $ M.insert s b g
doUnify g (s@(Var _), b@(Bound _)) = doUnify g (b, s)
doUnify g (Var v, v2@(Var _)) = Just $ M.insert v v2 g
doUnify g (App x [], App x2 []) = doUnify g (x, x2)
doUnify g (App x [], x2) = doUnify g (x, x2)
doUnify g (x, App x2 []) = doUnify g (x, x2)
doUnify g (App t ts, App t2 ts2) =
   doUnify g (t,t2) >>=
  (\g' ->
     foldl
    (\g'' ts -> g'' >>= flip doUnify ts)
    (Just g')
    $ zip ts ts2)
doUnify g (a@(App _ _), Var v) =
  if member v (vars a)
  then Nothing
  else Just $ M.insert v a g
doUnify _ (App _ _, Bound _) = Nothing
doUnify g (v@(Var _), a@(App _ _)) = doUnify g (a, v)
doUnify g (b@(Bound _), a@(App _ _)) = doUnify g (a, b)

unify :: Map Id Mono -> (Mono, Mono) -> Unify
unify g (t@(Var i), t2)
  | occurs i t2 = Nothing
  | otherwise = doUnify g (t, t2)
unify g (t, t2@(Var i))
  | occurs i t = Nothing
  | otherwise = doUnify g (t, t2)
unify g (t, t2) = doUnify g (t, t2)
