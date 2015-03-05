{-# LANGUAGE LambdaCase, TemplateHaskell, TupleSections, ViewPatterns #-}
module Data.Bifunctor.TH (deriveBifunctors, deriveBifunctor) where
import           Control.Applicative (pure, (<$>))
import qualified Control.Arrow       as A
import           Control.Monad       (unless, when)
import Control.Monad (replicateM)
import           Data.Bifunctor      (Bifunctor (..))
import           Data.Generics       (listify)
import           GHC.Exts            (Constraint)
import           Language.Haskell.TH (ClauseQ, Con (..), Dec (DataD, NewtypeD))
import           Language.Haskell.TH (DecsQ, Exp (VarE), Exp (..))
import           Language.Haskell.TH (Info (TyConI), Name, Q, TyVarBndr (..))
import           Language.Haskell.TH (Type (..), TypeQ, appT, appsE, clause)
import           Language.Haskell.TH (conE, conP, conT, funD, instanceD)
import           Language.Haskell.TH (isInstance, lamE, newName, normalB, reify)
import           Language.Haskell.TH (reportError, tupleDataName)
import           Language.Haskell.TH (unboxedTupleTypeName, varP, varT)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH (tupP)
import Language.Haskell.TH (appE)
import Language.Haskell.TH (tupE)
import Language.Haskell.TH (varE)
import Control.Monad (zipWithM)
import Language.Haskell.TH (Pat(..))
import Data.Generics (everywhereBut)
import Data.Generics (mkQ)
import Language.Haskell.TH (Match(..))
import Data.Generics (extQ)
import Data.Generics (mkT)
import Data.Generics (everywhere')

appsT :: [TypeQ] -> TypeQ
appsT = foldl1 appT

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV n)    = n
tyVarName (KindedTV n _) = n

deriveBifunctors :: [Name] -> DecsQ
deriveBifunctors = fmap concat . mapM deriveBifunctor

deriveBifunctor :: Name -> DecsQ
deriveBifunctor tyname = do
  DataD cxt nm vars cons _ <- reifyToData =<< reify tyname
  let (phs, ~targs@[f, s]) = splitAt (length vars - 2) $ map tyVarName vars -- $ zip vars rls
      irr = phs
  when (length targs < 2) $
    reportError "Bifunctor must have at least two type parameters!"
  pure <$> instanceD (return cxt)
             [t| Bifunctor $(appsT $ conT nm : map varT  irr) |]
             [ funD 'bimap (map (mkBimapClause  f s) cons)
             ]

mkBimapClause :: Name -> Name -> Con -> ClauseQ
mkBimapClause a b con = do
  f <- newName "_f"
  g <- newName "_g"
  (conN, vts) <- splitCon con
  exs <- map (return . fst) <$> (mapper b g =<< mapper a f (map (A.first VarE) vts))
  clause [varP f, varP g, conP conN $ map (varP . fst) vts]
         (normalB $ appsE $ conE conN : exs )
         []

mapper :: Name -> Name -> [(Exp, Type)] -> Q [(Exp, Type)]
mapper v f = mapM (fmapUntilFunVar v $ VarE f)

mapTups :: [ExpQ] -> ExpQ
mapTups funs = do
  let len = length funs
  vars <- replicateM len $ newName "v"
  lamE [tupP $ map varP vars] $ tupE $ zipWith (\f x -> f `appE` varE x) funs vars

fmapUntilFunVar :: Name -> Exp -> (Exp, Type) -> Q (Exp, Type)
fmapUntilFunVar n f (v, VarT m) | n == m = do
  (etaReduce $ AppE f v, ) <$> (varT =<< newName "dummy")
fmapUntilFunVar n f (e, tp)
  | (Tuple k, vs) <- splitTypes tp = do
      vss <- replicateM k (newName "els")
      ets <- zipWithM (\v t -> A.first (LamE [VarP v]) <$> fmapUntilFunVar n f (VarE v,t)) vss vs
      let t' = foldl1 AppT $ TupleT k : map snd ets
      (, t') . etaReduce . betaReduce . (`AppE` e) <$> mapTups (map (return . fst) ets)
  | ~(Name con, vts) <- splitTypes tp = do
    let (phs, targs) = splitAt (length vts - 2) vts
    when (any (containsVarT n) phs) $
      reportError "cannot fmap or bimap"
    case targs of
      [x] | containsVarT n x -> do
        go <- isInstance ''Functor [ foldl AppT (ConT con) phs ]
        unless go $
          reportError $ "Type " ++ show con ++ " is not a functor!"
        plh <- newName "e"
        (e', x') <- fmapUntilFunVar n f (VarE plh, x)
        ans <- [| fmap $(lamE [varP plh] $ return e') $(return e) |]
        return (etaReduce $ betaReduce ans, foldCon con (phs ++ [x']))
      [x, y]
        | containsVarT n x && containsVarT n y -> do
          go <- isInstance ''Bifunctor [foldl AppT (ConT con) phs]
          unless go $
            reportError $ "Type " ++ show con ++ " is not a bifunctor!"
          plhl <- newName "el"
          plhr <- newName "er"
          (l, tl) <- fmapUntilFunVar n f (VarE plhl, x)
          (r, tr) <- fmapUntilFunVar n f (VarE plhr, y)
          ans <- [| bimap $(lamE [varP plhl] $ return l)
                          $(lamE [varP plhr] $ return r)
                          $(return e) |]
          return (etaReduce $ betaReduce ans, foldCon con (phs ++ [tl, tr]))
        | containsVarT n x && not (containsVarT n y) -> do
          go <- isInstance ''Bifunctor [foldl AppT (ConT con) phs]
          unless go $
            reportError $ "Type " ++ show con ++ " is not a bifunctor!"
          plhl <- newName "el"
          (l, tl) <- fmapUntilFunVar n f (VarE plhl, x)
          ans <- [| first $(lamE [varP plhl] $ return l)
                          $(return e) |]
          return (etaReduce $ betaReduce ans, foldCon con (phs ++ [tl, y]))
        | not (containsVarT n x) && containsVarT n y -> do
          isF <- if isClosed x then isInstance ''Functor [foldl AppT (ConT con) $ phs ++ [x] ] else return False
          isB <- isInstance ''Bifunctor [foldl AppT (ConT con) phs]
          let mp | isF = [| fmap |]
                 | otherwise = [| second |]
          unless (isF || isB) $
            reportError $ "Type " ++ show con ++ " is not a functor!"
          plhr <- newName "er"
          (r, tr) <- fmapUntilFunVar n f (VarE plhr, y)
          ans <- [| $mp $(lamE [varP plhr] $ return r)
                        $(return e) |]
          return (etaReduce $ betaReduce ans, foldCon con (phs ++ [x, tr]))
      _ -> return (etaReduce $ betaReduce e, tp)

isClosed :: Type -> Bool
isClosed = null . listify (\case  VarT _ -> True ;  _ -> False)

splitCon :: Con -> Q (Name, [(Name, Type)])
splitCon (RecC nm tps) = splitCon $ NormalC nm $ map (\(_,b,c)->(b,c)) tps
splitCon (InfixC s nm t) = splitCon $ NormalC nm [s, t]
splitCon ForallC {} = error "Existential types are not supported"
splitCon (NormalC nm sts) = do
  vs <- mapM (const $ newName "v") sts
  return (nm, zip vs $ map snd sts)


containsVarT :: Name -> Type -> Bool
containsVarT v = not . null . listify (== VarT v)

foldCon :: Name -> [Type] -> Type
foldCon n tps = foldl AppT (ConT n) tps

data SplittedName = Name Name
                  | Tuple Int
                    deriving (Show, Eq, Ord)

splitTypes :: Type -> (SplittedName, [Type])
splitTypes (t0 `AppT` tps) =
  let (t, args) = splitTypes t0
  in (t, args ++ [tps])
splitTypes (ConT n)           = (Name n, [])
splitTypes (VarT n)           = (Name n, [])
splitTypes ListT              = (Name ''[], [])
splitTypes ArrowT             = (Name ''(->), [])
splitTypes PromotedNilT       = (Name '[], [])
splitTypes PromotedConsT      = (Name '(:), [])
splitTypes (PromotedTupleT n) = (Name $ tupleDataName n, [])
splitTypes (UnboxedTupleT  n) = (Name $ unboxedTupleTypeName n, [])
splitTypes (TupleT n)         = (Tuple n, [])
splitTypes ForallT{}          = error "Existential is not allowed!"
splitTypes (SigT t _)         = splitTypes t
splitTypes (PromotedT n)      = (Name n, [])
splitTypes (LitT _)           = error "Type literal not supported"
splitTypes ConstraintT        = (Name ''Constraint, [])
splitTypes StarT              = error "Kind ..."

reifyToData :: Monad m => Info -> m Dec
reifyToData (TyConI d@DataD{})  = return d
reifyToData (TyConI (NewtypeD cxt nm bs con dvs)) =
  return $ DataD cxt nm bs [con] dvs
reifyToData _ = error "Data or Newtype declaration needed!"

subst :: Name -> Exp -> Exp -> Exp
subst n new = everywhereBut (mkQ False stopE `extQ` stopMatch) $ mkT go
  where
    stopE (LamE pat _) = not $ null $ listify (== VarP n) pat
    stopE (LetE pat _) = not $ null $ listify (== VarP n) pat
    stopE _ = False
    stopMatch (Match pat _body _decs) = not $ null $ listify (== VarP n) pat

    go (VarE x) | x == n = new
    go e = e

betaReduce :: Exp -> Exp
betaReduce v@VarE{}    = v
betaReduce v@ArithSeqE{}    = v
betaReduce v@CompE{}    = v
betaReduce v@CaseE{}    = v
betaReduce v@MultiIfE{} = v
betaReduce v@ConE{}    = v
betaReduce v@LitE{}    = v
betaReduce (SigE e t)  = SigE (betaReduce e) t
betaReduce (RecConE n evs) = RecConE n $ map (second betaReduce) evs
betaReduce (RecUpdE n evs) = RecUpdE n $ map (second betaReduce) evs
betaReduce (ParensE v) = ParensE $ betaReduce v
betaReduce (InfixE e i d) =
  InfixE (betaReduce <$> e) (betaReduce i) (betaReduce <$> d)
betaReduce (UInfixE e i d) =
  UInfixE (betaReduce e) (betaReduce i) (betaReduce d)
betaReduce (CondE e t f) = CondE (betaReduce e) (betaReduce t) (betaReduce f)
betaReduce v@LamCaseE{} = v
betaReduce (LamE [] e) = betaReduce e
betaReduce (LamE ps e) = LamE ps $ betaReduce e
betaReduce (UnboxedTupE es) = UnboxedTupE $ map betaReduce es
betaReduce (TupE es) = TupE $ map betaReduce es
betaReduce v@DoE{} = v
betaReduce (LetE dcs e) = LetE dcs $ betaReduce e
betaReduce (ListE es) = ListE $ map betaReduce es
betaReduce (AppE a b) =
  case (betaReduce a, betaReduce b) of
    (LamE (VarP v : ps) body, e) ->
      let body' = subst v e body
      in if null ps
         then body'
         else LamE ps body'
    (e, d) -> AppE e d

etaReduce :: Exp -> Exp
etaReduce = everywhere' $ mkT red
  where
    red (LamE (VarP p : ps) (AppE e (VarE q)))
      | p == q && null (listify (== VarE p) e) =
        if null ps
        then e
        else LamE ps e
    red e = e
      
