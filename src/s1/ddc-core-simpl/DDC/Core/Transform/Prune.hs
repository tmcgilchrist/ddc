
-- | Erase contained let-bindings that have no uses.
--
--   Contained bindings are ones that do not perform effects that are
--   visible to anything in the calling context. This includes allocation
--   and read effects, but not writes or any globally visible effects.
--
module DDC.Core.Transform.Prune
        ( PruneInfo  (..)
        , pruneModule
        , pruneX)
where
import DDC.Core.Analysis.Usage
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.TransformUpX
import DDC.Core.Fragment
import DDC.Core.Check
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Data.Pretty
import Data.Typeable
import Control.Monad.Writer                             (Writer, runWriter, tell)
import DDC.Core.Env.EnvX                                (EnvX)
import qualified Data.Map                               as Map
import qualified DDC.Core.Transform.SubstituteXX        as S
import qualified DDC.Type.Exp.Simple                    as T
import qualified DDC.Type.Sum                           as TS
import qualified DDC.Core.Env.EnvT                      as EnvT
import Prelude                                          hiding ((<$>))


-------------------------------------------------------------------------------
-- | A summary of what the prune transform did.
data PruneInfo
    = PruneInfo
    { -- | How many let-bindings we erased.
      infoBindingsErased  :: Int }
    deriving Typeable


instance Pretty PruneInfo where
 ppr (PruneInfo remo)
  =  text "Prune:"
  <$> indent 4 (vcat
      [ text "Removed:        " <> int remo])


instance Monoid PruneInfo where
 mempty = PruneInfo 0

 mappend (PruneInfo r1) (PruneInfo r2)
        = PruneInfo (r1 + r2)


-------------------------------------------------------------------------------
-- | Erase pure let-bindings in a module that have no uses.
pruneModule
        :: (Show a, Show n, Ord n, Pretty n)
        => Profile n           -- ^ Profile of the language we're in
        -> Module a n
        -> Module a n

pruneModule profile mm
         -- If the language fragment has untracked effects then we can't do
         -- the prune transform because we risk dropping statements with global
         -- effects.
         | not $ featuresTrackedEffects
               $ profileFeatures profile
         = mm

         | otherwise
         = let  env     = moduleEnvX 
                                (profilePrimKinds    profile)
                                (profilePrimTypes    profile)
                                (profilePrimDataDefs profile)
                                mm
           in   mm { moduleBody      
                        = result $ pruneX profile env
                                 $ moduleBody mm }


-- | Erase pure let-bindings in an expression that have no uses.
pruneX
        :: (Show a, Show n, Ord n, Pretty n)
        => Profile n            -- ^ Profile of the language we're in
        -> EnvX n               -- ^ Type checker environment.
        -> Exp a n
        -> TransformResult (Exp a n)

pruneX profile env xx
 = {-# SCC pruneX #-}
   let  
        (xx', info)
                = transformTypeUsage profile env
                       (transformUpMX pruneTrans env)
                       xx

        progress (PruneInfo r) 
                = r > 0

   in TransformResult
        { result         = xx'
        , resultAgain    = progress info
        , resultProgress = progress info
        , resultInfo     = TransformInfo info }


-- The prune transform proper needs to have every expression annotated
-- with its type an effect, as well the variable usage map.
--
-- We generate these annotations here then pass the result off to
-- deadCodeTrans to actually erase dead bindings.
--
transformTypeUsage profile env trans xx
 = let  config  = configOfProfile profile
        rr      = checkExp config env Recon DemandNone xx
   in case fst rr of
        Right (xx1, _, _) 
         -> let xx2        = usageX xx1
                (x', info) = runWriter (trans xx2)
                x''        = reannotate (\(_, AnTEC { annotTail = a }) -> a) x'
            in  (x'', info)

        Left _
         -> error $  renderIndent
         $  vcat [ text "ddc-core-simpl.Prune: core type error" ]


-------------------------------------------------------------------------------
-- | Annotations used by the dead-code trasnform.
type Annot a n 
        = (UsedMap n, AnTEC a n)


-- | Apply the dead-code transform to an annotated expression.
pruneTrans
        :: Ord n
        => EnvX n               -- ^ Type checker environment.
        -> Exp (Annot a n) n    -- ^ Expression to transform.
        -> Writer PruneInfo 
                 (Exp (Annot a n) n)

pruneTrans _ xx
 = case xx of
        XLet a@(usedMap, antec) (LLet b x1) x2
         | isUnusedBind b usedMap
         , isContainedEffect $ annotEffect antec
         -> do      
                -- We still need to substitute value into casts
                let x2' = S.substituteXX b x1 x2

                -- Record that we've erased a binding.
                tell mempty {infoBindingsErased = 1}

                -- 
                return $ XCast a (weakEff antec)
                       $ x2'

        _ -> return xx

 where
        weakEff antec
         = CastWeakenEffect
         $ T.crushEffect EnvT.empty
         $ annotEffect antec


-- | Check whether this binder has no uses, 
--   not including weakclo casts, beause we'll substitute the bound
--   expression directly into those.
isUnusedBind :: Ord n => Bind n -> UsedMap n -> Bool
isUnusedBind bb (UsedMap um)
 = case bb of
        BName n _
         -> case Map.lookup n um of
                Just useds -> filterUsedInCasts useds == []
                Nothing    -> True

        BNone _ -> True
        _       -> False


filterUsedInCasts :: [Used] -> [Used]
filterUsedInCasts = filter notCast
 where  notCast UsedInCast      = False
        notCast _               = True


-- | A contained effect is one that is not visible to anything else
--   in the context. This is allocation and read effects, which are
--   not visible from outside the computation performing the effect. 
isContainedEffect :: Ord n => Effect n -> Bool
isContainedEffect eff 
 = all contained
        $ map T.takeTApps 
        $ sumList 
        $ T.crushEffect EnvT.empty eff
 where
        contained (c : _args)
         = case c of
                TCon (TyConSpec TcConAlloc)     -> True
                TCon (TyConSpec TcConDeepAlloc) -> True
                TCon (TyConSpec TcConRead)      -> True
                TCon (TyConSpec TcConHeadRead)  -> True
                TCon (TyConSpec TcConDeepRead)  -> True
                _                               -> False

        contained [] = False

        sumList (TSum ts) = TS.toList ts
        sumList tt            = [tt]

