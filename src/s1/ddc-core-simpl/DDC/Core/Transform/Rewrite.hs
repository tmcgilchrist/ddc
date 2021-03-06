
-- | Apply rewrite rules.
module DDC.Core.Transform.Rewrite
        ( RewriteRule(..)
        , rewriteModule
        , rewriteX)
where
import DDC.Data.Pretty
import DDC.Core.Exp.Annot                               as X
import DDC.Core.Module
import Data.Map                                         (Map)
import DDC.Core.Simplifier.Base (TransformResult(..), TransformInfo(..))
import qualified DDC.Core.Transform.AnonymizeX          as A
import qualified DDC.Core.Transform.Rewrite.Disjoint    as RD
import qualified DDC.Core.Transform.Rewrite.Env         as RE
import qualified DDC.Core.Transform.Rewrite.Match       as RM
import           DDC.Core.Transform.Rewrite.Rule
import qualified DDC.Core.Transform.SubstituteXX        as S
import qualified DDC.Type.Transform.SubstituteT         as S
import qualified DDC.Core.Transform.BoundX              as L
import qualified DDC.Type.Exp.Simple                    as T
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set
import Data.Maybe
import Control.Monad
import Control.Monad.Writer (tell, runWriter, Writer)
import Data.List
import Data.Typeable
import Prelude                                          hiding ((<$>))


-- Log ------------------------------------------------------------------------
-- | Tracks which rewrite rules fired.
data RewriteInfo 
        = RewriteInfo [RewriteLog]
        deriving Typeable

data RewriteLog
        = LogRewrite String
        | LogUnfold  String
        deriving Typeable


instance Pretty RewriteInfo where
 ppr (RewriteInfo rules) 
        =   text "Rules fired:"
        <$> indent 4 (vcat $ map ppr rules)


instance Pretty RewriteLog where
 ppr (LogRewrite name) = text "Rewrite: " <> text name
 ppr (LogUnfold  name) = text "Unfold:  " <> text name

isProgress = not . null


-- Rewrite --------------------------------------------------------------------
-- | Apply rewrite rules to a module.
rewriteModule
        :: (Show a, Show n, Ord n, Pretty n)
        => [NamedRewriteRule a n]       -- ^ Rewrite rule database.
        -> Module a n                   -- ^ Rewrite in this module.
        -> Module a n

rewriteModule rules mm
 = mm { moduleBody = result $ rewriteX' True rules $ moduleBody mm }


-- | Perform rewrites top-down, repeatedly.
rewriteX 
        :: (Show a, Show n, Ord n, Pretty n) 
        => [NamedRewriteRule a n]       -- ^ Rewrite rules database.
        -> Exp a n                      -- ^ Rewrite in this expression.
        -> TransformResult (Exp a n)
rewriteX = rewriteX' False


-- | Repeatedly perform rewrites top-down.
--   Usually any names bound in @letrec@s disable rules, because the
--   new binding makes the old rule meaningless.
--   For modules we do not want to do this, since the rules for that module
--   are probably about the functions exported from the module.
--   In this case, we ignore the top-level bindings when checking for rule shadowing.
rewriteX'
        :: forall a n
        .  (Show a, Show n, Ord n, Pretty n) 
        => Bool                         -- ^ Ignore top-level bindings when checking for shadowing?
        -> [NamedRewriteRule a n]       -- ^ Rewrite rules database.
        -> Exp a n                      -- ^ Rewrite in this expression.
        -> TransformResult (Exp a n)

rewriteX' ignore_toplevel rules x0
 = {-# SCC rewriteX #-}
   let  (x,info) = runWriter $ go RE.empty x0 [] ignore_toplevel
   in   TransformResult
         { result               = x
         , resultAgain          = isProgress info
         , resultProgress       = isProgress info
         , resultInfo           = TransformInfo (RewriteInfo info) }
 where
        -- ISSUE #280:  Rewrites should be done with the most specific rule.
        --   The rewrite engine should apply the most specific rule, instead
        --   of the first one that it finds that matches. If not, then we
        --   should give some warning about overlapping rules.
        -- 
        -- Look for rules in the list that match the given expression,
        -- and apply the first one that matches.
        rewrites env f args
         = rewrites' rules env f args

        rewrites' [] _ f args
         = return $ X.makeXAppsWithAnnots f args

        rewrites' ((n, rule) : rs) env f args
         = case rewriteWithX rule env f args of
                Nothing -> rewrites' rs env f args
                Just x  -> tell [LogRewrite n] >> go env x [] False


        downX env x  
         = go env x [] False

        downA env aa
         = case aa of
                RType{}         -> return aa
                RWitness{}      -> return aa
                RTerm x         -> fmap RTerm     $ downX env x
                RImplicit arg'  -> fmap RImplicit $ downA env arg'

        -- Decend into the expression, looking for applications that we 
        -- might be able to apply rewrites to.
        go      :: RE.RewriteEnv a n 
                -> Exp a n 
                -> [(Arg a n, a)] 
                -> Bool 
                -> Writer [RewriteLog] (Exp a n) 

        go env (XApp a f arg) args _toplevel
         = do   arg' <- downA env arg
                go env f ((arg',a):args) False

        go env x@XVar{}   args  _toplevel
         =      rewrites env x args

        go env x@XPrim{}  args  _toplevel
         =      rewrites env x args

        go env x@XCon{}   args  _toplevel
         =      rewrites env x args

        go env (XAbs a (MType b) e) args _toplevel
         = do   e' <- downX (RE.lift b env) e 
                rewrites env (XAbs a (MType b) e') args

        go env (XAbs a (MTerm b) e) args _toplevel
         = do   e' <- downX (RE.extend b env) e
                rewrites env (XAbs a (MTerm b) e') args

        go env (XAbs a (MImplicit b) e) args _toplevel
         = do   e' <- downX (RE.extend b env) e
                rewrites env (XAbs a (MImplicit b) e') args

        go env (XLet a l@(LRec _) e) args toplevel
         = do   -- Don't add the @letrec@'s bindings to the rule shadow list if we're at the top-level
                let env' = if   toplevel
                           then env
                           else RE.extendLets l env
                l'      <- goLets l env'
                e'      <- downX env' e 
                rewrites env' (XLet a l' e') args

        go env (XLet a l e)     args _toplevel
         = do   l'      <- goLets l env
                dh      <- goDefHoles rules a l' e env downX
                rewrites env dh args 

        go env (XCase a e alts) args _toplevel
         = do   e'      <- downX env e
                alts'   <- mapM (goAlts env) alts
                rewrites env (XCase a e' alts') args

        go env (XCast a c e)    args _toplevel
         = do   e'      <- downX env e
                rewrites env (XCast a c e') args

{-
        go env x@(XType{})      args _toplevel
         =      rewrites env x args

        go env x@(XWitness{})   args _toplevel
         =      rewrites env x args
-}

        goLets (LLet b e) ws 
         = do   e' <- downX ws e 
                return $ LLet b e'

        goLets (LRec bs) ws 
         = do   bs'     <- mapM (downX ws) $ map snd bs
                return $ LRec $ zip (map fst bs) bs'


        goLets l _ 
         = return $ l

        goAlts ws (AAlt p e) 
         = do   e' <- downX ws e 
                return $ AAlt p e'


-- If definitions match the holes of any rules,
-- clean it up and record it for later.
-- Eg with this rule,
--   RULE unbox {box s} = s
--
-- this expression:
--   let x = box (some expensive op)
--   in  ...
--
-- will be transformed to
--   let ^ = some expensive op
--       x = box ^0
--   in ...
--
goDefHoles rules a l@(LLet let_bind def) e env down
 | (((sub, []), name, RewriteRule { ruleBinds = bs, ruleLeft = hole }):_)
        <- checkHoles rules def env

 = let  -- only get value-level bindings
        bs'     = filter (isBMValue . fst) bs
        bas'    = lookupFromSubst a bs' sub

        -- check if it looks like something has already been unfolded
        isUIx x = case x of 
                      RTerm (XVar _ (UIx _))     -> True
                      RTerm (XVar _ (UPrim _ _)) -> True
                      _                          -> False

        already_done
                = all isUIx $ map snd bas'

        -- find kind-values and sub those in as well
        bsK'    = filter ((== BMSpec) . fst) bs
        basK    = lookupFromSubst a bsK' sub

        basK'   = concatMap (\(b, x) -> case X.takeRType x of
                                             Just t -> [(b,t)]
                                             Nothing-> []) basK

        -- surround whole expression with anon lets from sub
        values  = map     (\(b,v) ->   (BAnon (S.substituteTs basK' $ T.typeOfBind b), v))
                          (reverse bas')

        -- replace 'def' with LHS-HOLE[sub => ^n]
        anons   = zipWith (\(b,_) i -> (b, RTerm (XVar a (UIx i)))) 
                          bas' [0..]

        lets    = map     (\(b,v) -> LLet b v) 
                          [(b, x) | (b, RTerm x) <- values]

        def'    = S.substituteXArgs basK
                $ S.substituteXArgs anons hole

        let_bind'  = S.substituteTs basK' let_bind
        lets'      = lets ++ [LLet let_bind' def']

        -- lift e by (length bas)
        depth   = case let_bind of
                     BAnon _ -> 1 
                     _       -> 0

        e'      = L.liftAtDepthX (length bas') depth e

        -- SAVE in wit env
        env'     = foldl (flip RE.extendLets) env lets'

   in  if already_done
            then do
                e'' <- down (RE.extendLets l env) e
                return $ XLet a l e''

            else do
                tell [LogUnfold name]
                e'' <- down env' e'
                return $ X.xLets a lets' e''

 | otherwise
 = do   e' <- down (RE.extendLets l env) e
        return $ XLet a l e'

goDefHoles _rules a l e env down
 = do   e' <- down (RE.extendLets l env) e
        return $ XLet a l e'


-- Match a let-definition against the holes in all the rules
checkHoles 
        :: (Show n, Show a, Ord n, Pretty n)
        => [NamedRewriteRule a n]
        -> Exp a n
        -> RE.RewriteEnv a n
        -> [ ( (RM.SubstInfo a n, [(Arg a n, a)])
               , String
               , RewriteRule a n) ]

checkHoles rules def ws
 = let  rules'   = catMaybes $ map holeRule rules
        (f,args) = X.takeXAppsWithAnnots def

   in   catMaybes
         $ map (\(name,r) -> fmap (\s -> (s,name,r)) 
                          $ matchWithRule r ws f args RM.emptySubstInfo)
           rules'


holeRule (name, rule@RewriteRule { ruleLeftHole     = Just hole })
 = Just ( name
        , rule { ruleLeft     = hole
               , ruleLeftHole = Nothing })

holeRule _ = Nothing


-------------------------------------------------------------------------------
-- | Perform rewrite rule on expression if a valid substitution exists,
--   and constraints are satisfied.
rewriteWithX
        :: (Show n, Show a, Ord n, Pretty n)
        => RewriteRule a n
        -> RE.RewriteEnv a n
        -> Exp a n
        -> [(Arg a n, a)]
        -> Maybe (Exp a n)

rewriteWithX rule env f args 
 = do   
        let RewriteRule
             { ruleBinds         = binds
             , ruleConstraints   = constrs
             , ruleRight         = rhs
             , ruleWeakEff       = eff
             , ruleWeakClo       = _clo } 
             = rule
   
        -- Try to find a substitution for the left of the rule.
        (m, rest)       <- matchWithRule rule env f args RM.emptySubstInfo

        -- Check constraints, perform substitution and add weakens if necessary.
        let a           = X.annotOfExp f

        let bas2        = lookupFromSubst a binds m
        let rhs2        = A.anonymizeX rhs
        let (bas3,lets) = wrapLets a binds bas2
        let rhs3        = L.liftX (length lets) rhs2

        -- Substitute bindings into the effect of the right of the rule.
        let eff'        = liftM (substT bas3) eff

        -- Substitute bindings into rule constraints and
        -- check that they are all satisfied by the environment.
        let constrs'    = map (substT bas3) constrs
        when (not $ all (satisfiedContraint env) constrs')
                $ Nothing

        -- Build the rewritten expression.
        let x'  =  X.xLets a lets
                $  weakeff a eff'
                $  S.substituteXArgs bas3 rhs3

        -- Add the remaining arguments from the original expression
        -- that weren't matched by rule
        return   $  X.makeXAppsWithAnnots x' rest


-- | Check whether we can satisfy this constraint using witnesses
--   in the rewrite nevironment.
satisfiedContraint :: Ord n => RE.RewriteEnv a n -> Type n -> Bool
satisfiedContraint env c
        =  RE.containsWitness c env
        || RD.checkDisjoint   c env
        || RD.checkDistinct   c env


-- | Wrap an expression in an effect weakning.
weakeff :: a -> Maybe (Effect n) 
        -> Exp a n -> Exp a n

weakeff a meff x
 = maybe x (\e -> XCast a (CastWeakenEffect e) x) meff


wrapLets
        :: Ord n 
        => a 
        -> [(BindMode, Bind n)]         -- ^ Variables bound by the rule.
        -> [(Bind n,   Arg a n)]        -- ^ Substitution for the left of the rule.
        -> ( [(Bind n, Arg a n)]
           , [Lets a n])

wrapLets a binds bas 
 = let  isMkLet (_, (BMValue i, _)) = i /= 1
        isMkLet _                   = False

        (as, bs'') = partition isMkLet (bas `zip` binds)
        as'     = map fst as
        bs'     = map fst bs''

        anons   = zipWith (\(b,_) i -> (b, RTerm (XVar a (UIx i)))) 
                          as' [0..]

        values  = map     (\(b,v) ->   (BAnon (substT bs' $ T.typeOfBind b), v)) 
                          (reverse [(b, x) | (b, RTerm x) <- as'])

        lets    = map     (\(b,v) -> LLet b v) 
                          values

   in   (bs' ++ anons, lets)


-- | Substitute type bindings into a type.
substT :: Ord n => [(Bind n, Arg a n)] -> Type n -> Type n
substT bas x 
 = let  sub     = [(b, t) | (b, RType t) <- bas ] 
   in   S.substituteTs sub x


-------------------------------------------------------------------------------
-- | Attempt to find a rewrite substitution to match expression against rule.
--   Returns substitution and the left-over arguments that weren't matched
--   against.
--
--   matchWithRule
--      (RULE mapMapId [a b : *] (f : a -> b) (xs : List a).
--            map [:a b:] f (map [:a a:] id xs)
--          = map [:a b:] f xs)
--      map
--      [ [Int], [Int], (\x -> f), (map [:Int Int:] id [1,2,3]) ]
--
--      env
--
--      emptySubstInfo
--   ==>
--     Just ({a |-> Int, b |-> Int, f |-> (\x -> f), xs |-> [1,2,3] }, [])
--
--   However if we had passed a substitution such as {a |-> Float} instead of
--   emptySubstInfo, it would not have matched.
--
--   The environment is used for 'hole' rules, that can look up bound definitions
--   and match if inlining would let them, even when inlining won't occur.
--
matchWithRule
        :: (Show n, Show a, Ord n, Pretty n)
        => RewriteRule a n   -- ^ Rule to unify with.
        -> RE.RewriteEnv a n -- ^ Environment to rewrite in, contains witnesses
                             --   for the constraints on rules.
        -> Exp a n           -- ^ Function-part of the expression to rewrite.
        -> [(Arg a n, a)]    -- ^ Arguments of expression to rewrite, with the 
                             --   annotations we took from the XApp nodes.
        -> RM.SubstInfo  a n -- ^ Existing substitution to match with

        -> Maybe ( RM.SubstInfo a n
                 , [(Arg a n, a)])
                             -- ^ Substitution map and remaining (unmatched) args

-- Handle a rule without a hole.
matchWithRule
        RewriteRule
         { ruleBinds     = binds
         , ruleLeft      = lhs
         , ruleLeftHole  = Nothing
         , ruleFreeVars  = free }
         env f args sub
 = do   
        -- Check that none of the free variables have been rebound.
        when (any (flip RE.hasDef env) free)
         $ Nothing

        -- Get names of all the variables bound by the rule.
        --  This should always match because checked rules are guaranteed not to
        --  have `BAnon` or `BNone` binders.
        let Just vs 
                = liftM Set.fromList
                $ sequence 
                $ map T.takeNameOfBind
                $ map snd binds

        -- Split the left part of the rule in to the function part and its
        -- arguments.
        (l, ls) <- return $ X.takeXAppsAsList lhs

        -- Match the function part of the expression against
        -- the function part of the rule.
        sub'    <- RM.match sub vs l f

        -- Match each of the expression arguments against 
        -- the arguments of the rule.
        let go m [] rest
             = do return $ (m, rest)

            go m (l':ls') ((r,_):rs)
             = do m' <- RM.matchArg m vs l' r
                  go m' ls' rs

            go _ _ _ 
             = Nothing

        go sub' ls args


-- Handle a rule with a hole. 
-- An example rule with a holes is:
--      RULE (i : Int). unbox {box i} = i
matchWithRule
        rule@(RewriteRule { ruleLeftHole = Just hole })
        env f args sub

        -- Try to match against entire rule with no inlining.
        -- Eg (unbox (box 5))
        | a             <- X.annotOfExp f
        , lhs_full      <- XApp a (ruleLeft rule) (RTerm hole)
        , rule_full     <- rule { ruleLeft = lhs_full, ruleLeftHole = Nothing}
        , Just subst    <- matchWithRule rule_full env f args sub
        = Just subst

        -- Try to match against the part without the hole.
        --  eg unifyWithRule (RULE (i : Int). unbox) (unbox x)
        -- which will return a substitution (empty here),
        -- and the leftover argument 'x'.
        | rule_some     <- rule { ruleLeftHole = Nothing }
        , Just (sub', (RTerm (XVar _ b), _) : as)
                        <- matchWithRule rule_some env f args sub

        -- See if the 'x' variable is let-bound in an outer scope
        , Just d'       <- RE.getDef b env 
        , (fd, ad)      <- X.takeXAppsWithAnnots d' 

        -- Match the hole part with the right of the 'x' binding.
        -- This completes the match, so we merge this new substitution
        -- with the one for the outer part of the rule.
        , rule_hole        <- rule { ruleLeft = hole, ruleLeftHole = Nothing }
        , Just (subd, asd) <- matchWithRule rule_hole env fd ad sub'
        = Just (subd, asd ++ as)

        -- rule_some didn't match properly: failure
        | otherwise
        = Nothing


-------------------------------------------------------------------------------
-- | Lookup a binding from a rewrite rule substitution.
--
--    Eg: RULE [x : %] (x : Int x). ...
-- 
lookupFromSubst 
        :: Ord n
        => a
        -> [(BindMode,Bind n)]
        -> (Map n (Exp a n), Map n (Type n))
        -> [(Bind n, Arg a n)]

lookupFromSubst _ bs m
 = let  bas  = catMaybes $ map (lookupX m) bs
   in   map (\(b, a) -> (A.anonymizeX b, A.anonymizeX a)) bas
   
 where  lookupX (xs,_)  (BMValue _, b@(BName n _))
         | Just x <- Map.lookup n xs
         = Just (b, RTerm x)

        lookupX (_,tys) (BMSpec, b@(BName n _))
         | Just t <- Map.lookup n tys
         = Just (b, RType t)

        lookupX _ _ = Nothing

