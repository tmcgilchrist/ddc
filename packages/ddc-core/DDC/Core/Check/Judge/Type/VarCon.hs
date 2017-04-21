
module DDC.Core.Check.Judge.Type.VarCon
        (checkVarCon)
where
import DDC.Core.Check.Judge.Type.DaCon
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified DDC.Core.Env.Soup      as Soup
import qualified DDC.Type.Sum           as Sum
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set


-------------------------------------------------------------------------------
checkVarCon :: Checker a n

-- variables ------------------------------------
checkVarCon !table !ctx mode demand xx@(XVar a u)

 -- Look in the local context.
 | Just t       <- lookupType u ctx
 = case mode of
        -- Check subsumption against an existing type.
        -- This may instantiate existentials in the exising type.
        Check tExpect
         ->     checkSub table a ctx demand xx tExpect

        _ -> do ctrace  $ vcat
                        [ text "**  Var Local"
                        , indent 4 $ ppr xx
                        , text "    tVar: " <> ppr t
                        , indent 4 $ ppr ctx
                        , empty ]

                returnX a
                        (\z -> XVar z u)
                        t
                        (Sum.empty kEffect)
                        ctx

 -- Look in the top-level soup.
 | UName nameBase       <- u
 , Just mpName          <- Map.lookup nameBase 
                        $  Soup.soupTermTypes $ contextSoup ctx
 = case Map.toList mpName of
    [ (_modName, (nameSpecific, tActual)) ]
     -> case mode of
         -- Check subsumption against an existing type.
         -- This may instantiate existentials in the exising type.
         -- TODO: fix this we're not using actual type.
         Check tExpect
           -> do  (xx', tt', effs', ctx')
                   <- checkSub table a ctx demand xx tExpect

                  let ctx_used
                        = ctx' { contextSoupUsedTerms
                               = Set.insert nameSpecific (contextSoupUsedTerms ctx) }

                  return (xx', tt', effs', ctx_used)

         _ -> do
                ctrace  $ vcat
                        [ text "**  Var Soup"
                        , indent 4 $ ppr xx
                        , text "    nBase:      " <> ppr nameBase
                        , text "    nSpecific:  " <> ppr nameSpecific
                        , text "    tActual:    " <> ppr tActual
                        , indent 4 $ ppr ctx
                        , empty ]

                let ctx_used
                        = ctx   { contextSoupUsedTerms
                                = Set.insert nameSpecific (contextSoupUsedTerms ctx) }

                returnX a
                        (\z -> XVar z (UName nameSpecific))
                        tActual
                        (Sum.empty kEffect)
                        ctx_used

    -- TODO: proper error message.
    _ -> error "check: var is multiply bound"

 -- Look in the global environment.
 | Just t      <- EnvX.lookupX u (contextEnvX ctx)
 = case mode of
        -- Check subsumption against an existing type.
        -- This may instantiate existentials in the exising type.
        Check tExpect
         ->     checkSub table a ctx demand xx tExpect

        _ -> do ctrace  $ vcat
                        [ text "**  Var Global"
                        , indent 4 $ ppr xx
                        , text "    tVar: " <> ppr t
                        , indent 4 $ ppr ctx
                        , text (show $ contextSoup ctx)
                        , empty ]

                returnX a
                        (\z -> XVar z u)
                        t
                        (Sum.empty kEffect)
                        ctx

 -- Can't find this variable name in the environment.
 | otherwise
 = throw $ ErrorUndefinedVar a u UniverseData


-- constructors ---------------------------------
-- Recon or Synth the type of a constructor.
checkVarCon !table !ctx mode@Recon _demand xx@(XCon a dc) 
 = do   let config      = tableConfig table

        -- Lookup the type of the constructor from the environment.
        tCtor           <- checkDaConType config ctx a dc

        ctrace  $ vcat
                [ text "**  Con"
                , text "    MODE: " <> ppr mode
                , indent 4 $ ppr xx
                , text "    tCon: " <> ppr tCtor
                , indent 4 $ ppr ctx
                , empty ]

        -- Type of the data constructor.
        returnX a
                (\z -> XCon z dc)
                tCtor
                (Sum.empty kEffect)
                ctx


-- Check a constructor against an expected type.
checkVarCon !table !ctx (Check tExpect) demand xx@(XCon a _)
 = checkSub table a ctx demand xx tExpect


-- Synthesise the type of a data constructor.
checkVarCon !table !ctx mode@(Synth {}) _demand xx@(XCon a dc) 
 = do
        let config      = tableConfig table

        -- All data constructors need to have valid type annotations.
        tCtor           <- checkDaConType config ctx a dc

        ctrace  $ vcat
                [ text "**  Con"
                , text "    MODE: " <> ppr mode
                , indent 4 $ ppr xx
                , text "    tCon: " <> ppr tCtor
                , indent 4 $ ppr ctx
                , empty ]

        -- Type of the data constructor.
        returnX a
                (\z -> XCon z dc)
                tCtor
                (Sum.empty kEffect)
                ctx


-- others ---------------------------------------
checkVarCon _ _ _ _ _
 = error "ddc-core.checkVarCon: no match"


-------------------------------------------------------------------------------
-- | Lookup the type of this data constructor from the context,
--   or throw an error if we can't find it.
checkDaConType config ctx a dc
 = do   -- Check that the constructor is in the data type declarations.
        checkDaConM config ctx (XCon a dc) a dc

        -- Lookup the type of the constructor.
        case dc of
         DaConUnit   -> return tUnit

         DaConRecord{}
          -> do let Just t   = takeTypeOfDaCon dc
                return t

         DaConPrim{} 
          -> return $ daConType dc

         DaConBound n
          -- Types of algebraic data ctors should be in the defs table.
          |  Just ctor <- Map.lookup n (dataDefsCtors $ contextDataDefs ctx)
          -> return $ typeOfDataCtor ctor

          | otherwise
          -> throw  $ ErrorUndefinedCtor a $ XCon a dc
 
