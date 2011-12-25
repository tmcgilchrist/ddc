
-- | Single step evaluation of primitive operators and constructors.
-- 
--   This should implements the proper operational semantics of the core language,
--   so we're careful to check all premieses of the evaluation rules are satisfied.
module DDCI.Core.Eval.Prim
        ( primStep
        , primNewRegion
        , primDelRegion)
where
import DDCI.Core.Eval.Compounds
import DDCI.Core.Eval.Env
import DDCI.Core.Eval.Name
import DDC.Core.Exp
import DDCI.Core.Eval.Store             (Store, SBind(..))
import qualified DDCI.Core.Eval.Store   as Store


-- | Singe step a primitive operator or constructor.
primStep
        :: Name                 -- ^ Name of operator to evaluate.
        -> [Exp () Name]        -- ^ Arguments to operator.
        -> Store                -- ^ Current store.
        -> Maybe ( Store        
                 , Exp () Name) -- ^ New store and result expression, 
                                --   if the operator steps, otherwise Nothing.

primStep n xs store
-- = trace (show $ text "primStep: " <+> text (show n) <+> text (show xs))
 = primStep' n xs store


-- Alloction of Ints.
primStep' (NameInt i) [xR, xUnit] store
        -- unpack the args
        | XType tR      <- xR
        , Just rgn      <- takeHandleT tR
        , isUnitX xUnit

        -- the store must contain the region we're going to allocate into.
        , Store.hasRgn store rgn

        -- add the binding to the store.
        , (store1, l)   <- Store.allocBind rgn (SObj (NameInt i) []) store

        = Just  ( store1
                , XCon () (UPrim (NameLoc l) (tInt tR)))


-- Addition of Ints.
primStep' (NamePrimOp PrimOpAddInt) [xR1, xR2, xR3, xL1, xL2] store
        -- unpack the args
        | Just r1       <- takeHandleX xR1
        , Just r2       <- takeHandleX xR2
        , XType tR3     <- xR3
        , Just r3       <- takeHandleX xR3        
        , Just l1       <- takeLocX xL1
        , Just l2       <- takeLocX xL2

        -- get the regions and values of each location
        , Just (r1', SObj (NameInt i1) [])  <- Store.lookupRegionBind l1 store
        , Just (r2', SObj (NameInt i2) [])  <- Store.lookupRegionBind l2 store
        
        -- the locations must be in the regions the args said they were in
        , r1' == r1
        , r2' == r2
        
        -- the destination region must exist
        , Store.hasRgn store r3

        -- do the actual computation
        , i3    <- i1 + i2
        
        -- write the result to a new location in the store
        , (store1, l3)  <- Store.allocBind r3 (SObj (NameInt i3) []) store

        = Just  ( store1
                , XCon () (UPrim (NameLoc l3) (tInt tR3)))


-- Update of Ints.
primStep' (NamePrimOp PrimOpUpdateInt) [xR1, xR2, xMutR1, xL1, xL2] store
        -- unpack the args
        | Just r1       <- takeHandleX  xR1
        , Just r2       <- takeHandleX  xR2
        , Just r1W      <- takeMutableX xMutR1
        , Just l1       <- takeLocX     xL1
        , Just l2       <- takeLocX     xL2      

        -- the witness must be for the destination region
        , r1W == r1

        -- get the regions and values of each location
        , Just (r1L, SObj (NameInt _)  [])  <- Store.lookupRegionBind l1 store
        , Just (r2L, SObj (NameInt i2) [])  <- Store.lookupRegionBind l2 store

        -- the locations must be in the regions the args said they were in
        , r1L == r1
        , r2L == r2

        -- update the destination
        , store1     <- Store.addBind l1 r1 (SObj (NameInt i2) []) store

        = Just  ( store1
                , XCon () (UPrim (NamePrimCon PrimDaConUnit) tUnit))

primStep' _ _ _
        = Nothing

