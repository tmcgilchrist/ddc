
module DDC.Core.Llvm.LlvmM
        ( LlvmM
        , LlvmState(..)
        , llvmStateInit 
        , die

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel
        , newUniqueBlockId

          -- * Platform Specific
        , getPrimVarM
        , getStructM
        , getBytesOfTypeM
        , getBytesOfStructM)
where
import DDC.Core.Llvm.Platform
import DDC.Llvm.Instr
import Data.Map                 (Map)
import qualified Data.Map       as Map
import Control.Monad.State.Strict

type LlvmM = State LlvmState

-- | Called when we find a thing that cannot be converted to C.
die :: String -> a
die msg = error $ "DDC.Core.Llvm.Convert " ++ msg


-- LlvmState ------------------------------------------------------------------
-- | State for the LLVM conversion.
data LlvmState
        = LlvmState
        { -- Unique name generator.
          llvmStateUnique       :: Int 

          -- The current platform.
        , llvmStatePlatform     :: Platform 

          -- Primitives in the global environment.
        , llvmStatePrimVars     :: Map String Var }


-- | Initial LLVM state.
llvmStateInit :: Platform -> Map String Var -> LlvmState
llvmStateInit platform prims
        = LlvmState
        { llvmStateUnique       = 1 
        , llvmStatePlatform     = platform
        , llvmStatePrimVars     = prims }


-- Unique ---------------------------------------------------------------------
-- | Unique name generation.
newUnique :: LlvmM Unique
newUnique 
 = do   s       <- get
        let u   = llvmStateUnique s
        put     $ s { llvmStateUnique = u + 1 }
        return  $ u


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: Type -> LlvmM Var
newUniqueVar t
 = do   u <- newUnique
        return $ VarLocal ("_v" ++ show u) t


-- | Generate a new unique named register variable with the specified `LlvmType`.
newUniqueNamedVar :: String -> Type -> LlvmM Var
newUniqueNamedVar name t
 = do   u <- newUnique 
        return $ VarLocal ("_v" ++ show u ++ "_" ++ name) t


-- | Generate a new unique label.
newUniqueLabel :: String -> LlvmM Var
newUniqueLabel name
 = do   u <- newUnique
        return $ VarLocal ("_l" ++ show u ++ "_" ++ name) TLabel


-- | Generate a new unique blockid.
newUniqueBlockId :: LlvmM BlockId
 = do   u <- newUnique
        return   $ BlockId u


-- Platform Specific ----------------------------------------------------------
-- | Get a primitive variable.
getPrimVarM :: String -> LlvmM Var
getPrimVarM name
 = do   prims   <- gets llvmStatePrimVars 
        case Map.lookup name prims of
         Just var       -> return var
         _              -> error $ "getPrimVar: unknown prim " ++ show name

-- | Get a structure definition.
getStructM :: String -> LlvmM Struct
getStructM name
 = do   platform        <- gets llvmStatePlatform
        let Just struct = Map.lookup name $ platformStructs platform
        return struct


-- | Get the size of a type on this platform, in bytes.
getBytesOfTypeM :: Type -> LlvmM Integer
getBytesOfTypeM tt
 = do   platform        <- gets llvmStatePlatform
        let Just bytes  = takeBytesOfType (platformAddrBytes platform) tt
        return bytes


-- | Get the size of a struct on this platform, in bytes.
getBytesOfStructM :: Struct -> LlvmM Integer
getBytesOfStructM _struct
        = return 5
