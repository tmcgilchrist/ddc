
-- | Definition of names used in Source Tetra language.
module DDC.Source.Tetra.Prim.Base
        ( -- * Primitive Types
          PrimType      (..)

          -- ** Primitive machine type constructors.
        , PrimTyCon     (..)

          -- ** Primitive Tetra specific type constructors.
        , PrimTyConTetra(..)

          -- * Primitive Values
        , PrimVal       (..)

          -- ** Primitive arithmetic operators.
        , PrimArith     (..)

          -- ** Primitive casting operators.
        , PrimCast      (..)

          -- ** Primitive vector operators.
        , OpVector      (..)

          -- ** Primitive function operators.
        , OpFun         (..)

          -- ** Primitive error handling.
        , OpError       (..)

          -- ** Primitive literals.
        , PrimLit       (..)
        , primLitOfLiteral)
where
import DDC.Type.Exp.TyCon
import DDC.Core.Exp.Literal
import DDC.Core.Tetra    
        ( OpFun         (..)
        , OpVector      (..)
        , OpError       (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..))

import Data.Text        (Text)


---------------------------------------------------------------------------------------------------
-- | Primitive types.
data PrimType
        -- | Primitive sort constructors.
        = PrimTypeSoCon         !SoCon

        -- | Primitive kind constructors.
        | PrimTypeKiCon         !KiCon

        -- | Primitive witness type constructors.
        | PrimTypeTwCon         !TwCon

        -- | Other type constructors at the spec level.
        | PrimTypeTcCon         !TcCon

        -- | Primitive machine type constructors.
        | PrimTypeTyCon         !PrimTyCon

        -- | Primtiive type constructors specific to the Tetra fragment.
        | PrimTypeTyConTetra    !PrimTyConTetra
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
-- | Primitive type constructors specific to the Tetra language fragment.
data PrimTyConTetra
        -- | @TupleN#@. Tuples.
        = PrimTyConTetraTuple !Int
        
        -- | @Vector#@. Vectors.
        | PrimTyConTetraVector

        -- | @F#@.       Reified function values.
        | PrimTyConTetraF

        -- | @C#@.       Reified function closures.
        | PrimTyConTetraC

        -- | @U#@.       Explicitly unboxed values.
        | PrimTyConTetraU
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
-- | Primitive values.
data PrimVal
        -- | Primitive literals.
        = PrimValLit            !PrimLit

        -- | Primitive arithmetic operators.
        | PrimValArith          !PrimArith

        -- | Primitive numeric casting operators.
        | PrimValCast           !PrimCast

        -- | Primitive error handling.
        | PrimValError          !OpError
        
        -- | Primitive vector operators.
        | PrimValVector         !OpVector

        -- | Primitive function operators.
        | PrimValFun            !OpFun
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
data PrimLit
        -- | A boolean literal.
        = PrimLitBool           !Bool

        -- | A natural literal,
        --   with enough precision to count every heap object.
        | PrimLitNat            !Integer

        -- | An integer literal,
        --   with enough precision to count every heap object.
        | PrimLitInt            !Integer

        -- | An unsigned size literal,
        --   with enough precision to count every addressable byte of memory.
        | PrimLitSize           !Integer

        -- | A word literal,
        --   with the given number of bits precison.
        | PrimLitWord           !Integer !Int

        -- | A floating point literal,
        --   with the given number of bits precision.
        | PrimLitFloat          !Double !Int

        -- | A character literal.
        | PrimLitChar           !Char

        -- | Text literals (UTF-8 encoded)
        | PrimLitTextLit        !Text
        deriving (Eq, Ord, Show)


-- | Convert a literal to a Tetra name.
primLitOfLiteral :: Literal -> PrimLit
primLitOfLiteral lit
 = case lit of
        LNat    n       -> PrimLitNat     n
        LInt    i       -> PrimLitInt     i
        LSize   s       -> PrimLitSize    s
        LWord   i b     -> PrimLitWord    i b
        LFloat  f b     -> PrimLitFloat   f b
        LChar   c       -> PrimLitChar    c
        LString tx      -> PrimLitTextLit tx

