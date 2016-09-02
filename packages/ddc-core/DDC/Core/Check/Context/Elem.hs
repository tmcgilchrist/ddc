
module DDC.Core.Check.Context.Elem
        ( -- * Positions in the context.
          Pos    (..)

          -- * Roles of type variables.
        , Role   (..)

          -- * Existentials.
        , Exists (..)
        , typeOfExists
        , takeExists

          -- * Context elements.
        , Elem   (..))
where
import DDC.Type.Exp.Simple
import DDC.Base.Pretty


-- Positions --------------------------------------------------------------------------------------
-- | A position in the type checker context.
--   A position is used to record a particular position in the context stack,
--   so that we can pop elements higher than it.
data Pos
        = Pos !Int
        deriving (Show, Eq)

instance Pretty Pos where
 ppr (Pos p)
        = text "*" <> int p


-- Role -------------------------------------------------------------------------------------------
-- | The role of some type variable.
data Role
        -- | Concrete type variables are region variables that have been introduced
        --   in an enclosing lexical scope. All the capabilities for these will 
        --   also be in the context.
        = RoleConcrete
        
        -- | Abstract type variables are the ones that are bound by type abstraction
        --   Inside the body of a type abstraction we can assume a region supports
        --   any given capability. We only need to worry about if it really does
        --   when we actually run the enclosed computation.
        | RoleAbstract
        deriving (Show, Eq)


instance Pretty Role where
 ppr role
  = case role of
        RoleConcrete    -> text "Concrete"
        RoleAbstract    -> text "Abstract"


-- Exists -----------------------------------------------------------------------------------------
-- | An existential variable.
data Exists n
        = Exists !Int !(Kind n)
        deriving (Show)

instance Eq (Exists n) where
 (==)   (Exists i1 _) (Exists i2 _)     = i1 == i2
 (/=)   (Exists i1 _) (Exists i2 _)     = i1 /= i2


instance Ord (Exists n) where
 compare (Exists i1 _) (Exists i2 _)
        = compare i1 i2


instance Pretty (Exists n) where
 ppr (Exists i _) = text "?" <> ppr i


-- | Wrap an existential variable into a type.
typeOfExists :: Exists n -> Type n
typeOfExists (Exists n k)
        = TCon (TyConExists n k)


-- | Take an Exists from a type.
takeExists :: Type n -> Maybe (Exists n)
takeExists tt
 = case tt of
        TCon (TyConExists n k)  -> Just (Exists n k)
        _                       -> Nothing


-- Elem -------------------------------------------------------------------------------------------
-- | An element in the type checker context.
data Elem n
        -- | A context position marker.
        = ElemPos        !Pos

        -- | Kind of some variable.
        | ElemKind       !(Bind n) !Role

        -- | Type of some variable.
        | ElemType       !(Bind n)

        -- | Existential variable declaration
        | ElemExistsDecl !(Exists n)

        -- | Existential variable solved to some monotype.
        | ElemExistsEq   !(Exists n) !(Type n)
        deriving (Show, Eq)


instance (Pretty n, Eq n) => Pretty (Elem n) where
 ppr ll
  = case ll of
        ElemPos p
         -> ppr p

        ElemKind b role  
         -> ppr (binderOfBind b) 
                <+> text "::" 
                <+> (ppr $ typeOfBind b)
                <+> text "@" <> ppr role

        ElemType b
         -> ppr (binderOfBind b)
                <+> text "::"
                <+> (ppr $ typeOfBind b)

        ElemExistsDecl i
         -> ppr i

        ElemExistsEq i t 
         -> ppr i <+> text "=" <+> ppr t
