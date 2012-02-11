
-- | Type checker for the Disciple core language.
module DDC.Core.Check
        ( -- * Checking Expressions
          checkExp,     typeOfExp

          -- * Checking Witnesses
        , checkWitness, typeOfWitness
        , typeOfWiCon

          -- * Error messages
        , Error(..))
where
import DDC.Core.Check.Error
import DDC.Core.Check.ErrorMessage      ()
import DDC.Core.Check.CheckExp
import DDC.Core.Check.CheckWitness
