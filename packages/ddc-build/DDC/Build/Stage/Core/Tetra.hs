
module DDC.Build.Stage.Core.Tetra
        ( ConfigTetraToSalt (..)
        , tetraToSalt)
where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import DDC.Data.Pretty

import qualified DDC.Build.Stage.Core                   as B
import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Language.Tetra               as BE

import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Simplifier.Recipe             as C
import qualified DDC.Core.Transform.Namify              as CNamify
import qualified DDC.Core.Transform.Unshare             as CUnshare

import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Platform                 as A
import qualified DDC.Core.Salt.Runtime                  as A

import qualified DDC.Core.Tetra                         as E
import qualified DDC.Core.Tetra.Transform.Boxing        as EBoxing
import qualified DDC.Core.Tetra.Transform.Curry         as ECurry


---------------------------------------------------------------------------------------------------
data ConfigTetraToSalt
        = ConfigTetraToSalt
        { configSinkExplicit    :: B.Sink       -- ^ Sink after making explicit.
        , configSinkLambdas     :: B.Sink       -- ^ Sink after lambda lifting.
        , configSinkUnshare     :: B.Sink       -- ^ Sink after unsharing.
        , configSinkCurry       :: B.Sink       -- ^ Sink after curry transform.
        , configSinkBoxing      :: B.Sink       -- ^ Sink after boxing transform.
        , configSinkPrep        :: B.Sink       -- ^ Sink after prep before to-salt conversion.
        , configSinkChecked     :: B.Sink       -- ^ Sink after checking before to-salt converion.
        , configSinkSalt        :: B.Sink       -- ^ Sinl after conversion to salt.
        }

-- | Convert Core Tetra to Core Salt.
tetraToSalt
        :: A.Platform           -- ^ Platform configuation.
        -> A.Config             -- ^ Runtime config.
        -> C.Module () E.Name   -- ^ Core tetra module.
        -> ConfigTetraToSalt    -- ^ Sinker config.
        -> ExceptT [B.Error] IO (C.Module () A.Name)

tetraToSalt platform runtimeConfig mm config
 = do
        -- Expliciate the core module.
        mm_explicit     
         <- B.coreSimplify 
                BE.fragment (0 :: Int) C.expliciate
                mm

        liftIO $ B.pipeSink (renderIndent $ ppr mm_explicit)
                            (configSinkExplicit config)

        -- Re-check the module before lambda lifting.
        mm_checked_lambdas
         <-  B.coreCheck    
                "TetraToSalt/lambdas" BE.fragment C.Recon 
                B.SinkDiscard B.SinkDiscard
                mm_explicit


        -- Perform lambda lifting.
        mm_lambdas
         <-  B.coreSimplify
                BE.fragment (0 :: Int) C.lambdas mm_checked_lambdas

        liftIO $ B.pipeSink (renderIndent $ ppr mm_lambdas)
                            (configSinkLambdas config)


        -- Re-check the module before performing unsharing
        mm_checked_unshare
         <- B.coreCheck
                "TetraToSalt/unshare" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_lambdas


        -- Perform the unsharing transform.
        let mm_unshare  = CUnshare.unshareModule mm_checked_unshare

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_unshare)
                            (configSinkUnshare config)


        -- Perform the curry transform.
        mm_curry    
         <- case ECurry.curryModule mm_unshare of
                Left err        -> throwE [B.ErrorTetraConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_curry)
                            (configSinkCurry config)


        -- Prep before boxing transform.
        mm_prep_boxing
         <- B.coreSimplify BE.fragment (0 :: Int) 
                (C.anormalize
                        (CNamify.makeNamifier E.freshT)
                        (CNamify.makeNamifier E.freshX))
                mm_curry


        -- Perform the boxing transform.
        let mm_boxing
                = EBoxing.boxingModule mm_prep_boxing
        
        liftIO $ B.pipeSink (renderIndent $ ppr mm_boxing)      
                            (configSinkBoxing config)


        -- Prep before conversion to salt.
        mm_prep_salt
         <- B.coreSimplify BE.fragment (0 :: Int)
                (  C.anormalize
                        (CNamify.makeNamifier E.freshT)
                        (CNamify.makeNamifier E.freshX)
                `mappend` C.flatten)
                mm_boxing

        liftIO $ B.pipeSink (renderIndent $ ppr mm_prep_salt)
                            (configSinkPrep config)


        -- Re-check before conversion to salt.
        mm_checked_salt
         <- B.coreCheck
                "TetraToSalt/toSalt" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_prep_salt

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_salt)
                            (configSinkChecked config)


        -- Convert core tetra to core salt.
        mm_salt
         <- case E.saltOfTetraModule platform runtimeConfig
                (C.profilePrimDataDefs E.profile)
                (C.profilePrimKinds    E.profile)
                (C.profilePrimTypes    E.profile) mm_checked_salt of
                Left err        -> throwE [B.ErrorTetraConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_salt)
                            (configSinkSalt config)

        return mm_salt


