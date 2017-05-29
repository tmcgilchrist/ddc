
module DDC.Core.Env.Soup 
        ( Soup (..)
        , soupOfModule
        , restrictSoup
        , importSoup)
where
import DDC.Core.Module
import DDC.Type.Exp
import Data.Map.Strict                  (Map)
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map
import Data.Maybe

-- | Represents the soup of type and term declarations available at the
--   top-level of a module. This is enough information to type-check
--   the module.
-- 
--   A given name may be exported by multiple modules, so each field here maps
--   the base name of a thing to another map giving the details of that thing
--   according to the given module.
--
data Soup n
        = Soup
        { -- | Types of term names.
          soupTermTypes :: Map n (Map ModuleName (n, Type n)) }
        deriving Show


instance Ord n => Monoid (Soup n) where
 -- An empty soup contains no things.
 mempty = Soup
        { soupTermTypes = Map.empty }

 -- Combine two soups.
 mappend s1 s2
        = Soup
        { soupTermTypes = Map.unionWith Map.union (soupTermTypes s1) (soupTermTypes s2) }


-- | Extract the soup exported by a given module,
--   tagging each of the declarations with the module name.
soupOfModule 
        :: Ord n 
        => (ModuleName -> n -> n)
        -> Module a n 
        -> Soup n

soupOfModule makeSpecificName mm
 = let  mn      = moduleName mm

        specifics nBase t
         = let  nSpecific = makeSpecificName mn nBase 
           in   [ (nBase,     Map.singleton mn (nSpecific, t))
                , (nSpecific, Map.singleton mn (nSpecific, t)) ]

   in   Soup
        { soupTermTypes
                = Map.fromList $ concat
                $       [ fromMaybe [] 
                                $ fmap (specifics n) 
                                $ takeTypeOfExportSource exportSource 
                        | (n, exportSource)     <- moduleExportValues mm ]
        }


-- | Restrict a soup so that it only mentions specific type and term
--   names in the given sets.
restrictSoup
        :: Ord n
        => Set n        -- ^ Specific names of used type things.
        -> Set n        -- ^ Specific names of used term things.
        -> Soup n -> Soup n

restrictSoup _nsType nsTerm soup
        = Soup
        { soupTermTypes = Map.map (Map.filter (\(n, _t) -> Set.member n nsTerm)) 
                        $ soupTermTypes soup 
        }

-- | Add imports to a module for the things in the given soup.
importSoup 
        :: Ord n
        => Soup n
        -> Module a n -> Module a n

importSoup soup mm
 = mm
 { moduleImportValues
    = let
        mivsBase  = Map.fromList $ moduleImportValues mm
        mivsMore  = Map.fromList 
                  $ [ ( nSpecific
                      , ImportValueModule mn nSpecific t 
                                (if nLocal /= nSpecific 
                                        then Just nLocal
                                        else Nothing)
                                Nothing)
                    | (nLocal, mn, nSpecific, t) 
                        <- flattenSoupMap $ soupTermTypes soup ]

     in Map.toList $ Map.union mivsBase mivsMore
 }


-- | Flatten the map in one of the fields of a soup into a flat list.
flattenSoupMap 
        :: Ord n
        => Map n (Map ModuleName (n, Type n))
        -> [ (n, ModuleName, n, Type n) ]

flattenSoupMap mm
 = [ (nBase,  mn, nSpecific, t)
   | ((nBase, mn), (nSpecific, t)) <- Map.toList $ flatten mm ]


-- | Flatten a map of maps.
flatten :: (Ord a, Ord b)
        =>  Map a (Map b c) -> Map (a, b) c
flatten mp
        = Map.fromList
        $ concatMap (\(xa, mpbc) -> map (\(xb, xc) -> ((xa, xb), xc)) 
                                  $ Map.toList mpbc)
        $ Map.toList mp





