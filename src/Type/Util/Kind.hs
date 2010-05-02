
module Type.Util.Kind
	-- namespace things
	( defaultKindV
	, spaceOfKind
	, kindOfSpace 

	-- projections
	, tyConKind

	-- witnesses
	, makeKWitJoin
	, inventWitnessOfClass

	-- kind functions
	, makeKFun
	, takeKApps
	, resultKind
	, makeDataKind

	-- kind reconstruction
	, kindOfType
	, kindOfType_orDie
	
	-- fast kind utils
	, isClosure)
where
import Type.Pretty		()
import Type.Builtin
import Type.Util.Bits
import Type.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import qualified Debug.Trace	as Debug

-----
stage	= "Type.Util.Kind"
debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x


-- Namespace things --------------------------------------------------------------------------------
defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case varNameSpace v of
 	NameType		-> kValue
	NameRegion		-> kRegion
	NameEffect		-> kEffect
	NameClosure		-> kClosure
	

-- | Get the namespace associated with a kind.
spaceOfKind ::	Kind -> Maybe NameSpace
spaceOfKind  kind
	| kind == kValue	= Just NameType
	| kind == kRegion	= Just NameRegion
	| kind == kEffect	= Just NameEffect
	| kind == kClosure	= Just NameClosure
	| otherwise		= Nothing
	

-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Kind
kindOfSpace space
 = case space of
 	NameType		-> kValue
	NameRegion		-> kRegion
	NameEffect		-> kEffect
	NameClosure		-> kClosure
	NameClass		-> panic stage "kindOfSpace: witness"
	_			-> panic stage
				$  "kindOfSpace: no match for " % show space


-- Projections -------------------------------------------------------------------------------------
-- | Take the kind of a tycon
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun				
	 -> KFun kValue (KFun kValue (KFun kEffect (KFun kClosure kValue)))

	TyConData    { tyConDataKind }		
	 -> tyConDataKind

	TyConWitness { tyConWitnessKind }
	 -> tyConWitnessKind	 


-- Kind Functions ----------------------------------------------------------------------------------
-- | Get the result of applying all the paramters to a kind.
resultKind :: Kind -> Kind
resultKind kk
 = case kk of
 	KFun k1 k2	-> resultKind k2
	_		-> kk


-- | make a function kind
makeKFun :: [Kind] -> Kind
makeKFun [k]		= k
makeKFun (k : ks)	= KFun k (makeKFun ks)

-- | Flatten out a kind application into its parts
takeKApps :: Kind -> Maybe (Kind, [Type])
takeKApps kk
 = case kk of
	KApp k1 t2	-> Just (k1, [t2])
	KApps k1 ts	-> Just (k1, ts)
	_		-> Nothing

-- Make a kind from the parameters to a data type
makeDataKind :: [Var] -> Kind
makeDataKind vs
 	= foldl (flip KFun) kValue 
	$ map (\v -> kindOfSpace (varNameSpace v)) 
	$ reverse vs


-- Witnesses ---------------------------------------------------------------------------------------
-- | Join some kind classes
makeKWitJoin :: [Kind] -> Kind
makeKWitJoin ts
 = case ts of
 	[t]	-> t
	ts	-> KWitJoin ts

-- | Invent a place-holder witness that satisfies a type class constraint.
--	This is used in Desugar.ToCore when we don't know how to properly construct the
--	real witnesses yet.
inventWitnessOfClass :: Kind -> Maybe Type
inventWitnessOfClass (KApps k@(KCon kiCon s) ts)
	| Just tcWitness	<- takeTyConWitnessOfKiCon kiCon
	= let 	-- Get the kinds of the type arguments.
		Just ks	= sequence $ map kindOfType ts

		-- The resulting kind guarantees the constraint.
		kResult	= KApps k (map TIndex $ reverse [0 .. length ks - 1])
		k'	= makeKFuns ks kResult
		tyCon	= TyConWitness tcWitness k'

   	   in	trace ("invent " % ks)
			$ Just $ makeTApp (TCon tyCon : ts)

inventWitnessOfClass k
	= freakout stage
		("inventWitnessOfClass: don't know how to build witness for '" % k % "'\n")
		Nothing


-- Kind reconstruction -----------------------------------------------------------------------------
-- | Reconstruct the kind of this type, kind checking along the way
kindOfType :: Type -> Maybe Kind
kindOfType tt 
 = let 	kind	= kindOfType' tt
   in	trace 	("kindOfType: " % tt 	% "\n")
		$ Just $ kind

kindOfType' tt
 = trace ("kindOfType': " % tt) $ 
   case tt of
	TClass k _		-> k
	TVar k _		-> k
	TVarMore k _ _		-> k
	TCon tyCon		-> (tyConKind tyCon)
	TBot k			-> k
	TTop k			-> k
	
	-- we'll just assume kind annots on TSum and TMask are right, and save
	--	having to check all the elements
	TSum  k _		-> k

	-- application of KFun
	-- TODO: we're not checking the kinds match up atm, it's too much of a perf hit.
	--	 rely on core lint to detect kind problems.
	TApp t1 t2		
	 | KFun k11 k12		<- kindOfType' t1
	 , k2			<- kindOfType' t2
--	 , k11 == k2
	 -> betaTK 0 t2 k12

	-- application failed.. :(
	TApp t1 t2
	 -> kindOfType_freakout t1 t2
	
	TForall  b t1 t2	-> kindOfType' t2

	TContext t1 t2		-> kindOfType' t2
	TFetters t1 _		-> kindOfType' t1
	TConstrain t1 crs	-> kindOfType' t1
	
	
	-- effect and closure constructors should always be fully applied.
	TEffect{}		-> kEffect
	TFree{}			-> kClosure
	TDanger{}		-> kClosure

	TError k _		-> k
	TElaborate e t		-> kindOfType' t
	
	-- used in core -----------------------------------------------------
	-- The KJoins get crushed during Core.Util.Pack.packK
	TWitJoin ts
	 | ks			<- map kindOfType' ts
	 -> makeKWitJoin ks
			
	-- some of the helper constructors don't have real kinds ------------
	_			-> panic stage $ "kindOfType bad kind for: " % tt


kindOfType_freakout t1 t2
 = panic stage	
	( "kindOfType: kind error in type application (t1 t2)\n"
	% "    t1  = " % t1 		% "\n"
	% "          " % show t1	% "\n"
	% "\n"
	% "    t2  = " % t2 		% "\n"
	% "          " % show t2	% "\n")
	Nothing

kindOfType_orDie :: Type -> Kind
kindOfType_orDie tt
 = let  Just k	= kindOfType tt
   in	k

-- Beta --------------------------------------------------------------------------------------------
-- de bruijn style beta evalation
--	used to handle substitution arrising from application of KForall's in kindOfType.

betaTK :: Int -> Type -> Kind -> Kind
betaTK depth tX kk
 = trace ("betaTK " % tX % "\n") $
   case kk of
 	KNil		-> kk
	KFun k1 k2	-> KFun k1 (betaTK (depth + 1) tX k2)
	KCon{}		-> kk
	KApps k ts	-> KApps k (map (betaTT depth tX) ts)
	KWitJoin ks	-> kk

	_	-> panic stage
		$ "betaTK: no match for " % kk
		
	
betaTT :: Int -> Type -> Type -> Type
betaTT depth tX tt
 = let down	= betaTT depth tX
   in  case tt of
   	TNil		-> tt
	TForall b k t	-> TForall b k (down t)
	TContext k t	-> TContext k (down t)
	TFetters t fs	-> TFetters (down t) fs
	TApp t1 t2	-> TApp (down t1) (down t2)
	TSum k ts	-> TSum k (map down ts)
	TCon{}		-> tt
	TVar{}		-> tt
	TVarMore{}	-> tt

	TIndex ix
	 | ix == depth	-> tX
	 | ix > depth	-> TIndex (ix - 1)
	 | otherwise	-> tt
	 	
	TTop{}		-> tt
	TBot{}		-> tt
	TEffect v ts	-> TEffect v (map down ts)
	TFree v t	-> TFree v (down t)

	TWitJoin ts	-> TWitJoin (map down ts)

	_	-> panic stage
		$ "betaTT: no match for " % tt


-- Fast kind utils ---------------------------------------------------------------------------------

-- Used in Core.Subsumes
isClosure :: Type -> Bool
isClosure tt
 = case tt of
	-- closures are always fully constructed
	TApp{}			-> False
 	TSum	 k _		-> k == kClosure
	TVar	 k _		-> k == kClosure
	TVarMore k _ _		-> k == kClosure
	TClass	 k _		-> k == kClosure
	TFree{}			-> True
	TDanger{}		-> True
	TTop	k 		-> k == kClosure
	TBot	k 		-> k == kClosure

	TFetters t1 _		-> isClosure t1
	TConstrain t1 _		-> isClosure t1
	TContext   _  t1	-> isClosure t1
	_			-> False

