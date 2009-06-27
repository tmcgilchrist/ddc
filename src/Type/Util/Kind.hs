
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
	, resultKind
	, makeDataKind

	-- kind reconstruction
	, kindOfType
	, kindOfType_orDie
	
	-- fast kind utils
	, isClosure)

where
import Type.Pretty
import Type.Util.Bits
import Type.Exp

import Shared.Var		(NameSpace(..))
import Shared.Error
import Shared.Pretty
import qualified Shared.Var as Var

import Util

import Main.Arg
import qualified Debug.Trace

-----
stage	= "Type.Util.Kind"


-- Namespace things --------------------------------------------------------------------------------
defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case Var.nameSpace v of
 	NameType	-> KValue
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	

-- | Get the namespace associated with a kind.
spaceOfKind ::	Kind -> NameSpace
spaceOfKind  kind
 = case resultKind kind of
 	KValue		-> NameType
	KRegion		-> NameRegion
	KEffect		-> NameEffect
	KClosure	-> NameClosure
	_		-> panic stage
			$ "spaceOfKind: no space for " % kind

-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Kind
kindOfSpace space
 = case space of
 	NameType	-> KValue
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	NameClass	-> KFetter
	_		-> panic stage
			$  "kindOfSpace: no match for " % show space

-- Projections -------------------------------------------------------------------------------------
-- | Take the kind of a tycon
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun			-> KFun KValue (KFun KValue (KFun KEffect (KFun KClosure KValue)))
	TyConData { tyConDataKind }	-> tyConDataKind
	TyConClass { tyConClassKind }	-> tyConClassKind	 


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


-- Make a kind from the parameters to a data type
makeDataKind :: [Var] -> Kind
makeDataKind vs
 	= foldl (flip KFun) KValue 
	$ map (\v -> kindOfSpace (Var.nameSpace v)) 
	$ reverse vs


-- Witnesses ---------------------------------------------------------------------------------------
-- | Join some kind classes
makeKWitJoin :: [Kind] -> Kind
makeKWitJoin ts
 = case ts of
 	[t]	-> t
	ts	-> KWitJoin ts

-- | Invent an explicit witness needed to satisfy a certain constraint
--	This is used in Desugar.ToCore when we don't know how to properly construct our witnesses yet.
inventWitnessOfClass :: Kind -> Maybe Type
inventWitnessOfClass (KClass v ts)
 = let 	Just ks	= sequence $ map kindOfType ts
	kResult	= KClass v (map TIndex $ reverse [0 .. length ks - 1])
	k	= makeKForall ks kResult
   in	Just (makeTApp (TCon (TyConClass v k) : ts))

inventWitnessOfClass k
	= freakout stage
		("inventWitnessOfClass: don't know how to build witness for '" % k % "'\n")
		Nothing


-- Kind reconstruction -----------------------------------------------------------------------------
-- | Reconstruct the kind of this type, kind checking along the way
kindOfType :: Type -> Maybe Kind
kindOfType tt = {-# SCC "kindOfType" #-} kindOfType' tt

kindOfType' tt

	| TForall  b t1 t2	<- tt	= kindOfType t2

	| TContext t1 t2	<- tt	= kindOfType t2
	| TFetters t1 _		<- tt	= kindOfType t1
	
	-- we'll just assume kind annots on TSum and TMask are right, and save
	--	having to check all the elements
	| TSum  k _		<- tt	= Just k
	| TMask k _ _		<- tt	= Just k

	| TVar  k _		<- tt	= Just k
	| TVarMore k _ _	<- tt	= Just k
	| TCon tyCon		<- tt	= Just (tyConKind tyCon)
	| TBot k		<- tt	= Just k
	| TTop k		<- tt	= Just k

	-- application of KForall
	| TApp t1 t2			<- tt
	, Just (KForall k11 k12)	<- kindOfType t1
	, Just k2			<- kindOfType t2
	, k11 == k2
	= Just (betaTK 0 t2 k12)

	-- application of kind function (which is a sugared KForall)
	| TApp t1 t2			<- tt
	, Just (KFun k11 k12)		<- kindOfType t1
	, Just k2			<- kindOfType t2
	, k11 == k2
	= Just k12

	-- application failed.. :(
	| TApp t1 t2			<- tt
	= kindOfType_freakout t1 (kindOfType t1) t2 (kindOfType t2)
	
	-- effect and closure constructors should always be fully applied.
	| TEffect{}		<- tt	= Just KEffect
	| TFree{}		<- tt	= Just KClosure
	| TDanger{}		<- tt	= Just KClosure
	| TTag{}		<- tt	= Just KClosure		-- lies!

	-- wildcards have kinds embedded in them
	| TWild k		<- tt	
	= Just k

	-- used in type inferencer -------------------------------------------

	-- TData might be partially applied
	| TData k v ts		<- tt
	= do	ks	<- sequence $ map kindOfType ts
		case appKinds k ks of
		 Just k'	-> return k'
		 _		-> kindOfType_freakouts (TVar k v) k $ zip ts ks

	-- TFuns are always fully applied
	| TFun{}		<- tt
	= Just KValue

	| TClass k _		<- tt
	= Just k

	| TError k _		<- tt
	= Just k

	-- used in source / desugar -----------------------------------------
	| TElaborate e t	<- tt
	= kindOfType t

	-- used in core -----------------------------------------------------
	-- The KJoins get crushed during Core.Util.Pack.packK
	| TWitJoin ts		<- tt
	, Just ks		<- sequence $ map kindOfType ts
	= Just (makeKWitJoin ks)
			
	-- some of the helper constructors don't have real kinds ------------
	| otherwise
	= freakout stage 
		("kindOfType: cannot get kind for " % show tt % "\n")
		Nothing


kindOfType_freakout t1 k1 t2 k2
 = freakout stage	
	( "takeKindOfType: kind error in type application (t1 t2)\n"
	% "    t1  = " % t1 	% "\n"
	% "  K[t1] = " % k1	% "\n"
	% "\n"
	% "    t2  = " % t2 	% "\n"
	% "  K[t2] = " % k2	% "\n")
	Nothing


kindOfType_freakouts t1 k1 tks
 = freakout stage	
	( "takeKindOfType: kind error in type application t1 ts\n"
	% "    t1   = " % t1 	% "\n"
	% "  K[t1]  = " % k1	% "\n"
	% "\n"
	% "    tks  = " % tks 	% "\n")
	Nothing


-- | Apply some kinds to a kind function
--	If this results in a kind error then return Nothing
appKinds :: Kind -> [Kind] -> Maybe Kind
appKinds k []		= Just k

appKinds (KFun k1 k2) (k:ks)
	| k1 == k	= appKinds k2 ks
	
appKinds  k ks		= Nothing


-- | Get the kind of a type, or die if there is a kind error.
--	This is harder to debug with...
kindOfType_orDie :: Type -> Kind
kindOfType_orDie tt
 = case kindOfType tt of
 	Just k		-> k
	Nothing		-> panic stage
			$ "kindOfType: no match for " % tt % "\n"
			%> show tt


-- Beta --------------------------------------------------------------------------------------------
-- de bruijn style beta evalation
--	used to handle substitution arrising from application of KForall's in kindOfType.

betaTK :: Int -> Type -> Kind -> Kind
betaTK depth tX kk
 = let down 	= betaTK depth tX
   in case kk of
 	KNil		-> kk
	KForall k1 k2	-> KForall k1 (betaTK (depth + 1) tX k2)
	KFun	k1 k2	-> KFun (down k1) (down k2)
	KValue		-> kk
	KRegion		-> kk
	KEffect		-> kk
	KClosure	-> kk
	KClass v ts	-> KClass v (map (betaTT depth tX) ts)
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
	TMask k t1 t2	-> TMask k (down t1) (down t2)
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
	TTag{}		-> tt

	TWitJoin ts	-> TWitJoin (map down ts)
	TWild k		-> tt

	_	-> panic stage
		$ "betaTT: no match for " % tt


-- Fast kind utils ---------------------------------------------------------------------------------

-- Used in Core.Subsumes
isClosure :: Type -> Bool
isClosure tt
 = case tt of
	-- closures are always fully constructed
	TApp{}			-> False
	TData{}			-> False
	TFun{}			-> False

 	TSum	KClosure _	-> True
	TMask	KClosure _ _	-> True
	TVar	KClosure _	-> True
	TVarMore KClosure _ _	-> True
	TClass	KClosure _	-> True
	TFree{}			-> True
	TDanger{}		-> True
	TTop	KClosure 	-> True
	TBot	KClosure 	-> True
	TWild	KClosure	-> True

	TFetters t1 _		-> isClosure t1

	
	_			-> False
