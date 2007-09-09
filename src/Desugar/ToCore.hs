module Desugar.ToCore
(
	toCoreTree,
	toCoreP,
	toCoreX
)

where

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug
import Util

-----

import Shared.Var		(Var, VarBind, NameSpace(..))
import qualified Shared.Var 	as Var

import Shared.VarPrim
import Shared.Pretty
import Shared.Error
import qualified Shared.Exp	as S
import qualified Shared.Base	as S
import qualified Shared.Literal	as S

import qualified Desugar.Exp 	as D
import qualified Desugar.Pretty	as D
import qualified Desugar.Util	as D
import Desugar.Project		(ProjTable)

import qualified Type.Exp	as T
import qualified Type.Pretty	as T
import qualified Type.Util	as T
import Type.ToCore		(toCoreT, toCoreK)

import qualified Core.Exp 		as C
import qualified Core.Util		as C
import qualified Core.Pretty		as C
import qualified Core.Pack		as C
import qualified Core.Optimise.Boxing	as C	(unboxedType)
import qualified Core.Plate.FreeVars	as C
import qualified Core.Util.Mask		as C

import Desugar.ToCore.Base
import Desugar.ToCore.Util
import Desugar.ToCore.Lambda

import qualified Debug.Trace	as Debug

-----
stage	= "Desugar.ToCore"

debug		= False
trace ss x	= if debug 
			then Debug.trace (pretty ss) x
			else x

-----------------------
toCoreTree
	:: (Map Var Var)
	-> (Map Var T.Type)		-- typeSchemes
	-> (Map Var [T.Type])		-- typeInst
	-> ProjTable
	-> D.Tree Annot
	-> C.Tree

toCoreTree	
	sigmaTable
	typeTable
	typeInst
	projTable
	sTree

 = 	cTree			
 where
	initCoreS'
		= initCoreS
		{ coreSigmaTable	= sigmaTable
		, coreMapTypes		= typeTable
		, coreMapInst		= typeInst
		, coreProject		= projTable }
		
	mTree	= evalState 
			(toCoreTreeM sTree) 
			initCoreS'

	cTree	= mTree

toCoreTreeM tree
	= liftM concat
	$ mapM toCoreP tree
	

flattenEs e
 = case e of
 	C.TNil			-> []
	C.TSum C.KEffect es	-> es
	e			-> [e]
	


-----------------------
toCoreP	:: D.Top Annot	
	-> CoreM [C.Top]

toCoreP	p
 = case p of
	D.PNil
	 ->	return []

	D.PExtern _ v tv (Just to)
	 -> do
		let tv'	= toCoreT tv
		let to'	= toCoreT to
		
		return	$ [C.PExtern v tv' to']

	D.PData _ v vs ctors
	 -> do
		ctors'		<- mapM toCoreCtorDef ctors

		let d		= C.PData v vs ctors'
		let cs		= map (makeCtor primTObj v vs) ctors
		return (d:cs)

	D.PEffect _ v k
	 -> do
		let k'	= toCoreK k
		let p	= C.PEffect v k'
	 	return	[p]

	D.PBind nn (Just v) x
	 -> do	Just (C.SBind (Just v) x') 
			<- toCoreS (D.SBind nn (Just v) x)

		return	[C.PBind v x']

	D.PSig{}	-> return []
	D.PImport{}	-> return []


	-- classes
	D.PRegion _ v	
	 -> 	return	[ C.PRegion v ]


	D.PClass _ v k
	 -> 	return	[ C.PClass v (toCoreK k)]

	D.PClassDict _ v cts context sigs
	 -> do	let (vs, ts)	= unzip sigs
	 	let ts'		= map toCoreT ts
		let sigs'	= zip vs ts'

		let cts'	= map toCoreT cts

		return		$ [C.PClassDict v cts' [] sigs']

	D.PClassInst _ v cts context defs
	 -> do
		defs'		<- mapM (\(v, x)
					-> case x of
						D.XVarInst _ v2	-> return (v, C.XVar v2)
						D.XVar     _ v2 -> return (v, C.XVar v2))
					$ defs

		let cts'	= map toCoreT cts
		return	$ [C.PClassInst v cts' [] defs']

	-- projections
	D.PProjDict{}	-> return []

	_ -> panic stage
		$ "toCoreP: no match for " % show p % "\n"
	



-----
toCoreCtorDef	
	:: D.CtorDef Annot
	-> CoreM C.CtorDef
		
toCoreCtorDef	(D.CtorDef _ v fieldDefs)
 = do 	fieldDefs'	<- mapM toCoreFieldDef fieldDefs
	return	$ C.CtorDef v fieldDefs'

toCoreFieldDef	df
 = do
	cInit	<- case S.dInit df of
		    Just x 
		     -> do
		    	x'	<- toCoreX x
			return	$ Just (C.XDo [C.SBind Nothing x'])

		    Nothing -> return Nothing

	return	$ S.DataField 
		{ S.dPrimary	= S.dPrimary df
		, S.dLabel	= S.dLabel   df
		, S.dType	= toCoreT $ S.dType df
		, S.dInit	= cInit }
		

makeCtor 
	:: Var 
	-> Var 
	-> [Var] 
	-> D.CtorDef Annot
	-> C.Top

makeCtor    objVar dataVar ts (D.CtorDef _ ctorVar dataFields)
 = let
	argTs	= map toCoreT
		$ map S.dType
		$ filter (\df -> S.dPrimary df) 
		$ dataFields

	tv	= C.makeCtorTypeAVT argTs dataVar ts 

	to	= C.unflattenFun (replicate (length argTs + 1) 
		$ (C.TData objVar []))

   in   C.PCtor ctorVar tv to


-----------------------
-- Stmt
--
toCoreS	:: D.Stmt Annot	
	-> CoreM (Maybe C.Stmt)
		
toCoreS (D.SBind _ Nothing x)
 = do	
 	x'		<- toCoreX x
	returnJ	$ C.SBind Nothing x'


toCoreS (D.SBind _ (Just v) x) 
 = do	xCore		<- toCoreX x

	tScheme		<- getType v
	xLam		<- fillLambdas v tScheme xCore
	returnJ 	$ C.SBind (Just v) xLam


toCoreS	D.SSig{}
 	= return Nothing


-----------------------
-- Exp
--
toCoreX	:: D.Exp Annot -> CoreM C.Exp
toCoreX xx
 = case xx of

	D.XLambdaTEC 
		_ v x (T.TVar T.KData vTV) eff clo
	 -> do	
		vT	<- getType vTV
		x'	<- toCoreX x
		
		return	$ C.XLam v C.TNil x' C.TNil C.TNil


	D.XApp	_ x1 x2
	 -> do
	 	x1'	<- toCoreX x1
		x2'	<- toCoreX x2
		return	$ C.XApp x1' x2' C.TPure


	-- case match on a var
	D.XMatch _ (Just x@(D.XVar _ varX)) alts
	 -> do
		alts'	<- mapM (toCoreA (Just varX)) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts' C.TNil) ]

	-- case match on an exp
	D.XMatch _ (Just x) alts
	 -> do
		x'	<- toCoreX x
		varX	<- newVarN NameValue
		
		alts'	<- mapM (toCoreA (Just varX)) alts
		
		return	$ C.XDo	[ C.SBind (Just varX) x'
				, C.SBind Nothing (C.XMatch alts' C.TNil) ]
			
	-- regular  match
	D.XMatch _ Nothing alts
	 -> do	alts'	<- mapM (toCoreA Nothing) alts
		
		return	$ C.XDo	[ C.SBind Nothing (C.XMatch alts' C.TNil) ]
		
	D.XConst (Just (T.TVar T.KData vT, _)) 
		(S.CConst lit)

	 -> do
	 	t		<- getType vT
	 	case C.unboxedType t of
		 Just tU@(C.TData _ [tR])
		  -> return 	$ C.XPrim (C.MBox t tU) [C.XConst (S.CConstU lit) tU] (C.TEffect primRead [tR])
			
		 Nothing
		  -> panic stage
		  	$ "toCoreX: no unboxed version for type " % t

	D.XConst
		(Just (T.TVar T.KData vT, _))
		(S.CConstU lit)
	 -> do	
	 	t	<- getType vT
		return	$  C.XConst (S.CConstU lit) t

 
	D.XDo 	_ stmts
	 -> do	stmts'	<- liftM catMaybes
	 		$  mapM toCoreS stmts

		return	$ C.XDo stmts'
			
			
	D.XIfThenElse _ e1 e2 e3
	 -> do
		v	<- newVarN NameValue
		e1'	<- toCoreX e1
		e2'	<- toCoreA (Just v) (D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primTrue  [])] e2)
		e3'	<- toCoreA (Just v) (D.AAlt Nothing [D.GCase Nothing (D.WConLabel Nothing primFalse [])] e3)
		
		return	$ C.XDo	[ C.SBind (Just v) e1'
				, C.SBind Nothing (C.XMatch [ e2', e3' ] C.TNil) ]
	
{-	
	D.XProj	(Just (T.TVar T.KData vT, T.TVar T.KEffect vE))
		x j
	 -> do
	 	(x', xT) 	<- toCoreX x
		jT		<- getType vT

		let (C.TData xV _)	= xT

		case j of
		 D.JField _ v	-> return 
		 			C.XPrim (C.MFun primProjField  jT) [C.XVar xV, C.XVar v, x'] C.TNil
					, jT)
		
		 D.JFieldR _ v	-> return
		 			( C.XPrim (C.MFun primProjFieldR jT) [C.XVar xV, C.XVar v, x'] C.TNil
					, jT)


	D.XProjTagged 
		(Just 	( T.TVar T.KData vT
			, T.TVar T.KEffect vE))
		vTag x j
	 -> do
		(x', xT@(C.TData vCon _))	
			<- toCoreX x

		resultT			<- getType vT
		mapProj			<- gets coreProject
	
		let (Just [(t', implList)])	
			= Map.lookup vCon mapProj

		let projName		= case j of
						D.JField  _ v	-> Var.name v
						D.JFieldR _ v	-> "ref_" ++ Var.name v


		let mVarImpl		= liftM snd
					$ find (\(v, _) -> Var.name v == projName)
					$ Map.toList implList

		let vImpl	
			= case mVarImpl of
				Just varImpl	-> varImpl
				Nothing		-> panic stage 
						$ "toCoreX: can't find projection implementation for " % j % "\n"
						% "   node         = " % xx 		% "\n"
						% "   object       = " % x		% "\n"
						% "   object type  = " % xT		% "\n"
						% "   projName     = " % projName	% "\n"
						% "   implList     = " % implList	% "\n"

		funX		<- addTypeApps vImpl vTag (C.XVar vImpl)

		return	( C.XApp funX x' C.TNil
			, resultT )
-}

{-
	D.XVar	(Just (T.TVar T.KData vT, _))
		v
	 -> do	
	 	t	<- getType vT
		return	( C.XVar v
	 		, t)

	D.XVar	Nothing v
	 -> do	return	( C.XVar v
	 		, C.TNil)
-}	
	D.XVarInst 
		(Just (T.TVar T.KData vT, _))
		v
	 -> do	
		t		<- getType v
	
		mapInst		<- gets coreMapInst
		let Just tsInst	= (Map.lookup vT mapInst) :: Maybe [T.Type]

		let tsInstC	=  map toCoreT tsInst

		trace	( "XVarInst " % v % "\n"
			% "    vT      = " % vT		% "\n"
			% "    tsInstC = " % tsInstC 	% "\n")
			
			
		 $ return	
		 	$ C.unflattenApps (C.XVar v : map C.XType tsInstC)
			


	_ 
	 -> panic stage
	 	$ pretty
		$ "toCoreX: cannot convert expression to core.\n" 
		% "    exp = " % show xx	% "\n"



-----------------------
-- Alt
--
toCoreA	:: Maybe Var
	-> D.Alt Annot -> CoreM C.Alt
		
toCoreA mObj alt
 = case alt of
	D.AAlt _ gs x
	 -> do	
	 	gs'	<- mapM (toCoreG mObj) gs
		x'	<- toCoreX x
		
		return	$ C.AAlt gs' x'
			

	

-----------------------
-- Guard
--
toCoreG :: Maybe Var
	-> D.Guard Annot
	-> CoreM C.Guard

toCoreG mObj gg
	| D.GCase _ w		<- gg
	, Just objV		<- mObj
	= do	w'		<- toCoreW w
	 	return		$ C.GExp w' (C.XVar objV)
		
	| D.GExp _ w x		<- gg
	= do	w'		<- toCoreW w
	 	x'		<- toCoreX x
		return		$ C.GExp w' x'
		

-----------------------
-- Pat
--
toCoreW :: D.Pat Annot
	-> CoreM C.Pat
	
toCoreW ww
 = case ww of
 	D.WConLabel _ v lvs
	 -> do 
	 	lvts	<- mapM toCoreA_LV lvs
	 	return	$ C.WCon v lvts
	 
	D.WConst _ c
	 -> return	$ C.WConst c
	 

toCoreA_LV (D.LIndex nn i, v)
 = do	t	<- getType v
	return	(C.LIndex i, v, t)

toCoreA_LV (D.LVar nn vField, v)
 = do	t	<- getType v
 	return	(C.LVar vField, v, t)

