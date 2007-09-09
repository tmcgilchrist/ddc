
module Source.Rename
	( Rename
	, rename_VT
	, renameData
	, renameTrees )

where

-----
import qualified Debug.Trace	as Debug
import Util

-----
import Shared.Error		(panic)
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace (..), Module(..))
import Shared.VarUtil		(isCtorName)

import qualified Debug.Trace	as Debug

import Source.Exp
import Source.Util
import Source.Horror
import Source.RenameM
import Source.Separate
import Source.Slurp

import Type.Util
import Type.Plate.Collect

-----
stage	= "Source.Rename"

-----
renameTrees
	:: [(Module, Tree)]
	-> RenameM [(Module, Tree)]

renameTrees 
	(mTree1: mTreeHs)
 = do
 	let mTree1'	= mTree1

	mTreeHs'	<- renameTrees' mTreeHs	
	[mTree1']	<- renameTrees' [mTree1']	
				
	return 	$ (mTree1' : mTreeHs')

renameTrees' mTrees
 = do
	-- Slurp out all the top-level names.
	let topNamesHs	= concat 
			$ map (\(m, tree) -> [(v, m) | v <- catMap slurpTopNames tree])
			$ mTrees
		
	-- Add them to the rename state.
	mapM_	(\(v, m) 
		  -> bindZ 
			v { Var.nameModule	= m })
		topNamesHs
		
	-- Now rename trees
	--	
	mTrees'		<- mapM (\(m, tree) 
				-> do	tree' <- renameTree m tree
					return (m, tree'))
				mTrees
				
	return mTrees'
 	

renameTree :: Module -> Tree -> RenameM Tree
renameTree m tree
 = do	
 	modify (\s -> s { stateCurrentModule = m })
	tree'	<- rename tree
	return tree'
	
-----
-- Rename Top
--
instance Rename Top where
 rename	top
  = case top of
	PPragma es
	 -> do 	return	$ PPragma es 	

	PModule m
	 -> do 	m'	<- rename m
		modify (\s -> s { stateCurrentModule = m' })
		return	$ PModule m'

 	PImportExtern	v tv to
	 -> do	v'	<- lookupV v
		tv'	<- local $ rename tv
		to'	<- local $ rename to
		return	$ PImportExtern v' tv' to'

	PImportModule	ms
	 -> do	ms'	<- rename ms
		return	$ PImportModule ms'

	PForeign f
	 -> do	f'	<- rename f
	 	return	$ PForeign f'

	PType	sp v t		
	 -> do 	v'	<- lookupV v
	 	t'	<- local $ rename t
		return	$ PType sp v' t'
		
	PInfix 	m i vs
	 -> do 	vs'	<- mapM lbindV vs
		return	$ PInfix m i vs'
	
	PData	v vs cs		
	 -> do	(v', vs', cs')	<- renameData v vs cs
		return	$ PData v' vs' cs' 
	
	PRegion v
	 -> do	v'	<- lookupR v
	 	return	$ PRegion v'
	
	PEffect	v k
	 -> do 	v'	<- lookupE v
		return	$ PEffect v' k

	PStmt	s
	 -> do	s'	<- rename s
		return	$ PStmt s'

	
	-- classes
	PClass v k
	 -> do 	v'	<- lookupN NameClass v
	 	return	$ PClass v' k

	PClassDict v vs inh sigs
	 -> do 	v'	<- lbindN NameClass v

		(vs', inh', sigs')
		 <- local
		 $ do	vs'	<- mapM lbindT vs
			inh'	<- mapM renameClassInh inh
			sigs'	<- mapM renameClassSig sigs
			return	(vs', inh', sigs')
	
		return	$ PClassDict v' vs' inh' sigs'

	PClassInst v ts inh stmts
	 -> do
	 	v'	<- lbindN NameClass v

		(ts', inh')
		 <- local
		 $ do	ts'	<- rename ts
			inh'	<- mapM renameInstInh inh
			return	(ts', inh')

		stmts'	<- rename stmts

		return	$ PClassInst v' ts' inh' stmts'			

	-- projections
	PProjDict t ss
	 -> do	
	 	-- The way the projection dict is parsed, the projection funtions end up in the wrong namespace, 
		--	NameValue. Convert them to NameField vars here.
		--	
	 	m	<- gets stateCurrentModule
		let fixupV v	= v 	{ Var.nameSpace 	= NameField
					, Var.nameModule 	= m }
		
		let ssF		= map (\s -> case s of 
	 				SBind sp (Just v) x	-> SBind  sp	(Just $ fixupV v) x
					SSig  sp v t		-> SSig   sp 	(fixupV v) t
					SBindPats sp v xs x	-> SBindPats sp	(fixupV v) xs x)
			$ ss
	 
	   	t' 	<- local
		 	 $ do	t'	<- rename t
				return	t'
	
		ss'	<- rename ssF
		return	$ PProjDict t' ss'
		

instance Rename Module where
 rename m	= return m
	
rename_VT ::	(Var, Type)	-> RenameM (Var, Type)
rename_VT	(v, t)
 = do
 	v'	<- bindV v

	pushTREC
	t'	<- rename t
	popTREC
	
	return	(v', t')


renameClassInh :: (Var, [Var])  -> RenameM (Var, [Var])
renameClassInh	  (v, vs)
 = do
 	v'	<- lbindN NameClass v
	vs'	<- mapM lbindT vs
	return	(v, vs)


renameClassSig :: ([Var], Type) -> RenameM ([Var], Type)
renameClassSig    (vs, t)
 = do
 	vs'	<- mapM lbindV vs

	pushTREC
	t'	<- rename t
	popTREC
	
	return	(vs', t')


renameInstInh :: (Var, [Type])	-> RenameM (Var, [Type])
renameInstInh    (v, ts)
 = do
 	v'	<- lookupN NameClass v
	ts'	<- rename ts
	return	(v', ts')

-----
-- Foreign
--
instance Rename Foreign where
 rename ff
  = case ff of
  	OImport f
	 -> do	f'	<- rename f	
	 	return	$ OImport f'
		
	OExport f
	 -> do	f'	<- rename f
	 	return	$ OExport f'
		
	OCCall mS v t
	 -> do	v'	<- lookupV v
	 	t'	<- rename t
		return	$ OCCall mS v' t'
		
	OExtern mS v tv to 
	 -> do	v'	<- lookupV v
	 	tv'	<- rename tv
		to'	<- rename to
		return	$ OExtern mS v' tv' to' 
		

	
renameData 	
	:: Var -> [Var] -> [(Var, [DataField Exp Type])]	
	-> RenameM (Var, [Var], [Ctor])

renameData v vs cs
 = do
 	v'	<- lookupT v

	(vs', cs')	
	 	<- local	
		  $ do 	vs'	<- mapM bindZ vs
			cs'	<- mapM renameCtor cs
			return	(vs', cs')
	
	return	(v', vs', cs')

renameCtor 
	::	(Var, [DataField Exp Type])	
	-> RenameM (Var, [DataField Exp Type])

renameCtor	(v, fs)
 = do
 	v'	<- lookupV v
	fs'	<- mapM rename fs
	return	(v', fs')


-----
-- DataField Exp Type
--
instance Rename (DataField Exp Type) where
 rename df
  = do
	mLabel'	<- case dLabel df of
			Nothing		-> return Nothing
			Just label
			 -> do
			 	label'	<- lbindF label
				return	$ Just label'

	t'	<- rename $ dType df
	mExp'	<- rename $ dInit df
	 	
	return 	df
		{ dLabel	= mLabel'
		, dType		= t'
		, dInit		= mExp' }
		
	
-----------------------
-- Exp
--
instance Rename Exp where 
 rename exp
  = case exp of

	-- core
	XConst sp c		
	 -> return exp

	XVar sp v		
	 -> do 	v'	<- lookupV v
		return	$ XVar sp v'


	XProj sp x proj
	 -> do 	x'	<- rename x
		proj'	<- rename proj
		return	$ XProj sp x' proj'

	XLambda sp v e	
	 -> do	pushVTREC
	 	v'	<- bindV v
		e'	<- rename e
		popVTREC

		return	$ XLambda sp v' e'

	XApp sp e1 e2	
	 -> do 	e1'	<- rename e1
		e2'	<- rename e2
		return	$ XApp sp e1' e2'

	XCase sp e1 cs	
	 -> do 	e1'	<- rename e1
		cs'	<- rename cs
		return	$ XCase sp e1' cs'

 	XLet sp ss e	
	 -> do 	pushVTREC
	 	ss'	<- renameSs ss
		e'	<- rename e
		popVTREC
		return	$ XLet sp ss' e'
		
	XDo sp ss 
	 -> do 	pushVTREC
	 	ss'	<- renameSs ss
		popVTREC

		return	$  XDo sp ss'

	XIfThenElse sp e1 e2 e3 
	 -> do 	e1'	<- rename e1
		e2'	<- rename e2
		e3'	<- rename e3
		return	$ XIfThenElse sp e1' e2' e3'
		
	-- oop
	XObjField sp v
	 -> do	objV	<- peekObjectVar
		v'	<- lbindF v
		return	$ XProj sp (XVar sp objV) (JField v')
		
	-- sugar
	XLambdaPats sp ps x
	 -> do	pushV
	 	ps'	<- mapM bindPatX ps
		x'	<- rename x
		popV
		return	$ XLambdaPats sp ps' x'

	XLambdaCase sp cs
	 -> do 	cs'	<- rename cs
		return	$ XLambdaCase sp cs'
		
	XLambdaProj sp j xs
	 -> do	j'	<- rename j
	 	xs'	<- rename xs
	 	return	$ XLambdaProj sp j' xs'
		
	-- defix sugar
	XOp sp v 
	 | Var.name v	== "@"
	 -> 	return	$ XOp sp v
	 
	 | otherwise
	  -> do	v'	<- lookupV v
	 	return	$ XOp sp v'

	XDefix sp es 
	 -> do 	es'	<- rename es
		return	$ XDefix sp es'

	XDefixApps sp es
	 -> do	es'	<- rename es
	 	return	$ XDefixApps sp es'
		
	XAppSusp sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XAppSusp sp x1' x2'
		
	-- match sugar
	XMatch sp aa
	 -> do	aa'	<- rename aa
	 	return	$ XMatch sp aa'
		
	-- exception sugar
	XTry sp x aa w
	 -> do 	x'	<- rename x
	 	aa'	<- rename aa
		w'	<- rename w
		return	$ XTry sp x' aa' w'

	XThrow sp x
	 -> do	x'	<- rename x
	 	return	$ XThrow sp x'

	-- imperative sugar
	XWhile sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XWhile sp x1' x2'
		
	XWhen sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XWhen sp x1' x2'
		
	XUnless sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XUnless sp x1' x2'

	XBreak sp
	 ->	return	exp

	-- list sugar
	XListRange sp b x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XListRange sp b x1' x2'

	XListComp sp x qs
	 -> do	qs'	<- renameLCQuals qs

		let vs	= concat $ map boundByLCQual qs'

		pushV
		addN NameValue vs
		x'	<- rename x
		popV

		return	$ XListComp sp x' qs'

	-- patterns
	XCon sp v xx
	 -> do	v'	<- lookupV v
	 	xx'	<- rename xx
		return	$ XCon sp v' xx'
		
	XTuple sp xx
	 -> do	xx'	<- rename xx
	 	return	$ XTuple sp xx'
		
	XCons sp x1 x2
	 -> do	x1'	<- rename x1
	 	x2'	<- rename x2
		return	$ XCons sp x1' x2'
		
	XList sp xx
	 -> do	xx'	<- rename xx
	 	return	$ XList sp xx'
		

	_ -> panic stage
		$ "rename: cannot rename " % show exp

		
instance Rename Proj where
 rename jj 
  = case jj of
	JField v
	 -> do	v'	<- lbindF v
		return	$ JField v'
		
	JFieldR v
	 -> do	v'	<- lbindF v
	 	return	$ JFieldR v'

	JIndex x
	 -> do	x'	<- rename x
	 	return	$ JIndex x'
		
	JIndexR x
	 -> do	x'	<- rename x
	 	return	$ JIndexR x'
		

instance Rename Alt where
 rename a
  = case a of
	APat p x2
	 -> do	(p', x2')
	 	 	<- local
		  	$ do	p'	<- bindPat p
			 	x2'	<- rename x2
				return (p', x2')

		return	$ APat p' x2'

	AAlt gs x
	 -> do	(gs', x')
	 		<- local
			$ do	gs'	<- mapM bindGuard gs
				x'	<- rename x
				return	(gs', x')
				
		return	$ AAlt gs' x'

	ADefault x
	 -> do	x'	<- rename x
	 	return	$  ADefault x'

instance Rename Label where
 rename ll
  = case ll of
  	LIndex i	-> return ll

	LVar v 
	 -> do	v'	<- lbindF v
	 	return	$  LVar v'
	 

-- bindPatX
--	Bind all the variables in this pattern.
--
bindPatX :: Exp -> RenameM Exp
bindPatX xx
 = case xx of
 	XVar sp v		
	 | Var.isCtorName v
	 -> do	v'	<- lookupV v
	 	return	$ XVar sp v'
		
	 | otherwise
	 -> do	v'	<- bindV v
	 	return	$ XVar sp v'
		
	XList sp xx
	 -> do	xx'	<- mapM bindPatX xx
	 	return	$ XList sp xx'	

	XCons sp x1 x2
	 -> do	x1'	<- bindPatX x1
	 	x2'	<- bindPatX x2
		return	$ XCons sp x1' x2'
		
	XTuple sp xs
	 -> do	xs'	<- mapM bindPatX xs
	 	return	$ XTuple sp xs'

	XApp{} 
	 -> do	let (x:xs)	=  flattenApps xx
	 	x'		<- rename x
		xs'		<- mapM bindPatX xs
		return		$ unflattenApps (x':xs')

	XDefix sp xs
	 -> do	xs'	<- mapM bindPatX xs
	 	return	$ XDefix sp xs'
	 
	XOp sp v
	 -> do	--v'		<- lookupV v
	 	return	$ XOp sp v

	XConst{}
	 ->	return	$ xx

	_ 	-> panic stage
		$ "bindPatX: no match for " % show xx	% "\n"
	
bindGuard gg
 = case gg of
 	GCase pat
	 -> do	pat'	<- bindPat pat
	 	return	$ GCase pat'
		
	GExp  pat x
	 -> do	x'	<- rename x	
	 	pat'	<- bindPat pat
		return	$ GExp pat' x'
		
	GBool x
	 -> do	x'	<- rename x
	 	return	$ GBool x'
		
	GBoolU x
	 -> do	x'	<- rename x
	 	return	$ GBoolU x'
		
bindPat ww
 = case ww of
 	WVar v
	 -> do	v'	<- bindV v
	 	return	$ WVar v'
 
 	WExp x	
	 -> do	x'	<- bindPatX x
	 	return	$ WExp x'

	WConLabel v lvs 
	 -> do	v'		<- lookupV v

	 	let (ls, vs)	= unzip lvs
		ls'		<- rename ls
		vs'		<- mapM bindPat vs
		let lvs'	= zip ls' vs'
		
		return		$ WConLabel v' lvs'
		
	_	-> panic stage
		$ "bindPat: no match for " % show ww % "\n"
	 

-----------------------
-- renameLCQuals
--	Rename some list comprehension qualifiers.
--	In a sequence of qualifiers, the vars bound in a qualifier
--	are in-scope for subsequent qualifiers.
--
renameLCQuals :: [LCQual] -> RenameM [LCQual]
renameLCQuals qq
 = case qq of
  	[]			
	 -> 	return []

	(LCGen b (XVar sp v) x2 : qs)
	 -> do	x2'	<- rename x2
	 	
		pushV
		v'	<- bindV v
		qs'	<- renameLCQuals qs
		popV
		
		return	$ (LCGen b (XVar sp v') x2' : qs')
		
	(LCExp x : qs)
	 -> do	x'	<- rename x
	 	qs'	<- renameLCQuals qs
		return	$ LCExp x' : qs'
		

boundByLCQual :: LCQual -> [Var]
boundByLCQual    q
 = case q of
 	LCGen _ (XVar sp v) _	-> [v]
	LCExp{}			-> []


-----------------------
-- renameS
--
renameSs ::	[Stmt]	-> RenameM [Stmt]
renameSs	ss
 = do
	-- SBinds are recursive, so grab their names
	--	and add them to the env.
 	let mBound	= map takeStmtBoundV ss
	mapM lbindV $ catMaybes mBound
	
	-- Rename each statement.
	ss'	<- mapM rename ss
	return	ss'
		

instance Rename Stmt where
 rename s
  = case s of
	SBindPats sp v ps x
	 -> do	v'	<- lbindZ v

	 	pushV
		(ps', objVss)	<- liftM unzip
				$  mapM renamePat ps

		let objVs	= concat objVss

		(case objVs of
		  []	-> return ()
		  [v]	-> pushObjectVar v)

		x'		<- rename x

		(case objVs of
		  []	-> return ()
		  [v]	-> do { popObjectVar; return () })

		popV

		return	$ SBindPats sp v' ps' x'
	 	
	SBind sp mV x
	 -> do	mV'	<- liftMaybe lbindZ mV
		x'	<- local $ rename x
		return	$ SBind sp mV x'		
				

	SSig sp v t
	 -> do	v'	<- lbindZ v
		t'	<- local $ rename t
		return	$ SSig sp v' t'
		

		


-----------------------
-- renamePat
-- |	Rename a pattern. 
--
renamePat 
	:: Exp			-- expression to rename
	-> RenameM 
		( Exp		-- renamed expression
		, [Var])	-- bound object vars

renamePat	e
 = case e of
 	XApp sp e1 e2
	 -> do
	 	(e1', objVs1)	<- renamePat e1
		(e2', objVs2)	<- renamePat e2

		return	( XApp sp e1' e2'
			, objVs1 ++ objVs2)

	XTuple sp xs
	 -> do	(xs', vss)	<- liftM unzip
	 			$  mapM renamePat xs
				
		return	(XTuple sp xs', concat vss)
		
	XConst sp c
	 -> 	return	( XConst sp c, [])
	 
	XVar sp v
	 -> if Var.isCtorName v

		-- a constructor pattern
	 	then do
			v'	<- lookupV v
			return	(XVar sp v', [])

		-- a bound variable
		else do
			v'	<- bindV v
			return	(XVar sp v', [])

	XObjVar sp v
	 ->  do
	 	v'	<- bindV v
		return	( XVar sp v'
			, [v'])

	XDefix sp xx
	 -> do	(xx', vs)	<- renamePats [] [] xx
	 	return	( XDefix sp xx'
			, vs)
	
	XOp sp v
	 -> 	return	( XOp sp v
	 		, [])


	_ -> panic stage
		$ "renamePat: no match for " % show e

renamePats bound done []	
 = 	return (done, bound)
 
renamePats bound done (x:xs)
 = do	(x', boundHere)	<- renamePat x
 	renamePats (bound ++ boundHere) (done ++ [x']) xs
	


-----------------------
-- renameT
--
instance Rename Type where
 rename tt
  = case tt of
	TForall vs t
	 -> do 	(vs', t')	<- local 
		 $  do	vs'	<- mapMt2_1 bindZ vs
			t'	<- rename t
			return	(vs', t')

		return	$ TForall vs' t'

	TFetters fs t
	 -> do 	fs'	<- rename fs
		t'	<- rename t
		return	$ TFetters fs' t'

	TVar k v 	
	 -> do 	v'	<- lookupN (spaceOfKind k) v
		return	$ TVar k v'

	TSum k ts
	 -> do	ts'	<- rename ts
	 	return	$ TSum k ts'

	TTop k 
	 -> 	return tt
	 
	TBot k
	 ->	return tt


	-- data
 	TFun t1 t2 eff clo
	 -> do 	t1'	<- rename t1
		t2'	<- rename t2
		eff'	<- rename eff
		clo'	<- rename clo
		return	$ TFun t1' t2' eff' clo'

	TData v ts		
	 -> do 	v'	<- lookupT v
		ts'	<- mapM rename ts
		return	$ TData v' ts'
	
	
	-- effect
	TEffect v rs
	 -> do 	v'	<- lookupE v
		rs'	<- rename rs
		return	$ TEffect v' rs'
		
	
	-- closure
	TFree v t
	 -> do	pushTREC
	 	t'	<- rename t
		popTREC

	 	return	$ TFree v t'
	

	-- wildcards
	TWild k
	 -> 	return	$ TWild k



	-- 
	TElaborate t
	 -> do	t'	<- rename t
	 	return	$ TElaborate t'
		
	TMutable t
	 -> do	t'	<- rename t
	 	return	$ TMutable t'

	TFunV t1 t2 mLabel
	 -> do 	t1'	<- rename t1
		t2'	<- rename t2
		return	$ TFunV t1' t2' mLabel

	TSigExact t
	 -> do	t'	<- rename t
	 	return	$ TSigExact t'

	TSig t
	 -> do	t'	<- rename t	
	 	return	$ TSig t'


	-- Auto-quantify any vars which are free in the type.
	--
	TQuant t
	 -> do	let sameNameV v1 v2
	 		= Var.name v1 == Var.name v2
			
		let sameNameVK (v1, _) (v2, _)
			= Var.name v1 == Var.name v2
			
	 	let vs	= nubF sameNameV
	 		$ filter (not . isCtorName)
			$ slurpVarsT t

		let ks	= map kindOfSpace
			$ map Var.nameSpace vs
		
		let vks' = zip vs ks
		
		let tQ	= case t of
				TForall vks x 	-> TForall (nubF sameNameVK $ vks ++ vks') x
				_		-> case vks' of
							[]	-> t
							_	-> TForall vks' t
		
		t'	<- rename tQ
		return	t'
		
		

-----
instance Rename Fetter where
 rename f
  = case f of
	FConstraint v ts
	 -> do 	v'	<- lookupN NameClass v
		ts'	<- rename ts
		return	$ FConstraint v' ts'

  	FLet t1 t2
	 -> do 	t1'	<- rename t1
		t2'	<- rename t2
		return	$ FLet t1' t2'

	FFunInfo l e n
	 -> do	e'	<- rename e
	 	n'	<- rename n
		return	$  FFunInfo l e' n'

	

-----		
lookupZ :: 	Var -> RenameM Var
lookupZ	v
 = case Var.nameSpace v of
	NameValue	-> lookupV v
	NameType	-> lookupT v
 	NameRegion	-> lookupR v
	NameEffect	-> lookupE v		
	NameClosure	-> lookupC v

lbindZ ::	Var -> RenameM Var
lbindZ v
 = case Var.nameSpace v of
 	NameValue	-> lbindV v
	NameType	-> lbindT v
	NameRegion	-> lbindR v
	NameEffect	-> lbindE v
	NameClosure	-> lbindC v
	NameField	-> lbindN NameField v
	_		-> panic stage
			$  "lbindZ: no match for " % Var.nameSpace v % "\n"

bindZ :: 	Var -> RenameM Var
bindZ	 	v 	= bindN (Var.nameSpace v) v

-----
slurpVarsT :: 	Type -> [Var]
slurpVarsT	t
 = case t of
	TForall  vks t		-> collectVarsT t \\ map fst vks
	_			-> collectVarsT t
	


