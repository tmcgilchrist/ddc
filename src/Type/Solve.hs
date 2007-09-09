
module Type.Solve
	( squidSolve )

where

-----
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import qualified Util.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Data.Array.IO

import Util
import Util.Graph.Deps

import System.IO

import Shared.Error
import qualified Shared.Var	as Var

import qualified Main.Arg	as Arg
import Main.Arg			(Arg)

import Constraint.Exp
import Constraint.Pretty
import Constraint.Bits

import Type.Exp
import Type.Pretty
import Type.Util
import Type.Error
import Type.Plate.Update
import Type.Plate.Collect

import Type.State
import Type.Class
import Type.Scheme
import Type.Grind		(solveGrind)
import Type.Finalise
import Type.Feed
import Type.Trace

import Type.Check.CheckPure
import Type.Check.CheckConst

-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Solve"

instance Pretty a => Pretty (Set a) where
 prettyp ss	= "{" % ", " %!% Set.toList ss % "}"



-----
squidSolve 	
	:: [Arg]
	-> [CTree] 
	-> Map Var Var
	-> (Maybe Handle)
	-> IO SquidS

squidSolve 
	args 
	ctree 
	sigmaTable
	mTrace
 = do
	state1		<- squidSInit
 	let state2	= state1
			{ stateTrace		= mTrace
			, stateSigmaTable	= sigmaTable 
			, stateArgs		= Set.fromList args }
		
	state'		<- execStateT (solve args ctree)
			$ state2

	return state'
	   

-----
solve 	:: [Arg] 
	-> [CTree]
	-> SquidM ()

solve	args ctree	
 = do
	-- Slurp out the branch containment tree
	let treeContains	= Map.unions $ map slurpContains ctree
	modify (\s -> s { stateContains = treeContains })

	solveCs ctree

	-- Do a final grind to make sure the graph is up to date.
	solveGrind
	
	-- Check which branches have been fed but not yet generalised
	sGenSusp		<- gets stateGenSusp
	let sGeneraliseMe 	= Set.toList sGenSusp

	trace	$ "\n=== solve: Generalising left over types.\n"
		% "    sGeneraliseMe   = " % sGeneraliseMe % "\n"
	
	mapM_ solveGeneralise $ sGeneraliseMe
	
	
	return ()
 			
-----
solveCs :: [CTree] 
	-> SquidM Bool

solveCs []	
 = 	return True

solveCs	(c:cs)
 = case c of

	-- def
	CDef src t1@(TVar k vDef) t
 	 -> do	trace	$ "### Def  " % padR 20 (pretty vDef) % " = " % t % "\n"
		feedConstraint c

		-- Record that this type is good to go.
		modify (\s -> s 
			{ stateGenDone	= Set.insert vDef (stateGenDone s) })

		solveNext cs

	-- data fields
	CDataFields src v vs fs
	 -> do	trace	$ "### DataFields " % v % " " % vs % "\n"
		sDataFields	<##> Map.insert v (vs, fs)
		solveNext cs
		

	-----
	CBranch{}
	 -> do	traceIE
	 	trace	$ "\n### Branch" % "\n"

		-- record that we've entered this branch
		--	Don't add BGroups to the path, they help with working out the environmet only.
		let bind	= branchBind c
		pathEnter bind
			
		solveNext (branchSub c ++ [CLeave bind] ++ cs)


	-- A Leave token.
	--	This tells us that we've processed all the constraints from this branch.

	CLeave vs
	 -> do	trace	$ "\n### CLeave " % vs % "\n"
	 	path	<- gets statePath
		trace	$ "    path = " % path 	% "\n"
			 	
		-- We're leaving the branch, so pop ourselves off the path.
	 	pathLeave vs
		traceIL
	
		solveNext cs	 

	-- Equality Constraint
	CEq src t1 t2
 	 -> do	trace	$ "### CEq  " % padR 20 (pretty t1) % " = " % t2 % "\n"
		feedConstraint c
		solveNext cs
	
	CEqs src ts
	 -> do	trace	$ "### CEqs " % ts % "\n"
	 	feedConstraint c
		solveNext cs

	-- Generalisation
	CGen src t1@(TVar k v1)
	 -> do	trace	$ "### CGen  " % t1 %  "\n"
	 	modify (\s -> s { stateGenSusp
					= Set.insert v1 (stateGenSusp s) })
		solveNext cs

	-- Instantiation
	CInst{}		-> solveCInst cs c

	_ -> do
	 	trace $ "--- Ignoring constraint " % c % "\n"
		solveNext cs
	 	
solveNext cs
 = do 	err	<- gets stateErrors
	if isNil err
	 then 	solveCs cs
	 else	return False



-- Instantiate a type
--
solveCInst 	cs	c@(CInst src vUse vInst)
 = do
	path@(p:_)		<- gets statePath
	trace	$ "\n"
		% "### CInst " % vUse % " <- " % vInst					% "\n"
		% "    path          = " % path 					% "\n"

	-- Look at our current path to see what branch we want to instantiate was defined.
	sGenDone		<- gets stateGenDone
	let bindInst 
		-- var was imported, or already generalised.
		| Set.member vInst sGenDone
		= BLet [vInst]
		
		-- var was bound somewhere on our current path.
		| Just bind	<- find (\b -> elem vInst $ takeCBindVs b) path
		= case bind of
			BLambda _	-> BLambda [vInst]
			BDecon _	-> BDecon  [vInst]
			BLet _		-> BLet    [vInst]					
			BLetGroup _	-> BLet    [vInst]

	trace	$ "    bindInst      = " % bindInst					% "\n\n"
	
	-- Record the current branch depends on the one being instantiated
	-- 	Only record instances of Let bound vars, cause these are the ones we care
	--	about when doing the mutual-recusion check.
	--	For a Projection we'll add this after we work out what vInst should be
	graphInstantiatesAdd p bindInst

	sGenDone	<- gets stateGenDone

	solveCInst_simple cs c vUse vInst bindInst path sGenDone
	

-- These are the easy cases..

solveCInst_simple cs c vUse vInst bindInst path sGenDone

	-- IF   the var has already been generalised/defined 
	-- THEN then we can extract it straight from the graph.
	| Set.member vInst sGenDone
	= do	trace	$ prettyp "=== Scheme is in graph.\n"
		solveGrind
		tScheme	<- extractType vInst
		solveCInst_inst cs c tScheme
	
	-- If	The var we're trying to instantiate is on our path
	-- THEN	we're inside this branch.
	| elem vInst $ concat $ map takeCBindVs $ filter (\b -> not $ b =@= BLetGroup{}) path
	= do	trace	$ prettyp "=== Inside this branch\n"
		let t	= TVar KData vInst
		solveCInst_inst cs c t

	| otherwise
	= solveCInst_let cs c vUse vInst bindInst path
	

-- If we're not inside the branch defining it, it must have been defined 
--	somewhere at this level. Build dependency graph so we can work out if we're on a recursive loop.

solveCInst_let cs c vUse vInst bindInst path
 = do
	-- Load the info from the state that will help us work out
	--	what type to use for this binding.
	gContains	<- gets stateContains
	gInstantiates	<- gets stateInstantiates

	-- Restrict the instantiatesmap to just instantiations of let bindings.
	let gInstLet	= Map.map (Set.filter (\b -> b =@= BLet{})) gInstantiates

	-- The branch dependency graph graph is the union of the contains and instantiates graph
	let gDeps		=  Map.unionWith (Set.union) gContains gInstLet
	genSusp			<- gets stateGenSusp

	trace	$ "    gContains:\n" 	%> prettyBranchGraph gContains	% "\n\n"
		% "    gInstLet:\n" 	%> prettyBranchGraph gInstLet	% "\n\n"
		% "    gDeps:\n" 	%> prettyBranchGraph gDeps 	% "\n\n"
		% "    genSusp       = " % genSusp			% "\n\n"

	solveCInst_find cs c vUse vInst bindInst path gDeps genSusp
	

solveCInst_find cs c vUse vInst bindInst path gDeps genSusp
	
	-- If 	There is a suspended generalisation
	-- AND	we can reach the branch that we're in from the one we're trying to generalise
	-- THEN we're on a recursive loop and it's not safe to generalise.
 	| Set.member vInst genSusp
	, sDeps		<- graphReachableS gDeps (Set.singleton bindInst)
	, (p : _)	<- path
	, Set.member p sDeps
	= do 	trace	$ prettyp "=== Recursive use\n"

		let t	= TVar KData vInst
		solveCInst_inst cs c t
	
	
	-- IF	There is a suspended generalisation
	-- AND	it's not recursive
	-- THEN	generalise it and use that scheme for the instantiation
	| Set.member vInst genSusp
	= do	trace	$ prettyp "=== Generalisation\n"
		solveGrind
		tScheme	<- solveGeneralise vInst
		solveCInst_inst cs c tScheme
		
		
	-- The type we're trying to generalise is nowhere to be found. The branch for it
	--	might be later on in the constraint list, but we need it now.
	-- 	Reorder the constraints to process that branch first before
	--	we try the instantiation again.
	| otherwise
	= do	trace	$ "=== Reorder.\n"
			% "    queue =\n" %> (", " %!% map prettyCTreeS (c:cs)) % "\n\n"
	
		let floatBranch prev cc
			= case cc of
				(c@(CBranch { branchBind = BLet [vT] }) : cs)
				 | vT == vInst
				 -> c : (prev ++ cs)
				 
				(c : cs) 
				 -> floatBranch (prev ++ [c]) cs
				 
				[] -> panic stage
				 	$ "floatBranch: can't find branch for " % vInst % "\n"
					
		-- Reorder the constraints so the required branch is at the front
		let csReordered	= floatBranch [] (c:cs)
	
		trace	$ "    queue' =\n" %> (", " %!% map prettyCTreeS csReordered) % "\n\n"
	
		-- Carry on solving
		solveCs csReordered

	 
solveCInst_inst cs c@(CInst src vT vDef) tScheme
 = do
 	trace	$ "\n"
		% "=== solveCInst_scheme " % vT % " <- " % vDef 	% "\n"
		% "    scheme = " % tScheme				% "\n"

	-- Instantiate the scheme and record how it was instantiated.
	(tInst, tInstVs)	
		<- instantiateT_table instVar tScheme

	sInst <##> Map.insert vT tInstVs

	trace	$ "    instT  = " % tInst		% "\n"
		% "    instVs = " % tInstVs		% "\n"
		% "\n"

	-- Add type to the graph as a new constraint
	solveCs [CEq src (TVar KData vT) tInst]
	solveNext cs




-- | Extract and generalise the type scheme for this var.
--	The var should be present in the map of suspended generalisations

solveGeneralise ::	Var -> SquidM Type
solveGeneralise	vGen
 = do
	trace	$ "\n=============================================================\n"
		% "=== Generalise " % vGen % "\n"

	-- Work out the tyvars of all the bindings in this ones environment.
	--	We'll assume things are scoped properly, so the environment
	--	is just all the vars instantiated \\ all the vars bound.
	gContains	<- gets stateContains
	gInstantiated	<- gets stateInstantiates
	
	let bsBound	= Set.toList $ graphReachableS gContains (Set.singleton (BLet [vGen]))
	let vsBound	= catMap takeCBindVs bsBound
	
	-- all the vars instantiated by the contained branches
	let bsInst	= Set.toList
			$ Set.unions
			$ map (\b -> fromMaybe Set.empty $ Map.lookup b gInstantiated)
			$ bsBound
	
	let vsInst	= catMap takeCBindVs bsInst
	
	let vsEnv	= vsInst \\ vsBound
				
	trace	$ "    vsBound    = " % vsBound		% "\n"
		% "    vsInst     = " % vsInst		% "\n"
		% "    vsEnv      = " % vsEnv		% "\n"
	
	-- Extract the types present in the environment.
	--	No need to worry about generalisation here, if a var was in the environment
	--	then it was instantiated, so generalisation was already forced.
	tsEnv		<- mapM extractType vsEnv

	-- Collect up the cids free in the types in the environment.
	let cidsEnv	= nub $ catMap collectClassIds tsEnv

	trace	$ "    cidsEnv    = " % cidsEnv		% "\n"

	-- Extract the type from the graph.
	tGraph		<- extractType vGen

	-- Generalise the type into a scheme.
	tScheme		<- generaliseType vGen tGraph cidsEnv
	
	-- Add the scheme back to the graph.
	cidGen		<- makeClassV TSNil KData vGen 
	Just cls	<- lookupClass cidGen
	case tScheme of 
	 TClass{}	-> return ()
	 _		-> updateClass cidGen	
				cls { classType = tScheme }

	-- Record that this type has been generalised, and delete the suspended generalisation
	modify (\s -> s 
		{ stateGenDone	= Set.insert vGen (stateGenDone s) 
		, stateGenSusp	= Set.delete vGen (stateGenSusp s) })

	trace	$ "=== Generalise " % vGen % " done\n"
		% "=============================================================\n"
	
	return tScheme

-- | Work out which types are in the environmen of this branch
--	This makes use of the contains and instantiates maps from the state

traceEnvironment :: Var -> SquidM [Var]
traceEnvironment var
 = do	gContains	<- gets stateContains
 	gInstantiates	<- gets stateInstantiates

	-- Work out the names of the branches contained in this one
--	let branches	= graphReachableS gContains (Set.singleton [var])
	let branches	= []

	-- Collect the vars instantiated by all the 
	
	trace		$ "=== traceEnvironment " % var % "\n"
--			% "    branches = " % branches	% "\n"
	
	return []

	

prettyCTreeS :: CTree -> PrettyP
prettyCTreeS xx
 = case xx of
 	CBranch{} 
	 -> "\nBranch " 
	 	% branchBind xx 
			% " {" %> (", " %!% map prettyCTreeS (branchSub xx)) % "}"

--	CGen ts v env
--	 -> "(Gen " % v % ")"

	CLeave v
	 -> "(Leave " % v % ")"

	CInst ts vI vD
	 -> "(Inst " % vI % " " % vD % ")"

	_	  -> prettyp "X"
	







-- | Push a new var on the path queue.
--	This records the fact that we've entered a branch.

pathEnter :: CBind -> SquidM ()
pathEnter BNil	= return ()
pathEnter v
 = modify (\s -> s { statePath = v : statePath s })


-- | Pop a var off the path queue
--	This records the fact that we've left the branch.

pathLeave :: CBind -> SquidM ()
pathLeave BNil	= return ()
pathLeave v
 = do	path	<- gets statePath
 	
	case path of 
	 (v' : vs)	
	   | v' == v
	   -> modify (\s -> s { statePath = vs })

	 _ -> panic stage
	 	$ "pathLeave: can't leave " % v % "\n"
		% "  path = " % path % "\n"
		
-- | Add to the who instantiates who list
graphInstantiatesAdd :: CBind -> CBind -> SquidM ()
graphInstantiatesAdd    vBranch vInst
 = modify (\s -> s {
 	stateInstantiates
		= Map.adjustWithDefault 
			(Set.insert vInst) 
			Set.empty
			vBranch
			(stateInstantiates s) })

-- | Pretty print a branch graph	
-- prettyBranchGraph :: Map [Var] (Set [Var]) -> PrettyP
prettyBranchGraph graph
	= "\n" %!% ls
	where 	ls	= map (\(v, set)	
				-> (padR 16 $ pretty v) % " -> " % set)
			$ Map.toList graph



