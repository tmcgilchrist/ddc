
module Core.Graph
	( slurpAppGraph
	, dotAppGraph)
where
import Core.Exp
import Core.Util
import Core.Plate.Trans
import Core.Plate.FreeVars
import Type.Util
import Type.Exp
import Type.Builtin
import Util
import DDC.Main.Pretty
import DDC.Var
import qualified Shared.VarUtil	as Var
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-----
data GraphS
	= GraphS
	{ stateTopVs	:: Set Var
	, stateGraph	:: Map Var (App, [Var]) }
	
type GraphM
	= State GraphS 
	

data App
	= AppPure	
	| AppConst
	| AppEffect Effect
	| AppOther
	deriving (Show, Eq)
	
	
slurpAppGraph 
	:: Tree	
	-> Tree
	-> Map Var (App, [Var])

slurpAppGraph	
	cTree
	cHeader

 = let	tree		= cTree ++ cHeader

 	externVs	= [v	| PExtern v tv to 	<- tree]
	topVs		= [v	| PBind	  v x		<- tree]
  
 	state'	= execState
	 		(transZM transTableId { transS	= slurpS }
				cTree)

			GraphS 
			{ stateTopVs	= Set.fromList (externVs ++ topVs)
			, stateGraph	= Map.empty }

   in	stateGraph state'


slurpS :: Stmt -> GraphM Stmt
slurpS ss
 = case ss of
	SBind (Just v) x	
	 -> do	topVs	<- gets stateTopVs

	 	let fv	= [v 	| v 	<- Set.toList $ freeVars x
	 			, varNameSpace v == NameValue 
				, not $ Set.member v topVs 
				, not $ Var.isCtorName v ]
	 
	 	modify (\s -> s
	 		{ stateGraph	= Map.insert v 
						(appModeX x, fv)
						(stateGraph s) })
		
		return ss
		
	_ -> return ss		
	

appModeX ::	Exp	-> App
appModeX	xx
 = case xx of
 	XPrim (MBox)	 [XLit{}]
			-> AppConst
	
	XTau t x	-> appModeX x

	XApp{}		-> appModeX_app xx	
	XAPP{}		-> appModeX_app xx

	_		-> AppOther
	

appModeX_app xx
 = let	effs	= [eff | (x, eff) <- splitApps xx]
  	effs'	= flattenTSum  $ TSum kEffect effs
   in	case effs' of
   		[]	-> AppPure
		_	-> AppEffect (makeTSum kEffect effs')


dotAppGraph ::	Map Var (App, [Var])	-> String
dotAppGraph appMap
 = let	
 
 	--allVars	= Map.keys appMap
 	--	++ concat (map snd $ Map.elems appMap)
 	
   in	pprStrPlain
	$ "digraph G {\n"
--	% (catMap dotVarNode allVars)
	% (catMap dotBindVar $ Map.toList appMap)

	% (catMap (\(v, (app, vs)) -> dotApp v vs) $ Map.toList appMap)
	% "}\n"
	
-----
dotBindVar
	:: (Var, (App, [Var]))	-> String

dotBindVar (v, (app, freeVs))
 = case app of
 	AppConst 		-> dotBindVar_SC "\\nC" "blue"  v	 
	AppPure			-> dotBindVar_SC "\\nP" "green" v
	AppEffect eff		-> dotBindVar_SC "\\n!" "red"   v
	_			-> dotBindVar_SC ""    "black" v

dotBindVar_SC str colorStr v
 = pprStrPlain
	$ "\t"
	% quote (pprStrPlain $ varId v)
	% " [ label = " % quote (pprStrPlain v ++ str) 
	% " , color = " % colorStr 
	% "];\n"

dotApp :: Var -> [Var] -> String
dotApp	v vs
	=  pprStrPlain
	$ "\t"
	% (dotVarBind v)
	% " -> {" 
	% " " %!% (map dotVarBind vs)
	% "};\n" 

dotVarBind :: Var -> String
dotVarBind    v	
	= "\"" ++ (pprStrPlain $ varId v) ++ "\""

quote s	= "\"" ++ s ++ "\""
