{-# OPTIONS -fwarn-incomplete-patterns #-}

-----
-- Source.Pretty
--
-- Summary:
--	Pretty printer for Source.Exp expressions.
--	Must keep output format parseable by trauma parser.
--
--
module Source.Pretty
	()
where

-----
import Util

-----
import Util.Pretty

-----
import qualified Shared.Var 	as Var
import Shared.Error

import Source.Exp
import Source.Horror
import Source.Util
import Shared.Pretty

import Type.Pretty

-----
stage	= "Source.Pretty"

-----
-- prettyTop
--
instance Pretty (Top a) where
 ppr xx
  = case xx of
	PPragma _ es	 -> "pragma " % " " %!% es % ";\n"
	PModule _ v	 -> "module " % v % ";\n"

	PImportExtern _ v tv to
	 -> "import extern " % prettyVTT v tv to % "\n"

	PImportModule _ [m] -> "import " % m % ";\n"
	
	PImportModule _ xx
	 -> "import " 
		% "{\n"
			%> "\n" %!% map (\x -> x % ";") xx
		% "\n}\n\n"
		
	PForeign _ f 
	 -> "foreign " % f % ";\n\n"
	
	PData _ typeName vars [] 
	 -> "data " % " " %!% (typeName : vars) % ";\n\n"

	PData _ typeName vars ctors
	 -> "data " % " " %!% (typeName : vars) % "\n"
		%> ("= "  % "\n\n| " %!% (map prettyCtor ctors) % ";")
		%  "\n\n"

	PRegion _ v	 -> "region " % v % ";\n"
	PEffect _ v k	 -> "effect " % v %>> " :: " % k % ";\n"

	-- Classes
	PClass _ v k	 -> "class " % v %>> " :: " % k % ";\n"

	PClassDict _ c vs inh sigs
	 -> "class " % c % " " % (" " %!% vs) % " where\n"
		% "{\n"
	 	%> ("\n\n" %!% 
			(map (\(vs, t) -> ", " %!% 
				(map (\v -> v { Var.nameModule = Var.ModuleNil }) vs)
				% " :: " %> prettyTypeSplit t % ";") sigs)) % "\n"
		% "}\n\n"

	PClassInst _ v ts inh ss
	 -> "instance " % v % " " % " " %!% (map prettyTB ts) % " where\n"
		% "{\n"
		%> ("\n\n" %!% 
			(map 	(\s -> case s of 
					SBindPats sp v xs x	-> pprStr $ SBindPats sp (v { Var.nameModule = Var.ModuleNil }) xs x
					_			-> panic stage $ "pretty[Top]: malformed PClassInst\n")
					
				ss)
			% "\n")
		% "}\n\n"

	-- Projections
	PProjDict _ t ss
	 -> "project " % t % " where\n"
		% "{\n"
		%> ("\n\n" %!% ss) % "\n"
		% "}\n\n"
	 

	-- type sigs	 
	PType sp v t
         -> v %>> " :: " % prettyTS t % ";\n"
	 

	PStmt s		 -> ppr s % "\n\n"
	
	PInfix _ mode prec syms
	 -> mode % " " % prec % " " % ", " %!% (map Var.name syms) % " ;\n"


prettyVTT ::	Var -> 	Type -> Maybe Type	-> PrettyP
prettyVTT   	v	tv	mto
	=  v 	% "\n"
			%> (":: " % prettyTS tv % "\n" 
			% case mto of 
				Nothing	-> ppr ""
				Just to	-> ":$ " % to % "\n")


prettyCtor ::	(Var, [DataField (Exp a) Type])	-> PrettyP
prettyCtor	xx
 = case xx of
 	(v, [])		-> ppr v
	(v, fs)		
	 -> v % " {\n"
		%> ("\n" %!% (map pprStr fs)) % "\n"
		% "}"
-----
-- Foreign
--
instance Pretty (Foreign a) where
 ppr ff
  = case ff of
  	OImport f		-> "import " % f
	OExport f		-> "export " % f
	
	OExtern mS v tv mTo
	 -> let pName	= case mS of  { Nothing -> ppr " "; Just s  -> ppr $ show s }
		pTo	= case mTo of { Nothing -> ppr " "; Just to -> "\n:$ " % to }
	    in 
	 	"extern " % pName % "\n " 
		 % v 	%> ("\n:: " % prettyTS tv 	% pTo)

	OCCall mS v tv 
	 -> ppr "@CCall"

-----
-- InfixMode
--
instance Pretty (InfixMode a) where
 ppr mode
  = case mode of
 	InfixLeft	-> ppr "infixl"
	InfixRight	-> ppr "infixr"
	InfixNone	-> ppr "infix "
	InfixSuspend	-> ppr "@InfixSuspend"
	
-----
-- Exp
--
instance Pretty (Exp a) where
 ppr xx
  = case xx of
  	XNil		 -> ppr "@XNil"
--	XAnnot aa e	 -> aa % prettyXB e

	XUnit sp	 -> ppr "()"

	XVoid	sp	 -> ppr "_"
	XConst 	sp c	 -> ppr c

	XVar 	sp v	 -> ppr v

	XProj 	sp x p	 -> prettyXB x % p
	XProjT 	sp t p	 -> "@XProjT " % prettyTB t % " " % p


	XLambda sp v e	 -> "\\" % v % " -> " % e


	XLet 	sp ss e
	 -> "let {\n" 
		%> ";\n" %!% ss
		%  "\n} in " % e

	XDo 	sp ss	 	-> "do {\n" %> "\n" %!% ss % "\n}"
	XWhere  sp xx ss	-> xx % " where {\n" %> "\n" %!% ss % "\n"

	XCase 	sp co ca
	 -> "case " % co % " of {\n" 
	 	%> "\n\n" %!% ca
		%  "\n}"

	XLambdaPats sp ps e
	 -> "\\" % " " %!% ps % " -> " % e

	XLambdaProj sp j xs
	 -> "\\" % j % " " % xs

	XLambdaCase sp cs
	 -> "\\case {\n"
	 	%> "\n\n" %!% cs
		%  "\n}"

	XAppE	sp e1 e2 _
	 -> if orf e1 [isXVar, isXApp, isXUnit]
		then e1 % " " % prettyXB e2
		else "(" % e1 % ")\n" %> prettyXB e2
		

	XCaseE 	sp co ca eff
	 -> "case " % co % " of " % eff % " {\n" 
	 	%> "\n\n" %!% ca
		%  "\n}"

	XApp 	sp e1 e2
	 -> if orf e1 [isXVar, isXApp, isXUnit]

	 	then e1 % " " % prettyXB e2

		else "(" % e1 % ")\n" %> prettyXB e2
		
 
	XIfThenElse sp e1 e2 e3
	 ->  "if " % (if isEBlock e1 then "\n" else " ") % e1 
		%> ("\nthen " 	% (if isEBlock e2 then "\n" else " ") % e2)

		% (if isEBlock e2 then "\n" else " ")
		%> ("\nelse "	% (if isEBlock e3 then "\n" else " ") % e3)

	-----
	XAt 	sp v exp	 -> v % "@" % prettyXB exp

	-- object expressions
	XObjVar 	sp v	-> "^" % v
	XObjField 	sp v	-> "_" % v
	XObjFieldR	sp v	-> "_#" % v

	-- infix expressions
	XOp 		sp v	 -> "@XOp " % v
	XDefix 		sp es	 -> "@XDefix " % es
 	XDefixApps 	sp es	 -> "@XDefixApps " % es
	XAppSusp	sp x1 x2 -> "@XAppSusp " % x1 <> x2

	-- lambda sugar


	-- match sugar
	XMatch sp aa	
	 -> "match {\n" 
	 	%> "\n\n" %!% aa
		%  "\n}"
	
	-- exception sugar
	XTry sp x aa Nothing
	 -> "try " % prettyXB x % "\n"
	 %  "catch {\n" 
	 %> ("\n" %!% aa)
	 %  "};"

	XTry sp x aa (Just wX)
	 -> "try " % prettyXB x % "\n"
	 %  "catch {\n" 
	 %> ("\n" %!% aa)
	 %  "}\n"
	 %  "with " % wX % ";"

	XThrow sp x
	 -> "throw " % x

	-- imperative sugar
	XWhile sp x1 x2
	 -> "while (" % x1 % ")\n"
	 % x2
	 
	XWhen sp x1 x2
	 -> "when (" % x1 % ")\n"
	 % x2
	 
	XUnless sp x1 x2
	 -> "unless (" % x1 % ")\n"
	 % x2
	
	XBreak sp 
	 -> ppr "break"
	 
	-- list range sugar
	XListRange sp b x Nothing		-> "[" % x % "..]"
	XListRange sp b x (Just x2)	-> "[" % x % ".." % x2 % "]"
	
	
	XListComp sp x qs 		-> "[" % x % " | " % ", " %!% qs % "]"
	

	-- patterns
	XCon   sp v xx		-> v % " " % " " %!% xx
	XTuple sp xx		-> "(" % ", " %!% xx % ")"
	XCons  sp x1 x2		-> x1 % ":" % x2
	XList  sp xx		-> "[" % ", " %!% xx % "]"
	XWildCard sp		-> ppr "_"


instance Pretty (Proj a) where
 ppr f
  = case f of
  	JField  sp l	-> "." % l
	JFieldR sp l	-> "#" % l

	JIndex	sp x	-> ".[" % x % "]"
	JIndexR	sp x	-> "#[" % x % "]"

	JAttr	sp v	-> ".{" % v % "}"

-----
isEBlock x
	= orf x	[ isXLet 
		, isXCase
		, isXDo ]
	
prettyXB xx
 = case xx of
 	XVar sp v	-> ppr v
	XConst sp c	-> ppr c
	XUnit sp	-> ppr xx
	XProj{}		-> ppr xx
	e		-> "(" % e % ")"
	
prettyX_naked xx
 = case xx of
	XApp sp e1 e2	-> prettyX_appL (XApp sp e1 e2)
	e		-> ppr e


prettyX_appL xx
 = case xx of
	XApp sp e1 e2
	 -> prettyX_appL e1 
		%  " "
		% prettyX_appR e2

	e  -> prettyXB e


-----
prettyX_appR xx
 = case xx of
	XApp sp e1 e2	->  "(" % prettyX_appL e1 % " " % prettyX_appR e2 % ")"
	e 		-> prettyXB e

-----
{-instance Pretty Annot where
 ppr xx 
  = case xx of
  	ATypeVar   v	-> "@T " % v
	AEffectVar v	-> "@E " % v
-}
-----
instance Pretty (Alt a) where
 ppr a
  = case a of
	APat 	 sp p1 x2	-> p1	% "\n -> " % prettyX_naked x2 % ";"

	AAlt	 sp [] x	-> "\\= " % x % ";"
	AAlt	 sp gs x	-> "|"  % "\n," %!% gs % "\n=  " % x % ";"

	ADefault sp x		-> "_ ->" % x % "\n"
	
-----
instance Pretty (Guard a) where
 ppr gg
  = case gg of
  	GCase	sp pat		-> "- " % pat
	GExp	sp pat exp	-> " "  % pat %>> " <- " % exp
	GBool	sp exp		-> " "  % ppr exp

-----	
instance Pretty (Pat a) where
 ppr ww
  = case ww of
  	WVar 	sp v		-> ppr v
	WConst 	sp c		-> ppr c
	WCon 	sp v ps		-> v % " " % ps %!% " " 
	WConLabel sp v lvs	-> v % " { " % ", " %!% map (\(l, v) -> l % " = " % v ) lvs % "}"
	WAt 	sp v w		-> v % "@" % w
	WWildcard sp 		-> ppr "_"
	WExp 	x		-> ppr x
	WUnit	sp		-> ppr "()"
	WTuple  sp ls		-> "(" % ", " %!% ls % ")"
	WCons   sp x xs		-> x % " : " % xs
	WList   sp ls		-> "[" % ", " %!% ls % ")"

instance Pretty (Label a) where
 ppr ll
  = case ll of
  	LIndex sp i		-> "." % i
	LVar   sp v		-> "." % v

-----
instance Pretty (LCQual a) where
 ppr q
  = case q of
  	LCGen False p x	-> p % " <- " % x
	LCGen True  p x -> p % " <@- " % x
	LCExp x		-> ppr x
	LCLet ss	-> "let { " % ss % "}"

-----
instance Pretty (Stmt a) where
 ppr xx
  = case xx of
	SStmt sp  x		-> prettyX_naked x 					% ";"
	SBindPats sp v [] x	-> v % " " 		%>> (spaceDown x) % " = " % prettyX_naked x 	% ";"
	SBindPats sp v ps x	
		-> v % " " % " " %!% map prettyXB ps 
		%>> (spaceDown x) % "\n = " % prettyX_naked x 	% ";"


	SSig sp v t		-> v %> " :: " % t % ";"

spaceDown xx
 = case xx of
	XLambda{}	-> ppr "\n"
	XLambdaCase{}	-> ppr "\n"
 	XCase{}		-> ppr "\n"
	XCaseE{}	-> ppr "\n"
	XIfThenElse{}	-> ppr "\n"
	XDo{}		-> ppr "\n"
	_		-> pNil


