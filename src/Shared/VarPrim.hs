
-- | These are the variables that have a special meaning to the compiler.
--	They all need to be defined in the base libraries or runtime system.

module Shared.VarPrim

where

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..), VarBind(..), Module(..), VarInfo)
import Shared.VarBind
import Shared.Base

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)
	
-- Operational Types -------------------------------------------------------------------------------
primTObj	= primVar 	NameType	"Base.Obj"			TObj
primTData	= primVar 	NameType	"Base.Data"			TData
primTThunk	= primVar	NameType	"Base.Thunk"			TThunk
	
			
-- Primitive Type Variables ------------------------------------------------------------------------
primTVoidU	= primVarI	NameType	"Base.Void#"			TVoidU		[Var.ISeaName "void"]
primTPtrU	= primVarI	NameType	"Base.Ptr#"			TPtrU		[Var.ISeaName "Ptr"]

primTUnit	= primVar	NameType	"Base.Unit"			TUnit

primTBool	= primVarFmt	NameType	"Base.Bool"			TBool
primTWord	= primVarFmt	NameType	"Base.Word"			TWord
primTInt	= primVarFmt	NameType	"Base.Int"			TInt
primTFloat	= primVarFmt	NameType	"Base.Float"			TFloat
primTChar	= primVarFmt	NameType	"Base.Char"			TChar
primTString	= primVarFmt	NameType	"Data.String.String"		TString

primTRef	= primVar 	NameType	"Data.Ref.Ref"			TRef
primTList	= primVar 	NameType	"Data.List.List"		TList
primTTuple i	= primVar	NameType	("Data.Tuple.Tuple" ++ show i) 	(TTuple i)

-- Effects -----------------------------------------------------------------------------------------
primRead	= primVar NameEffect	"Base.Read"				ERead
primReadT	= primVar NameEffect	"Base.ReadT"				EReadT
primReadH	= primVar NameEffect	"Base.ReadH"				EReadH

primWrite	= primVar NameEffect	"Base.Write"				EWrite
primWriteT	= primVar NameEffect	"Base.WriteT"				EWriteT


-- Classes -----------------------------------------------------------------------------------------
primFShape i	= primVar NameClass	("Base.Shape" ++ show i)		(FShape  i)

primLazy	= primVar NameClass	"Base.Lazy"				FLazy
primConst	= primVar NameClass	"Base.Const"				FConst
primMutable	= primVar NameClass	"Base.Mutable"				FMutable
primDirect	= primVar NameClass	"Base.Direct"				FDirect

primConstT	= primVar NameClass	"Base.ConstT"				FConstT
primMutableT	= primVar NameClass	"Base.MutableT"				FMutableT
primDirectT	= primVar NameClass	"Base.DirectT"				FDirectT

primLazyT	= primVar NameClass	"Base.LazyT"				FLazyT
primLazyH	= primVar NameClass	"Base.LazyH"				FLazyH

primPure	= primVar NameClass	"Base.Pure"				FPure
primEmpty	= primVar NameClass	"Base.Empty"				FEmpty


-- Values ------------------------------------------------------------------------------------------
primUnit	= primVar NameValue	"Base.Unit"				VUnit

primProjField	= primVarI NameValue	"Base.primProjField"			VProjField
					[ Var.ISeaName "primProjField"]

primProjFieldR	= primVarI NameValue	"Base.primProjFieldR"			VProjFieldR
					[ Var.ISeaName "primProjFieldR"]

primIndex	= primVarI NameValue	"Data.Array.index"			VIndex
					[Var.ISeaName "primArray_index"]
					
primIndexR	= primVarI NameValue	"Data.Array.indexR"			VIndexR
					[Var.ISeaName "primArray_indexR"]

primSuspend i	= primVarI NameValue	("Base.Thunk.suspend" ++ show i) 	(VSuspend i) 
					[Var.ISeaName ("primSuspend" ++ show i)]

primNegate	= primVar NameValue	"Class.Num.negate"			VNegate

primTrue	= primVar NameValue 	"Data.Bool.True"			VTrue
primFalse	= primVar NameValue	"Data.Bool.False"			VFalse

primTuple i	= primVar NameValue	("Data.Tuple.Tuple" ++ show i) 		(VTuple i)

primNil		= primVar NameValue	"Data.List.Nil"				VNil	
primCons	= primVar NameValue	"Data.List.Cons"			VCons	
primAppend	= primVar NameValue	"Data.List.Append"			VAppend	
primRange	= primVar NameValue	"Data.List.rangeInt" 			VRange
primRangeL	= primVar NameValue	"Data.List.rangeIntL"			VRangeL
primRangeInfL	= primVar NameValue	"Data.List.rangeInfIntL"		VRangeInfL
primConcatMap	= primVar NameValue	"Data.List.concatMap"			VConcatMap
primConcatMapL	= primVar NameValue	"Data.List.concatMapL"			VConcatMapL

primThrow	= primVarI NameValue	"Control.Exception.primThrow"		VThrow
					[Var.ISeaName ("primException_throw")]

primTry		= primVarI NameValue	"Control.Exception.primTry"		VTry
					[Var.ISeaName ("primException_try")]

primExceptionBreak
		= primVar NameValue	"Base.ExceptionBreak"			VExceptionBreak	

primGateLoop	= primVar NameValue	"Control.Exception.gateLoop"		VGateLoop

primWhile	= primVar NameValue	"Control.Imperative.while"		VWhile	

primBind	= primVar NameValue	"Class.Monad.>>="			VBind


-- Utils -------------------------------------------------------------------------------------------

-- | Create a primitive variable
primVar :: NameSpace -> String -> VarBind -> Var
primVar space name bind
 = let	parts		= breakOns '.' name
 	Just modParts	= takeInit parts
	Just varName	= takeLast parts

   in	(Var.new varName)
 	{ Var.bind		= bind
	, Var.nameSpace		= space
	, Var.nameModule	= ModuleAbsolute modParts }


-- | Create a primitive variable with some extended info
primVarI :: NameSpace -> String -> VarBind -> [VarInfo] -> Var
primVarI space name bind info
  = (primVar space name bind) { Var.info = info }

-- | Create the var for this primitive type
primVarFmt :: NameSpace -> String -> (DataFormat -> VarBind) -> DataFormat -> Var
primVarFmt space name mkBind fmt
 = let	suffix = case fmt of
			Boxed		-> ""
			Unboxed		-> "#"
			BoxedBits d	-> show d
			UnboxedBits d	-> show d ++ "#"
	
	name_parts	= breakOns '.' name
	Just varName	= takeLast name_parts

   in	primVarI 
		space 
		(name ++ suffix) (mkBind fmt)			
		[Var.ISeaName $ varName ++ filter (/= '#') suffix]

-- | If this string has the given prefix then split it off and return the rest
--	of the string
splitPrefix :: Eq a => [a] -> [a] -> Maybe [a]
splitPrefix [] xs	= Just xs

splitPrefix (p:ps) (x:xs)
	| p == x	= splitPrefix ps xs
	| otherwise	= Nothing

splitPrefix (p:ps) []	= Nothing


-- | Check whether a var is for an unboxed type constructor
varIsUnboxedTyConData :: Var -> Bool
varIsUnboxedTyConData var
	= varBindIsUnboxedTyConData (Var.bind var)

varBindIsUnboxedTyConData bind
	| Just fmt	<- takeVarBind_dataFormat bind
	= dataFormatIsUnboxed fmt

	| TVoidU	<- bind	= True
	| TPtrU		<- bind	= True
	
	| otherwise		= False


-- renamePrimVar -------------------------------------------------------------------------------------	  
-- | If this var has a special meaning to the compiler then rewrite
--	its VarBind to the common one
renamePrimVar :: NameSpace -> Var -> Maybe Var
renamePrimVar n var
	| NameValue	<- n
	, Just bind	<- renamePrimVar_value $ Var.name var
	= Just var { Var.bind = bind }

	| NameType	<- n
	, Just bind	<- renamePrimVar_type  $ Var.name var
	= Just	$ defaultTypeVar 
		$ var { Var.bind = bind }

	| NameEffect	<- n
	, Just bind	<- renamePrimVar_effect $ Var.name var
	= Just var { Var.bind = bind }

	| NameClass	<- n
	, Just bind	<- renamePrimVar_class $ Var.name var
	= Just var { Var.bind = bind }
	
	| otherwise
	= Nothing
	

renamePrimVar_value :: String -> Maybe VarBind
renamePrimVar_value ss
	| Just xx	<- splitPrefix "Tuple" ss
	= Just (VTuple (read xx))
	
	| Just xx	<- splitPrefix "suspend" ss
	= Just (VSuspend (read xx))
	
	| otherwise
	= Map.lookup ss renamePrimVar_values
	
renamePrimVar_values
	= Map.fromList 
	[ ("Unit",		VUnit)
	, ("True",		VTrue)
	, ("False",		VFalse)
	, ("negate",		VNegate)
	, ("index",		VIndex)
	, ("indexR",		VIndexR)
	, ("primProjField",	VProjField)
	, ("primProjFieldR",	VProjFieldR)
	, ("Nil",		VNil)
	, ("Cons",		VCons)
	, ("Append",		VAppend)
	, ("ExceptionBreak",	VExceptionBreak)
	, ("gateLoop",		VGateLoop)
	, ("primThrow",		VThrow)
	, ("primTry",		VTry)
	, ("rangeInt",		VRange)
	, ("rangeIntL",		VRangeL)
	, ("rangeInfIntL",	VRangeInfL)
	, ("concatMap",		VConcatMap)
	, ("concatMapL",	VConcatMapL)
	, (">>=",		VBind)]

	
renamePrimVar_type :: String -> Maybe VarBind
renamePrimVar_type ss
	| Just xx	<- splitPrefix "Tuple" ss
	= Just (TTuple (read xx))
	
	| otherwise
	= Map.lookup ss renamePrimVar_types
	
-- | We manually list out all the primitive types to ensure
--	that invalid ones are not bound.	
renamePrimVar_types 
	= Map.fromList
	[ ("Obj",		TObj)
	, ("Data",		TData)
	, ("Thunk",		TThunk)

	, ("Void#",		TVoidU)
	, ("Ptr#",		TPtrU)
	
	, ("Bool",		TBool  Boxed)
	, ("Bool#",		TBool  Unboxed)

	, ("Word",		TWord Boxed)
	, ("Word64",		TWord  (BoxedBits 64))
	, ("Word32",		TWord  (BoxedBits 32))
	, ("Word16",		TWord  (BoxedBits 16))
	, ("Word8",		TWord  (BoxedBits 8))

	, ("Word#",		TWord Unboxed)
	, ("Word64#",		TWord  (UnboxedBits 64))
	, ("Word32#",		TWord  (UnboxedBits 32))
	, ("Word16#",		TWord  (UnboxedBits 16))
	, ("Word8#",		TWord  (UnboxedBits 8))

	, ("Int",		TInt   Boxed)
	, ("Int64",		TInt   (BoxedBits 64))
	, ("Int32",		TInt   (BoxedBits 32))
	, ("Int16",		TInt   (BoxedBits 16))
	, ("Int8",		TInt   (BoxedBits 8))

	, ("Int#",		TInt   Unboxed)
	, ("Int64#",		TInt   (UnboxedBits 64))
	, ("Int32#",		TInt   (UnboxedBits 32))
	, ("Int16#",		TInt   (UnboxedBits 16))
	, ("Int8#",		TInt   (UnboxedBits 8))
	
	, ("Float",		TFloat Boxed)
	, ("Float64",		TFloat (BoxedBits 64))
	, ("Float32",		TFloat (BoxedBits 32))

	, ("Float#",		TFloat Unboxed)
	, ("Float64#",		TFloat (UnboxedBits 64))
	, ("Float32#",		TFloat (UnboxedBits 32))
 
	, ("Char",		TChar  Boxed)
	, ("Char32",		TChar  (BoxedBits 32))

	, ("Char#",		TChar  Unboxed)
	, ("Char32#",		TChar (UnboxedBits 32))
	
	, ("String",		TString Boxed)
	, ("String#",		TString Unboxed) 
	
	, ("Unit",		TUnit)
	, ("Ref",		TRef)
	, ("List",		TList)]


defaultTypeVar :: Var -> Var
defaultTypeVar var
 = case Var.name var of
	"Word"		-> primVar NameType "Base.Word32"   (TWord  $ BoxedBits 32)
	"Int"		-> primVar NameType "Base.Int32"    (TInt   $ BoxedBits 32)
	"Float"		-> primVar NameType "Base.Float32"  (TFloat $ BoxedBits 32)
	"Char"		-> primVar NameType "Base.Char32"   (TChar  $ BoxedBits 32)

	"Word#"		-> primVar NameType "Base.Word32#"   (TWord  $ UnboxedBits 32)
	"Int#"		-> primVar NameType "Base.Int32#"    (TInt   $ UnboxedBits 32)
	"Float#"	-> primVar NameType "Base.Float32#"  (TFloat $ UnboxedBits 32)
	"Char#"		-> primVar NameType "Base.Char32#"   (TChar  $ BoxedBits   32)
	_		-> var

{-
var_withModule :: String -> Var -> Var
var_withModule str var
	= var { Var.ModuleAbsolute $ breakOns '.' str }
-}

renamePrimVar_effect :: String -> Maybe VarBind
renamePrimVar_effect ss
 = Map.lookup ss $ Map.fromList $
 	[ ("Read",		ERead)
	, ("ReadT",		EReadT)
	, ("ReadH",		EReadH)
	, ("Write",		EWrite)
	, ("WriteT",		EWriteT) ]


renamePrimVar_class :: String -> Maybe VarBind
renamePrimVar_class ss
	| Just xx	<- splitPrefix "Shape" ss
	= Just (FShape (read xx))
	
	| otherwise
	= Map.lookup ss renamePrimVar_classes
	
renamePrimVar_classes
 	= Map.fromList 
 	[ ("Const",		FConst)
	, ("ConstT",		FConstT)
	, ("Mutable",		FMutable)
	, ("MutableT",		FMutableT)
	, ("Direct",		FDirect)
	, ("DirectT",		FDirectT)
	, ("Lazy",		FLazy)
	, ("LazyT",		FLazyT)
	, ("LazyH",		FLazyH)
	, ("Pure",		FPure)
	, ("Empty",		FEmpty) ]
		
	
