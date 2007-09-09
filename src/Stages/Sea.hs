
module Stages.Sea
(
	seaSub,
	seaCtor,
	seaThunking,
	seaForce,
	seaSlot,
	seaFlatten,
	seaInit,
	seaMain,

	gotMain,
	outSea,

	invokeSeaCompiler,
	invokeLinker
)

where

-----
import System.Cmd
import System.Exit
import qualified Data.Map	as Map
import Data.Map			(Map)
import Data.Char

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util

-----
import Shared.Var		(Module(..))
import Shared.Error

import Sea.Pretty
import Sea.Exp
import Sea.Util

import Sea.Sub			(subTree)
import Sea.Ctor			(expandCtorTree)
import Sea.Proto		(addSuperProtosTree)
import Sea.Thunk		(thunkTree)
import Sea.Force		(forceTree)
import Sea.Slot			(slotTree)
import Sea.Flatten		(flattenTree)
import Sea.Init			(initTree, mainTree, gotMain)

import Sea.Plate.Trans

import qualified Main.Version	as Version
import Main.Path
import Main.Arg

import Stages.Dump

-----
stage	= "Stages.Sea"

------
-- seaSub
--
seaSub
	:: (?args :: [Arg])
	-> Tree ()
	-> IO (Tree ())
	
seaSub tree
 = do
 	let tree'
		= subTree tree
		
	dumpET DumpSeaSub "sea-sub" 
		$ eraseAnnotsTree tree'
	
	return tree'
	
		
-----------------------
-- expandCtor
--
seaCtor 
	:: (?args :: [Arg])
	-> Tree ()
	-> IO (Tree ())

seaCtor
	eTree
 = do
	let eExpanded	= addSuperProtosTree
			$ expandCtorTree
			$ eTree
	
	dumpET DumpSeaCtor "sea-ctor" 
		$ eraseAnnotsTree eExpanded
	
	return eExpanded
		

-----------------------
-- expandThunking
--
seaThunking 
	:: (?args :: [Arg])
	-> Tree ()
	-> IO (Tree ())
	
seaThunking	 
	eTree
 = do
	let tree'	= thunkTree eTree
	dumpET DumpSeaThunk "sea-thunk" 
		$ eraseAnnotsTree tree'
	
	return tree'


-----------------------
-- seaForce
--
seaForce 
	:: (?args :: [Arg])
	-> Tree ()
	-> IO (Tree ())
	
seaForce
	eTree
 = do
	let tree'	= forceTree eTree
	dumpET DumpSeaForce "sea-force" 
		$ eraseAnnotsTree tree'

 	return	tree'


-----------------------
-- seaSlot
--
seaSlot
	:: (?args :: [Arg])
	-> Tree ()		-- sea tree
	-> Tree ()		-- sea header
	-> Set Var		-- CAF vars
	-> IO (Tree ())
	
seaSlot	eTree eHeader cafVars
 = do
 	let tree'	= slotTree eTree eHeader cafVars
	dumpET DumpSeaSlot "sea-slot" 
		$ eraseAnnotsTree tree'
	
	return	tree'


-----------------------
-- seaFlatten
--
seaFlatten
	:: (?args :: [Arg])
	-> Tree ()
	-> IO (Tree ())
	
seaFlatten eTree
 = do	let tree'	= flattenTree eTree
	dumpET DumpSeaFlatten "sea-flatten" 
		$ eraseAnnotsTree tree'
	
	return	tree'
 

-----------------------
-- seaInit
--
seaInit
	:: (?args :: [Arg])
	-> Module
	-> (Tree ())
	-> IO (Tree ())
	
seaInit moduleName eTree
 = 	return	$ initTree
 			 moduleName
			 eTree
	
-----------------------
-- outSea
--		
outSea 
	:: (?args :: [Arg])
	-> Module
	-> (Tree ())		-- sea source
	-> FilePath		-- path of the source file
	-> [FilePath]		-- paths of the imported interfaces
	-> [String]		-- extra header files to include
	-> IO	( String
		, String )
		
outSea	
	moduleName
	eTree
	pathThis
	pathImports
	extraIncludes
	
 = do
	-- Break up the sea into Header/Code parts.
	let 	[ 	seaProtos, 		seaSupers
		 , 	seaCafProtos,		seaCafSlots,		seaCafInits
		 , 	seaAtomProtos,		seaAtoms
		 , 	seaStructs
		 , 	seaHashDefs ]

		 = partitionFs
			[ (=@=) PProto{}, 	(=@=) PSuper{}
			, (=@=) PCafProto{},	(=@=) PCafSlot{},	(=@=) PCafInit{}
			, (=@=) PAtomProto{},  	(=@=) PAtom{}
			, (=@=) PStruct{}
			, (=@=) PHashDef{} ]
			eTree
			
	
	-- Build the C header
	let defTag	= makeIncludeDefTag pathThis
	let seaHeader
		=  [ PHackery $ makeComments pathThis
		   , PHackery ("#ifndef _inc" ++ defTag ++ "\n")
		   , PHackery ("#define _inc" ++ defTag ++ "\n\n") 
		   , PInclude "runtime/Runtime.h"
		   , PInclude "runtime/Prim.h" ]
		++ modIncludes pathImports
		++ (map PInclude extraIncludes)

		++ [ PHackery "\n"]	++ seaHashDefs
		++ [ PHackery "\n"]	++ seaStructs
		++ [ PHackery "\n"]	++ seaAtomProtos
		++ [ PHackery "\n"]	++ seaCafProtos
		++ [ PHackery "\n"]	++ seaProtos
		++ [ PHackery "\n#endif\n\n" ]

	let seaHeaderS	
		= catMap pretty 
			$ eraseAnnotsTree seaHeader

	-- Build the C code
	let seaIncSelf	= modIncludeSelf pathThis
	
	let seaCode
		=  [PHackery $ makeComments pathThis]
		++ [seaIncSelf]
		++ [PHackery "\n"]	++ seaAtoms
		++ [PHackery "\n"]	++ seaCafSlots
		++ [PHackery "\n"]	++ seaCafInits
		++ [PHackery "\n"]	++ seaSupers

	let seaCodeS	
		= catMap pretty 
			$ eraseAnnotsTree seaCode
	
	--
	return	( seaHeaderS
		, seaCodeS )

modIncludeSelf p
 =	PInclude $ nameTItoH p

modIncludes pathImports
  = 	map (\p -> PInclude $ nameTItoH p) pathImports

nameTItoH nameTI
 = let	parts	= splitOns '.' nameTI
   in   concat (init parts ++ ["ddc.h"])
   
makeComments pathThis
  = unlines
	[ "// -----------------------"
	, "//       source: " ++ pathThis
	, "// generated by: " ++ Version.traumaName
	, "" ]
	
makeIncludeDefTag pathThis
 = filter (\c -> isAlpha c || isDigit c)
 	$ pathThis
		
	
-----------------------
-- invokeSeaCompiler
--
invokeSeaCompiler 
	:: (?args :: [Arg])
	-> IO ()

invokeSeaCompiler
 = do
	let Just (ArgPath paths)
		= find (=@= ArgPath{}) ?args

	let dsDir
		= concat
		$ init
		$ splitOns '/' (pathBase paths)
	

	let cmd	= "gcc"
		++ " -Werror"
		++ " -std=c99"

		++ (if elem Debug ?args
			then " -g"
			else " -O1 -march=i686")

		++ (if elem Profile ?args
			then " -pg"
			else "")

--		++ " -Wall -Werror"
		++ " -I."
		++ " -I"  ++ dsDir
		++ " -c " ++ (pathC paths)
		++ " -o " ++ (pathO paths)

 	when (elem Verbose ?args)
	 (do
		putStr	$ "\n"
	 	putStr	$ " * Invoking C compiler.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n")
		
	retCompile	<- system cmd
	
	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeSeaCompiler: compilation of C file failed.\n"
		% "    pathC = " % pathC paths % "\n"
		

----------------------
-- invokeLinker
--
invokeLinker 
	:: (?args :: [Arg])
	-> [FilePath]			-- paths of interfaces of all modules to link
	-> IO ()

invokeLinker 
	objects
 = do
	let Just (ArgPath paths)
		= find (=@= ArgPath{}) ?args

	let outFileName	
		= case filter (\x -> x =@= OutputFile{}) ?args of
			[OutputFile [fileName]] 	-> fileName
			_				-> "a.out"

	let moreObjs	= concat $ [files 	| LinkObj 	files 	<- ?args]
	let linkLibs	= concat $ [libs	| LinkLib 	libs	<- ?args]
	let linkLibDirs	= concat $ [dirs	| LinkLibDir	dirs	<- ?args]
			
	let cmd = "gcc"
		++ " -std=c99"
		++ " -o " ++ outFileName

		++ (if elem Profile ?args 
			then " -pg" 
			else "")

		++ " " ++ (catInt " " $ objects ++ moreObjs)
					
		++ (if elem StaticRuntime ?args 
			then " runtime/ddc-runtime.a"
			else " runtime/ddc-runtime.so")

		++ " " ++ (catInt " " ["-L" ++ dir | dir <- linkLibDirs])
		++ " " ++ (catInt " " ["-l" ++ lib | lib <- linkLibs])

	when (elem Verbose ?args)
	 (do
	 	putStr	$ "\n"
		putStr	$ " * Invoking linker.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n")
		
	retLink		<- system cmd

	case retLink of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLinker: link failed\n"
		% "     objects:\n"
		% (catMap (\s -> pretty $ "        " % s % "\n") objects) % "\n"

	return ()


nameTItoO s
 = let
 	parts@(_:_)	= splitOns '.' s
	name		= (concat $ init parts) ++ "o"
   in	name



-----------------------
-- seaMain
--
seaMain	:: (?args :: [Arg])
	-> [Module]
	-> IO (Tree ())
	
seaMain imports
	= return $ mainTree imports
	




