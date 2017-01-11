// https://www.campusnet.dtu.dk/cnnet/filesharing/download/eaaa0b71-ff17-4084-abbe-60d619a25790

// Michael R. Hansen 05-01-2016

// You must revise 4 pathes occurring in this file 
// The first three are:

#r @".\bin\Debug\FSharp.PowerPack.dll";;
#r @".\bin\Debug\Machine.dll";
#r @".\bin\Debug\VirtualMachine.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "TypeCheck.fs"
#load "CodeGen.fs"
#load "CodeGenOpt.fs"
#load "Util.fs"


open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

open Machine
open VirtualMachine

// You must revise this path
System.IO.Directory.SetCurrentDirectory @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\";;

List.iter exec ["B0.gc"; "B1.gc"; "B2.gc"];;

List.iter exec ["Ex8.gc"; "Ex9.gc"; "Ex10.gc"; "Ex11.gc"; "Ex12.gc"];;

List.iter exec ["A5.gc"; "A6.gc"; "A7.gc"; "A8.gc"; "A9.gc"; "A10.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;
