// https://www.campusnet.dtu.dk/cnnet/filesharing/download/eaaa0b71-ff17-4084-abbe-60d619a25790

// Michael R. Hansen 05-01-2016

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

System.IO.Directory.SetCurrentDirectory @"C:\Users\Helge\git\GuardedCommands\GuardedCommands\";;

// Test of programs covered by the first task (Section 3.7):
List.iter execOpt ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;

// Test of programs covered by the second task (Section 4.3):
List.iter execOpt ["Ex7.gc"; "fact.gc"; "factRec.gc"; "factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter execOpt ["A0.gc"; "A1.gc"; "A2.gc"; "A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter execOpt ["A4.gc"; "Swap.gc"; "QuickSortV1.gc"];;

// Test of programs covered by the fifth task (Section 7.4):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"];;

exec "par2.gc";;

