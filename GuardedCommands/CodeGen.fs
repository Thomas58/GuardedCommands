namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)

    type Var = 
        | GloVar of int                   (* absolute address in stack           *)
        | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
    keeps track of next available offset for local variables *)

    type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

    type ParamDecs = (Typ * string) list
    type funEnv = Map<string, label * Typ option * ParamDecs>

(* ------------------------------------------------------------------- *)

(* Function lookup: *)

    let findFunction f fEnv = 
        match Map.tryFind f fEnv with
        | Some x -> x
        | None   -> failwith("unknown function: " + string f)
  
(* Convert Dec list to ParamDecs: *)

    let toParamDec xs dec =
        match dec with
        | VarDec(t,s) -> (t,s)::xs                         
        | _           -> failwith "toParamDec"

    let toParamDecs decs : ParamDecs = List.fold toParamDec [] decs

(* Bind declared parameters in env: *)

    let bindParam (env, fdepth) dec = 
        match dec with
        | VarDec(typ,x) -> Map.add x (LocVar fdepth, typ) env, fdepth+1
        | _             -> failwith "bindParam"

    let bindParams prms (env, fdepth) = List.fold bindParam (env, fdepth) prms;

  
(* Bind declared variable in env and generate code to allocate it: *)   

    let allocate typVar (typ, x) (env, fdepth) =
        match typ with
        | ATyp (ATyp _, _) -> 
            failwith("allocate: array of arrays not permitted")
        | ATyp (t, Some i) -> 
            let newEnv = Map.add x (typVar (fdepth+i), typ) env, fdepth+i+1
            let code = [INCSP i; GETSP; CSTI (i-1); SUB]
            (newEnv, code)
        | _ -> 
            let newEnv = (Map.add x (typVar fdepth, typ) env, fdepth+1)
            let code = [INCSP 1]
            (newEnv, code)        

(* Add declarations to env using global allocation: *)

    let rec addGlobalVars vEnv fEnv = function
        | []           -> (vEnv, fEnv, [])
        | dec::decs -> 
          match dec with 
          | VarDec(typ, var) -> 
            let (vEnv1, code1)          = allocate GloVar (typ, var) vEnv
            let (vEnv2, fEnv2, code2)   = addGlobalVars vEnv1 fEnv decs
            vEnv2, fEnv2, code1 @ code2
          | FunDec(topt, f, fdecs, stm) -> 
            let prms = toParamDecs fdecs
            addGlobalVars vEnv (Map.add f (newLabel(), topt, prms) fEnv) decs

(* Add declarations to env using local allocation: *)

    let rec addLocalVars vEnv = function
        | []           -> (vEnv, [])
        | dec::topdecs -> 
          match dec with 
          | VarDec(typ, var) -> 
            let (vEnv1, code1)  = allocate LocVar (typ, var) vEnv
            let (vEnv2, code2)  = addLocalVars vEnv1 topdecs
            vEnv2, code1 @ code2
          | _ -> failwith("addLocalVars")

(* Build environments for global variables and functions *)        

    let makeGlobalEnvs decs = addGlobalVars (Map.empty, 0) Map.empty decs

(* ------------------------------------------------------------------- *)

/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
    let rec CE vEnv fEnv = 
        function
        | N n             -> [CSTI n]
        | B b             -> [CSTI (if b then 1 else 0)]
        | Access acc      -> CA vEnv fEnv acc @ [LDI]
        | Addr acc        -> CA vEnv fEnv acc 

        | Apply("-", [e]) -> CE vEnv fEnv e @ [CSTI 0; SWAP; SUB]

        | Apply("!", [e]) -> CE vEnv fEnv e @ [NOT]

        | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                 let labfalse = newLabel()
                                 CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                 @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

        | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "-"; "*"; "="; "<>"; "<"; "<="; ">"; ">="]
                                -> let ins = match o with
                                             | "+"  -> [ADD]
                                             | "-"  -> [SUB]
                                             | "*"  -> [MUL]
                                             | "="  -> [EQ] 
                                             | "<>" -> [EQ; NOT]
                                             | "<"  -> [LT]
                                             | ">=" -> [LT; NOT]
                                             | ">"  -> [SWAP; LT]
                                             | "<=" -> [SWAP; LT; NOT]
                                             | _    -> failwith "CE: this case is not possible"
                                   CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins 

        | Apply(f,es)     -> let (label, _, prms) = findFunction f fEnv
                             CEs vEnv fEnv es @ [CALL(List.length prms, label)]
                            
        | _            -> failwith "CE: not supported yet"

    and CEs vEnv fEnv es = List.collect (CE vEnv fEnv) es        

/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
    and CA vEnv fEnv = function | AVar x        -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [GETBP; CSTI addr; ADD]
                                | AIndex(acc,e) -> CA vEnv fEnv acc @ [LDI] @ CE vEnv fEnv e @ [ADD]
                                | ADeref e      -> CE vEnv fEnv e
                      
/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
    let rec CS vEnv fEnv = function
        | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

        | Ass(acc,e)       -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

        | Block(decs,stms) -> let (newEnv, code) = addLocalVars vEnv decs
                              code @ CSs newEnv fEnv stms @ [INCSP (snd vEnv - snd newEnv)]

        | Alt(GC list)     -> let labend = newLabel()
                              List.fold (fun acc (e,stms) -> 
                                  let labfalse = newLabel() 
                                  acc @ CE vEnv fEnv e @ [IFZERO labfalse] @ 
                                  CSs vEnv fEnv stms @ [GOTO labend; Label labfalse]
                                  ) [] list @ [STOP; Label labend]

        | Do(GC list)       -> List.fold (fun acc (e,stms) -> 
                                  let labstart = newLabel()
                                  let labfalse = newLabel()
                                  acc @ [Label labstart] @ CE vEnv fEnv e @ [IFZERO labfalse] @ 
                                  CSs vEnv fEnv stms @ [GOTO labstart; Label labfalse]                             
                                  ) [] list

        | Return e         -> CE vEnv fEnv e @ [RET (snd vEnv)]

        | Call(f,es)       -> let (label, _, paramDecs) = findFunction f fEnv
                              CEs vEnv fEnv es @ [CALL(List.length paramDecs, label)] @ [INCSP -1]

    and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 



(* ------------------------------------------------------------------- *)

/// CP prog gives the code for a program prog
    let CP (P(decs,stms)) = 
        let _ = resetLabels ()
        let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs

        let compileFunc (_, f, fdecs, stm) = 
            let (label, _, _) = findFunction f fEnv
            let lvEnv = bindParams fdecs (gvM, 0)
            let code = CS lvEnv fEnv stm
            [Label label] @ code @ [RET (List.length fdecs-1)]

        let functions = List.choose (function
            | FunDec(topt, f, fdecs, stm) -> Some(compileFunc(topt, f, fdecs, stm))
            | _                           -> None) decs

        initCode @ CSs gvEnv fEnv stms @ [STOP] @ List.concat functions



