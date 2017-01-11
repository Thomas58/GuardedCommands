namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck =     

(* Function lookup: *)

    let findFunction f gtenv = 
        match Map.tryFind f gtenv with
        | Some(FTyp(typs,topt)) -> (typs,topt)
        | _                     -> failwith("unknown function: " + string f)
        
(* Convert Dec list to Typ list: *)

    let toTyp xs dec =
        match dec with
        | VarDec(t,_) -> t::xs
        | _           -> failwith "illegal declaration"

    let toTyps decs = List.fold toTyp [] decs

/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables 
    let rec tcE gtenv ltenv = function                            
        | N _              -> ITyp   
        | B _              -> BTyp   
        | Access acc       -> tcA gtenv ltenv acc    
        | Addr acc         -> tcA gtenv ltenv acc
                  
        | Apply(f,[e])     when List.exists (fun x ->  x=f) ["-"; "!"]  
                           -> tcMonadic gtenv ltenv f e        
        
        | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+"; "-"; "*"; "="; "&&"; "<>"; "<"; "<="; ">"; ">="]        
                           -> tcDyadic gtenv ltenv f e1 e2   
        
        | Apply(f,es)      -> tcNaryFunction gtenv ltenv f es
        
        | e                -> failwith("tcE: not supported yet" + string e)

    and tcMonadic gtenv ltenv f e = 
        match (f, tcE gtenv ltenv e) with
        | ("-", ITyp) -> ITyp
        | ("!", BTyp) -> BTyp
        | _           -> failwith "illegal/illtyped monadic expression" 
   
    and tcDyadic gtenv ltenv f e1 e2 = 
        match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
        | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+"; "-"; "*"]                    -> ITyp
        | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["="; "<>"; "<"; "<="; ">"; ">="]  -> BTyp
        | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"="]                         -> BTyp 
        | _ -> failwith("illegal/illtyped dyadic expression: " + f)

    and matchParams gtenv ltenv typs exps = 
        List.iter2 (fun typ exp -> 
            match typ with
            | PTyp t 
            | ATyp (t,_) when t = tcE gtenv ltenv exp -> ()
            | _        when typ = tcE gtenv ltenv exp -> ()
            | _ -> failwith "illtyped parameter"
            ) typs exps

    and tcNaryFunction gtenv ltenv f es = let (typs,topt) = findFunction f gtenv
                                          matchParams gtenv ltenv typs es 
                                          match topt with 
                                          | Some typ  -> typ
                                          | None      -> failwith "illtyped function"
 
    and tcNaryProcedure gtenv ltenv f es = let (typs,_) = findFunction f gtenv
                                           matchParams gtenv ltenv typs es 
      

/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
    and tcA gtenv ltenv = 
        function 
        | AVar x         -> match Map.tryFind x ltenv with
                            | None   -> match Map.tryFind x gtenv with
                                        | None   -> failwith ("no declaration for : " + x)
                                        | Some t -> t
                            | Some t -> t            
        | AIndex(acc, e) -> if tcE gtenv ltenv e <> ITyp 
                            then failwith "illtyped index access"
                            else tcA gtenv ltenv acc
        | ADeref e       -> tcE gtenv ltenv e

/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
    and tcS gtenv ltenv retOpt = function                           
        | PrintLn e        -> ignore(tcE gtenv ltenv e)

        | Ass(acc,e)       -> if tcA gtenv ltenv acc <> tcE gtenv ltenv e 
                              then failwith "illtyped assignment"

        | Block(decs,stms) -> List.iter (tcS gtenv (tcLDecs ltenv decs) retOpt) stms

        | Do (GC list)
        | Alt (GC list)    -> List.iter (fun (e,stms) -> 
                                if tcE gtenv ltenv e <> BTyp
                                then failwith "illtyped guarded command"
                                List.iter (tcS gtenv ltenv retOpt) stms
                                ) list

        | Return e         -> if retOpt <> Some(tcE gtenv ltenv e) 
                              then failwith("illtyped return: " + string e)

        | Call(f,es)       -> tcNaryProcedure gtenv ltenv f es
                                                                   
    and tcGDec gtenv = function  
        | VarDec(t,s) -> 
            match t with
            | ATyp(typ,_) | PTyp typ -> Map.add s typ gtenv
            | _                      -> Map.add s t gtenv

        | FunDec(topt,f, decs, stm) -> 
            let ltenv = tcLDecs Map.empty decs
            let typs = toTyps decs
            // Define new env prior to tcS for recursion 
            let newEnv = Map.add f (FTyp(typs,topt)) gtenv 
            tcS newEnv ltenv topt stm
            newEnv

    and tcGDecs gtenv = function
        | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
        | _         -> gtenv

    and tcLDec ltenv = function
        | VarDec(t,s) -> if Map.containsKey s ltenv
                         then failwith "tcLDec: duplicate parameter"
                         else match t with
                              | ATyp(typ,_) | PTyp typ -> Map.add s typ ltenv
                              | _                      -> Map.add s t ltenv
        | _           -> failwith "tcLDec: illegal declaration"
                                 
    and tcLDecs ltenv = function
        | dec::decs -> tcLDecs (tcGDec ltenv dec) decs
        | _         -> ltenv

/// tcP prog checks the well-typeness of a program prog
    and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                             List.iter (tcS gtenv Map.empty None) stms

  
