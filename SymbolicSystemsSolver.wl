(* ::Package:: *)

solveInTermsOfRaw[equs_,solveFor_,with_,allVars_]:=Do[Module[{elim,equs2,ans},
elim=Complement[allVars,with]; (*elim is the set of variables that need to be eliinated*)
equs2=Eliminate[equs,elim]; (*removes the variables that we don't want*)
ans=Solve[equs2,solveFor]; (*finds the solution*)
Return[FullSimplify[ans]];
]]


extractVariablesFrom[z_]:=Do[Module[{h,u},
h=ToString[Head[z]];
If[h!="Symbol",Goto["iterate"]];
(*it's safe to assume that this is an expression*)
Return[Variables[z]];
(*now for the fun stuff*)
Label["iterate"];
u={};
Do[u=Union[u,extractVariablesFrom[z[[i]]]],{i,Length[z]}];
Return[u];
]]


solveInTermsOf[equs_,solveFor_,known_]:=Do[Module[{w,v},
w=Join[known,List[solveFor]]; (*w becomes with*)
v=extractVariablesFrom[List[equs,solveFor,known]]; (*produce a list v that names all variables*)
Return[solveInTermsOfRaw[equs,solveFor,w,v]];
]]


(*
Usage notes:
solveInTermsOf this the interface
it is passed 3 arguments
the first is a collection of equations (this can be in the form of a comma separated list, or using && notation)
the second is a variable that the user wishes to solve for
the third is those variables that the user wishes the answer to be written in terms of
*)


useTemplatedEquation[base_,reps_]:=Do[Module[{ou},
If[Head[base]==Symbol,Goto["sym"]];
(*if it's not a symbol, run this code*)
If[Length[base]==0,Return[base]];
ou={};
Do[ou=Join[ou,List[useTemplatedEquation[base[[i]],reps]]],{i,Length[base]}];
ou=Apply[Head[base],ou];
Return[ou];
(*nothing crosses this line*)
Label["sym"];
(*if it is a symbol, run this code*)
ou=Replace[base,reps];
Return[ou];
]]


multipleTemplatedEquations[bases_,reps_]:=Do[Module[{ou,n},
(*bases is a list*)
If[Length[bases]==0,Return[{}]];
ou={};
Do[ou=Join[ou,List[useTemplatedEquation[bases[[i]],reps]]],{i,Length[bases]}];
Return[ou];
]]


multipleTemplatedReplacements[bases_,repList_]:=Do[Module[{ou},
(*implements multipleTemplatedEquations where repList is now a list of replacement sets*)
ou={};
Do[ou=Join[ou,multipleTemplatedEquations[bases,repList[[i]]]],{i,Length[repList]}];
Return[ou];
]]
