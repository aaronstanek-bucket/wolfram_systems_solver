(* ::Package:: *)

(*This package needs SymbolicSystemsSolver to work*)
(*the Needs comand is not included because the author of this file has no idea what the user's file system looks like*)


oneDimensionalKinematics[solveFor_,known_]:=Do[Module[{int,motion,eq,ans},
(*known is a list of known variables*)
(*variables include:
xi,xf,xa,xd
vi,vf,va,vd
ti,tf,ta,td
a
*)
int=multipleTemplatedReplacements[{diff==term-start,ave==(term+start)/2},{{start->xi,term->xf,diff->xd,ave->xa},{start->vi,term->vf,diff->vd,ave->va},{start->ti,term->tf,diff->td,ave->ta}}];
motion={vd/td==a,xd==va td};
eq=Join[int,motion];
ans=solveInTermsOf[eq,solveFor,known];
Return[ans];
]]


twoDimensionalKinematics[solveFor_,known_]:=Do[Module[{int,motion,angle,eq,ans},
(*see 1 d case*)
int=multipleTemplatedReplacements[{diff==term-start,ave==(term+start)/2},{{start->xi,term->xf,diff->xd,ave->xa},{start->yi,term->yf,diff->yd,ave->ya},{start->vxi,term->vxf,diff->vxd,ave->vxa},{start->vyi,term->vyf,diff->vyd,ave->vya},{start->ti,term->tf,diff->td,ave->ta}}];
motion=multipleTemplatedReplacements[{vd/td==a,cod==va td},{{vd->vxd,a->ax,cod->xd,va->vxa},{vd->vyd,a->ay,cod->yd,va->vya}}];
angle=multipleTemplatedReplacements[{vhor==v Cos[ang],vvert==v Sin[ang]},{{vhor->vxi,vvert->vyi,v->vi,ang->anglei},{vhor->vxf,vvert->vyf,v->vf,ang->anglef}}];
eq=Join[int,motion,angle];
ans=solveInTermsOf[eq,solveFor,known];
Return[ans];
]]
