#!/usr/bin/env wolframscript -print all
If[Length[$ScriptCommandLine]<4,Quit[]];
p=FullSimplify[ToExpression[$ScriptCommandLine[[2]]]];
n=FullSimplify[ToExpression[$ScriptCommandLine[[3]]]];
d=FullSimplify[ToExpression[$ScriptCommandLine[[4]]]];
pm=FullSimplify[-p]; (* due to rounding to nearest *)
ep=FullSimplify[2^pm]; (* \varepsilon *)
raz[x_]:=Block[{f=Ceiling[x*(10^d)]/(10^d)},If[f<10," "<>ToString[N[f,d+1]],ToString[N[f,d+2]]]<>ToString[\[Epsilon]]];
ToString[\[Epsilon]]<>"=2^"<>ToString[pm]<>"; n="<>ToString[n]<>"; d="<>ToString[d]
d1m[e_]:=If[n<0,1,1-e];
ToString[\[Delta]]<>"1-=1-"<>raz[N[FullSimplify[(1-d1m[ep])/ep],Abs[n]]]
d1p[e_]:=If[n<0,1,1+e];
ToString[\[Delta]]<>"1+=1+"<>raz[N[FullSimplify[(d1p[ep]-1)/ep],Abs[n]]]
ddm[e_]:=(d1m[e]/(1+e))*(1-e);
ToString[\[Delta]]<>"d-=1-"<>raz[N[FullSimplify[(1-ddm[ep])/ep],Abs[n]]]
ddp[e_]:=(d1p[e]/(1-e))*(1+e);
ToString[\[Delta]]<>"1+=1+"<>raz[N[FullSimplify[(ddp[ep]-1)/ep],Abs[n]]]
(* TODO *)
