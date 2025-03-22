#!/usr/bin/env wolframscript -print all
If[Length[$ScriptCommandLine]<4,Quit[]];
p=FullSimplify[ToExpression[$ScriptCommandLine[[2]]]];
n=FullSimplify[ToExpression[$ScriptCommandLine[[3]]]];
d=FullSimplify[ToExpression[$ScriptCommandLine[[4]]]];
pm=FullSimplify[-p]; (* due to rounding to nearest *)
ep=FullSimplify[2^pm]; (* \varepsilon *)
raz[x_]:=Block[{f=Ceiling[x*(10^d)]/(10^d)},If[f<10," "<>ToString[N[f,d+1]],ToString[N[f,d+2]]]<>ToString[\[Epsilon]]];
ToString[\[Epsilon]]<>"=2^"<>ToString[pm]<>"; n="<>ToString[n]<>"; d="<>ToString[d]
(* TODO *)
