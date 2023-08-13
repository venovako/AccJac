#!/usr/bin/env wolframscript -print all
If[Length[$ScriptCommandLine]<4,Quit[]];
p=ToExpression[$ScriptCommandLine[[2]]];
n=ToExpression[$ScriptCommandLine[[3]]];
b=ToExpression[$ScriptCommandLine[[4]]];
p1=-p-1; (* due to rounding to nearest *)
p2=2^p1; (* \varepsilon *)
dam[e_]:=If[b==1,1,(1-e)/(1+e)]; (* \delta_{\alpha}^- *)
dap[e_]:=If[b==1,1,(1+e)/(1-e)]; (* \delta_{\alpha}^+ *)
"dam["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[N[FullSimplify[(1-dam[p2])/p2],n]]<>ToString[\[Epsilon]]
"dap["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[N[FullSimplify[(dap[p2]-1)/p2],n]]<>ToString[\[Epsilon]]
d2m[e_]:=((1-e)^b)/(1+e); (* \delta_2^- *)
d2p[e_]:=((1+e)^b)/(1-e); (* \delta_2^+ *)
"d2m["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[N[FullSimplify[(1-d2m[p2])/p2],n]]<>ToString[\[Epsilon]]
"d2p["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[N[FullSimplify[(d2p[p2]-1)/p2],n]]<>ToString[\[Epsilon]]
dtm[e_]:=((1-e)^(b+2))/((1+e)^(b+3)); (* \delta_{\varphi}^- *)
dtp[e_]:=((1+e)^(b+2))/((1-e)^(b+3)); (* \delta_{\varphi}^+ *)
"dtm["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[N[FullSimplify[(1-dtm[p2])/p2],n]]<>ToString[\[Epsilon]]
"dtp["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[N[FullSimplify[(dtp[p2]-1)/p2],n]]<>ToString[\[Epsilon]]
