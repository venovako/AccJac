#!/usr/bin/env wolframscript -print all
If[Length[$ScriptCommandLine]<4,Quit[]];
p=ToExpression[$ScriptCommandLine[[2]]];
n=ToExpression[$ScriptCommandLine[[3]]];
b=ToExpression[$ScriptCommandLine[[4]]];
d=ToExpression[$ScriptCommandLine[[5]]];
p1=-p-1; (* due to rounding to nearest *)
p2=2^p1; (* \varepsilon *)
raz[x_]:=N[Ceiling[x*(10^d)]/(10^d),d+2];
dam[e_]:=If[b==1,1,(1-e)/(1+e)]; (* \delta_{\alpha}^- *)
dap[e_]:=If[b==1,1,(1+e)/(1-e)]; (* \delta_{\alpha}^+ *)
ToString[\[Delta]\[Alpha]]<>"-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[raz[N[FullSimplify[(1-dam[p2])/p2],n]]]<>ToString[\[Epsilon]]
ToString[\[Delta]\[Alpha]]<>"+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[raz[N[FullSimplify[(dap[p2]-1)/p2],n]]]<>ToString[\[Epsilon]]
d2m[e_]:=((1-e)^b)/(1+e); (* \delta_2^- *)
d2p[e_]:=((1+e)^b)/(1-e); (* \delta_2^+ *)
ToString[\[Delta]]<>"2-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[raz[N[FullSimplify[(1-d2m[p2])/p2],n]]]<>ToString[\[Epsilon]]
ToString[\[Delta]]<>"2+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[raz[N[FullSimplify[(d2p[p2]-1)/p2],n]]]<>ToString[\[Epsilon]]
dtm[e_]:=((1-e)^(b+2))/((1+e)^(b+3)); (* \delta_t^- *)
dtp[e_]:=((1+e)^(b+2))/((1-e)^(b+3)); (* \delta_t^+ *)
ToString[\[Delta]]<>"t-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[raz[N[FullSimplify[(1-dtm[p2])/p2],n]]]<>ToString[\[Epsilon]]
ToString[\[Delta]]<>"t+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[raz[N[FullSimplify[(dtp[p2]-1)/p2],n]]]<>ToString[\[Epsilon]]
dcm[e_]:=1; (* TODO: \delta_c^- *)
dcp[e_]:=1; (* TODO: \delta_c^+ *)
ToString[\[Delta]]<>"c-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[raz[N[FullSimplify[(1-dcm[p2])/p2],n]]]<>ToString[\[Epsilon]]
ToString[\[Delta]]<>"c+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[raz[N[FullSimplify[(dcp[p2]-1)/p2],n]]]<>ToString[\[Epsilon]]
dsm[e_]:=1; (* TODO: \delta_s^- *)
dsp[e_]:=1; (* TODO: \delta_s^+ *)
ToString[\[Delta]]<>"s-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[raz[N[FullSimplify[(1-dsm[p2])/p2],n]]]<>ToString[\[Epsilon]]
ToString[\[Delta]]<>"s+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[raz[N[FullSimplify[(dsp[p2]-1)/p2],n]]]<>ToString[\[Epsilon]]
dem[e_]:=1; (* TODO: \delta_{\mathbf{e}}^- *)
dep[e_]:=1; (* TODO: \delta_{\mathbf{e}}^+ *)
ToString[\[Delta]]<>"e-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1- "<>ToString[raz[N[FullSimplify[(1-dem[p2])/p2],n]]]<>ToString[\[Epsilon]]
ToString[\[Delta]]<>"e+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+ "<>ToString[raz[N[FullSimplify[(dep[p2]-1)/p2],n]]]<>ToString[\[Epsilon]]
