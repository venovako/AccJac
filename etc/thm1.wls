#!/usr/bin/env wolframscript -print all
If[Length[$ScriptCommandLine]<4,Quit[]];
p=ToExpression[$ScriptCommandLine[[2]]];
n=ToExpression[$ScriptCommandLine[[3]]];
b=ToExpression[$ScriptCommandLine[[4]]];
d=ToExpression[$ScriptCommandLine[[5]]];
p1=-p-1; (* due to rounding to nearest *)
p2=2^p1; (* \varepsilon *)
raz[x_]:=Block[{f=Ceiling[x*(10^d)]/(10^d)},If[f<10," "<>ToString[N[f,d+1]],ToString[N[f,d+2]]]<>ToString[\[Epsilon]]];
dam[e_]:=If[b==1,1,(1-e)/(1+e)]; (* \delta_{\alpha}^- *)
dap[e_]:=If[b==1,1,(1+e)/(1-e)]; (* \delta_{\alpha}^+ *)
ToString[\[Delta]\[Alpha]]<>"-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-dam[p2])/p2],n]]
ToString[\[Delta]\[Alpha]]<>"+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(dap[p2]-1)/p2],n]]
d2m[e_]:=((1-e)^b)/(1+e); (* \delta_2^- *)
d2p[e_]:=((1+e)^b)/(1-e); (* \delta_2^+ *)
ToString[\[Delta]]<>"2-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-d2m[p2])/p2],n]]
ToString[\[Delta]]<>"2+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(d2p[p2]-1)/p2],n]]
dtm[e_]:=((1-e)^(b+2))/((1+e)^(b+3)); (* \delta_t^- *)
dtp[e_]:=((1+e)^(b+2))/((1-e)^(b+3)); (* \delta_t^+ *)
ToString[\[Delta]]<>"t-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-dtm[p2])/p2],n]]
ToString[\[Delta]]<>"t+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(dtp[p2]-1)/p2],n]]
dqm[e_]:=dtm[e]*Sqrt[1-e]; (* \delta_q^- *)
dqp[e_]:=dtp[e]*Sqrt[1+e]; (* \delta_q^+ *)
ToString[\[Delta]]<>"q-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-dqm[p2])/p2],n]]
ToString[\[Delta]]<>"q+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(dqp[p2]-1)/p2],n]]
dcm[e_]:=(1-e)/dqp[e]; (* \delta_c^- *)
dcp[e_]:=(1+e)/dqm[e]; (* \delta_c^+ *)
ToString[\[Delta]]<>"c-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-dcm[p2])/p2],n]]
ToString[\[Delta]]<>"c+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(dcp[p2]-1)/p2],n]]
dsm[e_]:=dtm[e]*dcm[e]*(1-e); (* \delta_s^- *)
dsp[e_]:=dtp[e]*dcp[e]*(1+e); (* \delta_s^+ *)
ToString[\[Delta]]<>"s-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-dsm[p2])/p2],n]]
ToString[\[Delta]]<>"s+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(dsp[p2]-1)/p2],n]]
dem[e_]:=dam[e]*dsm[e]*If[b==1,1,1-e]; (* \delta_{21}^- *)
dep[e_]:=dap[e]*dsp[e]*If[b==1,1,1+e]; (* \delta_{21}^+ *)
ToString[\[Delta]]<>"e-["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1-"<>raz[N[FullSimplify[(1-dem[p2])/p2],n]]
ToString[\[Delta]]<>"e+["<>ToString[p1]<>","<>ToString[n]<>","<>ToString[b]<>"]=1+"<>raz[N[FullSimplify[(dep[p2]-1)/p2],n]]
