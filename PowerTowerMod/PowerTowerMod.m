(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


(*\:8986\:5199\:4f18\:5316\:5e42\:6a21\:51fd\:6570*)
powerMod[x_, y_, z_] := 1 /; x == 1 || z == 1;
powerMod[x_, y_, z_] := PowerMod[x, y, z]


PowerTowerMod[{a_}, n_] := Mod[a, n];
PowerTowerMod[{a_, b_}, n_] := powerMod[a, b, n];
PowerTowerMod[list_?VectorQ, n_] := Block[
	{a = First@list, bc = Rest@list, g, d, f},
	g = GCD[a, n];
	d = a / g;
	f = n / g;
	If[a == 1 || n == 1, Return[1]];
	If[g == 1, Return[powerMod[a, PowerTowerMod[bc, EulerPhi[n]], n]]];
	Mod[g * Mod[powerMod[g, Mod[(PowerTowerMod[bc, EulerPhi[f]] - 1), EulerPhi[f]], f] * powerMod[d, PowerTowerMod[bc, EulerPhi[f]], f], f], n]
];
