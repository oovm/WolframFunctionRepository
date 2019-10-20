(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


FactorialMod[n_Integer, p_Integer] := Block[
	{},
	If[n >= p, Return@0];
	Fold[Mod[#1 #2, p]&, NTT[n]]
];


(* ::Subsubsection:: *)
(*Main Function*)


NTT[n_] := If[
	IntegerQ@Sqrt[n - 1],
	m = Sqrt[n - 1];
	Table[Pochhammer[1 + i, m], {i, 0, m(m - 1), m}] ~ Join ~ {n},
	m = Floor[Sqrt[n]];
	Table[Pochhammer[1 + i, m], {i, 0, m(m - 1), m}] ~ Join ~ Range[1 + Floor[Sqrt[n]]^2, n]
];
