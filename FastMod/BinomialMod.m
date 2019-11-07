(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


BinomialMod[n_Integer, m_Integer, p_Integer] := Block[
	{},
	If[n < m, Return[0]];
	Which[
		PrimeQ@p, LucasTheorem[n, m, p],
		SquareFreeQ@p, SquareFreeMod[n, m, p],
		True, Mod[Binomial[n, m], p]
	]
];


(* ::Subsubsection:: *)
(*Main Function*)


LucasTheorem[a_, b_, p_?PrimeQ] := Block[
	{pn = IntegerDigits[#, p]& /@ {a, b}, sn},
	sn = PadLeft[#, Max[Length /@ pn]]& /@ pn;
	Mod[Times @@ Mod[Binomial @@@ Transpose@sn, p], p]
];
SquareFreeMod[a_, b_, p_?SquareFreeQ] := Block[
	{pp, xx},
	pp = FactorInteger[p][[All, 1]];
	xx = LucasTheorem[a, b, #]& /@ pp;
	ChineseRemainder[xx, pp]
];
