(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
OrthogonalSeries[expr_, {var_, {min_, max_}, order_}] := Block[
	{U, inner},
	If[min >= max, Return[]];
	inner[f_, g_] := Integrate[f * g, {x, min, max}];
	U = Orthogonalize[var^Range[0, order], inner];
	SeriesData[var, 0, CoefficientList[Tr[inner[expr, #]#& /@ U], var], 0, order + 1, 1]
];


(* ::Subsection:: *)
(*Auxiliary Functions*)
