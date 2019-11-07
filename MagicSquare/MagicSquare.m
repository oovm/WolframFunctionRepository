(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Magic::nosol = "No solution.";
MagicSquare[n_?Internal`PositiveIntegerQ] := Block[
	{},
	If[n == 2, Message[Magic::nosol];Return[]];
	If[OddQ@n, Return@MagicOdd2D[n]];
	If[Mod[n, 4] == 0, Return@MagicDoubleEven2D[n]];
	MagicEven2D[n]
];


(* ::Subsubsection:: *)
(*Main Functions*)


MagicOdd2D[n_] := Block[
	{p = Range[n]},
	Outer[Plus, p, p - (n + 3) / 2] ~ Mod ~ n * n + Outer[Plus, p, 2 p - 2] ~ Mod ~ n + 1
];
MagicEven2D[n_] := Block[
	{p, M, i, j, k},
	p = n / 2;
	M = MagicOdd2D[p];
	M = ArrayFlatten@{{M, M + 2 p^2}, {M + 3 p^2, M + p^2}};
	If[n == 2, Return[M]];
	i = Transpose@{Range@p};
	k = (n - 2) / 4;
	j = Range[k] ~ Join ~ Range[n - k + 2, n];
	M[[Flatten@{i, i + p}, j]] = M[[Flatten@{i + p, i}, j]];
	i = k + 1; j = {1, i};
	M[[Flatten@{i, i + p}, j]] = M[[Flatten@{i + p, i}, j]];
	Return@M
];
MagicDoubleEven2D[n_] := Block[
	{J, K1, M},
	J = Floor[(Range[n] ~ Mod ~ 4) / 2.0];
	K1 = Abs@Outer[Plus, J, -J] ~ BitXor ~ 1;
	M = Outer[Plus, Range[1, n^2, n], Range[0, n - 1]];
	M + K1 (n * n + 1 - 2 M)
] // Experimental`CompileEvaluate;


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
