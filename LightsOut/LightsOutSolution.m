(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


Options[LightsOutSolution] = {
	Method -> "Unique"
};
LightsOutSolution[n_Integer, o : OptionsPattern[]] := Block[
	{},
	If[n <= 0, Return[]];
	Switch[
		OptionValue@Method,
		"Unique", uniqueSolutions[n],
		"All", allSolutions[n],
		"AllCount", countAll[n],
		"Essential", essentialSolutions[n],
		"EssentialCount", countEssential[n],
		_, Return[]
	]
];


(* ::Subsection:: *)
(*Main Functions*)


(* ::Subsubsection:: *)
(*All Solutions*)


a[n_, i_, j_] := Table[If[Total[Abs[{i, j} - {r, s}]] <= 1, 1, 0], {r, n}, {s, n}] // Flatten;
a[n_, k_] := a[n, Quotient[k + n - 1, n], Mod[k, n, 1]];
m[n_] := a[n, #]& /@ Range[n^2];
ker[n_] := NullSpace[m[n], Modulus -> 2];
b[n_] := Table[1, {n^2}];
sol[n_] := LinearSolve[m[n], b[n], Modulus -> 2];


allSolutions[n_] := Block[
	{s = sol[n], k = ker[n], solutions},
	solutions = Mod[(s + #)& /@ (Total[(# * k)]& /@ Tuples[{0, 1}, Length[k]]), 2];
	Partition[#, n]& /@ solutions
];


(* ::Subsubsection:: *)
(*Essential Solutions*)


MatrixRotate[m_] := Transpose@Reverse[m];
MatrixRotate[m_, n_] := Nest[MatrixRotate, m, Mod[n, 4]];
DihedralOrbit[m_] := Union@Join[
	MatrixRotate[m, #]& /@ Range[0, 3],
	MatrixRotate[Reverse[m], #]& /@ Range[0, 3]
];
essentialSolutions[n_] := essentialSolutions[n] = Module[
	{as = Partition[#, n]& /@ allSolutions[n]},
	Union[as, SameTest -> (MemberQ[DihedralOrbit[#1], #2]&)]
];


(* ::Subsubsection:: *)
(*Count Solutions*)


countAll[n_Integer] := If[
	n > Length@$A075462,
	Missing[],
	$A075462[[n]]
];
countEssential[n_Integer] := If[
	n > Length@$A075463,
	Missing[],
	$A075463[[n]]
];
$A075462 := $A075462 = Last /@ Import["http://oeis.org/A075462/b075462.txt", "Data"];
$A075463 := $A075463 = Last /@ Import["http://oeis.org/A075463/b075463.txt", "Data"];


(* ::Subsubsection:: *)
(*Unique Solutions*)


uniqueSolutions[n_] := Block[
	{mat = AdjacencyMatrix[GridGraph[{n, n}]] + IdentityMatrix[n * n]},
	Partition[LinearSolve[mat, Table[1, {n * n}], Modulus -> 2], n]
];


(* ::Subsection:: *)
(*Auxiliary Functions*)
