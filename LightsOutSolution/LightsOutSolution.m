(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


ClearAll[LightsOutSolution];
SetAttributes[LightsOutSolution, HoldFirst];
$OptionsDefault = {
	Unique -> True,
	Mesh -> All,
	RawData -> False,
	Frame -> False
};
Options[LightsOutSolution] = DeleteDuplicatesBy[Join[$OptionsDefault, Options[ArrayPlot]], First];


LightsOutSolution[n_Integer, o : OptionsPattern[]] := Block[
	{sols},
	If[n <= 0, Return[]];
	sols = If[
		TrueQ@OptionValue@Unique,
		essentialSolutions[n],
		Partition[#, n]& /@ allSolutions[n]
	];
	If[TrueQ@OptionValue@RawData, Return[sols]];
	With[
		{opts = Sequence @@ FilterRules[DeleteDuplicatesBy[Flatten@{o, $OptionsDefault}, First], Options[ArrayPlot]]},
		ArrayPlot[#, opts]& /@ sols
	]
];


LightsOutSolution /: Length[LightsOutSolution[n_, OptionsPattern[]]] := Block[
	{},
	If[n <= 0, Return[]];
	If[n > Length@$FastLength, Message[General::ovfl];Return[]];
	Return[$FastLength[[n]]]
];
$FastLength := $FastLength = Last /@ Import["http://oeis.org/A075463/b075463.txt", "Data"];


(* ::Subsection:: *)
(*LightsOutSolution*)


(* ::Subsubsection:: *)
(*All Solutions*)


a[n_, i_, j_] := Table[If[Total[Abs[{i, j} - {r, s}]] <= 1, 1, 0], {r, n}, {s, n}] // Flatten;
a[n_, k_] := a[n, Quotient[k + n - 1, n], Mod[k, n, 1]];
m[n_] := a[n, #]& /@ Range[n^2];
ker[n_] := NullSpace[m[n], Modulus -> 2];
b[n_] := Table[1, {n^2}];
sol[n_] := LinearSolve[m[n], b[n], Modulus -> 2];


allSolutions[n_] := Module[
	{s = sol[n], k = ker[n]},
	Mod[(s + #)& /@ (Total[(# * k)]& /@ Tuples[{0, 1}, Length[k]]), 2]
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
