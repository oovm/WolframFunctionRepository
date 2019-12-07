(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


RandomSplit[in_List, n_?Internal`PositiveIntegerQ] := Block[
	{l = Length@in, s, p},
	If[
		l < 1000,
		s = RandomSample[Range[1, l - 1], n - 1],
		s = DeleteDuplicates@RandomInteger[l - 1, n - 1];
		While[
			Length@s < n,
			AppendTo[s, getDifferent[s, l - 1]]
		]
	];
	p = Differences@Join[{0}, Sort@s, {l}];
	Internal`PartitionRagged[in, p]
];


(* ::Subsection:: *)
(*Auxiliary Functions*)


getDifferent[l_List, max_Integer] := Block[
	{r = RandomInteger[{1, max}]},
	While[
		MemberQ[l, r],
		r = RandomInteger[{1, max}]
	];
	Return@r
];
