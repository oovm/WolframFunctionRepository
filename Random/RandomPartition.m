(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


RandomPartition[l_List, max_] := RandomPartition[l, Array[1&, max] -> Range[1, max]];
RandomPartition[l_List, min_ ;; max_] := RandomPartition[l, Array[1&, max - min + 1] -> Range[min, max]];
RandomPartition[l_List, vList_List] := RandomPartition[l, Array[1&, Length@vList] -> vList];
RandomPartition[l_List, pList_List -> vList_List] := Block[
	{p = DoPartition[Length@l, vList, pList]},
	Internal`PartitionRagged[l, p]
];


(* ::Subsection:: *)
(*Auxiliary Functions*)


DoPartition[n_Integer, vList_, pList_] := Block[
	{l = {}, r},
	Do[
		r = RandomChoice[pList -> vList];
		Which[
			Tr@l + r < n, AppendTo[l, r],
			Tr@l + r == n, Return@Append[l, r],
			True, Null
		],
		{i, Ceiling[n / Min@vList]}
	];
	Append[l, n - Tr@l]
]
