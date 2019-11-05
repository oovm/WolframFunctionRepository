(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)
broadcastingTable[h_, f_, arrays_, dims_, maxDims_] := Module[
	{inactive, tableVars = Table[Unique["i"], Length[maxDims]]},
	Prepend[
		inactive[h] @@ Transpose[{tableVars, maxDims}],
		inactive[f] @@ MapThread[
			inactive[Part][#1, Sequence @@ #2] &,
			{
				arrays,
				MapThread[
					If[#1 === 1, 1, #2] &,
					{#, PadLeft[tableVars, Length[#]]}
				] & /@ dims
			}
		]
	] /. inactive[x_] :> x
];
broadcasted::incompDims = "Objects with dimentions `1` can't be broadcasted.";
broadcasted[f_, lists__] := Module[
	{listOfLists, dims, dimColumns},
	listOfLists = {lists};
	dims = Dimensions /@ listOfLists;
	dimColumns = Transpose@PadLeft[dims, Automatic, 1];
	broadcastingTable[Table, f, listOfLists, dims, Max /@ dimColumns] /;
		If[MemberQ[dimColumns, dimCol_ /; ! SameQ @@ DeleteCases[dimCol, 1]],
			Message[broadcasted::incompDims, dims];
			False
			(* else *),
			True
		]
];

(* ::Subsubsection:: *)
(*Auxiliary Functions*)
