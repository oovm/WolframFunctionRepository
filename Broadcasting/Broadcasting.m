(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


SetAttributes[Broadcasting, HoldAll];
Broadcasting::dim = "Objects with dimensions `1` and `2` can't be broadcasted.";
Broadcasting[f_Symbol][ls__] := broadcasted[f, ls];
Broadcasting[f_Symbol[ls__]] := Switch[f,
	List, f[ls],
	_, broadcasted[f, ls]
];


(* ::Subsubsection:: *)
(*Main Functions*)


broadcastingTable[h_, f_, arrays_, dims_, maxDims_] := Block[
	{tableVars = Table[Unique["i"], Length[maxDims]]},
	Activate@Prepend[
		Inactive[h] @@ Transpose[{tableVars, maxDims}],
		Inactive[f] @@ MapThread[
			Inactive[Part][#1, Sequence @@ #2] &,
			{
				arrays,
				MapThread[If[#1 === 1, 1, #2] &, {#, PadLeft[tableVars, Length[#]]}] & /@ dims
			}
		]
	]
];
broadcasted[f_, lists__] := Block[
	{listOfLists, dims, dimColumns},
	listOfLists = {lists};
	dims = Dimensions /@ listOfLists;
	dimColumns = Transpose@PadLeft[dims, Automatic, 1];
	If[
		MemberQ[dimColumns, dimCol_ /; !SameQ @@ DeleteCases[dimCol, 1]],
		Message[Broadcasting::dim, First@dims, Last@dims];Return[],
		broadcastingTable[Table, f, listOfLists, dims, Max /@ dimColumns]
	]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
