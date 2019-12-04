(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


NetChainToGraph[other___] := other;
NetChainToGraph[net_NetChain] := Block[
	{nets = Normal@net},
	NetGraph[nets,
		Rule @@@ Partition[Range@Length@nets, 2, 1],
		"Input" -> NetExtract[net, "Input"],
		"Output" -> NetExtract[net, "Output"]
	]
];



(* ::Subsubsection:: *)
(*Auxiliary Functions*)
