(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)

(* ::Subsubsection:: *)
(*Main Function*)
LightsOutSolve[in : EventHandler[___], o : OptionsPattern[]] := Block[
	{take = in[[1, 1, 1, -1]]},
	take = take[[1, 1, 1, 1, 1, 5]];
	Transpose[Reverse /@ take] // ArrayPlot
]
game = LightsOutGame[5]
game // LightsOutSolve
