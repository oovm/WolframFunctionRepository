(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[TupperPlot] = {Prime -> 17, Times -> 6};
TupperPlot[n_, OptionsPattern[]] := Module[
	{bin, p, t},
	p = OptionValue[Prime]; t = OptionValue[Times] ;
	bin = IntegerDigits[n / p, 2, t p^2 - 1];
	ArrayPlot[Reverse /@ Transpose@Partition[bin, p]]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)


