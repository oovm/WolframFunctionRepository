(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[TupperPlot] = {
	"Shift" -> 6
};
TupperPlot[n_, o : OptionsPattern[]] := TupperPlot[n, 17, o];
TupperPlot[n_, p_?PrimeQ, o : OptionsPattern[]] := Module[
	{bin, t},
	t = Max[Ceiling@OptionValue["Shift"], 1];
	Print[Length@IntegerDigits[n / p, 2]];
	bin = IntegerDigits[n / p, 2, p * Ceiling[IntegerLength[n / p, 2] / p]];
	ArrayPlot[Reverse /@ Transpose@Partition[bin, p]]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)


