(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[TupperDecipher] = {
	"Shift" -> Automatic
};
TupperDecipher[n_, o : OptionsPattern[]] := TupperDecipher[n, 17, o];
TupperDecipher[n_, p_?PrimeQ, o : OptionsPattern[]] := Module[
	{bin, t},
	t = Max[Ceiling@OptionValue["Shift"], 1];
	bin = IntegerDigits[n / p, 2, p * Ceiling[IntegerLength[n / p, 2] / p]];
	ArrayPlot[
		Reverse /@ Transpose@Partition[bin, p]
	]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)


