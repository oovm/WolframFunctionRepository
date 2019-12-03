(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


Options[TupperDecipher] = {
	"Shift" -> Automatic,
	Mesh -> True,
	Frame -> True,
	FrameStyle -> Opacity[0],
	FrameTicksStyle -> Opacity[1],
	FrameTicks -> All
};
TupperDecipher[n_, o : OptionsPattern[]] := TupperDecipher[n, 17, o];
TupperDecipher[n_, p_?PrimeQ, o : OptionsPattern[]] := Block[
	{s, ws, pts},
	If[!IntegerQ[n / p], Message[Decrypt::failed];Return[]];
	s = If[
		Internal`PositiveIntegerQ@OptionValue["Shift"],
		OptionValue["Shift"],
		pts = IntegerDigits[n / p, 2];
		ws = First@FirstPosition[Reverse[Tr /@ Partition[pts, p]], _?(# != 0&)] - 1;
		Ceiling[Length@pts / p] + ws
	];
	ArrayPlot[
		Reverse /@ Transpose@Partition[IntegerDigits[n / p, 2, p * s], p],
		Mesh -> OptionValue[Mesh],
		Frame -> OptionValue[Frame],
		FrameStyle -> OptionValue[FrameStyle],
		FrameTicksStyle -> OptionValue[FrameTicksStyle],
		FrameTicks -> OptionValue[FrameTicks]
	]
]


(* ::Subsection:: *)
(*Auxiliary Functions*)
