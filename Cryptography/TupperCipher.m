(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[TupperK] = {Prime -> 17, Times -> 6};
TupperK[img_, OptionsPattern[]] := Module[
	{i, m, r, p},
	p = OptionValue[Prime];
	i = Rasterize[img, ImageSize -> {OptionValue[Times] p, p}];
	m = Map[Boole[Max@# < 1] &, ImageData[i], {2}];
	r = p FromDigits[Flatten[Reverse@Transpose[m]], 2]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)

