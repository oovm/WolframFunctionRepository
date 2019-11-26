(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[TupperK] = {};
TupperK[img_Image, o : OptionsPattern[]] := TupperK[img, 17, o];
TupperK[img_Image, p_?PrimeQ, o : OptionsPattern[]] := Module[
	{i, m, r},
	i = ImageResize[img, {Automatic, p}];
	m = Map[Boole[Max@# < 1]&, ImageData[i], {2}];
	r = p * FromDigits[Flatten[Reverse@Transpose[m]], 2]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)

