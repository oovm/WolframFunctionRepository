(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


TupperCipher[text_String] := TupperCipher[text, 17];
TupperCipher[text_String, p_?PrimeQ] := TupperCipher[
	Rasterize[Text@text, ImageSize -> {Automatic, p}],
	p
];
TupperCipher[img_Image] := TupperCipher[img, 17];
TupperCipher[img_Image, p_?PrimeQ] := Block[
	{i, m},
	i = Binarize@ImageResize[img, {Automatic, p}];
	m = Map[Boole[Max@# < 1]&, ImageData[i], {2}];
	p * FromDigits[Flatten[Reverse@Transpose[m]], 2]
];


(* ::Subsection:: *)
(*Auxiliary Functions*)
