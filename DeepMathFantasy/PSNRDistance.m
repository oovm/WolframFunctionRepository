(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


PSNRDistance::size = "Input size `1` does not match `2` of the target size";
PSNRDistance[target_] := PSNRDistance[#, target]&;
PSNRDistance[input_, target_] := Block[
	{a = normalize@input, b = normalize@target},
	If[input == target, Return[Infinity]];
	If[
		Dimensions@a != Dimensions@b,
		Message[PSNRDistance::size, Dimensions@a, Dimensions@b];
		Null,
		PSNR[a, b]
	]
];


(* ::Subsubsection:: *)
(*Main Functions*)


PSNR[a_, b_] := -10Log10[Mean@Flatten[(a - b)^2]];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


normalize[i_?StringQ] := Import@i;
normalize[i_?NumericArrayQ] := Normal@i;
normalize[i_?ImageQ] := ImageData@i;
normalize[i_] := i;
