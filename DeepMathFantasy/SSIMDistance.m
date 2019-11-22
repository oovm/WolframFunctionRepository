(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


SSIMDistance::size = "Input size `1` does not match `2` of the target size";
Options[SSIMDistance] = {
	"C1" -> 0.01^2, "C2" -> 0.03^2,
	"Window" -> GaussianMatrix[{{(11 - 1) / 2, (11 - 1) / 2}, 1.5}, Method -> "Gaussian"],
	"Blocks" -> 1
};
SSIMDistance[target_] := SSIMDistance[#, target]&;
SSIMDistance[input_, target_, o : OptionsPattern[]] := Block[
	{a = normalize@input, b = normalize@target},
	If[input == target, Return[Infinity]];
	If[
		Dimensions@a != Dimensions@b,
		Message[SSIMDistance::size, Dimensions@a, Dimensions@b];
		Null,
		SSIM[Image[a, Interleaving -> False], Image[b, Interleaving -> False], o]
	]
];


(* ::Subsubsection:: *)
(*Main Functions*)


Options[SSIM] = Options[SSIMDistance] ;
SSIM[img1_Image, img2_Image, o : OptionsPattern[]] := Block[
	{c1, c2, window, mx, my, vx, vy, cov, r},
	{c1, c2, window} = OptionValue[{"C1", "C2", "Window"}];
	mx = ImageCorrelate[img1, window, Padding -> None];
	my = ImageCorrelate[img2, window, Padding -> None];
	vx = ImageCorrelate[img1^2, window, Padding -> None] - mx^2;
	vy = ImageCorrelate[img2^2, window, Padding -> None] - my^2;
	cov = ImageCorrelate[img1 * img2, window, Padding -> None] - mx * my;
	r = (2mx * my + c1) / (mx^2 + my^2 + c1) * (2cov + c2) / (vx + vy + c2);
	Mean@ImageMeasurements[r, "Mean"]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


normalize[i_?StringQ] := Import@i;
normalize[i_?NumericArrayQ] := Normal@i;
normalize[i_?ImageQ] := ImageData@i;
normalize[i_] := i;
