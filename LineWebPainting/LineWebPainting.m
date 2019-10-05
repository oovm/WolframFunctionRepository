(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


LineWebPainting[img_Image, k_ : 100] := Block[
	{radon, halfL, invRadon, lines, w, h},
	If[k == 0, Return[img]];
	radon = Radon[ColorNegate@ColorConvert[img, "Grayscale"]];
	{w, h} = ImageDimensions[radon];
	halfL = Table[N@Sin[Pi i / h], {i, 0, h - 1}, {j, 0, w - 1}];
	invRadon = Image@Chop@InverseFourier[halfL Fourier[ImageData[radon]]];
	lines = ImageApply[With[{p = Clip[k #, {0, 1}]}, RandomChoice[{1 - p, p} -> {0, 1}]]&, invRadon];
	ColorNegate@ImageAdjust[InverseRadon[lines, ImageDimensions[img], Method -> None], 0, {0, k}]
];
