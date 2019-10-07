(* ::Package:: *)

(* ::Subsection:: *)
(*Bitmap*)


(*img=ImageResize[ExampleData[{"TestImage","Lena"}],256]*)
img = Rasterize[Graphics@Circle[]];
k = 10;
maxLine = 999999;


radon = Radon[ColorNegate@ColorConvert[img, "Grayscale"]];
{w, h} = ImageDimensions[radon];
halfL = Table[N@Sin[Pi i / h], {i, 0, h - 1}, {j, 0, w - 1}];
invRadon = Image@Chop@InverseFourier[halfL Fourier[ImageData[radon]]]
lines = ImageApply[With[{p = Clip[k #, {0, 1}]}, RandomChoice[{1 - p, p} -> {0, 1}]]&, invRadon]
selected = RandomChoice[
	PixelValuePositions[lines, White],
	Min[Length@ImageValuePositions[lines, White], maxLine]
];
less = ImageRotate[Image@SparseArray[# -> 1& /@ selected, ImageDimensions@lines], Pi / 2];
ColorNegate@InverseRadon[less, ImageDimensions[img], Method -> None]
ColorNegate@ImageAdjust[InverseRadon[less, ImageDimensions[img], Method -> None], 0, {0, k}]


(* ::Subsection:: *)
(*Vector *)


img = Rasterize[Graphics@Circle[]];
k = 10;


radon = Radon[ColorNegate@ColorConvert[img, "Grayscale"]];
{w, h} = ImageDimensions[radon];
halfL = Table[N@Sin[Pi i / h], {i, 0, h - 1}, {j, 0, w - 1}];
(invRadon = Image@Chop@InverseFourier[halfL Fourier[ImageData[radon]]]) * 100
lines = ImageApply[With[{p = Clip[k #, {0, 1}]}, RandomChoice[{1 - p, p} -> {0, 1}]]&, invRadon]

{h, w} = ImageDimensions@lines;
point2line[{x_, y_}] := Block[
	{d, th},
	d = Rescale[y, {0, h}, {-Norm[{w, h}] / 2., Norm[{w, h}] / 2.}];
	th = Rescale[x, {0, w}, {-Pi / 2., Pi / 2.}];
	InfiniteLine[2 d {Cos[th], Sin[th]}, {Cos[Pi / 2 + th], Sin[Pi / 2 + th]}]
];
Graphics[
	Flatten@{Thickness[0.0001], point2line /@ PixelValuePositions[lines, White]},
	Frame -> True,
	AspectRatio -> ImageAspectRatio@img
]



