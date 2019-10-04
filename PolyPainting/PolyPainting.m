(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


PolyPainting[img_, n_ : 1000] := Block[
	{x, y, gr, pt},
	{x, y} = ImageDimensions[img];
	gr = ListDensityPlot[
		Transpose@{RandomReal[x, n], RandomReal[y, n], RandomReal[1, n]},
		InterpolationOrder -> 0, Frame -> False, Mesh -> All,
		AspectRatio -> Automatic
	];
	pt = Polygon[#[[1]]]& /@ Cases[Normal@gr, _Polygon, Infinity];
	Graphics[With[
		{col = RGBColor@ImageValue[img, Mean @@ #]},
		{EdgeForm@col, col, #}]& /@ pt, AspectRatio -> ImageAspectRatio[img]
	]
];
(*PolyPainting[\:56fe\:7247,{\:7cbe\:7ec6\:5ea6,\:7578\:53d8\:5ea6,\:7f51\:683c}]*)
PolyPainting[img_, {n_, m_ : 1, mesh_ : None}] := Block[
	{im, pts, dat},
	im = ImageAdjust[ImageResize[img, n]];
	dat = Apply[RGBColor, Flatten[Transpose[Reverse[ImageData[im]]], 1], {1}];
	pts = Flatten[Table[{x, y} + RandomReal[m{-1, 1}], {x, 1, n}, {y, 1, n}], 1];
	ListDensityPlot[
		Flatten /@ Transpose[{pts, Range[1, n^2]}],
		InterpolationOrder -> 0, Mesh -> mesh,
		MeshStyle -> Thickness[Small], Frame -> False,
		ColorFunction -> dat
	]
];
