(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


distance = Compile[
	{{pt, _Real, 1}, {centers, _Real, 2}, {radii, _Real, 1}},
	(Sqrt[Abs[First@# - First@pt]^2 + Abs[Last@# - Last@pt]^2] & /@ centers) - radii,
	Parallelization -> True,
	CompilationTarget -> "C",
	RuntimeOptions -> "Speed"
]


max = 8;(*largest disk radius*)
min = 2;(*smallest disk radius*)
pad = 1;(*padding between disks*)
color1 = ColorData["Aquamarine"];
color2 = ColorData["SunsetColors"];
timeconstraint = 2;
background = 12;

shape = Binarize@ColorNegate@ImageResize[Rasterize@Style["GGG", Bold, FontFamily -> "Arial", FontSize -> 300], Scaled[1 / 2]]



centers = radii = colors = {};
Module[
	{dim, dt, pt, m, d, r},
	dim = ImageDimensions[shape];
	dt = DistanceTransform[shape];
	TimeConstrained[
		While[True,
			While[
				While[
					pt = RandomReal[{1, #}] & /@ (2 dim);
					(m = If[Norm[pt - dim] < 200, background, 0] + If[pt[[1]] < dim[[1]] 3 / 2 && pt[[1]] > dim[[1]] / 2 && pt[[2]] < dim[[2]]3 / 2 && pt[[2]] > dim[[2]] / 2, ImageValue[dt, pt - dim / 2], 0]) < min
				];
				(d = Min[distance[pt, centers, radii]] - pad) < min
			];
			r = Min[max, d, m ];
			centers = Join[centers, {pt}];
			radii = Join[radii, {r}];
			colors = Join[colors, {Blend[{color2@RandomReal[{0.4, 0.7}], color1@RandomReal[{0.4, 0.7}]}, Piecewise[{{1 / max * (m - background), m < background + max / 2}, {1, m >= background + max / 2}}]]}]
		],
		timeconstraint
	]
]
(*Min[Length/@{colors, centers, radii}]*)
Graphics@MapThread[{#1, Disk[#2, #3]} &, GeneralUtilities`TrimRight[{colors, centers, radii}]]


(* ::Input:: *)
(*background*)


(* ::Input:: *)
(*RLE*)
