(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)

Options[RandomColorBlindTest] = {

};
RandomColorBlindTest[o : OptionsPattern[]] := {};


max = 8;
min = 2;
pad = 1;
background = 12;
shape = Binarize@ColorNegate@ImageResize[
	Rasterize@Style[
		"GGG",
		Bold,
		FontFamily -> "Arial",
		FontSize -> 300
	],
	Scaled[1 / 2]
];
generator[shape, max, min, pad, background]

(* ::Subsubsection:: *)
(*Main Function*)


getDistance := getDistance = Compile[
	{{pt, _Real, 1}, {centers, _Real, 2}, {radii, _Real, 1}},
	(Sqrt[Abs[First@# - First@pt]^2 + Abs[Last@# - Last@pt]^2] & /@ centers) - radii,
	Parallelization -> True,
	CompilationTarget -> "C",
	RuntimeOptions -> "Speed"
];


Options[generator] = Options[RandomColorBlindTest];
generator[shape_, max_, min_, pad_, background_, o : OptionsPattern[]] := Block[
	{
		centers , radii , colors, color1, color2,
		time, shape, dim, dt, pt, m, d, r
	},
	centers = radii = colors = {};
	color1 = ColorData["Aquamarine"];
	color2 = ColorData["SunsetColors"];
	time = 2;
	dim = ImageDimensions[shape];
	dt = DistanceTransform[shape];
	TimeConstrained[
		While[True,
			While[
				While[
					pt = RandomReal[{1, #}] & /@ (2 dim);
					(m = If[Norm[pt - dim] < 200, background, 0] + If[pt[[1]] < dim[[1]] 3 / 2 && pt[[1]] > dim[[1]] / 2 && pt[[2]] < dim[[2]]3 / 2 && pt[[2]] > dim[[2]] / 2, ImageValue[dt, pt - dim / 2], 0]) < min
				];
				(d = Min[getDistance[pt, centers, radii]] - pad) < min
			];
			r = Min[max, d, m ];
			centers = Join[centers, {pt}];
			radii = Join[radii, {r}];
			colors = Join[colors, {Blend[{color2@RandomReal[{0.4, 0.7}], color1@RandomReal[{0.4, 0.7}]}, Piecewise[{{1 / max * (m - background), m < background + max / 2}, {1, m >= background + max / 2}}]]}]
		],
		time
	];
	(*Min[Length/@{colors, centers, radii}]*)
	Graphics@MapThread[{#1, Disk[#2, #3]} &, GeneralUtilities`TrimRight[{colors, centers, radii}]]
]
