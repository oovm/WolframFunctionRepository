(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[RandomColorBlindTest] = {
	TimeConstraint -> 2,
	FontSize -> 300,
	FontFamily -> "Arial",
	ColorFunction -> {
		ColorData["Aquamarine"],
		ColorData["SunsetColors"]
	}
};
RandomColorBlindTest[in_, o : OptionsPattern[]] := Block[
	{text, shape},
	text = Style[
		ToString@in, Bold,
		FontFamily -> OptionValue[FontFamily],
		FontSize -> OptionValue[FontSize]
	];
	shape = Binarize@ColorNegate@ImageResize[
		Rasterize@text,
		Scaled[1 / 2]
	];
	generator[shape, 8, 2, 1, 12, o]
];


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
		dim, dt, pt, m, d, r
	},
	centers = radii = colors = {};
	{color1, color2} = OptionValue[ColorFunction];
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
		OptionValue[TimeConstraint],
		(*Min[Length/@{colors, centers, radii}]*)
		Graphics@MapThread[{#1, Disk[#2, #3]} &, GeneralUtilities`TrimRight[{colors, centers, radii}]]
	];
]
