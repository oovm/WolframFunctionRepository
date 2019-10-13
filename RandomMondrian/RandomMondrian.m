(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[RandomMondrian] = {
	AspectRatio -> 0.618,
	MaxIterations -> 7,
	ImageSize -> Large,
	EdgeThickness -> Thickness[1 / 150],
	EdgeColor -> Black,
	"ColorMap" -> Automatic,
	"Grids" -> 20
};
RandomMondrian[o : OptionsPattern[]] := Black[
	{c, w, splitted},
	{c, w} = Transpose@If[
		SameQ[OptionValue["ColorMap"], Automatic],
		$DefaultColorMap,
		OptionValue["ColorMap"]
	];
	splitted = Nest[split, #, OptionValue[MaxIterations]]&@Graphics[
		{
			EdgeForm@OptionValue[{EdgeThickness, EdgeColor}],
			Rectangle[N@{0, 0}, N@{1, OptionValue[AspectRatio]}]
		},
		ImageSize -> OptionValue[ImageSize]
	];
	splitted
		/. r_Rectangle :> {RandomChoice[w -> c], r}
		/. x_Real :> Round[x, 1 / Round[OptionValue["Grids"]]]
];


(* ::Subsubsection:: *)
(*Main Function*)



$DefaultColorMap = {
	{RGBColor["#000000"], 1}(*Black*),
	{RGBColor["#878787"], 1}(*Gray*),
	{RGBColor["#194F9A"], 4}(*Blue*),
	{RGBColor["#BC0118"], 4}(*Red*),
	{RGBColor["#FACA02"], 4}(*Yellow*),
	{RGBColor["#FDFDFD"], 16}(*White*)
};






split = # /. d : Rectangle[{x1_, y1_}, {x2_, y2_}] :> With[
	{t = RandomReal@BetaDistribution[10, 10], r = RandomReal[]},
	Which[
		r < 0.3(x2 - x1) / (y2 - y1),
		{
			Rectangle[{x1, y1}, {x1 + (x2 - x1) t, y2}],
			Rectangle[{x1 + (x2 - x1) t, y1}, {x2, y2}]
		},
		1 - r < 0.5 (y2 - y1) / (x2 - x1),
		{
			Rectangle[{x1, y1}, {x2, y1 + (y2 - y1) t}],
			Rectangle[{x1, y1 + (y2 - y1) t}, {x2, y2}]
		},
		True,
		d
	]
]&;

