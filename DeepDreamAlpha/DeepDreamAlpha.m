(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Initialization*)


ClearAll["`*"];


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


Options[DeepDream\[Alpha]] = {
	"Depth" -> 24,
	"StepSize" -> 1,
	TargetDevice -> "CPU",
	WorkingPrecision -> "Real32"
};
DeepDream\[Alpha][img_Image, steps_Integer : 10, o : OptionsPattern[]] := Module[
	{
		VGG, net, res, i, $save,
		step = OptionValue["StepSize"], $start = AbsoluteTime[]
	},
	VGG = NetModel["ImageRestyleChoppedVGG16"];
	net = NetFlatten@NetChain[{VGG, SummationLayer[]}];
	If[
		0 < OptionValue[Depth] < 31,
		net = NetTake[net, {1, OptionValue[Depth]}],
		Return[]
	];
	net = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@img}]];

	i = 1;
	$save = {img};
	CheckAbort[
		PrintTemporary@GeneralUtilities`InformationPanel[
			"It doesn't look like anything to me.",
			{
				Center :> ProgressIndicator[i / Floor[steps / step]],
				"Method" :> "DeepDream \[Alpha]",
				"Elapsed" :> GeneralUtilities`TimeString[AbsoluteTime[] - $start],
				"ETA" :> GeneralUtilities`TimeString[Abs[1 - Floor[steps / step] / i](AbsoluteTime[] - $start)],

				Center :> Last@$save,
				Right :> GeneralUtilities`NiceButton["Awake!", Abort[]]
			},
			UpdateInterval -> 1,
			TrackedSymbols -> {i, $save}
		];
		While[
			i < Floor[steps / step],
			Check[
				AppendTo[$save, applyGradient[net, Last@$save, step, o]],
				Return[$save]
			];
			i++
		],
		Return[$save];
	];
	Return[$save]
];


(* ::Subsubsection:: *)
(*Auxiliary Function*)


Options[applyGradient] = Options[DeepDream\[Alpha]];
applyGradient[net_, img_, stepsize_, o : OptionsPattern[]] := Block[
	{imgt, gdimg, gddata, max, dim},
	gdimg = net[img,
		NetPortGradient["Input"],
		TargetDevice -> OptionValue@TargetDevice,
		WorkingPrecision -> OptionValue@WorkingPrecision
	];
	gddata = ImageData[NetDecoder["Image"][gdimg]];
	max = Max@Abs@gddata;
	Image@Clip[ImageData[img] + stepsize * gddata / max]
];
