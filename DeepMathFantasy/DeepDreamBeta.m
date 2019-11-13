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


Options[DeepDreamBeta] = {
	"StepSize" -> 1,
	"Activation" -> Identity,
	"Eyes" -> 2,
	"Zoom" -> 1, (*Discard, useless now*)
	Resampling -> "Cubic",
	TargetDevice -> "CPU",
	WorkingPrecision -> "Real32"
};
DeepDreamBeta[net_, image_, steps_Integer : 10, o : OptionsPattern[]] := Module[
	{
		i, $save, zooms, resize, pt,
		step = OptionValue["StepSize"],
		$start = AbsoluteTime[]
	},
	i = 1;
	$save = image;
	CheckAbort[
		pt = PrintTemporary@GeneralUtilities`InformationPanel[
			"It doesn't look like anything to me.",
			{
				Center :> ProgressIndicator[i / Floor[steps / step]],
				"Method" :> "DeepDream \[Beta]",
				"Elapsed" :> GeneralUtilities`TimeString[AbsoluteTime[] - $start],
				"ETA" :> GeneralUtilities`TimeString[Abs[1 - Floor[steps / step] / i](AbsoluteTime[] - $start)],
				Center :> $save,
				Right :> GeneralUtilities`NiceButton["Awake!", Abort[]]
			},
			UpdateInterval -> 1,
			TrackedSymbols -> {i, $save}
		];
		While[
			i < Floor[steps / step],
			Check[
				zooms = ImagePyramid[$save][1 ;; OptionValue["Eyes"]];
				resize = ImageResize[applyGradient[net, #, 1, o], ImageDimensions[image], Resampling -> OptionValue[Resampling]]&;
				$save = Mean[resize /@ zooms],
				Return[$save]
			];
			i++
		],
		Return[$save];
	];
	NotebookDelete@pt;
	Return[$save]
];



(* ::Subsubsection:: *)
(*Auxiliary Function*)


Options[getGradient] = Options[DeepDreamBeta];
getGradient[net_, img_, o : OptionsPattern[]] := Block[
	{new, forward},
	new = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@img}]];
	forward = NetChain[{new, SummationLayer[]}];
	NetDecoder["Image"][forward[
		img,
		NetPortGradient["Input"],
		TargetDevice -> OptionValue[TargetDevice],
		WorkingPrecision -> OptionValue[WorkingPrecision]
	]]
];
Options[applyGradient] = Options[DeepDreamBeta];
applyGradient[net_, img_, step_, o : OptionsPattern[]] := Block[
	{gd = ImageData[getGradient[net, OptionValue["Activation" ][img], o]]},
	Image[Clip[ImageData[img] + step * gd / Max@Abs@gd]]
];
