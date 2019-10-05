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
	Resampling -> "Cubic",
	TargetDevice -> "CPU",
	WorkingPrecision -> "Real32"
}
DeepDreamBeta[net_, image_, steps_Integer : 10, o : OptionsPattern[]] := Module[
	{
		i, $save, zooms, resize, img = image,
		step = OptionValue["StepSize"], $start = AbsoluteTime[]
	},
	i = 1;
	$save = {image};
	CheckAbort[
		PrintTemporary@GeneralUtilities`InformationPanel[
			"It doesn't look like anything to me.",
			{
				Center :> ProgressIndicator[i / Floor[steps / step]],
				"Method" :> "DeepDream \[Beta]",
				"Elapsed" :> GeneralUtilities`TimeString[AbsoluteTime[] - $start],
				"ETA" :> GeneralUtilities`TimeString[Abs[1 - Floor[steps / step] / i](AbsoluteTime[] - $start)],
				Center :> img,
				Right :> GeneralUtilities`NiceButton["Awake!", Abort[]]
			},
			UpdateInterval -> 1,
			TrackedSymbols -> {i, img}
		];
		While[
			i < Floor[steps / step],
			Check[
				img = Last@$save;
				zooms = ImagePyramid[If[True, img, ImageTake[img, {2, -2}, {2, -2}]]][1 ;; OptionValue["Eyes"]];
				resize = ImageResize[applyGradient[net, #, 1, o], ImageDimensions[image], Resampling -> OptionValue[Resampling]]&;
				AppendTo[$save, Mean[resize /@ zooms]],
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


getGradient[net_, img_] := Block[
	{new, forward},
	new = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@img}]];
	forward = NetChain[{new, SummationLayer[]}];
	NetDecoder["Image"][forward[img, NetPortGradient["Input"], TargetDevice -> "GPU"]]
];
Options[applyGradient] = Options[DeepDreamBeta];
applyGradient[net_, img_, stepsize_, o : OptionsPattern[]] := Block[
	{imgt, gdimg, gddata, max, dim},
	gdimg = getGradient[net, img];
	gddata = ImageData[gdimg];
	max = Max@Abs@gddata;
	Image[Clip[ImageData[img] + stepsize * gddata / max]]
];

