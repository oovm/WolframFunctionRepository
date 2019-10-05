(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


ClearAll["`*"];
Options[DeepDream\[Alpha]] = {
	"Depth" -> 24,
	"StepSize" -> 1,
	"LastStepOnly" -> True,
	TargetDevice -> "CPU",
	WorkingPrecision -> "Real32"
};
DeepDream\[Alpha][img_Image, steps_Integer : 10, o : OptionsPattern[]] := Module[
	{VGG, net, res, step = OptionValue["StepSize"], $start = AbsoluteTime[]},
	VGG = NetModel["ImageRestyleChoppedVGG16"];
	net = NetFlatten@NetChain[{VGG, SummationLayer[]}];
	If[
		0 < OptionValue[Depth] < 31,
		net = NetTake[net, {1, OptionValue[Depth]}],
		Return[]
	];
	net = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@img}]];
	Check[
		res = NestList[applyGradient[net, #, step, o]&, img, Floor[steps / step]],
		Return[$Failed]
	];
	If[OptionValue["LastStepOnly"], Return[Last@res], Return[res]]
];


(* ::Subsubsection:: *)
(*Auxiliary Function*)


Options[applyGradient] = Options[DeepDream\[Alpha]];
applyGradient[net_, img_, stepsize_, o : OptionsPattern[]] := Module[
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
