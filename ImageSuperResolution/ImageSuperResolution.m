(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[ImageSuperResolution] = {
	Method -> "VDSR",
	PerformanceGoal -> Automatic,
	TargetDevice -> "CPU",
	"SizeCheck" -> False
};
ImageSuperResolution[image_Image, scale_ : 2, o : OptionsPattern[]] := Block[
	{img, result},
	If[scale <= 0, Return[]];
	If[scale <= 1, Return[ImageResize[image, Scaled[scale], Resampling -> "Cubic"]]];
	img = Switch[
		Length@ColorSeparate[image],
		3, image,
		4, RemoveAlphaChannel[image],
		1, ColorConvert[image, "RGB"]
	];
	result = Switch[
		OptionValue[Method],
		"VDSR", evaluator$VDSR[img, scale, o],
		"ESRGAN", evaluator$ESRGAN[img, scale, o],
		_, Null
	];
	If[
		OptionValue["SizeCheck"],
		ImageResize[result, scale * ImageDimensions@image],
		result
	]
];


(* ::Subsubsection:: *)
(*Main Functions*)


evaluator$VDSR[i_, s_, o : OptionsPattern[]] := Block[
	{net, interpolated, ycbcr, channels, resizedNet, diff, rgb},
	net = NetModel["Very Deep Net for Super-Resolution"];
	interpolated = ImageResize[i, Scaled[s], Resampling -> "Cubic"];
	(*upscale to the final size*)
	ycbcr = ImageApply[{{0.257, 0.504, 0.098}, {-0.148, -0.291, 0.439}, {0.439, -0.368, -0.071}}.# + {0.063, 0.502, 0.502}&, interpolated];
	channels = ColorSeparate[ycbcr];
	resizedNet = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@interpolated, ColorSpace -> "Grayscale"}]];
	diff = Image@resizedNet[channels[[1]], TargetDevice -> OptionValue[TargetDevice]];
	ycbcr = ColorCombine[{channels[[1]] + diff, channels[[2]], channels[[3]]}];
	rgb = ImageApply[{{1.164, 0., 1.596}, {1.164, -0.392, -0.813}, {1.164, 2.017, 0.}}.# + {-0.874, 0.532, -1.086}&, ycbcr];
	rgb
];


Options[ImageSuperResolution] = Options[ImageSuperResolution];
evaluator$ESRGAN[i_, s_, o : OptionsPattern[]] := Block[
	{net, resizedNet, resizeScale, goal, rgb, numResizes},
	net = NetModel["Enhanced Super-Resolution GAN Trained on DIV2K, Flickr2K and OST Data"];
	goal = OptionValue[PerformanceGoal];
	If[goal === Automatic && s <= 4, goal = "Quality"];
	If[goal === Automatic && s > 4, goal = "Speed"];
	numResizes = Ceiling[Log[4, s]];
	resizeScale = s / (4^numResizes);
	rgb = i;
	Do[
		resizedNet = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@rgb}]];
		rgb = resizedNet[rgb, TargetDevice -> OptionValue[TargetDevice]],
		numResizes - 1
	];
	If[goal == "Speed", rgb = ImageResize[rgb, Scaled[resizeScale]]];
	resizedNet = NetReplacePart[net, "Input" -> NetEncoder[{"Image", ImageDimensions@rgb}]];
	rgb = resizedNet[rgb, TargetDevice -> OptionValue[TargetDevice]];
	If[goal == "Quality", rgb = ImageResize[rgb, Scaled[resizeScale]]];
	rgb
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
