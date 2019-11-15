(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)
NetMisleading[net_, img_Image, o : OptionsPattern[]] := NetMisleading[net, img, Automatic, o]
NetMisleading[net_?ImageClassifyQ, i_Image, target_, o : OptionsPattern[]] := Block[
	{size, img, forward, trainedNet},
	size = NetExtract[net, {"Input", "ImageSize"}];
	img = ImageResize[RemoveAlphaChannel@ColorConvert[i, "RGB"], size];
	forward = NetChain[
		<|
			"image" -> ConstantArrayLayer["Array" -> ImageData[img, Interleaving -> False]],
			"encoder" -> ImageEncoderToLayer[NetExtract[net, "Input"]],
			"network" -> net
		|>,
		"Output" -> NetExtract[net, "Output"]
	];
	trainedNet = NetTrain[
		forward,
		<|"Output" -> {task}|>,
		LearningRateMultipliers -> {"image" -> 1, _ -> None},
		MaxTrainingRounds -> 64,
		TargetDevice -> "GPU"
	];
	Image[NetExtract[trainedNet, {"image", "Array"}], Interleaving -> False]
]

(* ::Subsubsection:: *)
(*Auxiliary Functions*)
