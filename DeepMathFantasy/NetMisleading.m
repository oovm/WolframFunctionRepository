(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)
NetMisleading[net_, img_, o : OptionsPattern[]] := NetMisleading[net, img, Automatic, o];
NetMisleading[net_?ImageClassifyQ, i_Image, task_, o : OptionsPattern[]] := Block[
	{encoder, decoder, target, img, forward, trainedNet},
	encoder = NetExtract[net, "Input"];
	decoder = NetExtract[net, "Output"];
	target = If[
		task === Automatic,
		RandomChoice@decoder[["Labels"]],
		If[
			!MemberQ[decoder[["Labels"]], task],
			Return[Missing["NoSuchLabel"]],
			task
		]
	];
	img = ImageResize[RemoveAlphaChannel@ColorConvert[i, "RGB"], encoder[["ImageSize"]]];
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
		<|"Output" -> {target}|>,
		LearningRateMultipliers -> {"image" -> 1, _ -> None},
		MaxTrainingRounds -> 64,
		TargetDevice -> "GPU"
	];
	Image[NetExtract[trainedNet, {"image", "Array"}], Interleaving -> False]
];

(* ::Subsubsection:: *)
(*Auxiliary Functions*)
ImageClassifyQ[net_] := Block[
	{encoder, decoder, type},
	If[!NeuralNetworks`ValidNetQ@net, Return[False]];
	encoder = NetExtract[net, "Input"];
	type = encoder /. HoldPattern[NetEncoder[kind_, __]] :> kind ;
	If[type != "Image", Return[False]];
	decoder = NetExtract[net, "Output"];
	type = encoder /. HoldPattern[NetEncoder[kind_, __]] :> kind ;
	If[type != "Class", Return[False]];
	Return[True]
];
