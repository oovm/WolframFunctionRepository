(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)


Options[NetMisleading] = {
	TargetDevice -> "CPU",
	MaxTrainingRounds -> 64,
	LearningRateMultipliers -> 1
};
NetMisleading[net_, img_, o : OptionsPattern[]] := NetMisleading[net, img, Automatic, o];
NetMisleading[net_?ImageClassifyQ, i_Image, task_, o : OptionsPattern[]] := Block[
	{encoder, decoder, target, forward, trainedNet},
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
	encoder = NetEncoder[{"Image", encoder[["ImageSize"]], "ColorSpace" -> encoder[["ColorSpace"]]}];
	decoder = NetDecoder[encoder];
	forward = NetChain[
		<|
			"image" -> ConstantArrayLayer["Array" -> encoder@i],
			"encoder" -> ImageEncoderToLayer[NetExtract[net, "Input"]],
			"network" -> net
		|>,
		"Output" -> NetExtract[net, "Output"]
	];
	trainedNet = NetTrain[
		forward,
		<|"Output" -> {target}|>,
		LearningRateMultipliers -> {
			"image" -> OptionValue[LearningRateMultipliers],
			_ -> None
		},
		MaxTrainingRounds -> OptionValue[MaxTrainingRounds],
		TargetDevice -> OptionValue[TargetDevice]
	];
	decoder@NetExtract[trainedNet, {"image", "Array"}]
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
	type = decoder /. HoldPattern[NetEncoder[kind_, __]] :> kind ;
	If[type != "Class", Return[False]];
	Return[True]
];
ImageEncoderToLayer[encoder_NetEncoder] := Block[
	{avg = encoder[["MeanImage"]], std = encoder[["VarianceImage"]]},
	avg = Which[
		ImageQ@avg, ConstantArrayLayer["Array" -> ImageData[-avg, Interleaving -> False]],
		VectorQ@avg, {
			ConstantArrayLayer["Array" -> -avg],
			ReplicateLayer[{Automatic, Automatic}, 2]
		},
		True, {
			ConstantArrayLayer["Array" -> Array[0&, encoder[["ColorChannels"]]]],
			ReplicateLayer[{Automatic, Automatic}, 2]
		}
	];
	std = Which[
		ImageQ@std, ConstantArrayLayer["Array" -> ImageData[1 / Sqrt@std, Interleaving -> False]],
		VectorQ@std, {
			ConstantArrayLayer["Array" -> 1 / Sqrt@std],
			ReplicateLayer[{Automatic, Automatic}, 2]
		},
		True, {
			ConstantArrayLayer["Array" -> Array[1&, encoder[["ColorChannels"]]]],
			ReplicateLayer[{Automatic, Automatic}, 2]
		}
	];
	NetFlatten@NetGraph[
		{
			avg, ThreadingLayer[Plus],
			std, ThreadingLayer[Times]
		},
		{
			{NetPort["Input"], 1} -> 2,
			{2, 3} -> 4 -> NetPort["Output"]
		}
	]
];
