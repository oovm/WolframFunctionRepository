(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


NetEncoderToLayer[encoder_NetEncoder] := Block[
	{type = encoder /. HoldPattern[NetEncoder[kind_, __]] :> kind},
	Switch[type,
		"Image", ImageEncoderToLayer[encoder],
		_, Return[]
	]

];


(* ::Subsection:: *)
(*Main Functions*)


(* ::Subsubsection:: *)
(*Image*)


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


(* ::Subsection:: *)
(*Auxiliary Functions*)
