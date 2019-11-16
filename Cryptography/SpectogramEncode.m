(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)


Options[SpectogramEncode] = {
	"Duration" -> 10
};
SpectogramEncode[s_String, o : OptionsPattern[]] := SpectogramEncode[s, 2000, o];
SpectogramEncode[s_String, sample_Integer, o : OptionsPattern[]] := Block[
	{time, text, imgdata, list, listcompete},
	time = OptionValue["Duration"];
	time = Switch[
		Head@time,
		Quantity, First@UnitConvert[time, "Seconds"],
		Integer, time,
		_, Return[]
	];
	text = Text@TemplateApply[" `1` ", {s}];
	imgdata = Reverse@ImageData[ColorNegate@Binarize[ImageResize[Rasterize[text, RasterSize -> 100], {100, Automatic}]]];
	list = Flatten[Position[#, 1]]& /@ Transpose[imgdata];
	listcompete = Flatten[Table[Table[Total[Sin[2.0 Pi sample / 60 # t]&@(#& /@ list)[[k]]], {t, 0, time / 100, 1 / sample}], {k, 1, Length@list}], 1];
	Audio[Sound[SampledSoundList[listcompete, sample]]]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
