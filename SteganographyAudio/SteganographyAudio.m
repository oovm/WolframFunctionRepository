(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)


Options[SteganographyAudio] = {
	"Time" -> 10,
	Rescale -> 60(*Hidden*)
};
SteganographyAudio[s_String, o : OptionsPattern[]] := SteganographyAudio[s, 2000, o];
SteganographyAudio[s_String, sample_Integer, o : OptionsPattern[]] := Block[
	{time, r, text, imgdata, list, listcompete},
	{time, r} = OptionValue[{"Time", Rescale}];
	text = Text@TemplateApply[" `1` ", {s}];
	imgdata = Reverse@ImageData[ColorNegate@Binarize[ImageResize[Rasterize[text, RasterSize -> 100], {100, Automatic}]]];
	list = Flatten[Position[#, 1]]& /@ Transpose[imgdata];
	listcompete = Flatten[Table[Table[Total[Sin[2.0 Pi sample / r # t]&@(#& /@ list)[[k]]], {t, 0, time / 100, 1 / sample}], {k, 1, Length@list}], 1];
	Audio[Sound[SampledSoundList[listcompete, sample]]]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
