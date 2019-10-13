(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[DigitalSpiral] = {
	FontFamily -> "Helvetica",
	FontSize -> 0.0655,
	ImageSize -> Large,
	"Base" -> 10,
	"Rotation" -> Pi / 4
};
DigitalSpiral[n_?NumericQ, i_Integer : 5000 , o : OptionsPattern[]] := Block[
	{fs, number, num },
	fs = OptionValue[FontSize];
	{number, num} = RealDigits[n , OptionValue["Base"], i];
	Graphics[
		MapIndexed[
			Block[
				{angle, scale},
				angle = (-(#2[[1]] - 2) + Switch[#2[[1]], 1, -0.1, 2, 0, _, 0.6]) fs;
				scale = (1 - 1.5 fs)^(-angle / (2 Pi));
				GeometricTransformation[
					getRender[#, OptionValue[FontFamily]],
					RightComposition[
						ScalingTransform[{1, 1} 0.1 fs * scale],
						TranslationTransform[{0, scale}],
						RotationTransform[OptionValue["Rotation"] + angle]
					]
				]
			] &,
			If[
				num > 0,
				Insert[number, -1, num + 1],
				Insert[Join[Array[0&, 1 - num], number], -1, 2]
			]
		],
		PlotRange -> {{-1.1, 1.1}, {-1.1, 1.1}},
		ImageSize -> OptionValue[ImageSize]
	]
];


(* ::Subsubsection:: *)
(*Main Function*)


$DefaultDigitSet = Join[CharacterRange["0", "9"], CharacterRange["a", "z"]];
$DefaultDigitFont = "Arial";
DigitRender[char_String, font_String] := Block[
	{export, trans = Translate[#, {-4.5, -10}] & },
	export = ExportString[ Style[char, FontSize -> 24, FontFamily -> font], "PDF"];
	trans /@ ImportString[export, "PDF", "TextMode" -> "Outlines"][[1, 1]]
];
getRender[-1, f_String] := getRender[-1, f] = DigitRender[".", f]
getRender[i_, f_String] := getRender[i, f] = DigitRender[$DefaultDigitSet[[i + 1]], f]
