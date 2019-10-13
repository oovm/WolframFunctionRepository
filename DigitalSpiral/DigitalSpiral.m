(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


$DefaultDigitSet = Join[CharacterRange["0", "9"], CharacterRange["a", "z"]];
$DefaultDigitFont = "Arial";
DigitRender[set_List, font_String] := DigitRender[set, font] = Block[
	{
		chars = Union@Flatten[{".", set}],
		trans = Translate[#, {-4.5, -10}] &
	},
	trans /@ ImportString[ExportString[ Style[#, FontSize -> 24, FontFamily -> font], "PDF"], "PDF", "TextMode" -> "Outlines"][[1, 1]] & /@ chars
];




Block[
	{fontsize, digits, imgs, number, num},
	fontsize = 0.0655;
	digits = 1000;
	imgs = DigitRender[$DefaultDigitSet[[1 ;; 10]], $DefaultDigitFont];
	{number, num} = RealDigits[E * Pi * EulerGamma, 10, 1000];
	Graphics[
		MapIndexed[
			With[
				{angle = (-(#2[[1]] - 2) + Switch[#2[[1]], 1, -0.1, 2, 0, _, 0.6]) fontsize},
				With[
					{scale = (1 - 1.5 fontsize)^(-angle / (2 Pi))},
					GeometricTransformation[
						imgs[[# + 2]],
						RightComposition[
							ScalingTransform[{1, 1} 0.1 fontsize * scale],
							TranslationTransform[{0, scale}],
							RotationTransform[Pi / 4 + angle]
						]
					]
				]
			] &,
			If[
				num > 0,
				Insert[digits, -1, num + 1],
				Insert[Join[Array[0&, 1 - num], digits], -1, 2]
			]
		],
		PlotRange -> {{-1.1, 1.1}, {-1.1, 1.1}}
	]
]
