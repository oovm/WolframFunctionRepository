(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


numbers = Translate[#, {-4.5, -10}] & /@
	First[First[
		ImportString[ExportString[
			Style[#, FontSize -> 24, FontFamily -> "Arial"],
			"PDF"], "PDF", "TextMode" -> "Outlines"]
	]] & /@ {"."} ~ Join ~ CharacterRange["0", "9"];




With[{fontsize = 0.0655, digits = 10000},
	Graphics[
		MapIndexed[
			With[{angle = (-(#2[[1]] - 2) +
				Switch[#2[[1]], 1, -0.1, 2, 0, _, 0.6]) fontsize},
				With[{scale = (1 - 1.5 fontsize)^(-angle / (2 Pi))},
					GeometricTransformation[
						numbers[[# + 2]],
						RightComposition[
							ScalingTransform[{1, 1} 0.1 fontsize * scale],
							TranslationTransform[{0, scale}],
							RotationTransform[Pi / 4 + angle]
						]
					]
				]
			] &,
			Insert[First@RealDigits[Pi, 10, digits], -1, 2]
		],
		PlotRange -> {{-1.1, 1.1}, {-1.1, 1.1}}
	]
]
