(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
TooltipBox[
	GraphicsBox[
		{
			{GrayLevel[0], RectangleBox[{0, 0}]},
			{GrayLevel[0], RectangleBox[{1, -1}]},
			{RGBColor[0.5`, 1, 0.5`, 1], RectangleBox[{0, -1}, {2, 1}]}
		},
		DefaultBaseStyle -> "ColorSwatchGraphics",
		AspectRatio -> 1,
		Frame -> True,
		FrameStyle -> Darker[RGBColor[0.5`, 1, 0.5`, 1]],
		FrameTicks -> None,
		PlotRangePadding -> None,
		ImageSize -> {Automatic, 10.8}
	],
	StyleBox[RowBox[{"RYBColor[", "0.5`", ",", "1", ",", "0.5`", ",", "1", "]"}], NumberMarks -> False]
]
ToExpression[%, StandardForm]