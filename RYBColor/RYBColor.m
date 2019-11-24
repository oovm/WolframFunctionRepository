(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


RYBColor /: MakeBoxes[RYBColor[r_, y_, b_, a_], StandardForm] := Block[
	{rgb = RYB2RGB@RYBColor[r, y, b, a]},
	With[
		{c = rgb, f = Darker@rgb},
		TooltipBox[
			GraphicsBox[
				{
					{GrayLevel[0], RectangleBox[{0, 0}]},
					{GrayLevel[0], RectangleBox[{1, -1}]},
					{c, RectangleBox[{0, -1}, {2, 1}]}
				},
				DefaultBaseStyle -> "ColorSwatchGraphics",
				AspectRatio -> 1,
				Frame -> True,
				FrameStyle -> f,
				FrameTicks -> None,
				PlotRangePadding -> None,
				ImageSize -> {Automatic, 10.8}
			],
			StyleBox[RowBox[{"RYBColor[", r, ",", y, ",", b, ",", a, "]"}], NumberMarks -> False]
		]
	]
];


RYBColor /: RGBColor[RYBColor[r_, y_, b_, a_]] := RYB2RGB@RYBColor[r, y, b, a];
RYBColor[RGBColor[r_, g_, b_]] := RGB2RYB@RGBColor[r, g, b, 1];
RYBColor[RGBColor[{r_, g_, b_}]] := RGB2RYB@RGBColor[r, g, b, 1];
RYBColor[RGBColor[r_, g_, b_, a_]] := RGB2RYB@RGBColor[r, g, b, a];
RYBColor[RGBColor[{r_, g_, b_, a_}]] := RGB2RYB@RGBColor[r, g, b, a];


RYBColor[{r_, y_, b_}] := RYBColor @@ Normalize[{r, y, b, 1}, Max];
RYBColor[{r_, y_, b_, a_}] := RYBColor @@ Most@Normalize[{r, y, b, a, 1}, Max];


(* ::Subsubsection:: *)
(*Main Functions*)


RYB2RGB[RYBColor[r_, y_, b_, a_]] := RGBColor @@ (RYB2RGB[{r, y, b}] ~ Append ~ a);
RYB2RGB[RGBColor[r_, g_, b_, a_]] := RYBColor @@ (RGB2RYB[{r, g, b}] ~ Append ~ a);
RGB2RYB[colors_List] := GeneralUtilities`Scope[
	{R, G, B} = colors - Min[colors];
	r = R - Min[R, G];
	y = (G + Min[r, G]) / 2;
	b = (B + G + Min[r, G]) / 2;
	{r, y, b} *= If[Max[r, y, b] == 0, 1, Max[R, G, B] / Max[r, y, b]];
	{r, y, b} + Min[1 - colors]
];
RYB2RGB[colors_List] := GeneralUtilities`Scope[
	{R, Y, B} = colors - Min[colors];
	r = R + Y - Min[Y, B];
	g = Y + 2Min[Y, B];
	b = 2(B - Min[Y, B]);
	{r, g, b} *=  If[Max[r, g, b] == 0, 1, Max[R, Y, B] / Max[r, g, b]];
	{r, g, b} + Min[1 - colors]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
