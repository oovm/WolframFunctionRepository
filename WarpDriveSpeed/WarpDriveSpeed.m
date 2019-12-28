(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


WarpDriveSpeed["Expression"] := WarpDriveSpeed["Expression"] = Function[w,
	Evaluate@Block[
		{l = Log[10 - w], a, b, c, d},
		a = Exp[-29 (Log[10000] + l)^5 / 5000];
		b = 388269 Pi - 376538 ArcTan[10^w (Log[2000] + l)] ;
		c = Exp[-49369 (Log[4 / 5] + l)^4 / 1000];
		d = -1 + 2 Cos[10 Pi (Log[4 / 5] + l)];
		Power[w, 10 / 3 + 20467 * a * b * (3 + c * d) / 18000000000 / Pi]
	]
];
WarpDriveSpeed[w_] := Piecewise[{
	{Power[w, (a[w] * b[w] * c[w] + 1) * 10 / 3], 0 < w < 10},
	{0, w <= 0},
	{Infinity, w >= 10}
}];


(* ::Subsection:: *)
(*Auxiliary Functions*)


a[w_] := Piecewise[{
	{20467 Exp[-29Log[100000 - 10000w]^5 / 5000] / 100000, w > 8},
	{0, w <= 8}
}];


b[w_] := Piecewise[{
	{With[{s = Log[8 / (100 - 10 w)]}, 1 + Exp[-49369s^4 / 1000](2 Cos[10 Pi s] - 1) / 3], 5 <= w <= 49 / 5},
	{1, w < 5 || w > 49 / 5}
}];


c[w_] := 1 + (Pi / 2 - ArcTan[10^w Log[20000 - 2000 w]]) * (188269 / 100000 / Pi)
