(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


WarpDriveSpeed[w_] := Power[w, (a[w] * b[w] * c[w] + 1) * 10 / 3];
WarpDriveSpeed[w_] := 0 /; w <= 0;
WarpDriveSpeed[w_] := Infinity /; w >= 10;


(* ::Subsection:: *)
(*Auxiliary Functions*)


a[w_] := 0.20467Exp[-0.0058Log[100000 - 10000w]^5];
a[w_] := 0 /; w < 7;


b[w_] := 1 /; w < 5 || w > 9.8;
b[w_] := With[{s = Log[8 / (100 - 10 w)]}, 1 + Exp[-49.369s^4](2 Cos[10 Pi s] - 1) / 3];


c[w_] := 1 + (Pi / 2 - ArcTan[10^w Log[20000 - 2000 w]])(1.88269 / Pi);
