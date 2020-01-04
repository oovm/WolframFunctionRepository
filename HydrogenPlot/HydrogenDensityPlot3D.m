(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)

n = 4;l = 2;m = 0;
(*分别为主量子数，角量子数以及磁量子数*)

\[CapitalPsi] = Times[
	E^(-(Sqrt[x^2 + y^2 + z^2] / n)),
	(Sqrt[x^2 + y^2 + z^2] / n)^l ,
	LaguerreL[-1 - l + n, 1 + 2 l, (2 Sqrt[x^2 + y^2 + z^2]) / n] ,
	SphericalHarmonicY[l, m, ArcTan[z, Sqrt[x^2 + y^2]], ArcTan[x, y]]
];
(*未归一化的氢原子定态波函数*)
SliceDensityPlot3D[
	Conjugate[\[CapitalPsi]] * \[CapitalPsi], "CenterPlanes",
	{x, -50, 50}, {y, -50, 50}, {z, -50, 50},
	PlotPoints -> 100,
	PerformanceGoal -> "Quality",
	ColorFunction -> "SunsetColors",
	PlotLegends -> Automatic
]

(* ::Subsection:: *)
(*Auxiliary Functions*)

HydrogenDensityPlot[2, 1, 0, {-8, 8}, {-8, 8}]
(*分别为主量子数，角量子数以及磁量子数*)