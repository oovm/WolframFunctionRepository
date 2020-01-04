(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)

Options[HydrogenDensityPlot] = {
	ColorFunction -> "SunsetColors",
	PlotLegends -> Automatic,
	PlotPoints -> 200,
	MaxRecursion -> 4,
	AspectRatio -> 1,
	PlotRange -> Automatic
};
HydrogenDensityPlot[n_, l_, m_, o : OptionsPattern[]] := HydrogenDensityPlot[
	n, l, m, {-30, 30}, {-35, 35}, o
];
HydrogenDensityPlot[n_, l_, m_, {x1_, x2_}, {z1_, z2_}, o : OptionsPattern[]] := Block[
	{x, y, z, \[CapitalPsi]},
	y = 0;
	\[CapitalPsi] = Times[
		E^(-(Sqrt[x^2 + y^2 + z^2] / n)),
		(Sqrt[x^2 + y^2 + z^2] / n)^l ,
		LaguerreL[-1 - l + n, 1 + 2 l, (2 Sqrt[x^2 + y^2 + z^2]) / n] ,
		SphericalHarmonicY[l, m, ArcTan[z, Sqrt[x^2 + y^2]], ArcTan[x, y]]
	];
	DensityPlot[
		Conjugate[\[CapitalPsi]] * \[CapitalPsi],
		{x, x1, x2}, {z, z1, z2},
		ColorFunction -> OptionValue[ColorFunction],
		PlotLegends -> OptionValue[PlotLegends],
		PlotPoints -> OptionValue[PlotPoints],
		MaxRecursion -> OptionValue[MaxRecursion],
		AspectRatio -> OptionValue[AspectRatio],
		PlotRange -> OptionValue[PlotRange]
	]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)

HydrogenDensityPlot[2, 1, 0, {-8, 8}, {-8, 8}]
(*分别为主量子数，角量子数以及磁量子数*)