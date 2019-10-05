(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


Magic3D::nosol = "No solution.";
Magic3D::nodef = "Undefined.";


Magic3D[2] := Message[Magic3D::nosol];
Magic3D[n_?OddQ] := Table[n^2Mod[i - j + k - 1, n] + n Mod[i - j - k, n] + Mod[i + j + k - 2, n] + 1, {i, 1, n}, {j, 1, n}, {k, 1, n}];
Magic3D[n_?EvenQ] := Block[
	{QMagic, XMagic, upTab, downTab, d, u, v, i, j, k},
	QMagic[x_] := If[1 <= x <= n / 2, 0, 1];
	XMagic[x_] := Min[x, n + 1 - x];
	u = Mod[XMagic[i] - XMagic[j] + XMagic[k], n / 2] + 1;
	v = 4QMagic[i] + 2QMagic[j] + QMagic[k] + 1;
	d[1, v_] := {7, 3, 6, 2, 5, 1, 4, 0}[[v]];
	d[2, v_] := {3, 7, 2, 6, 1, 5, 0, 4}[[v]];
	d[3, v_] := {0, 1, 3, 2, 5, 4, 6, 7}[[v]];
	d[u_, v_] := If[Mod[u, 2] === 0, v - 1, 8 - v];
	upTab = Table[(n / 2)^3d[u, v], {i, 1, n}, {j, 1, n}, {k, 1, n}];
	downTab = Table[(n / 2)^2Mod[i - j + k - 1, n / 2] + (n / 2)Mod[i - j - k, n / 2] + Mod[i + j + k - 2, n / 2] + 1, {i, 1, n}, {j, 1, n}, {k, 1, n}];
	upTab + downTab
];
Magic3D[n_ /; n ~ Mod ~ 4 == 0] := Block[
	{QMagic, FMagic},
	QMagic[x_] := If[1 <= x <= n / 2, 0, 1];
	FMagic[i_, j_, k_] := Mod[i + j + k + QMagic[i] + QMagic[j] + QMagic[k], 2];
	Table[If[FMagic[i, j, k] == 1, (i - 1)n^2 + (j - 1)n + k, 1 - k + n(1 - j + n(1 - i + n))], {i, 1, n}, {j, 1, n}, {k, 1, n}]
];
Magic3D[x_] := Message[Magic3D::nodef];
