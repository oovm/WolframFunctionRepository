(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[NQueenSolution] = {
	Method -> Unique
};
NQueenSolution[n_Integer, o : OptionsPattern[]] := Switch[
	OptionValue[Method],
	Unique, LinearProgrammingSolver[n],
	All, IterationAllSolver[n],
	Count, FastLength[n],
	_, Null
];


(* ::Subsubsection:: *)
(*Main Function*)


IterationAllSolver[n_Integer] := Nest[Join @@ Table[Append[a, b], {a, #}, {b, With[{t = Range[Length@a, 1, -1]}, Complement[Range[n], a - t, a, a + t]]}]&, {{}}, n];


LinearProgrammingSolver[n_Integer] := Module[
	{c = cQueens[n], m = mQueens[n], lp},
	lp = Quiet@LinearProgramming[c, m, bQueens[Length@m], vQueens[n], Integers];
	1 + Mod[Flatten@Position[lp, 1], n]
];
mQueens[n_] := Module[
	{t, t2, t3, t4},
	t = mQueensH[n];
	t2 = Append[t, mQueensV[n]];
	t3 = Append[t2, mQueensD[n]];
	t4 = Append[t3, mQueensDM[n]];
	Partition[Flatten[t4], n^2]
];
vQueens[n_] := Array[{0, 1}&, n^2];
cQueens[n_] := Array[-1 &, n^2];
bQueens[l_] := Array[{1, -1} &, l];

mQueensH[n_] := Block[
	{t},
	t = Table[0, {i, n}, {j, n^2}];
	For[i = 1, i <= n, i++, For[j = 1, j <= n, j++, t[[i, ((i - 1) * n) + j]] = 1]];
	Return@t
];
mQueensV[n_] := Block[
	{t},
	t = Table[0, {i, n}, {j, n^2}];
	For[i = 1, i <= n, i++, For[j = 1, j <= n, j++, t[[j, ((i - 1) * n) + j]] = 1]];
	Return@t
];
mQueensD[n_] := Block[
	{t},
	t = Table[0, {i, (2 * n) - 1}, {j, n^2}];
	For[k = 2, k <= 2 n, k++,
		For[i = 1, i <= n, i++,
			For[j = 1, j <= n, j++,
				If[i + j == k, t[[k - 1, ((i - 1) * n) + j]] = 1]
			]
		]
	];
	Return@t
];
mQueensDM[n_] := Block[
	{t},
	t = Table[0, {i, Sum[1, {i, 1 - n, n - 1}]}, {j, n^2}];
	For[k = 1 - n, k <= n - 1, k++,
		For[i = 1, i <= n, i++,
			For[j = 1, j <= n, j++,
				If[i == j - k, t[[k + n, ((i - 1) * n) + j]] = 1]
			]
		]
	];
	Return@t
];




$A000170 = {
	1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 73712, 365596,
	2279184, 14772512, 95815104, 666090624, 4968057848, 39029188884,
	314666222712, 2691008701644, 24233937684440, 227514171973736,
	2207893435808352, 22317699616364044, 234907967154122528
};
FastLength[n_] := Block[
	{},
	If[n <= 0, Return[]];
	If[
		n > Length@$A000170,
		Missing[],
		$A000170[[n]]
	]
];
