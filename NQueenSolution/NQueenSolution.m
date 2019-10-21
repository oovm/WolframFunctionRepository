(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[NQueenSolution] = {};
NQueenSolution[n_Integer] := Block[
	{},
	LinearProgrammingSolver[n]
];


(* ::Subsubsection:: *)
(*Main Function*)


IterationSolverAll[n_] := With[
	{step = Join @@ Table[Append[a, b], {a, #}, {b, With[{t = Range[Length@a, 1, -1]}, Complement[Range[n], a - t, a, a + t]]}]&},
	Nest[step, {{}}, n]
];

LinearProgrammingSolver[n_] := Module[
	{c, m, b, vars},
	c = cQueens[n];
	m = mQueens[n];
	vars = mQueens2[n];
	b = bQueens[Length[m]];
	1 + Mod[Flatten@Position[LinearProgramming[c, m, b, vars, Integers], 1], n]
];


mQueens[n_] := Module[
	{t, t2, t3, t4},
	t = mQueensH[n];
	t2 = Append[t, mQueensV[n]];
	t3 = Append[t2, mQueensD[n]];
	t4 = Append[t3, mQueensDM[n]];
	Partition[Flatten[t4], n^2]
];
mQueens2[n_] := Table[{0, 1}, {i, n^2}];
cQueens[n_] := Table[-1, {i, n^2}];
bQueens[l_] := Table[{1, -1}, {i, l}];

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


LinearProgrammingSolver[4] // AbsoluteTiming

