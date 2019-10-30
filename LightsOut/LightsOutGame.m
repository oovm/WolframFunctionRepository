(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[LightsOutGame] = {
	Mode -> Random,
	ImageSize -> {400, 400}
};
LightsOutGame[n_Integer, o : OptionsPattern[]] := LightsOutGame[n, n, o];
LightsOutGame[m_Integer, n_Integer, o : OptionsPattern[]] := Block[
	{game},
	If[m < 1 || n < 1, Return[]];
	game = Switch[
		OptionValue[Mode],
		Random, RandomInteger[1, {m, n}],
		Normal, LightsNormal[m, n],
		_, Return[]
	];
	LightsOutGame[game, o]
];
LightsOutGame[M_List, o : OptionsPattern[]] := gamePlay[M, o];


(* ::Subsubsection:: *)
(*Main Function*)


Options[gamePlay] = Options[LightsOutGame];
gamePlay[inSquare_, o : OptionsPattern[]] := DynamicModule[
	{

		$win = False,
		$start = Now,
		$finish = 0,
		$step = 0,
		$final = 0,
		truth = Transpose@inSquare,
		w, h, state, updateState
	},
	{w, h} = Length /@ {truth, First@truth};
	state[] := Table[EvenQ[getShowValue[i, j, w, h, truth]], {i, 1, w}, {j, 1, h}];
	updateState[{i_, j_}] := (
		truth[[i, j]] = 1 - truth[[i, j]];
		If[
			AllTrue[Flatten@state[], TrueQ] && !TrueQ@$win,
			$win = True;
			$final = $step + 1 ;
			$finish = Now - $start
		];
		$step += 1;
	);
	EventHandler[
		Panel@Column[{
			"The Lights Out Game",
			Dynamic[Row[{Style["Steps: ", Bold], If[$win, $final, $step], If[$win, Style["  You Win!", Red], Nothing ]}]],
			Dynamic[Row[{Style["Time: ", Bold], If[$win, $finish, Now - $start]}], UpdateInterval -> 1 / 24],
			Graphics[
				Dynamic[
					Table[
						If[
							EvenQ[getShowValue[i, j, w, h, truth]],
							{Black, showSquare[{i, j}]},
							{White, showSquare[{i, j}]}
						],
						{i, 1, w},
						{j, 1, h}
					]
				],
				Background -> Gray,
				ImageSize -> OptionValue[ImageSize]
			]
		}],
		"MouseDown" :> updateState[
			Floor[MousePosition["Graphics"]]
		]
	],
	SaveDefinitions -> True
];



(* ::Subsubsection:: *)
(*Auxiliary Functions*)


showSquare[{i_, j_}] := Rectangle[{i + 0.02, j + 0.02}, {i + 1 - 0.02, j + 1 - 0.02}];
getSquareValue[i_, j_, w_, h_, gs_] := If[i >= 1 && i <= w && j >= 1 && j <= h, gs[[i, j]], 0];
getShowValue[i_, j_, w_, h_, gs_] := Plus[
	getSquareValue[i, j, w, h, gs],
	getSquareValue[i, j + 1, w, h, gs],
	getSquareValue[i, j - 1, w, h, gs],
	getSquareValue[i - 1, j, w, h, gs],
	getSquareValue[i + 1, j, w, h, gs]
];


LightsNormal[m_, n_] := Module[
	{mat = AdjacencyMatrix[GridGraph[{n, m}]] + IdentityMatrix[n * m]},
	Partition[LinearSolve[mat, Table[1, {n * m}], Modulus -> 2], n]
];
