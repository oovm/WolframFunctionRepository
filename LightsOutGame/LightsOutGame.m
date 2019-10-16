(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


CC


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)





gs = {{0, 0}, {0, 0}}
Options[gamePlay] = {
	ImageSize -> {400, 400}

};
gamePlay[gameSquare_, o : OptionsPattern[]] := Module[
	{
		$start = Now,
		$win = False,
		$step = 0,
		gs = gameSquare,
		w, h
	},
	w = Length[First@gs];
	h = Length[gs];
	EventHandler[
		Panel@Column[{
			"The Lights Out Game",
			Dynamic[Row[{Style["Steps: ", Bold], If[$win, Null, $step, $step], If[$win, Nothing, Nothing, Style["  You Win!", Red]]}]],
			Dynamic[Row[{Style["Time: ", Bold], If[$win, Null, Now - $start, $win]}], UpdateInterval -> 1 / 24],
			Graphics[
				Dynamic[
					Table[
						If[
							EvenQ[getShowValue[i, j, w, h, gs]],
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
			Flatten@{Floor[MousePosition["Graphics"]], $start},
			gs,
			$step,
			$win
		]
	]
]



SetAttributes[updateState, HoldRest];
updateState[{i_, j_, start_}, gs_, n_, win_] := Block[
	{},
	gs[[i, j]] = 1 - gs[[i, j]];
	If[Total[1 - gs, 2] == 0, win = Now - start];
	n += 1;
];
showSquare[{i_, j_}] := Rectangle[{i + 0.02, j + 0.02}, {i + 1 - 0.02, j + 1 - 0.02}];

getSquareValue[i_, j_, w_, h_, gs_] := If[i >= 1 && i <= w && j >= 1 && j <= h, gs[[i, j]], 0];
getShowValue[i_, j_, w_, h_, gs_] := Plus[
	getSquareValue[i, j, w, h, gs],
	getSquareValue[i, j + 1, w, h, gs],
	getSquareValue[i, j - 1, w, h, gs],
	getSquareValue[i - 1, j, w, h, gs],
	getSquareValue[i + 1, j, w, h, gs]
];