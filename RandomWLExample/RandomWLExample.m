(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[RandomWLExample] = {};
RandomWLExample[o : OptionsPattern[]] := Block[
	{title, text, file, inputs, output, cap, in, i = 0, j = 1},
	file = RandomChoice[$ExampleNotebooks];
	inputs = Import[file, {"Cells", "Input"}];
	output = Import[file, {"Cells", "Output"}][[1]];
	cap = CellLabel /. Options[output];
	If[!StringQ[cap], Return[$Failed]];
	cap = ToExpression[StringReplace[cap, "Out[" ~~ x__ ~~ "]" ~~ __ :> x]];
	title = Cell[StringReplace[file, __ ~~ "ExamplePages" :> "ExamplePages"], "Subsubsection"];
	text = Reap@While[
		i < cap && j <= Length[inputs],
		in = CellLabel /. Options[inputs[[j]]];
		If[StringQ[in], i = ToExpression[StringReplace[in, "In[" ~~ x__ ~~ "]" ~~ __ :> x]]];
		Sow[inputs[[j++]]]
	];
	CellPrint@Cell[
		CellGroup[Flatten@{title, text[[-1, 1]], output}, CellGrouping -> Automatic],
		"Output",
		CellAutoOverwrite -> True,
		GeneratedCell -> True
	]
];


(* ::Subsubsection:: *)
(*Main Function*)


$ExampleNotebooks := $ExampleNotebooks = FileNames["*", DirectoryName[FindFile["ExamplePages/3DLaplacians.nb"]]];
