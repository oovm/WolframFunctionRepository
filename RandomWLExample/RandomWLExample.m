(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


Options[RandomWLExample] = {};
RandomWLExample[o : OptionsPattern[]] := Block[
	{title, locallink, text, file, inputs, output, cap, in, i = 0, j = 1},
	file = RandomChoice[$ExampleNotebooks];
	inputs = Import[file, {"Cells", "Input"}];
	output = Import[file, {"Cells", "Output"}][[1]];
	cap = CellLabel /. Options[output];
	If[!StringQ[cap], Return[$Failed]];
	cap = ToExpression[StringReplace[cap, "Out[" ~~ x__ ~~ "]" ~~ __ :> x]];
	locallink = NotebookImport[file, "AnchorBarGrid"] // Extract[#, Position[#, "\"Copy Wolfram Documentation Center URL\""][[1, ;; -3]]][[1, 1]]& // StringTake[#, {2, -2}]&;
	title = Cell[
		StringReplace[file, __ ~~ "ExamplePages" :> "ExamplePages"] // ButtonBox[#, BaseStyle -> "Link", ButtonData -> "paclet:" <> locallink]& // TextData,
		"Subsubsection", CellTags -> {"delete me"}
	];
	text = Reap@While[
		i < cap && j <= Length[inputs],
		in = CellLabel /. Options[inputs[[j]]];
		If[StringQ[in], i = ToExpression[StringReplace[in, "In[" ~~ x__ ~~ "]" ~~ __ :> x]]];
		Sow[inputs[[j++]]]
	];
	Cells[CellTags -> "delete me"] /. {c_, ___} :> SelectionMove[c, All, CellGroup];
	If[SelectedCells[] =!= {}, NotebookDelete[]];
	CellPrint /@ Flatten@{title, text[[-1, 1]], output};
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


$ExampleNotebooks := $ExampleNotebooks = FileNames["*", DirectoryName[FindFile["ExamplePages/3DLaplacians.nb"]]];
