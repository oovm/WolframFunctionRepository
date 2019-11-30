(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[SelfAssembleGray] = {CellSize -> Automatic};
SelfAssembleGray[img_, w_, o : OptionsPattern[]] := SelfAssembleGray[img, {w, w}, o];
SelfAssembleGray[img_, {w_, h_}, o : OptionsPattern[]] := Block[
	{mean, i, grid},
	mean = Mean@Flatten[ImageData[img]];
	i = If[OptionValue[CellSize] === Automatic,
		ImageResize[img, ImageDimensions[img] / {w, h}],
		ImageResize[img, checkTuple@OptionValue[CellSize]]
	];
	grid = ImageData[ImageResize[img, {w, h}]];
	ImageAssemble@Map[i * #&, grid / mean, 2]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)
checkTuple[i_] := {i, i};
checkTuple[{i_, j_}] := {i, j};
