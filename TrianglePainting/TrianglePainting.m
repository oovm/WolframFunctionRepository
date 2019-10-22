(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


TriPainting[i_Image, n_Integer : 1000] := Block[
	{x, y, pt, pts},
	{x, y} = ImageDimensions[i];
	pt = Reverse /@ RandomChoice[Flatten@ImageData@GradientFilter[i, 2] -> Tuples@{Range[y, 1, -1], Range[x]}, n];
	pts = Join[pt, {{0, 0}, {x, 0}, {x, y}, {0, y}}];
	Graphics[With[
		{col = RGBColor@ImageValue[i, Mean @@ #]},
		{EdgeForm@col, col, #}]& /@ MeshPrimitives[DelaunayMesh@pts, 2]
	]
];
