(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


ClearAll[Polyomino];
Options[Polyomino] = {ImageSize -> 60};
SetAttributes[Polyomino, HoldFirst]
Polyomino[0]={};
Polyomino[n_Integer, OptionsPattern[]] := Block[
	{reg = ArrayMesh@*matrify /@ polyominoes[n]},
	If[n<0,Return[]];
	Table[MeshRegion[
		reg[[i]], ImageSize -> OptionValue[ImageSize],
		MeshCellStyle -> {1 -> Black, 2 -> ColorData[101, "ColorFunction"][i]}
	], {i, Length[reg]}]
];


Polyomino /: Length[Polyomino[n_, OptionsPattern[]]] := Block[
	{},
	If[n>=Length@$FastLength,Message[General::ovfl];Return[]];
	If[n<0,Return[]];
	Return[$FastLength[[n+1]]]
]
$FastLength = {
	0,1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655, 17073, 63600, 238591,
	901971, 3426576, 13079255, 50107909, 192622052, 742624232,
	2870671950, 11123060678, 43191857688, 168047007728, 654999700403,
	2557227044764, 9999088822075, 39153010938487, 153511100594603
};


(* ::Subsubsection:: *)
(*Polyominoes*)


polyominoQ[p_List] := And @@ ((IntegerQ[Re[#]] && IntegerQ[Im[#]])& /@ p);
rot[p_?polyominoQ] := I * p;
ref[p_?polyominoQ] := (# - 2 Re[#])& /@ p;
cyclic[p_] := Module[{i = p, r}, r = Reap@While[(i = rot[i]) != p, Sow@i];Join[{p}, r[[-1, 1]]]];
dihedral[p_?polyominoQ] := Flatten[{#, ref[#]}& /@ cyclic[p], 1];
canonical[p_?polyominoQ] := Union[(# - (Min[Re[p]] + Min[Im[p]] * I))& /@ p];
allPieces[p_] := Union[canonical /@ dihedral[p]];

polyominoes[1] = {{0}};
polyominoes[n_] := polyominoes[n] = Block[
	{f, fig, ans = {}},
	fig = ((f = #1;({f, #1 + 1, f, #1 + I, f, #1 - 1, f, #1 - I}&) /@ f)&) /@ polyominoes[n - 1];
	fig = Partition[Flatten[fig], n];
	f = Select[Union[canonical /@ fig], Length[#1] == n&];
	While[f != {}, ans = {ans, First[f]};
	f = Complement[f, allPieces[First[f]]]];
	Partition[Flatten[ans], n]
];
matrify = SparseArray@Thread[ReIm[# + (1 + I)] -> 1]&;
