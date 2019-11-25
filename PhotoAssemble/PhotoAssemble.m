(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)
$ImageBlockSize = 50;

(* ::Subsection:: *)
(*Main Functions*)


(* ::Subsection:: *)
(*Auxiliary Functions*)

getCache[l_List, max_] := Block[
	{images, cache},
	images = Take[l, UpTo[max]];
	Echo[Row[{Text["cache save to  "], Style[".mosaic-cache.mat", Bold]}], "Cache: "];
	cache = Monitor[
		Table[getThumbnail[images[[i]]], {i, Length@images}],
		ProgressIndicator[i, {1, Length@images}]
	];
	Export[".mosaic-cache.mat", cache, "WXF"];
	Return@cache
];
getCache[path_String, max_] := Block[
	{temp, cache, files},
	If[!DirectoryQ@path, Return@loadCache[path]];
	temp = FileNameJoin[{path, ".mosaic-cache.mat"}];
	If[FileExistsQ@temp, Return@loadCache[temp]];
	Echo[Row[{Text["cache save to  "], Style[temp, Bold]}], "Cache: "];
	files = FileNames[{"*.jpg", "*.jpeg", "*.png", "*.tiff"}, path, Infinity];
	files = Take[files, UpTo[max]];
	cache = Monitor[
		Table[getThumbnail[files[[i]]], {i, Length@files}],
		ProgressIndicator[i, {1, Length@files}]
	];
	Export[temp, cache, "WXF"];
	Return@cache
];
loadCache[file_String] := (
	Echo[Row[{Text["cache load from  "], Style[file, Bold]}], "Cache: "];
	Check[Import[file, "WXF"], {}]
)