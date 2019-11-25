(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)


(* ::Subsection:: *)
(*Auxiliary Functions*)

getCache[path_String] := Block[
	{temp, cache, files},
	If[!DirectoryQ@path, Return@loadCache[path]];
	temp = FileNameJoin[{dir, "mosaic.cache.mat"}];
	If[FileExistsQ@temp, Return@loadCache[temp]];
	files = FileNames[{"*.jpg", "*.jpeg", "*.png", "*.tiff"}, dir, Infinity];
	cache = getThumbnail /@ files;
	saveCache[temp, cache];
	Return@cache
]
loadCache[file_String] := (
	Echo[Row[{Text["cache load from  "], Style[file, Bold]}], "Cache: "];
	Check[Import[file, "WXF"], {}]
);
saveCache[path_String, o_] := (
	Echo[Row[{Text["cache save to  "], Style[path, Bold]}], "Cache: "];
	Export[path, o, "WXF"]
);