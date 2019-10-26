(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)


decodeMorseSignal[audio_] := Module[
	{rms, rounded, crossings, transients, shifted, dit, list},
	rms = AudioLocalMeasurements[audio, "RMSAmplitude", PartitionGranularity -> {.01, .002}];
	rounded = Round[rms / Max@rms];
	crossings = TimeSeriesInsert[
		TimeSeries[CrossingDetect[rounded["Values"] - .5, CornerNeighbors -> True], {rounded["Times"]}],
		{0, 1}
	];
	transients = TimeSeries@Select[Normal@crossings, #[[2]] == 1 &];
	shifted = TimeSeriesShift[transients, -transients["FirstTime"]];
	dit = MinimumTimeIncrement[shifted];
	list = StringJoin[
		Table[
			{Differences[shifted["Times"]][[i]], Mod[i, 2]},
			{i, Length@Differences[shifted["Times"]]}] /. {
			{x_, 1} /; .5 dit < x < 1.5 dit -> ".",
			{x_, 1} /; 2.5 dit < x < 3.5 dit -> "-",
			{x_, 0} /; 2.5 dit < x < 3.5 dit -> "/",
			{x_, 0} /; .5 dit < x < 1.5 dit -> Nothing,
			{x_, 0} /; 5 dit < x < 12 dit -> "/_/"
		}
	];
	StringSplit[list, "/"] /. "_" -> " "
]


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
