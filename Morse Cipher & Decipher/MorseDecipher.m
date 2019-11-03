(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


MorseDecipher[a_Audio] := MorseDecipher@decodeMorseSignal[a];
MorseDecipher[s_String] := MorseDecipher@StringSplit[s, "/"];
MorseDecipher[l_List] := StringJoin[decodeCharacter /@ l];


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
	StringJoin[
		Table[
			{Differences[shifted["Times"]][[i]], Mod[i, 2]},
			{i, Length@Differences[shifted["Times"]]}] /. {
			{x_, 1} /; .5 dit < x < 1.5 dit -> ".",
			{x_, 1} /; 2.5 dit < x < 3.5 dit -> "-",
			{x_, 0} /; 2.5 dit < x < 3.5 dit -> "/",
			{x_, 0} /; .5 dit < x < 1.5 dit -> Nothing,
			{x_, 0} /; 5 dit < x < 12 dit -> "/_/"
		}
	]
];
decodeCharacter[s_String] := decode[s] = Block[
	{x, v = $MorseRulesInverse[s]},
	If[!MissingQ@v, Return[v]];
	x = FromDigits[Characters[s] /. { "-" -> 1, "." -> 0}, 2];
	FromCharacterCode[x - 64, "Unicode"]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


$MorseRulesInverse = <|
	".-" -> "a",
	"-..." -> "b",
	"-.-." -> "c",
	"-.." -> "d",
	"." -> "e",
	"..-." -> "f",
	"--." -> "g",
	"...." -> "h",
	".." -> "i",
	".---" -> "j",
	"-.-" -> "k",
	".-.." -> "l",
	"--" -> "m",
	"-." -> "n",
	"---" -> "o",
	".--." -> "p",
	"--.-" -> "q",
	".-." -> "r",
	"..." -> "s",
	"-" -> "t",
	"..-" -> "u",
	"...-" -> "v",
	".--" -> "w",
	"-..-" -> "x",
	"-.--" -> "y",
	"--.." -> "z",
	".----" -> "1",
	"..---" -> "2",
	"...--" -> "3",
	"....-" -> "4",
	"....." -> "5",
	"-...." -> "6",
	"--..." -> "7",
	"---.." -> "8",
	"----." -> "9",
	"-----" -> "0",
	".-.-.-" -> ".",
	"--..--" -> ",",
	"-.-.--" -> "!",
	"..--.." -> "?",
	" " -> " ",
	"_" -> " "
|>;
