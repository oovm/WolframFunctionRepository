(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


ClearAll["`*"];
Options[MorseCipher] = {
	Duration -> 0.07,
	TargetFunctions -> Audio
};
MorseCipher[s_String, o : OptionsPattern[]] := Switch[
	OptionValue[TargetFunctions],
	Audio, createMorseAudio[s, o],
	String, createMorseString[s, o],
	List, createMorseList[s, o],
	_ , Return[]
];
MorseCipher[Rule] = $MorseRules;


(* ::Subsubsection:: *)
(*Main Functions*)


Options[createMorseAudio] = Options[MorseCipher];
createMorseAudio[s_String, OptionsPattern[]] := Block[
	{t, chars, events, ts, amps},
	t = OptionValue[Duration];
	chars = Characters@ToLowerCase[s];
	events = ToExpression@Characters@StringJoin[MorseMapDigit /@ chars];
	ts = TimeSeries[events, {0, (Length[events] - 1) * t, t}];
	amps = AudioGenerator[ts, SampleRate -> 1000];
	AudioGenerator[{"Sin", 800}, Duration@amps, SampleRate -> 8000] * amps
];

Options[createMorseList] = Options[MorseCipher];
createMorseList[s_String, OptionsPattern[]] := Block[
	{chars = Characters@ToLowerCase[s]},
	StringJoin@*MorseMap /@ chars
];

Options[createMorseString] = Options[MorseCipher];
createMorseString[s_String, OptionsPattern[]] := Block[
	{list = createMorseList[s]},
	StringRiffle[list, "/"]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


MorseMap[char_] := Block[
	{digits},
	If[
		MemberQ[$MorseRegistered, char],
		Characters[$MorseRules[char]],
		digits = IntegerDigits[First@ToCharacterCode[char, "Unicode"], 2];
		digits /. {1 -> "-", 0 -> "."}
	]
];
MorseMapDigit[" "] = "0000000";
MorseMapDigit[char_] := MorseMapDigit[char] = StringReplace[
	StringRiffle[MorseMap[char], "_"] <> "___",
	{"-" -> "111", "." -> "1", "_" -> "0"}
];


$MorseBasicRule = <|
	"a" -> ".-",
	"b" -> "-...",
	"c" -> "-.-.",
	"d" -> "-..",
	"e" -> ".",
	"f" -> "..-.",
	"g" -> "--.",
	"h" -> "....",
	"i" -> "..",
	"j" -> ".---",
	"k" -> "-.-",
	"l" -> ".-..",
	"m" -> "--",
	"n" -> "-.",
	"o" -> "---",
	"p" -> ".--.",
	"q" -> "--.-",
	"r" -> ".-.",
	"s" -> "...",
	"t" -> "-",
	"u" -> "..-",
	"v" -> "...-",
	"w" -> ".--",
	"x" -> "-..-",
	"y" -> "-.--",
	"z" -> "--..",
	"1" -> ".----",
	"2" -> "..---",
	"3" -> "...--",
	"4" -> "....-",
	"5" -> ".....",
	"6" -> "-....",
	"7" -> "--...",
	"8" -> "---..",
	"9" -> "----.",
	"0" -> "-----"
|>;
$MorseExtendRule = <|
	"." -> ".-.-.-",
	"," -> "--..--",
	"!" -> "-.-.--",
	"?" -> "..--..",
	" " -> " "
|>;
$MorseRules = Join[$MorseBasicRule, $MorseExtendRule];
$MorseRegistered = Keys@$MorseRules;
