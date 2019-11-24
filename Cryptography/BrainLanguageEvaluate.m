(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


BrainLanguageEvaluate::env = "The `1` environment not found.";
Options[BrainLanguageEvaluate] = {
	CompilationTarget -> "Output",
	TranslationOptions -> "Normal"
};
BrainLanguageEvaluate[Initialize] := reInitialize[];
BrainLanguageEvaluate[input_String, o : OptionsPattern[]] := Block[
	{mapped},
	If[!checkInitialize[], Return["UnknownError"]];
	mapped = StringReplace[input, getRule[ToLowerCase@OptionValue[TranslationOptions]]];
	Switch[
		OptionValue[CompilationTarget],
		"Output", FromCharacterCode[ToCharacterCode[interpret@mapped], "UTF-8"],
		"OutputDebug", ToCharacterCode[interpret@mapped],
		"Operations", ToUpperCase@parse[mapped],
		"Refined", mapped,
		_, Return[]
	]
];


(* ::Subsubsection:: *)
(*Main Functions*)


checkInitialize[] := reInitialize[];
reInitialize[] := Block[
	{envs},
	envs = FindExternalEvaluators["Python"];
	If[
		Length@envs == 0,
		Message[BrainLanguageEvaluate::env, "python"];
		Return[False]
	];
	If[
		RunProcess[{envs[1, "Executable"], "-m", "pip", "show", "bfi"}, "StandardOutput"] == "",
		RunProcess[{envs[1, "Executable"], "-m", "pip", "install", "bfi"}]
	];
	$Session = StartExternalSession["Python"];
	ExternalEvaluate[$Session, "\
import bfi
def parse(program):
    opcodes = bfi.parse(program)
    output = []
    for c in opcodes:
        output.append(str(c))
    return output
def interpret(program):
    out = bfi.interpret(program, buffer_output=True, tape_size=65535)
    return out
"];
	parse = ExternalFunction[$Session, "parse"];
	interpret = ExternalFunction[$Session, "interpret"];
	checkInitialize[] := True;
	Return[True]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


getRule[name_] := getRule[name] = Append[Thread[charSet[name] -> charSet["normal"]], _ -> ""];
charSet[list_List] := ToString /@ list;
charSet["normal"] = {">", "<", "+", "-", "[", "]", ".", ","};
charSet["number"] = {"0", "1", "2", "3", "4", "5", "6", "7"};
charSet["chinese"] = {"\:53f3", "\:5de6", "\:589e", "\:51cf", "\:59cb", "\:7ec8", "\:5199", "\:8bfb"};
charSet["dao"] = {"\:2634", "\:2633", "\:2631", "\:2636", "\:2630", "\:2637", "\:2632", "\:2635"};
charSet["\:5468\:6613"] = {"\:5dfd", "\:9707", "\:514c", "\:826e", "\:5764", "\:4e7e", "\:96e2", "\:574e"}
charSet["ook"] = {
	"Ook. Ook?", "Ook? Ook.",
	"Ook. Ook.", "Ook! Ook!",
	"Ook! Ook?", "Ook? Ook!",
	"Ook! Ook.", "Ook. Ook!"
};
