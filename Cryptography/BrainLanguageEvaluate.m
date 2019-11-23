(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


BrainLikeEvaluate::env = "The `1` environment not found.";
BrainLikeEvaluate[Initialize] := checkInitialize[];

Options[BrainLikeEvaluate] = {
	CompilationTarget -> "Output",
	TranslationOptions -> "Normal"
};
BrainLikeEvaluate[input_String, o : OptionsPattern[]] := Block[
	{mapped},
	If[!checkInitialize[], Return[]];
	mapped = StringReplace[input,getRule[OptionValue[TranslationOptions]]];
	Switch[
		OptionValue[CompilationTarget],
		"Output", interpret[mapped],
		"Operations", ToUpperCase@parse[mapped],
		"Refined", mapped,
		_, Return[]
	]
];


(* ::Subsubsection:: *)
(*Main Functions*)


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


checkInitialize[] := Block[
	{envs},
	envs = FindExternalEvaluators["Python"];
	If[
		Length@envs == 0,
		Message[BrainLikeEvaluate::env, "python"];
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


getRule[name_]:=getRule[name]=Append[Thread[charSet[name]->charSet["Normal"]],_->""];
charSet[list_List]:=ToString/@list;
charSet["Normal"]={">","<","+","-","[","]",".",","};
charSet["Chinese"]={"\:53f3","\:5de6","\:589e","\:51cf","\:59cb","\:7ec8","\:5199","\:8bfb"};
charSet["Dao"]={"\:2634","\:2633","\:2631","\:2636","\:2630","\:2637","\:2635","\:2632"};
(*charSet["\:5468\:6613"]={"\:5dfd","\:9707","\:514c","\:826e","\:5764","\:4e7e","\:574e","\:96e2"}*)
