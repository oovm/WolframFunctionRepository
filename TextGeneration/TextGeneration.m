(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
Options[evaluator$GPT2] = Options[TextGeneration];
evaluator$GPT2[input_String, words_ ?Internal`PositiveIntegerQ, o : OptionsPattern[]] := Block[
	{GPT2, next, window, forward, w, t, device},
	{w, t, device} = OptionValue[{"Window", "Temperature", TargetDevice}];
	GPT2 = Switch[
		OptionValue[Method],
		"GPT2 EN Small", NetModel[{"GPT-2 Transformer Trained on WebText Data", "Task" -> "LanguageModeling", "Size" -> "117M"}],
		"GPT2 EN Medium", NetModel[{"GPT-2 Transformer Trained on WebText Data", "Task" -> "LanguageModeling", "Size" -> "345M"}],
		_, Return[]
	];
	next[text_] := (
		window = StringTake[text, -Min[w, StringLength@text]];
		forward = GPT2[window, {"RandomSample", "Temperature" -> t}, TargetDevice -> device];
		If[StringQ@forward, StringJoin[text, forward], text]
	);
	Nest[next, input, words]
]

(* ::Subsection:: *)
(*Auxiliary Functions*)
