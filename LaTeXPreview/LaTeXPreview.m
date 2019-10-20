(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


Options[LaTeXPreview] = {
	APIFunction -> "Codecogs",
	ImageResolution -> 120
};
LaTeXPreview[tex_String, args : OptionsPattern[]] := Switch[
	OptionValue[APIFunction],
	"Codecogs", Codecogs[tex, args],
	"LaTeX2PNG", LaTeX2PNG[tex, args],
	_, Null
];


Codecogs[tex_String, OptionsPattern[LaTeXPreview]] := Import["https://latex.codecogs.com/png.latex?" <> URLEncode@tex, "PNG"];


LaTeX2PNG[tex_String, OptionsPattern[LaTeXPreview]] := Module[
	{api, body, http, raw},
	api = "https://latex2png.com/api/convert";
	body = <|
		"auth" -> <|"user" -> "guest", "password" -> "guest"|>,
		"latex" -> tex,
		"resolution" -> OptionValue@ImageResolution,
		"color" -> "000000"
	|>;
	http = HTTPRequest[api, <|"Body" -> ExportString[body, "json"], Method -> "POST"|>];
	raw = URLExecute[http, "Interactive" -> False, "RawJSON"];
	Switch[
		raw["result-code"],
		0, Return@Import["https://latex2png.com" <> raw["url"], "PNG"],
		_, "Error"
	]
];
