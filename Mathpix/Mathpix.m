(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


Options[Mathpix] = {Print -> False, Method -> Normal};
Mathpix[path_String, ops : OptionsPattern[]] := Mathpix[Import@path, ops];
Mathpix[img_Image, ops : OptionsPattern[]] := Block[
	{raw, ans},
	raw = MathpixPOST@MathpixHTTP@img;
	If[raw["error"] != "", Echo[raw["error"], "Error: "];Return[Null]];
	ans = raw["latex_styled"];
	CopyToClipboard@ans;
	If[TrueQ@OptionValue[Print], Print@Codecogs[ans]];
	Switch[OptionValue[Method],
		Full, raw,
		Normal, ans,
		_, Null
	]
];


MathpixHTTP[img_Image] := Block[
	{jpeg, api, header, body},
	jpeg = "data:image/jpg;base64," <> ExportString[img, {"Base64", "JPEG"}];
	api = "https://api.mathpix.com/v3/latex";
	header = {
		"app_id" -> "jcarroll",
		"app_key" -> "13f1584b2f9edb8220bf619c0b4e3d5a",
		"Content-type" -> "application/json"
	};
	body = {
		"src" -> jpeg,
		"ocr" -> {"math", "text"},
		"formats" -> {
			"mathml" -> True,
			"wolfram" -> True
		},
		"format_options" -> <|
			"latex_styled" -> <|
				"transforms" -> {"rm_spaces"},
				"math_delims" -> {"$", "$"},
				"displaymath_delims" -> {"$$", "$$"}
			|>
		|>
	};
	HTTPRequest[api, <|"Headers" -> header, "Body" -> ExportString[body, "json"], Method -> "POST"|>]
];
MathpixPOST[http_HTTPRequest] := URLExecute[http, "Interactive" -> False, "RawJSON"];


Codecogs[tex_String] := Import["https://latex.codecogs.com/png.latex?" <> URLEncode@tex, "PNG"];
