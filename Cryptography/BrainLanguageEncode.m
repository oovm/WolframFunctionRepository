(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Functions*)


apiEncode[text_String] := Block[
	{api, body, type, http, xml},
	api = "http://ctf.ssleye.com/ctf/ctf_brainfuck_en";
	body = TemplateApply["text=`1`&encode_flag=utf8", {URLEncode@text} ];
	type = "application/x-www-form-urlencoded";
	http = HTTPRequest[api, <|"Body" -> body, Method -> "POST", "ContentType" -> type|>];
	xml = ImportString[URLRead[http, "Body"], {"HTML", "XMLObject"}];
	xml[[2, -1, -1, -1, -1, -1, -2, -1, -1, -1, 1]]
];


code = apiEncode["\:82df\:5229\:56fd\:5bb6\:751f\:6b7b\:4ee5"]


(* ::Subsubsection:: *)
(*Auxiliary Functions*)
