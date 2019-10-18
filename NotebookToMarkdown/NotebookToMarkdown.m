(* ::Package:: *)

Clear["`*"];


NotebookToMarkdown[nb_NotebookObject] := StringJoin@Flatten[NotebookToMarkdown /@ Cells[nb]];
NotebookToMarkdown[co_CellObject] := NotebookToMarkdown[NotebookRead[co], co];
NotebookToMarkdown[c_Cell, co_CellObject] := NotebookToMarkdown[#2, #, co]& @@ c;
NotebookToMarkdown[s_, o___] := (
	Echo[Inactive[NotebookToMarkdown][s, o], "Todo: "];
	TemplateApply["[//]: # (No rules defined for ``)\n\n", {s}]
);


NotebookToMarkdown["Title", data_, co_CellObject] := {"#", parseData@data, "\n\n"};
NotebookToMarkdown["Subtitle", data_, co_CellObject] := {"##", parseData@data, "\n\n"};
NotebookToMarkdown["Chapter", data_, co_CellObject] := {"###", parseData@data, "\n\n"};
NotebookToMarkdown["Section", data_, co_CellObject] := {"####", parseData@data, "\n\n"};
NotebookToMarkdown["Subsection", data_, co_CellObject] := {"#####", parseData@data, "\n\n"};
NotebookToMarkdown["Subsubsection", data_, co_CellObject] := {"######", parseData@data, "\n\n"};


NotebookToMarkdown["Text", data_, co_CellObject] := {parseData@data, "\n\n"};


codeStyleQ = MemberQ[{"Code", "Input"}, #] &;
parseCodeData[data_, indent_ : 4] := Block[
	{s = ConstantArray[" ", indent], fe},
	fe = FrontEndExecute[FrontEnd`ExportPacket[data, "PlainText"]];
	s <> StringReplace[First@fe , "\r\n" | "\n" -> "\n" <> s]
];
NotebookToMarkdown[style_?codeStyleQ, data_, cellObj_CellObject] := {
	"```mathematica\n",
	parseCodeData[data, 0],
	"\n```\n\n"
};


parseData[list_List] := parseData /@ list;
parseData[string_String] := string;
parseData[data_(BoxData | TextData)] := List @@ (parseData /@ data);
parseData[cell_Cell] := parseData@First@cell;
parseData[boxes_] := (
	Echo[Inactive[parseData][boxes], "Todo: "];
	parseData@First@boxes
);

(*
parseData[StyleBox[expr_, opts___]] := styleWrapper[opts]@parseData[expr];
styleWrapper[opts___] := Module[
	{italic, bold, wrapper },
	italic = MemberQ[{opts}, Verbatim[Rule][FontSlant, "Italic"]];
	bold = MemberQ[{opts}, Verbatim[Rule][FontWeight, "Bold"]];
	wrapper = Which[
		bold, "**",
		italic, "*",
		True, ""
	];
	wrapper <> # <> wrapper &
];


parseData[FormBox[boxes : Except[_TagBox], TraditionalForm, ___]] := Module[{teXForm}
	, teXForm = boxesToTeX@boxes
	; "$" <> teXForm <> "$"
];
boxesToTeX = ToString[ToExpression@#, TeXForm] &;

parseData[box : ButtonBox[_, ___, BaseStyle -> "Hyperlink", ___]] := Module[{label, url}
	, {label, url} = {#, #2} & @@ ToExpression[box]
	;  TemplateApply["[``](``)", {StringJoin@Flatten@{parseData@label}, url}]
];
*)
