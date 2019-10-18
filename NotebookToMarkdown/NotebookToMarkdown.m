(* ::Package:: *)

Clear["`*"];


NotebookToMarkdown[nb_NotebookObject] := StringJoin@Flatten[NotebookToMarkdown /@ Cells[nb]];
NotebookToMarkdown[co_CellObject] := NotebookToMarkdown[NotebookRead[co], co];
NotebookToMarkdown[c_Cell, co_CellObject] := NotebookToMarkdown[#2, #, co]& @@ c;
NotebookToMarkdown[s_, o___] := (
	Echo[Inactive[NotebookToMarkdown][s, o], "Todo: "];
	TemplateApply["[//]: # (No rules defined for ``)\n\n", {s}]
);


NotebookToMarkdown["Title", t_, co_CellObject] := TemplateApply["# ``\n\n", {t}]
NotebookToMarkdown["Subtitle", t_, co_CellObject] := TemplateApply["## ``\n\n", {t}]
NotebookToMarkdown["Chapter", t_, co_CellObject] := TemplateApply["### ``\n\n", {t}]
NotebookToMarkdown["Section", t_, co_CellObject] := TemplateApply["#### ``\n\n", {t}]
NotebookToMarkdown["Subsection", t_, co_CellObject] := TemplateApply["##### ``\n\n", {t}]
NotebookToMarkdown["Subsubsection", t_, co_CellObject] := TemplateApply["###### ``\n\n", {t}]

