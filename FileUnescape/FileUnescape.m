(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


(* ::Subsubsection:: *)
(*Main Function*)


$FixRule = {
	char : RegularExpression["\\\\:.{4}"] :> ParseCharacter@char,
	char : ("\\[" ~~ Shortest[c__] ~~ "]") :> ParseCharacter@char
};
ParseCharacter = With[{t = ToExpression[#, InputForm, Unevaluated]}, SymbolName@t]&;


Options[FileUnescape] = {CharacterEncoding -> $CharacterEncoding};
FileUnescape[File[name_], o : OptionsPattern[]] := FileUnescape[name, o];
FileUnescape[file_String, o : OptionsPattern[]] := Block[
	{unescaped},
	unescaped = StringReplace[Import[file, "Text"], $FixRule];
	Export[file, ToCharacterCode[unescaped, OptionValue[CharacterEncoding]], "Binary"];
]
