(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[BrainLanguageEncode] = {
	TranslationOptions -> "Normal"
};
BrainLanguageEncode[in_String, o : OptionsPattern[]] := Block[
	{codes, state, out},
	codes = ToCharacterCode[in, "UTF8"];
	state = Normal /@ RandomChoice@$States;
	out = Fold[update, state, codes]["Head"];
	wrap[ToLowerCase@OptionValue[TranslationOptions], out]
];
wrap["normal", out_String] := out;
wrap["refined", out_String] := StringReplace[out, Whitespace -> ""];
wrap["ook", out_String] := GeneralUtilities`Scope[
	orders = Characters@StringReplace[out, Whitespace -> ""] /. getRule["Ook"];
	StringRiffle[Partition[orders, UpTo@8], "\n", " "]
];
wrap["dao", out_String] := GeneralUtilities`Scope[
	orders = Characters@StringReplace[out, Whitespace -> ""] /. getRule["Dao"];
	StringRiffle[Partition[orders, UpTo@64], "\n", ""]
];
wrap["\:5468\:6613", out_String] := GeneralUtilities`Scope[
	orders = Characters@StringReplace[out, Whitespace -> ""] /. getRule["\:5468\:6613"];
	StringRiffle[Partition[orders, UpTo@8], "\n", " "]
];
wrap["chinese", out_String] := GeneralUtilities`Scope[
	orders = Characters@out /. getRule["Chinese"];
	StringRiffle[orders, ""]
];
wrap["number", out_String] := "0o" <> StringReplace[StringReplace[out, Whitespace -> ""], getRule["Number"]];
wrap["octal", out_String] := wrap["number", out];


(* ::Subsubsection:: *)
(*Main Functions*)


mod[a_, b_] := If[b - a > 128, 256 - b + a, b - a];
distance[{pos_, target_, value_, {index_}}] := Abs[pos - index] + mod[Min[value, target], Max[value, target]];
format[{pos_, target_, value_, {index_}}] := Block[
	{move, change},
	move = Which[
		pos < index, ConstantArray[">", index - pos],
		pos > index, ConstantArray["<", pos - index],
		True, ""
	];
	change = Which[
		value < target, ConstantArray["+", target - value],
		value > target, ConstantArray["-", value - target],
		True, ""
	];
	move <> change <> "."
];
newState[head_, layout_, selected_] := Block[
	{copy = layout, pos = selected[[-1, 1]]},
	copy[[pos]] = selected[[2]];
	<|
		"Head" -> TemplateApply["`1`\n`2`", {head, format@selected}],
		"Layout" -> copy,
		"Index" -> pos
	|>
];
update[state_Association, target_Integer] := Block[
	{head, layout, index, selected},
	head = state["Head"];
	layout = state["Layout" ];
	index = state["Index" ];
	selected = First@MinimalBy[MapIndexed[{index, target, #1, #2}&, layout], distance];
	newState[head, layout, selected]
];


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


apiEncode[text_String] := Block[
	{api, body, type, http, xml},
	api = "http://ctf.ssleye.com/ctf/ctf_brainfuck_en";
	body = TemplateApply["text=`1`&encode_flag=utf8", {URLEncode@text} ];
	type = "application/x-www-form-urlencoded";
	http = HTTPRequest[api, <|"Body" -> body, Method -> "POST", "ContentType" -> type|>];
	xml = ImportString[URLRead[http, "Body"], {"HTML", "XMLObject"}];
	xml[[2, -1, -1, -1, -1, -1, -2, -1, -1, -1, 1]]
];


getRule[name_] := getRule[name] = Thread[charSet["Normal"] -> charSet[name]];
charSet[list_List] := ToString /@ list;
charSet["Normal"] = {">", "<", "+", "-", "[", "]", ".", ","};
charSet["Number"] = {"0", "1", "2", "3", "4", "5", "6", "7"};
charSet["Chinese"] = {"\:53f3", "\:5de6", "\:589e", "\:51cf", "\:59cb", "\:7ec8", "\:5199", "\:8bfb"};
charSet["Dao"] = {"\:2634", "\:2633", "\:2631", "\:2636", "\:2630", "\:2637", "\:2632", "\:2635"};
charSet["\:5468\:6613"] = {"\:5dfd", "\:9707", "\:514c", "\:826e", "\:5764", "\:4e7e", "\:96e2", "\:574e"};
charSet["Ook"] = {
	"Ook. Ook?", "Ook? Ook.",
	"Ook. Ook.", "Ook! Ook!",
	"Ook! Ook?", "Ook? Ook!",
	"Ook! Ook.", "Ook. Ook!"
};


(*pre-computed states*)
$States = {
	<|
		"Head" -> "-[[<+>->+>+<<]>]",
		"Layout" -> ByteArray["WXfQRxdeddNIG2N+4V9An99+Xds4E0teqQewt2cehaMoy/O+sW8gj68+7SsYQ1ue+ZeQJ7felXMIe4P+gX8AAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "+[[<+>->+>+<<]>]",
		"Layout" -> ByteArray["p4kwuemiiy245Z2CH6HAYSGCoyXI7bWiV/lQSZnie13YNQ1CT5HgcVHCE9XovaViB2lw2Ukia434hX0Cf4EAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "--[[<+>->+>+<<]>]",
		"Layout" -> ByteArray["Mm6gDq68aiaQtkb8Qj6Avj78OjZwpha80o5g7k48isZQFmZ84l5Ant58WtYwBjY8cq4gzu68qmYQdob8gn4AAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "++[[<+>->+>+<<]>]",
		"Layout" -> ByteArray["zpJg8lJEltpwSroEvsKAQsIExsqQWupELnKgErLEdjqw6pqEHqLAYiKEpirQ+srEjlLgMhJEVprwinoEfoIAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-[[<+>->+>--->-<<<]>+++]",
		"Layout" -> ByteArray["5zyQ+A+aeJ6fUNhMd74QYndEgEB/QohGbxiI1Ccm4EqHzPAIb2oYbr9guNxXDjCyF9TgUN8SKBaPKGhkB3YAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "+<-[[<+>->+>--->-<<<]>++]",
		"Layout" -> ByteArray["QofE7x6PSH8aV4xvdp/Qf3InVG9Or1j/Svcc76a/4P+ix+Tvfs9of3qXrG/W3/B/0md0b67veP+qNzzvBv8AAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-<++[[<+>->->+++>+<<<]->]",
		"Layout" -> ByteArray["IDbT8cDoSy+cPseRBHgnR6hW68FY2PPvxP4/gTyIrydwtsPRMAhb7yz+d7G02PdHeFZbIUh4gy/UPm8hbGj/AA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-<<+[+[<+>--->->->-<<<]>]",
		"Layout" -> ByteArray["AAAAAAAAAAAAVjl7/XCzSzFGFSfVUOenYQZRw03gW9NR1q0PJWDPj8H26csdEAObcabFt/Uwt7chJgGTbQAAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-<<+[+[<+>--->->->-<<<]>]",
		"Layout" -> ByteArray["AAAAAAAAAAAAVjl7/XCzSzFGFSfVUOenYQZRw03gW9NR1q0PJWDPj8H26csdEAObcabFt/Uwt7chJgGTbQAAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "+[++[<+++>->+++<]>+++++++]",
		"Layout" -> ByteArray["AAAAAAAJNr1SEU4FKpnmzYKh/hVaKZbdsjGuJYq5Ru3iwV41ukn2/RJRDkXq2aYNQuG+VRppVh1ycW5lSvkAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "+<-[[<+>->+>--->-<<<]>+++]",
		"Layout" -> ByteArray["AAAAAAAAAAAAAAAAAAAA/wMJBOnXG7CLYxVkxYfXgHcjQWSBFzNww0ONBB2HL4Bvw/lEmdfLsHujhST1BwcAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "++<-[[<+>->+>--->-<<<]>+++]",
		"Layout" -> ByteArray["foNCPvj/3Opaw84uBK94alajOv6wf3RKciOGrvxv0IquQ7I+6H+MKgoDvq50r6gqhmOq/qD/JIoiY3YubG8AAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-[++[<++>->+++>+++<<]--->+]",
		"Layout" -> ByteArray["LVeh/e/ZbecR/T/JLfeBfQ+5bYfxfV+pLZdh/S+ZbSfR/X+JLTdBfU95bcexfZ9pLdch/W9ZbWeR/b9JLXf9AA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-<-<<+[+[<+>--->->->-<<<]>]",
		"Layout" -> ByteArray["QdGvwOuNQ8IlnX9AN1m7ksn5j5CjFZOS7aWf8O+BiwJRYW+g211jIrXtv+Anadsy2QlP8JNls3J9dd8Q3xEAAA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "-[++[<++>->+++>+++<<]---->+]",
		"Layout" -> ByteArray["LFag/O7YbOYQ/D7ILPaAfA64bIbwfF6oLJZg/C6YbCbQ/H6ILDZAfE54bMawfJ5oLNYg/G5YbGaQ/L5ILHb8AA=="],
		"Index" -> 64
	|>,
	<|
		"Head" -> "--<-<<+[+[<+>--->->->-<<<]>]",
		"Layout" -> ByteArray["f5hpK0diD8PvwIXv74ozx3/YoQPXgjd77yB9J7+K25//2NkbZ+Lf829AdZ+PygM3/5gRc/eCBytvIG1XX0oAAA=="],
		"Index" -> 64
	|>
};
