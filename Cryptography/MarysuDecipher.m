(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


(* ::Subsection:: *)
(*Main Functions*)
CharSet["MarySue"] = StringDelete[StringPartition["\
	丝丹丽之乐云亚仪伊优伤佳依俏倩倾兮兰冰凌凝凡凤凪利千华卿可叶吉君咏哀嘉园城基塔墨夏多奥如妍妖妙妮妲姆姣姬娅娜娣娥娴婉婵婷媛嫩宁安宜\
	寂寇寒岚巧希幻幽弥彩影御心思怡恋恩悠悦情慕慧拉文斯春昭晓晗晶曦曼月朵枝枫柒柔格桂梅梦樱欢欣殇残毓沫泪洁洛浅海涅淑清温渺滢澜澪灵烟然\
	燕燢爱爽玉玖玛玥玫环玲珊珍珠琉琦琪琬琰琳琴琼瑗瑞瑟瑰瑶瑷璃璎璐璧白百盘眉真碎离秀秋筱米素紫红纨纯纱绯缈美羽翠翼育舒舞艳艺艾芊芝芬花\
	芳芸苏苑英茉茗茜茹荔荷莉莎莲莳莹莺菁菲萌萍萝萦萨落蒂蓉蓓蓝蔷蕊蕴蕾薇薰蝶融血裳语贞迷邪铃银锦阳陌雁雅雨雪霄霜霞霭露青静音韵颖颜风飘\
	香馥馨魂魅魑鸢黎黛", 1
], {"\t", "\n"}];
ExEncryptMarySue[expr_] := Block[
	{byte, ans, ins, set = CharSet["MarySue"]},
	byte = Normal@BinarySerialize[{RandomReal[], expr}, PerformanceGoal -> "Size"];
	ans = set[[IntegerDigits[FromDigits[Reverse@byte, 256], Length[set]] + 1]];
	ins = Select[Accumulate[{RandomInteger[{3, 6}]} ~ Join ~ RandomInteger[{2, 8}, Length@ans]], # < Length@ans&];
	StringInsert[StringJoin[ans], "\[CenterDot]", ins]
];

ExDecryptMarySue[str_] := Block[
	{byte, ans, truth},
	ans = StringPartition[StringDelete[str, "\[CenterDot]"], 1];
	byte = Flatten[FirstPosition[CharSet["MarySue"], #]& /@ ans] - 1;
	truth = Reverse@IntegerDigits[FromDigits[byte, Length[set]], 256];
	BinaryDeserialize[ByteArray@truth] // Last
];

(* ::Subsection:: *)
(*Auxiliary Functions*)
