# Cryptography




## Changelog

### v1.2

- 新增 SpectogramEncode

### v1.1 

修复了 `MorseCipher` 中 unicode 编码和原始 morse 编码重复的问题

### v1.0

- 新增 MorseCipher
- 新增 MorseDecipher

## Reference

https://zhuanlan.zhihu.com/p/80587769

https://www.zhihu.com/question/22506052

http://ctf.ssleye.com/

https://gist.github.com/codexss/596be5acbf2362e9995755c1063be3e6



```mathematica
print = "." <> StringJoin@ConstantArray["<.", 63];
getLayout[head_String] := Block[
	{out = BrainLanguageEvaluate[head <> print, CompilationTarget -> "OutputDebug"]},
	<|"Head" -> head, "Layout" -> ByteArray@Reverse@out, "Index" -> 64|>
]


SortBy[{
	"--<-<<+[+[<+>--->->->-<<<]>]",
	"-<-<<+[+[<+>--->->->-<<<]>]",
	"-<<+[+[<+>--->->->-<<<]>]",
	"-[++[<++>->+++>+++<<]---->+]",
	"+[[<+>->+>+<<]>]",
	"++<-[[<+>->+>--->-<<<]>+++]",
	"+<-[[<+>->+>--->-<<<]>+++]",
	"-<++[[<+>->->+++>+<<<]->]",
	"+[++[<+++>->+++<]>+++++++]",
	"-[++[<++>->+++>+++<<]--->+]",
	"++[[<+>->+>+<<]>]",
	"+<-[[<+>->+>--->-<<<]>++]",
	"-[[<+>->+>--->-<<<]>+++]",
	"-<<+[+[<+>--->->->-<<<]>]"
}, StringReverse] // ResourceFunction["ReadableForm"]


getLayout /@ SortBy[{
	"+[[<+>->+>+<<]>]",
	"-[[<+>->+>+<<]>]",
	"++[[<+>->+>+<<]>]",
	"--[[<+>->+>+<<]>]",
	"-[[<+>->+>--->-<<<]>+++]",
	"+<-[[<+>->+>--->-<<<]>++]",
	"-<++[[<+>->->+++>+<<<]->]",
	"-<<+[+[<+>--->->->-<<<]>]",
	"-<<+[+[<+>--->->->-<<<]>]",
	"+[++[<+++>->+++<]>+++++++]",
	"+<-[[<+>->+>--->-<<<]>+++]",
	"++<-[[<+>->+>--->-<<<]>+++]",
	"-[++[<++>->+++>+++<<]--->+]",
	"-<-<<+[+[<+>--->->->-<<<]>]",
	"-[++[<++>->+++>+++<<]---->+]",
	"--<-<<+[+[<+>--->->->-<<<]>]"
}, StringReverse] // ResourceFunction["ReadableForm"]
```