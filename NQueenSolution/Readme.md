# NQueenSolution


## Reference

- https://rosettacode.org/wiki/N-queens_problem#Mathematica

- https://mathematica.stackexchange.com/questions/164930




```mathematica
Count[Permutations[v=Range@8],And@@Unequal@@@{#+v,#-v}&]
```

Only 57 characters to give all solutions!

```mathematica
n = 16;
f = With[
	{X = Symbol["x" <> ToString[#]]&},
	Table[
		{
			X[j + 1],
			If[
				Or @@ Table[X[j] == X[j - i] || Abs[X[j] - X[j - i]] == i, {i, j - 1}] /. f : _Or :> Sort[f],
				0,
				Evaluate@If[j < n, n, 1]
			]
		},
		{j, n}
	]
];
AbsoluteTiming[cf = f /. {iter__} :> Compile[
	{{x1, _Integer}},
	Module[
		{cnt = 0},
		Do[cnt++, iter];cnt
	],
	CompilationTarget -> "C",
	RuntimeOptions -> "Speed",
	RuntimeAttributes -> {Listable}
]];
Total@cf@Range[n] // AbsoluteTiming
2Total@Most[#] + If[OddQ@n, Last@#, 0]&@cf@Range[n / 2 + 1] // AbsoluteTiming
```


## Changelog

