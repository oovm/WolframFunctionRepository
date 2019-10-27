# NQueenSolution


## Reference

- https://rosettacode.org/wiki/N-queens_problem#Mathematica

- https://mathematica.stackexchange.com/questions/164930

- https://mathematica.stackexchange.com/questions/47590


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

Get the queen Image:

```mathematica
Import@"https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Chess_qlt45.svg/200px-Chess_qlt45.svg.png"
```

![](https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Chess_qlt45.svg/200px-Chess_qlt45.svg.png)

## Changelog



#### V1.2

```mathematica
range = Partition[Range[n^2], n];
If[
	OddQ@n,
	range = Mod[range, 2],
	range = MapAt[Boole[EvenQ[#]] &, range, 1 ;; n ;; 2];
	range = MapAt[Boole[OddQ[#]] &, range, 2 ;; n ;; 2];
];
```

 ```mathematica
 range = Boole@NestList[RotateLeft, Table[EvenQ[i], {i, Range@n}], n - 1];
 ```


#### V1.1

PrettyView of solution

#### V1.0

Solution