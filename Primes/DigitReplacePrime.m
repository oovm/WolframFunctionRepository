(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsection:: *)
(*Wrap Function*)


DigitReplacePrime[n_, m_, p_] := Select[#, PrimeQ] & /@ Sort[Select[end[n, m], Total@Boole@PrimeQ@# == p &]];


(* ::Subsection:: *)
(*Main Functions*)


base[k_] := Array[a, k];
space[k_, i_] := Subsets[Range[k - 1], {i}];
remain[k_, i_] := Select[
	IntegerDigits /@ Range[10^(k - 2 - i), 10^(k - 1 - i) - 1],
	Mod[Total[#], 3] != 0 &
];
replace[k_, i_] := Complement[Range[k - 1], #] & /@ space[k, i];
last = {1, 3, 7, 9};
tab[k_, i_] := Flatten[#, 1] &@Table[
	base[k] /. (Thread[Rule[base[k][[#]] & /@ o, m]]),
	{o, replace[k, i]},
	{m, remain[k, i]}
];
lastNum[n_, k_] := Table[tab[n, k] /. a[n] :> z, {z, last}] // Flatten[#, 1] &;
d = Table[a[_] -> i, {i, 0, 9}];
f = DeleteCases[#, a_ /; First[a] == 0] &;
end[n_, k_] := Map[FromDigits, #, {2}] &@(f /@ Outer[ReplaceAll, lastNum[n, k], d, 1])


(* ::Subsection:: *)
(*Auxiliary Functions*)
