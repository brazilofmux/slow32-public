* CA: SAMPLE/ITERATOR.PRG
LOCAL a, n := 0
IEval(4, {|i| n += i})
? n
a := Collect({1, 2, 3}, {|x| x * 10})
? LEN(a), a[1], a[3]
a := Extract({1, 2, 3, 4, 5, 6}, {|x| x % 2 == 0})
? LEN(a), a[1], a[3]
