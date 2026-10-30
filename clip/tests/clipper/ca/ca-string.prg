* CA: SAMPLE/STRING.PRG
LOCAL a
? "[" + CityState("Austin   ", "TX", "78701") + "]"
a := ListAsArray("apple,pear,,plum", ",")
? LEN(a), "[" + a[1] + "]", "[" + a[3] + "]", "[" + a[4] + "]"
? Occurs("a", "banana"), Occurs("an", "banana"), Occurs("z", "banana")
? "[" + Proper("hello WORLD foo") + "]"
