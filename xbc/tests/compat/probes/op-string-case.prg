* Operators: strings compare byte for byte, case and all -- =, ==, <, >,
* and through ASCAN and ASORT
LOCAL a := {"b", "B", "a", "A"}
? "abc" = "ABC", "abc" == "ABC", "abc" < "ABC", "abc" > "ABC", "ab" $ "AB"
? "Zebra" < "apple", "a" > "Z", "abc" = "ABc"
? ASCAN(a, "A"), ASCAN(a, "a")
ASORT(a)
? a[1], a[2], a[3], a[4]
