* Functions: AEVAL with a code block that sees element and index
LOCAL a := {5, 6, 7}, s := 0
AEVAL(a, {|e, i| s += e * i})
? s
