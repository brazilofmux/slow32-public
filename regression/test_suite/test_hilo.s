.text
lui r1, %hi(test_symbol)
ori r1, r1, %lo(test_symbol)
.data
test_symbol: .word 42
