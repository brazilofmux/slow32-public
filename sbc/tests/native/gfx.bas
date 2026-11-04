' graphics in s32-bc: operands evaluated and made integers in order
SCREEN 12
X = 10.6: PSET (X, 20), 4: PRESET (X + 1, 21)
LINE (0, 0)-(X * 2, 50), 2, BF: LINE -STEP(5, 5), 3
CIRCLE (100, 100), 30.4, 1: PAINT (100, 100), 2, 1
PALETTE 1, 65536: PALETTE
PRINT "drawn"
