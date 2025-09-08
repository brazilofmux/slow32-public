; Expected patterns for i64 arithmetic
; We're looking for the optimized carry/borrow sequences

; For test_add_simple, expect:
; - add low words
; - sltu for carry detection 
; - add high words with carry

; For test_sub_simple, expect:
; - sltu for borrow detection
; - sub high words with borrow
; - sub low words

; The actual verification is that:
; 1. Code compiles without errors
; 2. Uses sltu for carry/borrow (not complex bit operations)
; 3. Results in ~4 instructions per operation