' HEX$ and OCT$ of a negative number: an INTEGER's 16 bits, anything
' else's 32 -- QBasic 1.1's answers
x! = -1: y# = -2.5: z& = -3
PRINT HEX$(-1); " "; HEX$(-1!); " "; HEX$(x!); " "; HEX$(y#); " "; HEX$(z&); " "; HEX$(-40000); " "; HEX$(65535); " "; HEX$(-32768)
PRINT OCT$(-1); " "; OCT$(x!); " "; OCT$(-40000)
