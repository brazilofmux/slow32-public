# Simple test: print "Hi" using DEBUG and halt
.text
    li r3, 72
    debug r3
    li r3, 105
    debug r3
    li r3, 10
    debug r3
    halt
