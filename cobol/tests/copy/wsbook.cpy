*> WORKING-STORAGE items brought in by COPY
01  ws-count        pic 9(4) comp value 0.
01  ws-total        pic s9(9)v99 comp-3 value 0.
01  ws-eof          pic x value 'N'.
    88  at-eof      value 'Y'.
01  ws-amt          pic s9(9)v99.
