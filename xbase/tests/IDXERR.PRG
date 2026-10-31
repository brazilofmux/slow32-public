* INDEX ON with a key that cannot be evaluated: reported, not
* silently built empty; the line is the continued statement's last
USE IDXERRT
APPEND BLANK
REPLACE PE WITH "1", DT WITH CTOD("01/02/86")
x = 1 + ;
    2
INDEX ON PE+DT ;
   TO IDXERRT
? "after"
USE
ERASE IDXERRT.DBF
ERASE IDXERRT.NDX
