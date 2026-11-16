* test_safety: SET SAFETY's questions, answered by the script's keys
USE SAFET
COPY TO SAFET2
COPY TO SAFET2
? "after No"
COPY TO SAFET2
? "after Yes"
ZAP
? "kept:", RECCOUNT()
ZAP
? "zapped:", RECCOUNT()
USE
ERASE SAFET2.DBF
