\ m2.fth — control flow, loops, r-stack, comparisons.

: YES 89 EMIT ;   : NO 78 EMIT ;

: T-IF   1 IF YES ELSE NO THEN
         0 IF NO ELSE YES THEN CR ;

: T-UNTIL  0 BEGIN 1+ DUP 5 >= UNTIL . CR ;

: T-WHILE  10 BEGIN DUP 0 > WHILE DUP . 3 - REPEAT DROP CR ;

: T-DO     5 0 DO I . LOOP CR ;

: T-NEST   3 0 DO 2 0 DO J . I . 32 EMIT LOOP LOOP CR ;

: T-?DO    7 7 ?DO NO LOOP YES CR
           9 7 ?DO I . LOOP CR ;

: T-+LOOP  20 0 DO I . 5 +LOOP CR ;

: T-LEAVE  100 0 DO I DUP . 3 = IF LEAVE THEN LOOP CR ;

: T-RSTK   42 >R 7 . R@ . R> . CR ;

: T-CMP    3 4 <  . 4 3 <  . 3 3 =  . 3 4 <> . CR
           5 2 >  . 2 2 >= . 2 3 <= . -1 1 U< . CR
           0 0=   . 7 0=   . -5 0<  . 5 0<>  . CR
           12 10 AND . 12 10 OR . 12 10 XOR . CR ;

: MAIN T-IF T-UNTIL T-WHILE T-DO T-NEST T-?DO T-+LOOP
       T-LEAVE T-RSTK T-CMP ;
