' ON TIMER: when a handler runs, what it sees, what holds it back.  What
' prints does not depend on how fast this machine is: each wait is half a
' second and more away from the tick it is counting
DECLARE SUB waits (t!)
DECLARE SUB insub ()
DECLARE SUB deep (d%)
DECLARE FUNCTION give2% ()
ON ERROR GOTO oops
ON TIMER(1) GOSUB tick
PRINT "not on"; n
waits 1.5
PRINT n
TIMER ON
waits 2.5
TIMER OFF
PRINT "on, off"; n
waits 1.5
PRINT n
n = 0
TIMER ON
TIMER STOP
waits 1.5
PRINT "stopped"; n
TIMER ON
x = 1
PRINT "kept one"; n
TIMER OFF
n = 0
' in a procedure: the handler's names are the main program's, and it
' comes back there; so too in a recursion and a FUNCTION
TIMER ON
insub
TIMER OFF
PRINT "after sub"; n; v
n = 0
TIMER ON
deep 3
PRINT give2%; n
TIMER OFF
' held while its own handler runs, and one event kept
n = 0: m = 0
ON TIMER(1) GOSUB slow
TIMER ON
waits 2.5
TIMER OFF
PRINT "slow"; n; m
' a rebind and a turn on start the count again; GOSUB 0 takes the handler away
n = 0
ON TIMER(2) GOSUB tick
TIMER ON
waits 1.5
ON TIMER(2) GOSUB tick
waits 1
PRINT "rebound"; n
TIMER OFF
TIMER ON
ON TIMER(1) GOSUB 0
waits 1.5
PRINT "none"; n
TIMER OFF
' 1.4 is 1
n = 0
ON TIMER(1.4) GOSUB tick
TIMER ON
waits 2.5
TIMER OFF
PRINT "rounded"; n
' SLEEP ends for it
n = 0
ON TIMER(1) GOSUB tick
TIMER ON
s! = TIMER
SLEEP 3
PRINT "slept"; INT(TIMER - s! + .5); n
TIMER OFF
' an error in the handler is the handler's handler's; none during an error's
n = 0: z = 0
ON TIMER(1) GOSUB bad
TIMER ON
waits 1.5
TIMER OFF
PRINT "bad"; n; z
n = 0
ON TIMER(1) GOSUB tick
TIMER ON
ERROR 7
PRINT "after error"; n
TIMER OFF
' CLEAR turns it off
TIMER ON
CLEAR
ON ERROR GOTO oops
waits 1.5
PRINT "cleared"; n
' numbered handlers, and RETURN to a label from under a procedure
ON TIMER(1) GOSUB 900
TIMER ON
insub
PRINT "not reached"
SYSTEM
tick: n = n + 1: v = 7: RETURN
slow:
n = n + 1
waits 1.2
m = m + 1
RETURN
bad: n = n + 1: ERROR 9: n = n + 10: RETURN
oops:
z = ERR
IF ERR = 7 THEN waits 1.5: PRINT "in error"; n
RESUME NEXT
900 n = n + 1: RETURN away
away:
TIMER OFF
PRINT "away"; n
SYSTEM

SUB waits (t!)
    s0! = TIMER
    DO WHILE TIMER - s0! < t!: LOOP
END SUB

SUB insub
    v = 1
    waits 1.5
    PRINT "v"; v
END SUB

SUB deep (d%)
    IF d% = 0 THEN waits 1.5 ELSE deep d% - 1
END SUB

FUNCTION give2%
    waits 1.2
    give2% = 42
END FUNCTION
