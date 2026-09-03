# stdin for the scheduler demo: quiet for half a second, then one line, then
# EOF.  The reader task's async read stays in flight the whole time, while
# the ticker task's short timers fire -- so the ticks provably happen while
# the read is pending, deterministically on every engine.
sleep 0.5
printf 'async payload\n'
