# The test's stdin: a pipe that is not readable for half a second, then
# carries one line, then is at end of file.  A short timer must win the
# first wait; a POST of stdin must own the line; EOF readiness after.
sleep 0.5
printf 'hi\n'
