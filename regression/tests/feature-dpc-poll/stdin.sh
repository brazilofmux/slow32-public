# The test's stdin: a pipe that is not readable for half a second, then
# carries one line, then is at end of file.  A short timer must win the
# first wait; readiness must win the second and third.
sleep 0.5
printf 'hi\n'
