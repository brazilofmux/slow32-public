identification division.
program-id. nosig.
*> A function named in REPOSITORY whose signature is nowhere: not defined
*> earlier in this source, no elsewhere.s32fn beside the output or on -I.
environment division.
configuration section.
repository.
    function elsewhere.
procedure division.
    display elsewhere(1)
    stop run.
end program nosig.
