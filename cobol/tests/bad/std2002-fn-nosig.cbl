identification division.
program-id. nosig.
*> A function named in REPOSITORY whose signature is nowhere: no prototype
*> or definition earlier in this group, no elsewhere.s32fn beside the
*> output, beside the source or on -I (2023 12.3.8.3 rule 10).
environment division.
configuration section.
repository.
    function elsewhere.
procedure division.
    display elsewhere(1)
    stop run.
end program nosig.
