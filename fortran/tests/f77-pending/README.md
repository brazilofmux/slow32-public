# Pending differential cases

Fortran programs that the oracle runs but our compiler cannot yet
compile. They are kept here, not deleted, so the backlog is visible and
each one can move into `../f77/` the moment its feature lands.

- `sumsq.f` — needs `WRITE`/`FORMAT` (milestone 4) and `ATAN2`
  (intrinsics). The FORMAT engine is the largest single piece of the
  whole project.
