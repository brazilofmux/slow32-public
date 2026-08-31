# Pending differential cases

Fortran programs that the oracle runs but our compiler cannot yet
compile. They are kept here, not deleted, so the backlog is visible and
each one can move into `../f77/` the moment its feature lands.

- `sumsq.f` — FORMAT has landed; still needs `ATAN2` (and the rest of
  the real-exponent / transcendental intrinsics).
