{
    Basic Processor information for SLOW-32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}

type
  bestreal = double;
  bestrealrec = TDoubleRec;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts128real = extended;
  ts64comp = comp;

  pbestreal = ^bestreal;

  { SLOW-32 has no optional extensions - just a single capability: hardware multiply }
  tcpuflags =
     (CPUS32_HAS_MUL
     );

  tfputype =
    (fpu_none,
    fpu_libgcc,
    fpu_soft
    );

const
  fputypestr: array[tfputype] of string[8] = (
    'NONE',
    'LIBGCC',
    'SOFT'
    );
