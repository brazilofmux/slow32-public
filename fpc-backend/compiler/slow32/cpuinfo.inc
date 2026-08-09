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
    fpu_soft,
    fpu_slow32
    );

  tfpuflags =
    (
      FPUSLOW32_SINGLE, { native f32 instructions (FADD.S etc.) in GPRs }
      FPUSLOW32_DOUBLE  { native f64 via even int pairs + FADD.D etc. }
    );

const
  fputypestr: array[tfputype] of string[8] = (
    'NONE',
    'LIBGCC',
    'SOFT',
    'SLOW32'
    );

  fpu_capabilities: array[tfputype] of set of tfpuflags = (
    { fpu_none   } [],
    { fpu_libgcc } [],
    { fpu_soft   } [],
    { fpu_slow32 } [FPUSLOW32_SINGLE,FPUSLOW32_DOUBLE]
    );
