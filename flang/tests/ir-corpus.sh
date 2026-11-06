#!/bin/zsh
# ir-corpus.sh new-dir [flang-binary] [module-dir]: LLVM IR for every FCVS
# program and flang/tests/*.f90, so two builds of flang can be compared:
#   diff -r a b
# new-dir must not exist.  It keeps the sources it compiled, by relative path,
# because a source's name goes into the IR.
# Expected difference between hosts: a last bit in a constant flang folds through
# the host's libm (FM828 between macOS and musl).  Anything else is a bug.
HERE=${0:A:h}; ROOT=${HERE:h:h}; O=${1:A}; B=${2:-$HOME/llvm-project/build-flang/bin/flang}; M=${3:-$ROOT/flang/out/mod}
FCVS=${FCVS:-$HOME/refs/fcvs/fcvs21}
[ ! -e $O ] || { print -u2 "$O exists"; exit 2; }
mkdir -p $O/m $O/src; n=0; bad=0
one() { # src out flags...
  local s=$1 o=$2; shift 2
  if ! $B -fc1 -triple slow32-unknown-none -O2 "$@" -fintrinsic-modules-path $M -module-dir $O/m -emit-llvm $s -o $o.raw 2>>$O/errors.log; then print "FAIL $s"; bad=$((bad+1)); fi
  sed -E '/^source_filename|^; ModuleID|^!llvm.ident|^![0-9]+ = !\{!"flang version/d' $o.raw > $o
  mv $o $o.ll; mv $o.raw $O/m/${o:t}.raw
  n=$((n+1))
}
cd $O/src
for f in $FCVS/FM*.FOR; do b=${f:t:r}; awk 'BEGIN{s=0} /^      PROGRAM /&&!s&&c>0{s=1;next} /^      PROGRAM /{c++} {print}' $f > $b.f; one $b.f $O/$b -ffixed-form; done
cp $HERE/*.f90 .
for f in *.f90; do one $f $O/${f:r}; done
print "ir: $n files, $bad failed"
[ $bad -eq 0 ]
