#!/bin/bash
# Replace runtime/libm/musl with a musl release's math sources (README.md).
#   runtime/libm/update-musl.sh VERSION
set -euo pipefail
V="${1:?usage: update-musl.sh VERSION (e.g. 1.2.7)}"
HERE="$(cd "$(dirname "$0")" && pwd)"
KEY_FPR="836489290BB6B70F99FFDA0556BCDB593020450F"
W="$(mktemp -d)"
trap 'rm -rf "${W:?}"' EXIT

cd "$W"
curl -fsSLO "https://musl.libc.org/releases/musl-$V.tar.gz"
curl -fsSLO "https://musl.libc.org/releases/musl-$V.tar.gz.asc"
curl -fsSLO "https://musl.libc.org/musl.pub"
mkdir -m 700 gnupg
gpg --homedir gnupg --import musl.pub 2>/dev/null
gpg --homedir gnupg --status-fd 1 --verify "musl-$V.tar.gz.asc" "musl-$V.tar.gz" 2>/dev/null \
    | grep -q "VALIDSIG $KEY_FPR" || { echo "signature does not verify with $KEY_FPR" >&2; exit 1; }
echo "signature good: $KEY_FPR"
echo "sha256: $(shasum -a 256 "musl-$V.tar.gz" | cut -d' ' -f1)"

mkdir src
tar -xzf "musl-$V.tar.gz" -C src
M="src/musl-$V"
old="$(cd "$HERE/musl/src/math" && ls | sort)"
new="$(cd "$M/src/math" && ls -p | grep -v / | sort)"
echo "--- files gone:";  comm -23 <(echo "$old") <(echo "$new") || true
echo "--- files new:";   comm -13 <(echo "$old") <(echo "$new") || true

# replace the snapshot: the same set of paths, from the new release
NEWTREE="$W/musl"
mkdir -p "$NEWTREE/src/math" "$NEWTREE/src/internal" "$NEWTREE/arch/generic"
cp "$M"/src/math/*.c "$M"/src/math/*.h "$NEWTREE/src/math/"
cp "$M/src/internal/libm.h" "$NEWTREE/src/internal/"
cp "$M/arch/generic/fp_arch.h" "$NEWTREE/arch/generic/"
cp "$M/COPYRIGHT" "$NEWTREE/"
OLDTREE="$HERE/musl.old.$$"
mv "$HERE/musl" "$OLDTREE"
mv "$NEWTREE" "$HERE/musl"
rm -rf "${OLDTREE:?}"

python3 "$HERE/gen.py"
echo "updated to musl $V: update README.md's table, rebuild both libraries, run the gates"
