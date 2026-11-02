#!/bin/sh
# benchmarks/dev-sweep.sh -- run limit-sweep.rkt's scenarios against the ORIGINAL library.
#
# The oracle is ../racket-rope under its dev branch. Its collection roots at `rope`; the
# reimplementations collection roots at `data/rope`. Resolution is forced with racket CLI
# flags (raco make has no such flags, so the sweep runs from source, compiled in-memory):
#
#   -U   drops the user links file, which maps `rope` to a different branch checkout
#   -X   sets the overlay as the main collects root: overlay/rope/ holds the dev tree
#        (contents map to collection `rope`), main.rkt -> rope.rkt serves the bare module
#   -S   roots *after* the overlay: the dev-shim root (data/rope/*: the shim presenting the
#        oracle under the harness names) and Racket's own collects (builtins)
#
# The oracle tree is re-extracted and the overlay rebuilt each run, so no stale state from a
# previous branch can load. Compiled .zo files are NOT used: the sweep is run uncompiled,
# because the project's existing .zo (racket-facts §2) records paths for the real core.
#
# Usage: benchmarks/dev-sweep.sh [key=value ...]   (same config grammar as limit-sweep)
# e.g.   benchmarks/dev-sweep.sh elem=string sizes=1000000 scenarios=type,randref
set -eu
repo=$(pwd)
oracle=/tmp/opencode/rope-dev
overlay=/tmp/opencode/rope-olay
rm -rf "$oracle" "$overlay"
mkdir -p "$oracle" "$overlay/rope" "$oracle/compiled"
git -C ../racket-rope archive dev | tar -x -C "$oracle"

# Overlay layout: the dev tree IS collection `rope`, so its contents sit under overlay/rope/.
for entry in class class.rkt generic generic.rkt signature.rkt stxclasses.rkt type type.rkt instance.rkt private cursor.rkt rope.rkt; do
  ln -s "$oracle/$entry" "$overlay/rope/$entry"
done
cp "$oracle/rope.rkt" "$overlay/rope/main.rkt"
ln -s "$oracle/compiled" "$overlay/rope/compiled"

exec racket -U -X "$overlay" \
  -S "$repo/benchmarks/dev-shim" \
  -S "$repo" \
  -S /usr/share/racket/collects \
  benchmarks/limit-sweep.rkt "$@"
