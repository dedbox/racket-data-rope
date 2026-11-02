#!/usr/bin/env bash
# tools/lint.sh -- comment-style self-check for AGENTS.md "Prose" and "Typography".
# Prints every finding, one per line; exits 1 when any rule is violated.
set -uo pipefail
paths=(data/rope tests benchmarks)
fail=0
report() { if [ -n "$1" ]; then printf '%s\n' "$1"; fail=1; fi; }

# trailing whitespace in comments
report "$(grep -rn --include='*.rkt' '[[:space:]]$' "${paths[@]}")"
# typography: lambda should be written as λ
report "$(grep -rn --include='*.rkt' '(lambda (' "${paths[@]}")"
# definition docs stay imperative, result-first
report "$(grep -rn --include='*.rkt' -E '^ *(;;;?)[[:space:]]+(Returns|Builds|Concatenates|Replaces)' "${paths[@]}")"
# comment lists use -, not *
report "$(grep -rn --include='*.rkt' -E '^ *(;;;?)[[:space:]]+\* ' "${paths[@]}")"
# note citations must name the notes/ path
report "$(grep -rn --include='*.rkt' -oE '[A-Za-z0-9./-]+\.org' "${paths[@]}" | grep -v 'notes/')"
report "$(grep -rnE --include='*.rkt' 'notes/[a-z0-9-]+([^.A-Za-z0-9-]|$)' "${paths[@]}")"
# comment lines respect the 93-column cap
report "$(awk '/^ *(;;;?)/ && length($0) > 93' data/rope/*.rkt tests/*.rkt benchmarks/*.rkt)"

exit "$fail"
