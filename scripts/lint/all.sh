#! /bin/sh
set -eu

# Usage:
# ./lint/check.sh

# Runs all linters found at lint/$extension/$linter.sh
# ($linter may not have periods in the name)

CHECK_FAILED="overall-lint-check-failed.txt"

echo "Running all linters"
echo ""

for linter in scripts/lint/by-extension/*/*.sh; do
  if ./scripts/lint/lint-all-with.sh "$linter"; then
    echo ""
  else
    echo ""
    echo "fail" >> $CHECK_FAILED
  fi
done

for linter in scripts/lint/fixed/*.sh; do
  if "./$linter"; then
    echo ""
  else
    echo "./$linter"
    echo ""
    echo 'fail' >> $CHECK_FAILED
  fi
done

if [ -e $CHECK_FAILED ]; then
  rm $CHECK_FAILED
  echo "One or more linters FAILED"
  echo "$0 $*"
  exit 1
else
  echo "All linters passed"
fi
