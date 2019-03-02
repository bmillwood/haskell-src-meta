#! /bin/sh
set -eu

# Given $1=lint/$ext/$blah.sh exists,
# Runs it on all of the $ext files
# Sample usage:
# ./lint/linter-check-all.sh ./lint/hs/stylish-haskell.sh

linter=$1
extension=$(basename "$(dirname "$linter")")

COMPLAINTS="$linter-complaints.txt"
echo "Linting .$extension files with $linter"

find . \
     -name "*.$extension" \
     -and -not -path "*/.stack-work/*" \
     -and -not -path "*/dist/*" \
     -and -not -path "*/dist-newstyle/*" \
     -exec "./$linter" {} \; | tee "$COMPLAINTS"

if [ -s "$COMPLAINTS" ]; then
  rm "$COMPLAINTS"
  echo "$0 $*"
  exit 1
else
  rm "$COMPLAINTS"
  echo "All .$extension files passed $linter"
fi
