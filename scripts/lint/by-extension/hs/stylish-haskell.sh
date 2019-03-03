#! /bin/sh
set -eu

# requires stylish-haskell
# requires colordiff
# TODO gracefully fall back to diff if no colordiff

if stylish-haskell "$1" | colordiff "$1" /dev/stdin 2>/dev/null; then
  :
else
  echo "$0 $*"
  exit 1;
fi
