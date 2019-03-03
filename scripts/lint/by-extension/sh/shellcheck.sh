#! /bin/sh
set -eu

# TODO: color sometimes not? ok in CI?
if shellcheck --color=always "$1"; then
  :
else
  echo "$0 $*"
  exit 1
fi
