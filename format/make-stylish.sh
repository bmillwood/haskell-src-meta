#! /bin/sh
set -eu

find . \
  -name "*.hs" \
  -and -not -path "*/.stack-work/*" \
  -and -not -path "*/dist/*" \
  -and -not -path "*/dist-newstyle/*" \
  -exec stylish-haskell -i {} \;
