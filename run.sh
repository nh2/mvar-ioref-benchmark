#!/bin/bash
set -e
stack --resolver lts-7.14 --install-ghc exec --package criterion --package async -- ghc -O --make mvar-ioref-benchmark.hs -threaded
./mvar-ioref-benchmark modify -o results.html +RTS -N4
