#!/bin/sh

# Run PLT lab 3 testsuite on directory src/

root="$PWD"
cd testsuite
cabal run plt-test-lab3 -- "$root/src"
