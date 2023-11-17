#!/bin/sh

# Run PLT lab 1 testsuite on grammar file CC.cf

root="$PWD"
cd testsuite
cabal run plt-test-lab1 -- "$root/CC.cf"
