#!/bin/sh

# Run PLT lab 2 testsuite on directory src/

root="$PWD"
cd testsuite
runghc plt-test-lab2 -- "$root/src"
