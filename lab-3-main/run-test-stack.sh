#!/bin/sh

# Run PLT lab 3 testsuite on directory src/

root="$PWD"
cd testsuite
stack run -- "$root/src"
