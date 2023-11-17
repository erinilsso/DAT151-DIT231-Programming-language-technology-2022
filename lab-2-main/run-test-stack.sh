#!/bin/sh

# Run PLT lab 2 testsuite on directory src/

root="$PWD"
cd testsuite
stack run -- "$root/src"
