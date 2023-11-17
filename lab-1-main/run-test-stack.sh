#!/bin/sh

# Run PLT lab 1 testsuite on grammar file CC.cf

root="$PWD"
cd testsuite
stack run -- "$root/CC.cf"
