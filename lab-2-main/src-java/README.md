# Solution stub in Java

Files to edit:

- [`TypeChecker.java`](TypeChecker.java)
- [`Interpreter.java`](Interpreter.java)
- other classes you might need to implement type checker and interpreter

## Invoking `java lab2` robustly

This stub contains 3 ways of simple tools to invoke `java lab2` with the correct `CLASSPATH` setting.

1. `lab2.hs` (default):
   The `Makefile` goal `lab2` creates `lab2` from `lab2.hs` using `ghc`.
   If you have `ghc` set up, this should work out of the box.

2. `lab2.sh`:
   Rather than using `lab2.hs`, you can use this script by these two steps:
   - Rename `lab2.sh` to `lab2` and ensure it is executable.
   - Remove the `lab2` goal from the `default` goal in the `Makefile`.

3. `lab2.bat`:
   If you are working under Windows, you can use this simple script.
   To this end:
   - Remove the `lab2` goal from the `default` goal in the `Makefile`.
