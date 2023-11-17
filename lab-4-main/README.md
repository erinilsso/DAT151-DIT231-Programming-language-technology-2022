# Lab 4: Interpreter for a functional language

## Summary

The objective of this lab is to write an interpreter for a small, untyped functional programming language.
This language is a tiny subset of Haskell.
The interpreter should walk through programs and print out the value of the `main` function.

Before the lab can be submitted, the interpreter has to pass the [test suite](testsuite/).

The recommended implementation is via a BNF grammar processed by the BNF Converter (BNFC) tool.
No type checker is needed.

The approximate size of the grammar is 15 rules, and the interpreter code should be about 100 lines, depending on the programming language used for the implementation.
You can use grammar [`Fun.cf`](./Fun.cf) if you want.

All BNFC supported languages can be used, but guidance is guaranteed only for Haskell and Java.

## Language specification

The language is the same as in the PLT book, Chapter 7.

### Syntax

The main category is `Program`.
A program is a sequence of **definitions**, which are terminated by semicolons.
A definition is a function name followed by a (possibly empty) list of variable names followed by the equality sign `=` followed by an **expression**:
```haskell
f x1 ... xn = e ;
```
Both `f` and the variables `x1` ... `xn` are lexical identifiers.
Thus `f` is the function to be defined, and `x1` ... `xn` are its arguments.
These variables are considered **bound** in `e`.
Notice that the all such definitions can be converted to definitions of just `f` with a lambda abstraction over its arguments.

The last definition has a special form.  Its name is `main`, it has no variables, and its body is a call to the (undefined) unary function `print`.
For example:
```haskell
main = print (2 + 2) ;
```
The purpose of this special form is to make our language a subset of Haskell.
You can run well-formed programs in Haskell to check the expected result.

Expressions are of the following forms:

| precedence | expression  | syntax                  | example              |
|:----------:|:------------|:------------------------|:---------------------|
|  3         | identifier  | `x`                     | `foo`                |
|  3         | integer     | `i`                     | `512`                |
|  2         | application | `e2 e3`                 | `f x`                |
|  1         | operation   | `e1 op e2`              | `3 + x`              |
|  0         | conditional | `if e0 then e0 else e0` | `if c then a else b` |
|  0         | abstraction | `\x -> e0`              | `\x -> x + 1`        |

Applications and operations are left-associative.
Conditionals and abstractions are prefix operators and thus necessarily right-associative.

The available operations are `+`, `-`, and `<`.

Here is an example of a program in the language:
```haskell
-- Multiplication with unnecessary special case for y=1.

mult x y =
  if (y < 1) then 0 else
  if (y < 2) then x else
  (x + (mult x (y-1))) ;

mult_with_less_parentheses x y =
  if y < 1 then 0 else
  if y < 2 then x else
  x + mult_with_less_parentheses x (y-1) ;

-- The factorial function.

fact = \x -> if (x < 3) then x else mult x (fact (x-1)) ;

-- Compute factorial of 6.

main = print (fact 6) ;
```
Lines starting with `--` are comments.

### Scoping

Every function is in scope in the entire program, including the expression part of its definition (which results in recursive and mutually recursive functions).
Exception: the `main` function is not in scope, thus, cannot be called from another function.

The variables bound on the left-hand-side of a definition are in scope in the expression part of the definition.

The variable `x` in an abstraction `\x -> e` is bound in the body of the abstraction, i.e., `e`.

Bindings made inside a scope overshadow those made outside.

### Semantics

There is just one type of basic values: integers.
The operations `+, -, <` have their usual integer semantics.
The comparison `<` has value `1` if it is true, `0` if false.

The conditional `if c then a else b` is evaluated "lazily" so that if `c` has value `0` then `b` is evaluated, otherwise `a` is evaluated.

The output of a program is the value of the expression passed to `print` in the `main` definition, and it must be an integer.

Besides integer expressions, a program usually contains expressions that denote functions, which orginate from definitions and abstractions.
Functions can be applied to arguments, be passed as arguments to other functions and also be the result of a function call.

The interpretation of a program may exit with an error, in one of these cases:
* The `main` function is missing.
* An identifier is not bound.
  It should then report the unbound identifier by name.
* Something which is not a function is applied to an argument.
* An arithmetic operation is attempted on non-integers, e.g.
  ```haskell
  f x = x + x ;
  main = print (f + f) ;
  ```

All these errors occur at run time, because there is no type checker.

Evaluation is parametrized so that it can be performed in both call-by-value and call-by-name.

## Lab format

### Solution templates

For Haskell and Java there are stubs that can be extended to the full solution.
Just link one of these subdirectory to `src/`:

- [`src-haskell/`](src-haskell/): Haskell template
- [`src-java/`](src-java/): Java template

These directories contain the grammar, stubs for the interpreter, and suitable makefiles.
If you start from these stubs, you will likely match the requirements for the solution format as detailed in the following:

### Input and output

Calling the interpreter should work by the command
```console
$ lab4 [-n|-v] <File>
```
The flag `-n` forces call-by-name evaluation, the flag `-v` forces call-by-value. The default, i.e., when no flag is present, is call-by-value.

The output at success must be just the output defined by the interpreter.

The output at failure is an `INTERPRETER ERROR`.
The error message should also give some useful explanation, which we leave to your imagination.

### Example of success

Source file:
```haskell
-- File: good.hs

mult x y = if y < 1 then 0 else x + mult x (y-1) ;
fact     = \x -> if x < 3 then x else mult x (fact (x-1)) ;
main     = print (fact 6) ;
```

Running the interpreter:
```console
$ ./lab4 good.hs
720
```

### Example of failure

Source file:
```haskell
-- File: bad.hs

mult x y = if y < 1 then 0 else x + mult x (y-1) ;
fact     = \x -> if x < 3 then x else mul x (fact (x-1)) ;
main     = print (fact 6) ;
```

Running the interpreter:
```console
$ ./lab4 bad.hs
INTERPRETER ERROR: unknown identifier mul
```

### Example of call-by-name vs. call-by-value

Source file:
```haskell
-- File: infinite.hs

grow  x   = 1 + grow x ;
first x y = x ;
main      = print (first 5 (grow 4)) ;
```

Running the interpreter:
```console
$ ./lab4 infinite.hs
<infinite loop>

$ ./lab4 -n infinite.hs
5
```

### Compiling the interpreter

Use a `Makefile` similar to Lab 2.
The interpreter should be compilable via calling
```console
$ make
```

## Test programs

<!--
_If you have any problems getting the test program to run, or if you think that there is an error in the test suite, contact the teachers of the course via the mailing list._
-->

Run the programs in the [test suite](testsuite/) before submitting the lab.

## Success criteria

The interpreter must give acceptable results for the [test suite](testsuite/) and meet the specification in this document in all respects.

All "good" programs must work with at least one of the evaluation strategies; need not work on both (because of loop or long time); see comments in test programs to see which one is expected to work.

The solution must be written in an easily readable and maintainable way.
In particular, tailoring it for the programs in the test suite is not maintainable!

# Submission

1. Do not submit your solution before it passes the [test suite](testsuite/) if you want proper grading.

2. Make sure your solution has the following structure:
   - Subdirectory `src/` contains the sources of your solution plus a `Makefile`.
   - After `make` is invoked in `src/`, there should be a runnable program `src/lab4`.
   - This program takes options and a filepath as command-line parameter and interprets this file, printing interpreter errors and/or interpreter results to the standard output.

   See the example solution stubs `src-haskell/` and `src-java/`.
   If you solve this lab in Haskell or Java, you can symlink `src/` to one of `src-haskell/` or `src-java/` and work there.
   For another programming language, create `src/` and place a suitable `Makefile` there.

3. Make sure all your source files are pushed to your lab repository on `git.chalmers.se`.
   Do not submit generated files, only submit source files.

   Run the testsuite _on the server_ by pushing a `test` tag (e.g. `test0`, `test1` etc.).

4. Test that your submission builds in a fresh clone:
   ```console
   TMPDIR=`mktemp -d`
   git clone YOUR_LAB_REPO $TMPDIR
   cd $TMPDIR/src
   make
   ./lab4
   ```
    On Windows, the tools used here (`bash`, `mktemp`, `make`) are provided by both WSL and [MSYS2](https://www.msys2.org/).

5. Finalize your submission by adding a `submission` tag (e.g. `submission0`, `submission1`).
   The last submission before the deadline counts.
