# SimpleLambda

A lambda expression interpreter, written as a personal
project. This isn't the best, most accurate, or advanced
system. It just gets the job done because I thought it
sounded like it'd be an easy and short project when I
started it (lambda calculus has so few rules, after all!)

To use, build with stack. Then call the binary with the
location of a file with lambda expressions in it. The
output will look like:

    0. (λx.xx)(λy.zy)(λs.ss)(λq.uq)(v)
    1. (λy.zy)(λy.zy)(λq.uq)(λq.uq)(v)
    2. (z)(λy.zy)(u)(λq.uq)(v)
    3. (z)(z)(u)(u)(v)

The above code doesn't do anything useful, mind you.

# Project Structure

Syntax.hs
    Defines the parse tree.

Parse.hs
    Processes strings and return trees.

Alpha.hs
    Defines a custom data structure for avoiding naming
    conflicts, and also includes eta rules for giving a
    canonical form to lambda expressions so they may be
    compared for equality.

Beta.hs
    Beta reduction defines how values are passed into
    lambda expressions.

Eval.hs
    Interpret and evaluate lambda expressions.
