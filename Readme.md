# RPN calculator#

This is a double precision RPN calculator, written in Haskell.

It is mostly intended for interactive use, but it is also scriptable.

For basic use, it is very intuitive if you know RPN

It also has more powerful advanced features, but the interface for these is (unfortunately) somewhat less intuitive.
Documentation on these more advanced features can be found below.

## Compilation

`ghc rpnico.hs`

## Use

### Basic use

For basic use, you almost don't need any instructions, it does what you expect it to.
You write your stack on one line, and it will reduce the operations as much as possible and reply

Example of simple use session:
```
5 4 8 +
5 12.0
7 4 *
5 12.0 28.0
sum
45.0
```

You can turn on hidden stack mode with `-s`.
You can turn the stack back on with `s`.

`q` to exit.

#### Implemented binary operations:

`+, -, *, /, //, %, ^`

`x` is recognized as a synonym for `*` and `**` is a synonym for `^`.

#### Implemented unary operations:

`!, log, exp, sin, cos, tan, cot, trunc`

#### Implemented full stack operations:

`sum, prod`

### Advanced use

RPNico supports multiple stacks.
This allows you to do things such as perform full stack operations on only part of your input, or create user defined functions.

For simple functions this is not difficult, but the interface is a bit odd.

#### How multiple stacks work

The stacks have user defined names.
Only the current working stack is ever shown.
There is no limit to how many stacks can exit at once.

Stacks do not have to be evaluated.
Stacks can be copied onto the end of your current stack.
Writing an unevaluated stack works as a function.
Concatenate an unevaluated stack onto the current stack to apply the function to a stack (see example section).

#### Commands for multiple stacks

#### Examples with multiple stacks
