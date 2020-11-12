# RPNico RPN calculator #

This is a double precision RPN calculator, written in Haskell.

It is mostly intended for interactive use, but it is also scriptable.

For basic use, it is very intuitive if you know RPN

It also has more powerful advanced features, but the interface for these is (unfortunately) somewhat less intuitive.
Documentation on these more advanced features can be found below.

## Compilation

`ghc rpn.hs`

For the simple version:

`ghc rpn-simple.hs`

## Use

### Basic use

rpn-simple and rpn have the same behavior for basic use.

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

`sum, prod, mean, var`

"var" does not use Bessel's correction. 

### Advanced use

The full version of RPNico supports multiple stacks.
This allows you to do things such as perform full stack operations on only part of your input, or create user defined functions.

For simple functions this is not difficult, but the interface is a bit odd.

#### How multiple stacks work

The stacks have user defined names.
Only the current working stack is ever shown.
There is no limit to how many stacks can exist at once.
The default stack is named "main" unless this name is changed.

Stacks do not always have to be evaluated.
Stacks can be copied onto the end of your current stack.
Writing a stack that is not fully evaluated can behave as a function.
Copy an unevaluated stack onto the end of the current stack to apply the function to a stack (see example section).

#### Commands for multiple stacks

These commands are read in RPN format: The arguments come first, then the command.
In other words, to swap your current stack for the stack named _x_, write `x swap`

- `swap` swaps the working stack for the named stack.
- `copy` copies the (single) stack just named onto the end of the working stack (it is not evaluated, so it gives you a chance to see what you did first. To evaluate, just enter an empty line.)
- `name` renames your working stack. If a stack with that name already exists, it will be replaced.
- `del` deletes a stack. This is useful if you have overwritten a stack by mistake, deleting the stack again will only delete the replacement, and the original stack will be accessible again.

There are also two commands that are not technically multi-stack but are only really useful when working with multiple stacks:

- `noeval` adds the content without evaluating it
- `move` takes a number and moves that many elements from the end of the stack to the beginning of the stack. This is useful to include in functions when you want them to have multiple arguments (to swap between them).

With the exception of `move`, no stack commands are allowed inside of stacks.
Allowing this could lead to recursion, loops, etc.
The result would be less like a calculator and more like a programming language; I do not want to add memory management, ending conditions, or the possibility of going into an infinite loop, so this has deliberately not been implemented.

#### Stacks in files

You can write stacks into files to save data and/or functions for repeated use.

The file can have any name.
The structure for the files is:

```
name of first stack
contents of first stack
name of second stack
contents of second stack
.
.
.
```

There is no limit to how many stacks can be contained in a file.
Lines starting with `#` are comments.
Blank lines and lines that begin with `#` are ignored.

To import a file with stacks, simply use the filenames (or paths) as command line arguments.
There is no limit to how many files you can import.

To save all current stacks to a file, use `filename save`.
Be careful because this can overwrite data!

#### Example session multiple stacks

The following session uses 2 stacks, which are named "stack1" and "stack2". It showcases most of the commands (except `del`).

Some operations are performed, and then the main stack is renamed stack1. A new stack is created named stack2, and it is used to write a function which adds 4 and then takes the sine of the result. This function is then applied to the last element in stack1. Further messing around with the stacks is then done just to showcase the functionality further.

```
5 4 8 +
5 12.0
stack1 name
5 12.0
stack2 swap

4 + sin noeval
4 + sin
stack1 swap
5 12.0
stack2 copy
5 12.0 4 + sin

5 -0.2879033
stack2 swap
4 + sin
stack1 copy
4 + sin 5 -0.2879033
stack1 swap
5 -0.2879033
stack2 copy
5 -0.2879033 4 + sin 5 -0.2879033
sum
9.172041
5 3 1
9.172041 5 3 1
2 move
3 1 9.172041 5
```

### Known problems and To-do

#### Problems

##### Won't fix
- The simple version does no error checking and will crash if you input an illegal command.

##### Might fix
- Deleting the last stack crashes the program.

##### Will fix
- There is no error checking when reading files from command line arguments. If a file doesn't exist, the program will crash. If a file has an odd number of lines, the program will simply assume that all but the last line are correctly formed and will import them. 

--------------------------
