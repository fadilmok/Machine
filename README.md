#Machine

A machine is composed of:
*	an array of 32 positive integer entries (known as the *memory*).
*	a source program of a sequence of instructions (the *program*)
*	a single positive integer indicating the index of the next instruction to be run (the *program counter*), indexed from 0

In this guide we use the notation cell(n) to refer to the entry at index n in memory

Instructions can take the following form:
*	`Zn`      - zeroes the memory at cell(n) , where n is an index from 0 to 31 inclusive. E.g. `Z3` sets cell(3) to be equal to 0.
*	`In`      - increments the memory at cell(n), where n is an index from 0 to 31 inclusive. E.g. `I4` sets cell(4) to be equal to one greater than the previous value of cell(4).
*	`Jn,m,t`  - if the values of cell(n) and cell(m) are **NOT** equal (where n,m are both indices from 0 to 31 inclusive), then set the program counter to be equal to t. E.g. `J2,3,0` will jump to instruction 0 if cell(2) != cell(3)

After executing any instruction, the program counter is incremented by 1, *unless* a J instruction succesfully branched. If the J instruction does *not* branch, the program counter is still incremented.

When running a program, the program counter begins at 0. At each step, the instruction indexed by the program counter is run, and the program counter updated appropriately. Execution ends when the program counter is increased past the end of the source program. The value in cell(0) is then returned.

**1) Write an implementation (*in the language of your choice*) of this machine.**

Your program should take a list of filenames of one or more source programs to be run as command line arguments, e.g.

```
*./machine <sourcefile1> [sourcefile2 sourcefile3 ...]*
```

Your program should run each program in turn, *without zeroing memory between executions*, and should print the result of the final program to stdout. This allows one program to initialize memory, and the second program to then perform operations on it.

Source programs can be assumed to be ASCII text files with each instruction on a single line. White space should be ignored.

E.g. The below program sets cell(1) to equal 3, then copies that value into cell(0), increments that value, and then returns that value: 4.

```
Z1
I1
I1
I1
Z0
I0
J1,0,5
I0
```

**2) Write a program that subtracts the value of cell(2) from cell(1) and stores it in cell(0). You may assume that cell(1) is strictly greater than cell(2).**

**3) Write a program that calculates the absolute difference between cell(2) and cell(1), regardless of which is bigger, and stores it in celll(0).**
