---
title: "Hello World"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/c-cookbook/c-cookbook-primary/hello-world).

</aside>

```c
#include <stdio.h>

int main(void)
{
  printf("Hello, world!\n"); // Output: Hello, world!
  return 0;
}
```

Let's break this down:

1. `#include <stdio.h>`: This preprocessor directive tells the compiler to include the program's standard input/output library. The library contains functions the program will use for input/output operations - in this case, `printf`.
2. `int main(void)`: This is the program's main function. Every C program must have a main function as this is where program execution begins. The `int` before `main` signifies that the function will return an integer. The `(void)` is an optional part that signifies that the function takes no arguments.
3. `{...}`: The pair of curly braces denotes a block of code associated with a function. In this case, it contains the body of the `main` function. All the code that the `main` function executes is placed within these braces.
4. `printf("Hello, world!\\n");`: This is a call to the `printf` function, defined in `stdio.h`. `printf` is used to output text to the console. The argument provided, `"Hello, world!\\n"`, is a string to be printed. The `\\n` at the end is an escape sequence representing a newline character, causing the cursor to go to the next line after printing the text.
5. `// Output: Hello, world!`: This is a single-line comment. In C, any text that comes after `//` on the same line is considered a comment and is ignored by the compiler. Comments are used to provide information about the code to human readers. In this case, it explains what the output of the `printf` statement will be.
6. `return 0;`: This line signifies the end of the `main` function. The `return` keyword is followed by an integer value that signifies the program's exit status. A return value of 0 typically signifies that the program has been executed successfully. Different non-zero return values can indicate different types of errors, which can be helpful for debugging.
7. `}`: This is the closing brace for the `main` function. It signifies the end of the function's code block.

This program is a simple "Hello, World!" program and is often used as an introductory example for people learning the C language. When executed, it simply prints "Hello, World!" and a newline to the console and then exits with a status of 0.
