---
title: "Control Structure"
date: 2025-03-16T07:20:00+07:00
draft: false
---

In Java, control structures are fundamental building blocks that govern the flow of execution within a program. They provide the mechanisms that allow you to control how and when different parts of your program are executed based on specified conditions. Control structures are pivotal in developing simple to complex applications, enabling decision-making, looping over code blocks, and the sequential execution of statements.

Java control structures are classified into three categories: sequential, selection, and looping. Sequential control is the most straightforward, where code is executed line by line in the order it appears in the program. Selection control structures like `if`, `if-else`, `else-if`, and `switch` allows decision-making based on conditions. Looping control structures such as `for`, `while`, `do-while`, and `for-each` can repeatedly execute a code block. These control structures' careful and clever use is the key to creating efficient and effective Java programs.

## Example

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/java-cookbook/java-cookbook-primary/src/main/java/com/ayokoding/cookbook/control_structure).

</aside>

```java

public class ControlStructures {
  public static void main(String[] args) {

    // Sequence Structure
    int x = 1;
    int y = x + 2;
    System.out.println("x is " + x + ", y is " + y); // Output: x is 1, y is 3

    // Selection Structures

    // If-then
    if (x > y) {
      System.out.println("x is greater than y"); // This line will not be executed
    }

    // If-then-else
    if (x > y) {
      System.out.println("x is greater than y"); // This line will not be executed
    } else {
      System.out.println("x is not greater than y"); // Output: x is not greater than y
    }

    // If-then-else-if
    if (x > y) {
      System.out.println("x is greater than y"); // This line will not be executed
    } else if (x == y) {
      System.out.println("x is equal to y"); // This line will not be executed
    } else {
      System.out.println("x is less than y"); // Output: x is less than y
    }

    // Switch
    switch (x) {
      case 1:
        System.out.println("x is 1"); // Output: x is 1
        break;
      case 2:
        System.out.println("x is 2"); // This line will not be executed
        break;
      default:
        System.out.println("x is neither 1 nor 2"); // This line will not be executed
        break;
    }

    // Loop Structures

    // While loop
    while (x < 5) {
      System.out.print(x + " - "); // Output: 1 - 2 - 3 - 4 -
      x++;
    }
    System.out.println(x); // Output: 5

    // Do-while loop
    x = 0;
    do {
      System.out.print(x + " - "); // Output: 0 - 1 - 2 - 3 - 4 -
      x++;
    } while (x < 5);
    System.out.println(x); // Output: 5

    // For loop
    for (int i = 0; i < 5; i++) {
      System.out.print(i + " "); // Output: 0 1 2 3 4
    }
    System.out.println();

    // For-each loop
    int[] arr = { 1, 2, 3, 4, 5 };
    for (int num : arr) {
      System.out.print(num + " "); // Output: 1 2 3 4 5
    }
    System.out.println();

    String str = "HelloWorld!";
    for (String ch : str.split("")) {
      System.out.print(ch + " "); // Output: H e l l o W o r l d !
    }
    System.out.println();
  }
}
```

Here is a list of explanations for each section of the code:

1. `public class ControlStructures`: This line declares a public class named `ControlStructures`. A class in Java is a blueprint for creating objects (a particular data structure).
2. `public static void main(String[] args)`: The main method serves as the entry point for the program. The Java Virtual Machine (JVM) calls the main method when the program starts.
3. `// Sequence Structure`: This comment indicates that the code following it will demonstrate a sequence control structure.
4. `int x = 1; int y = x + 2;`: These lines initialize an integer variable `x` with a value of `1`, and an integer variable `y` with a value of `3`.
5. `System.out.println("x is " + x + ", y is " + y);`: This line prints the string "x is 1, y is 3" to the console.
6. `// Selection Structures`: This comment indicates that the code following it will demonstrate selection control structures.
7. `if (x > y) {...}`: This is an `if` statement. It checks whether `x` is greater than `y`. If `x` is greater than `y`, the code inside the braces `{}` is executed. However, in this case, `x` is not greater than `y`, so the code inside the braces will not be executed.
8. `if (x > y) {...} else {...}`: This is an `if-else` statement. It checks whether `x` is greater than `y`. If `x` is greater than `y`, it executes the code in the first braces `{}`. If not, it executes the code in the `else` braces. In this case, `x` is not greater than `y`, so it prints "x is not greater than y".
9. `if (x > y) {...} else if (x == y) {...} else {...}`: This is an `if-else-if-else` statement. It checks whether `x` is greater than `y`, and if `x` is equal to `y`. If neither of these conditions is true, it executes the code in the `else` braces. In this case, `x` is less than `y`, so it prints "x is less than y".
10. `switch (x) {...}`: This is a `switch` statement. It checks the value of `x` and executes the code corresponding to the matching `case`. Here, since `x` is `1`, it prints "x is 1".
11. `// Loop Structures`: This comment indicates that the code following it will demonstrate loop control structures.
12. `while (x < 5) {...}`: This is a `while` loop. It repeats the block of code within the braces as long as `x` is less than `5`. Here, it prints `x` and increments `x` on each iteration.
13. `do {...} while (x < 5);`: This is a `do-while` loop. Unlike the `while` loop, this loop checks its condition at the end of the loop, ensuring that the loop is executed at least once. In this case, it resets `x` to `0` and then prints `x`, incrementing `x` until `x` becomes `5`.
14. `for (int i = 0; i < 5; i++) {...}`: This is a `for` loop. It initializes an integer `i` to `0` and repeats the loop until `i` is less than `5`. The loop increments `i` by `1` at the end of each iteration. It prints `i` on each iteration.
15. `for (int num : arr) {...}`: This is a `for-each` loop, used to iterate over an array or a collection. It prints each number in the array `arr`.
16. `for (String ch : str.split("")) {...}`: This is another `for-each` loop. It splits the string `str` into an array of substrings where each substring contains a single character, then iterates over this array, printing each character.
