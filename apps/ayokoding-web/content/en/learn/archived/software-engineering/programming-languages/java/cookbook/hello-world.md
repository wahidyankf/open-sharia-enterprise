---
title: "Hello World"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/java-cookbook/java-cookbook-primary/src/main/java/com/ayokoding/cookbook/hello_world).

</aside>

## Code

```java
package com.ayokoding.cookbook.hello_world;

public class HelloWorld {
  public static void main(String[] args) {
    System.out.println("Hello World!");
  }
}
```

## Explanation

This simple Java program outputs the text "Hello World!" when it is run. Here's a breakdown of the different parts:

1. `package com.ayokoding.cookbook.hello_world;` - This line defines the package in which this Java class is located. Packages in Java are a way to group related classes and interfaces together. They also help avoid class name conflicts in large projects. In this case, `com.ayokoding.cookbook.hello_world` is the package.
2. `public class HelloWorld { }` - This line declares a public class named `HelloWorld`. Classes are the basic building blocks in object-oriented programming (OOP). The `public` keyword is an access modifier which means that the class is visible to all classes everywhere, whether they are in the same package or have imported the package containing this class.
3. `public static void main(String[] args) { }` - This is the main method, which is the entry point for any Java program. The Java Virtual Machine (JVM) calls this method when the program is executed.
   - `public` is an access modifier meaning that this method is accessible anywhere.
   - `static` means that this method belongs to the `HelloWorld` class rather than an instance of the class.
   - `void` means this method doesn't return any value.
   - `main` is the name of the method.
   - `String[] args` is the parameter to the main method, representing any command-line arguments.
4. `System.out.println("Hello World!");` - This line prints the string "Hello World!" to the standard output (usually the console or terminal). Here, `System` is a predefined class, `out` is an instance of `PrintStream` type, which is a public and static member field of the `System` class, and `println` is a method of `PrintStream` class.

So, when you run this program, it will simply print "Hello World!" to the console.
