---
title: "Methods: Basics"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/java-cookbook/java-cookbook-primary/src/main/java/com/ayokoding/cookbook/methods_basics).

</aside>

## Introduction

One of the core concepts in Java, and in most programming languages, is the method. A method is a named group of statements or a block of code designed to perform a specific task. In a broader sense, methods provide a way to structure your code and make it more readable, reusable, and maintainable. Now, let's delve into the various aspects of methods and related concepts in Java:

- **Java Methods**: Methods in Java are a collection of instructions that perform a specific task. They are essentially blocks of code that only run when called, and they can be used to perform computations, print information, modify class variables, and more. Methods improve code reusability and modularity, making programs easier to understand and manage.
- **Method Parameters and Return Types**: Java methods can receive input through parameters and return a result using a return type. Parameters are specified in the method declaration and act as placeholders for the values passed when the method is called. The method's return type indicates the type of value the method sends back to the caller.
- **Method Overloading**: Method overloading in Java is a feature that allows a class to have multiple methods with the same name but different parameters. It increases the program's readability and provides multiple ways to perform a single task with different inputs.
- **Access Modifiers**: Access modifiers in Java define the accessibility (scope) of classes, methods, and variables. There are three access modifiers: public, private, and protected. Public methods are accessible from any class, private methods can only be accessed within the class defined, and protected methods are accessible within the same class and any subclasses.
- **Recursion**: Recursion is a programming concept where a method calls itself to solve a problem. A method that uses this programming technique is called a recursive method. It's a powerful tool, but it must be used carefully, as it can lead to infinite loops or stack overflow errors if mishandled.
- **Static vs. Instance Methods**: In Java, methods can be instance or static. Instance methods belong to instances of a class, and you need to create an instance or object of the class to call them. On the other hand, static methods belong to the class itself and can be called without creating an instance of the class.
- **Constructors**: In Java, a constructor is a special method to initialize objects. The constructor is called when an object of a class is created. It can take parameters to initialize the object with specific values, and it's consistently named the same as the class.
- **Functional Interfaces and Lambda Expressions**: Functional interfaces are Java interfaces with only one abstract method. They are often used as assignment targets for lambda expressions and method references. Lambda expressions provide a concise way to represent a method interface without creating an anonymous class.
- **Exception Handling**: Exception handling is a mechanism that handles runtime errors, allowing the normal flow of the program to continue. Java's exception-handling mechanism revolves around the try, catch, and finally, keywords. This makes Java code robust, as it can handle unexpected exceptions during runtime.
- **Javadoc Comments**: Javadoc is a tool that generates API documentation in HTML format using comments written in the Java source code. Javadoc comments are multi-line comments `[/** ... */]` that are placed before class, method, or field definitions. They can contain information about what a method does, its parameters, return values, and any exceptions it may throw.

## Codes

### Animal.java: Code

```java
// File: Animal.java

package com.ayokoding.cookbook.java_methods;

public class Animal {
  private String species;
  private String name;

  public Animal(String species, String name) {
    this.species = species;
    this.name = name;
  }

  public void makeSound() {
    System.out.println(species + " makes a sound");
  }

  public String getName() {
    return name;
  }
}
```

### Animal.java: Explanations

1. This Java code represents a simple class named `Animal`. Here are the key points about this code:
2. **Package**: The class belongs to the package `com.ayokoding.cookbook.java_methods`. The package acts as a namespace for the class and is generally used to organize related classes.
3. **Class Declaration**: The `public class Animal` line is the declaration of the `Animal` class. The `public` keyword indicates that any other class can access this class.
4. **Instance Variables**: The class has two private instance variables: `species` and `name`. The `private` keyword means these variables can only be accessed within this class.
   - `species` is a String that represents the species of the animal.
   - `name` is a String that represents the name of the animal.
5. **Constructor**: The `Animal(String species, String name)` is a constructor for the `Animal` class. The constructor is called when a new object of the class is created. It initializes the `species` and `name` fields of the `Animal` object with the values passed in as parameters. The `this` keyword is used to refer to the current object.
6. **Methods**:
   - The `makeSound()` method is a public method that, when called, prints to the console a string containing the species of the animal and the text " makes a sound".
   - The `getName()` method is a public method that returns the `name` of the `Animal` object.
7. **Encapsulation**: This class is an example of encapsulation where the class's data (in this case, `species` and `name`) is hidden from outside classes and can be accessed only through the methods of their current class (`getName()` and `makeSound()`). Encapsulation in Java is a mechanism of wrapping code and data together into a single unit.

### AnimalBuilder.java: Code

```java
// File: AnimalBuilder.java

package com.ayokoding.cookbook.java_methods;

public class AnimalBuilder {
  private String species;
  private String name;

  public AnimalBuilder setSpecies(String species) {
    this.species = species;
    return this;
  }

  public AnimalBuilder setName(String name) {
    this.name = name;
    return this;
  }

  public Animal build() {
    return new Animal(species, name);
  }
}
```

### AnimalBuilder.java: Explanations

This Java code represents a class named `AnimalBuilder`. This class implements the Builder Design Pattern, used for creating `Animal` objects. Here are the main points about this code:

1. **Package**: Similar to the previous example, this class belongs to the package `com.ayokoding.cookbook.java_methods`.
2. **Class Declaration**: The `public class AnimalBuilder` is the declaration of the `AnimalBuilder` class. It's `public` so any other class can access it.
3. **Instance Variables**: The class has two private instance variables, `species` and `name`, like in the `Animal` class. They are `private` and can only be accessed within this class.
4. **Methods**:
   - `setSpecies(String species)` and `setName(String name)` are methods used to set the values of the `species` and `name` instance variables, respectively. Each takes one parameter and returns the `AnimalBuilder` instance, allowing for method chaining.
   - The `build()` method is used to create a new `Animal` object using the `species` and `name` instance variables of the `AnimalBuilder`. This method is generally the final method called in the builder pattern to return the constructed object.
5. **Builder Design Pattern**: This class implements the Builder design pattern. The Builder pattern is a design pattern that provides a build object used to construct a complex object step by step. It separates the construction of a complex object from its representation so that the same construction process can create different representations. This pattern is used when a class has many constructor arguments or when you want to make an object immutable.

### JavaMethods.java: Code

```java
// File: JavaMethods.java

package com.ayokoding.cookbook.java_methods;

public class JavaMethods {
  // 1. Method declaration and syntax
  public void makeSound() {
    System.out.println("Animal makes a sound");
  }

  // 2. Method parameters and arguments
  public void printName(String name) {
    System.out.println("Animal name: " + name);
  }

  // 3. Return types and the "void" keyword
  public String getSpecies() {
    return "Unknown";
  }

  // 4. Method overloading
  public void makeSound(String sound) {
    System.out.println("Animal makes sound: " + sound);
  }

  // 5. Access modifiers (public, private, protected) for methods
  public void publicMethod() {
    System.out.println("This is a public method");
  }

  private void privateMethod() {
    // Accessible only within the same class
    System.out.println("This is a private method");
  }

  protected void protectedMethod() {
    // Accessible within the same class and subclasses
  }

  // 6. Method scope and local variables
  public void printMessage() {
    String message = "Hello";
    System.out.println(message);
  }

  // 7. Recursion and recursive methods
  public int factorial(int n) {
    if (n == 0) {
      return 1;
    } else {
      return n * factorial(n - 1);
    }
  }

  // 8. Static methods and instance methods
  public static void staticMethod() {
    // Accessible without creating an instance
    System.out.println("This is a static method");
  }

  public void instanceMethod() {
    // Accessible on instances of the class
    System.out.println("This is an instance method");
  }

  // 9. Constructors and their role in method invocation
  public class Dog extends Animal {
    public Dog(String name) {
      super("Dog", name);
    }
  }

  // 10. Method references and functional interfaces
  public interface AnimalSound {
    void makeSound();
  }

  // 11. Lambda expressions and functional programming with methods
  AnimalSound animalSound = () -> System.out.println("Animal makes a sound");

  // 12. Exception handling within methods
  public void divide(int a, int b) {
    try {
      int result = a / b;
      System.out.println("Result: " + result);
    } catch (ArithmeticException e) {
      System.out.println("Error: " + e.getMessage());
    }
  }

  // 13. Method documentation and Javadoc comments
  /**
   * Adds two numbers and returns the result.
   *
   * @param a the first number
   * @param b the second number
   * @return the sum of the two numbers
   */
  public int addNumbers(int a, int b) {
    return a + b;
  }

  public static void main(String[] args) {
    JavaMethods examples = new JavaMethods();

    // Example usage of methods
    examples.makeSound(); // Output: Animal makes a sound
    examples.printName("Lion"); // Output: Animal name: Lion

    String species = examples.getSpecies();
    System.out.println("Species: " + species); // Output: Species: Unknown

    examples.makeSound("Meow"); // Output: Animal makes sound: Meow

    examples.publicMethod(); // Output: This is a public method
    examples.printMessage(); // Output: Hello

    int factorialResult = examples.factorial(5);
    System.out.println("Factorial: " + factorialResult); // Output: Factorial: 120

    JavaMethods.staticMethod(); // Output: This is a static method

    examples.instanceMethod(); // Output: This is an instance method
    examples.privateMethod(); // Output: This is a private method

    AnimalBuilder builder = new AnimalBuilder();
    Animal animal = builder.setSpecies("Cat").setName("Tom").build();
    System.out.println("Animal name: " + animal.getName()); // Output: Animal name: Tom

    Animal dog = examples.new Dog("Buddy");
    dog.makeSound(); // Output: Dog makes a sound

    examples.divide(10, 2); // Output: Result: 5
    examples.divide(10, 0); // Output: Error: / by zero

    int sumResult = examples.addNumbers(2, 3); // You can see the documentation by hovering over the method in your IDE
    System.out.println("Sum: " + sumResult); // Output: Sum: 5
  }
}
```

### JavaMethods.java: Explanations

Here are explanations for each section of the code:

1. **Method Declaration**: `makeSound()` is a public void method. "public" means it is accessible from anywhere, and "void" means it does not return any value. When this method is called, it will print "Animal makes a sound" to the console.
2. **Method Parameters and Arguments**: `printName(String name)` is a method that takes a single String parameter called `name`. When called, it prints "Animal name: " followed by the value of the parameter `name`.
3. **Return Types and the "void" Keyword**: `getSpecies()` is a method that returns a String. The keyword "void" in the previous methods indicated they returned nothing. Here, instead of "void", we have "String, " meaning the method will return a String. In this case, it always returns the string "Unknown".
4. **Method Overloading**: The second `makeSound(String sound)` method demonstrates method overloading, which allows multiple methods with the same name but different parameters. This `makeSound` method takes a String parameter `sound` and prints "Animal makes sound: " followed by the sound.
5. **Access Modifiers**: Three methods - `publicMethod()`, `privateMethod()`, and `protectedMethod()` - demonstrate access modifiers for methods.
   - `publicMethod()` is a `public` method, meaning it can be accessed from any other class in the application.
   - `privateMethod()` is a `private` method, which means it can only be accessed within the `JavaMethods` class.
   - `protectedMethod()` is a `protected` method, which means it can only be accessed within the `JavaMethods` class and any subclasses.
6. **Method Scope and Local Variables**: `printMessage()` demonstrates a local variable. `message` is a String variable that only exists within the scope of `printMessage()`. If you try to use `message` in another method, it will not be recognized.
7. **Recursion and Recursive Methods**: `factorial(int n)` is a method that calculates the factorial of a number using recursion. A recursive method is a method that calls itself in its execution. Here, `factorial(n)` calls `factorial(n - 1)`.
8. **Static Methods and Instance Methods**: `staticMethod()` and `instanceMethod()` show the difference between static and instance methods. `staticMethod()` is a `static` method, meaning it belongs to the class itself, not any class instance. `instanceMethod()` is a non-static method belonging to class instances.
9. **Constructors and Their Role in Method Invocation**: The `Dog` class inside `JavaMethods` extends `Animal` and has a constructor that takes a `name` and passes it along with the string "Dog" to the `Animal` constructor using the `super` keyword. Constructors are special methods used to initialize new objects.
10. **Method References and Functional Interfaces**: `AnimalSound` is a functional interface with only one abstract method. It's used as a type for the `animalSound` field and can be implemented using a lambda expression or method reference.
11. **Lambda Expressions and Functional Programming with Methods**: `animalSound` is a lambda expression that implements the `AnimalSound` interface. Lambda expressions are a feature from Java 8 that provide a concise way to write functional programming code.
12. **Exception Handling within Methods**: `divide(int a, int b)` shows how to handle exceptions within methods. It tries to divide `a` by `b` and if an `ArithmeticException` is thrown (which occurs when `b` is zero), it catches the exception and prints an error message.
13. **Method Documentation and Javadoc Comments**: `addNumbers(int a, int b)` is a method with Javadoc comments. Javadoc comments (`/** ... */`) are used to generate the API documentation for the code. This method takes two integers, adds them together, and returns the result.
14. **Main Method**: The `main` method is the entry point of any Java application. It first creates an instance of the `JavaMethods` class, then calls several methods on this instance, demonstrating their use. It also calls the static `staticMethod` directly on the `JavaMethods` class, creates an `Animal` instance using the `AnimalBuilder`, and demonstrates exception handling with the `divide` method.
