---
title: "Class: Basics"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

## Concepts

In Java, a class is a fundamental building block of object-oriented programming. It serves as a blueprint for creating objects and encapsulating data and behavior. Here are the core characteristics of a Java class:

### Encapsulation

Encapsulation hides internal details and provides a public interface to interact with the class. In Java, this is achieved by using access modifiers (`public`, `private`, `protected`) to control the visibility of class members (variables and methods). Encapsulation helps achieve data abstraction and protects the class's internal state from being accessed directly.

### Inheritance

Inheritance is a mechanism in Java that allows a class to inherit properties and behaviors from another class. It promotes code reuse and establishes a hierarchical relationship between classes. The `extends` keyword is used to create a subclass (child class) inherited from a superclass (parent class). The subclass can access the public and protected members of the superclass.

### Polymorphism

Polymorphism allows objects of different classes to be treated as objects of a common superclass. It enables a single interface to represent different types of objects. Polymorphism is achieved through method overriding and method overloading. Method overriding allows a subclass to provide its own implementation of a method defined in the superclass. In contrast, method overloading allows multiple methods with the same name but different parameters to coexist in a class.

### Abstraction

Abstraction is the process of simplifying complex systems by breaking them down into smaller, more manageable units. In Java, abstraction is achieved through abstract classes and interfaces. An abstract class cannot be instantiated and serves as a blueprint for subclasses. It can contain both abstract and non-abstract methods. Interfaces, on the other hand, define a contract that classes must adhere to. They provide a way to achieve multiple inheritances and define a set of methods that implementing classes must implement.

### Class Members

A Java class consists of various members, including variables, methods, constructors, and nested classes. Variables represent the state of an object, methods define its behavior, constructors are used to create objects, and nested classes provide a way to define classes within classes. Class members can have different access modifiers to control their visibility and accessibility.

## Mechanics

### Java Class Creation

A class is a fundamental concept in object-oriented programming (OOP). It serves as a blueprint or template for creating objects. In Java, you define a class using the `class` keyword followed by the class name. A class can have fields (variables), methods (functions), constructors, and other members.

To create an object of a class, you use the `new` keyword followed by the class name and parentheses. This invokes the constructor of the class and allocates memory for the object. For example:

```java
public class Car {
    // Class variables, instance variables, methods, etc.
}

Car myCar = new Car();
```

### Fields (Class Variables)

Fields, also known as class variables or instance variables, are variables defined within a class. They represent the state or data of an object. Fields can have different data types such as `String`, `int`, `boolean`, etc. They can also have different access modifiers like `public`, `private`, or `protected`.

Fields are declared within a class but outside of any method. They can be accessed and modified by the methods of the class. For example:

```java
public class Car {
    String color; // Field declaration
    int year;
}
```

### Methods

Methods are functions defined within a class that describe the behaviors of an object. They can perform actions, manipulate data, and return values. Methods are declared using the method signature, which includes the return type, method name, and parameters (if any).

Methods can be called on objects of the class to perform specific operations. They can access and modify the fields of the class. For example:

```java
public class Car {
    void startEngine() { // Method declaration
        System.out.println("Engine started");
    }
}
```

### 'this' Keyword

The `this` keyword refers to the current object within a method or constructor. It differentiates between instance variables and parameters or local variables with the same name. Using `this`, you can access the instance variables of the current object.

The `this` keyword is particularly useful when there is a naming conflict between instance variables and parameters. It helps to clarify which variable you are referring to. For example:

```java
public class Car {
    String color;

    void setColor(String color) {
        this.color = color; // Assign the parameter value to the instance variable
    }
}
```

### Access Modifiers

Access modifiers define the visibility and accessibility of a class, constructor, variable, or method. Java has four access modifiers: `public`, `private`, `protected`, and default (no modifier).

- `public`: The public access modifier allows the class, constructor, variable, or method to be accessed from anywhere.
- `private`: The private access modifier restricts the access to within the same class. It cannot be accessed from outside the class.
- `protected`: The protected access modifier allows access within the same class, subclasses, and classes in the same package.
- Default (no modifier): If no access modifier is specified, it is considered as default. It allows access within the same package only.

Access modifiers control the visibility and encapsulation of the members of a class. For example:

```java
public class Car {
    public String color; // Public access modifier
    private int year; // Private access modifier
}
```

### Constructor

A constructor is a special type of method used for initializing an object. It is called when an object of a class is created using the `new` keyword. Constructors have the same name as the class and do not have a return type.

Constructors are used to set initial values to an object's instance variables. They can take parameters to initialize the object with specific values. Java provides a default constructor with no parameters if no constructor is defined in a class. For example:

```java
public class Car {
    String color;

    public Car(String color) { // Constructor declaration
        this.color = color;
    }
}
```

### Static Keyword

The `static` keyword is used to declare members (variables or methods) that belong to the class itself rather than any specific class instance. When a member is declared static, it can be accessed before any objects of its class are created without reference to any object.

Static members are shared among all instances of a class. They can be accessed using the class name followed by the member name. Static methods can only access other static members of the class. For example:

```java
public class Car {
    static int numberOfCars; // Static variable

    static void increaseCount() { // Static method
        numberOfCars++;
    }
}
```

### Method Overloading

Method overloading occurs when a class has multiple methods with the same name but different parameters. The methods can have different numbers of parameters, different types of parameters, or both. Java determines which method to call based on the arguments passed during the method invocation.

Method overloading allows you to provide different ways of calling a method with different parameter combinations. It improves code readability and flexibility. For example:

```java
public class Car {
    void accelerate() {
        // Code to accelerate the car
    }

    void accelerate(int speed) {
        // Code to accelerate the car to a specific speed
    }
}
```

## Full Example

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/java-cookbook/java-cookbook-primary/src/main/java/com/ayokoding/cookbook/class_basics).

</aside>

### Code

```java
package com.ayokoding.cookbook.java_class.basics;

public class Car {
  // Fields (Class Variables)
  private String color;
  private int year;

  // Constructor
  public Car(String color, int year) {
    this.color = color;
    this.year = year;
  }

  // Methods
  public void startEngine() {
    System.out.println("Engine started");
  }

  public void setColor(String color) {
    this.color = color;
  }

  public String getColor() {
    return color;
  }

  public void printCarInfo() {
    System.out.println("Car color: " + color + "; " + "Car year: " + year);
  }

  // Static Variable
  private static int numberOfCars;

  // Static Method
  public static void increaseCount() {
    numberOfCars++;
  }

  // Method Overloading
  public void accelerate() {
    System.out.println("Car is accelerating");
  }

  public void accelerate(int speed) {
    System.out.println("Car is accelerating at " + speed + " mph");
  }

  public static void main(String[] args) {
    // Creating objects of the Car class
    Car myCar = new Car("Red", 2021);
    Car anotherCar = new Car("Blue", 2022);

    // Accessing fields and calling methods
    myCar.startEngine(); // Print: Engine started
    myCar.setColor("Green");
    System.out.println("My car color: " + myCar.getColor()); // Print: My car color: Green
    myCar.printCarInfo(); // Print: Car color: Green; Car year: 2021

    anotherCar.startEngine(); // Print: Engine started
    anotherCar.accelerate(); // Print: Car is accelerating
    anotherCar.accelerate(60); // Print: Car is accelerating at 60 mph

    // Accessing static variable and method
    Car.increaseCount();
    Car.increaseCount();
    System.out.println("Number of cars: " + Car.numberOfCars); // Print: Number of cars: 2
  }
}
```

### Explanation

Here's an explanation of the Java code you've provided as a list of items:

1. **Package Declaration**: `package com.ayokoding.cookbook.java_class.basics;` is declaring the package name for the Java class. It is used for organizing related classes and interfaces into a package, making them easier to manage.
2. **Class Declaration**: `public class Car { ... }` is declaring a class named `Car`. This class will represent a template or blueprint from which individual car objects can be created.
3. **Fields or Class Variables**: `private String color;` and `private int year;` are the class variables or properties that every `Car` object will have. These variables are declared as private for encapsulation, which means they can only be accessed or modified within the `Car` class.
4. **Constructor**: `public Car(String color, int year) {...}` is a constructor, a special method in the class that gets called when a new object is created. It initializes the `color` and `year` fields of the car object.
5. **Methods**: The `startEngine`, `setColor`, `getColor`, and `printCarInfo` methods perform actions related to a `Car` object. `startEngine` prints a message, `setColor` changes the car's color, `getColor` returns the car's current color, and `printCarInfo` displays the car's information.
6. **Static Variable**: `private static int numberOfCars;` is a static variable shared by all class instances. This variable keeps track of the total number of `Car` objects created.
7. **Static Method**: `public static void increaseCount() {...}` is a static method that increases the count of the `numberOfCars` variable. This method is called whenever a new car object is created.
8. **Method Overloading**: The `accelerate` method is overloaded. This means there are two versions of the `accelerate` method, one that takes no parameters and another that takes an `int` parameter for the speed. One of these methods will be used depending on how it's called.
9. **Main Method**: The `main` method is the entry point for the program. This is where objects of the `Car` class are created and the methods and variables are used to perform actions on those objects.
10. **Object Creation**: In the main method, `Car myCar = new Car("Red", 2021);` and `Car anotherCar = new Car("Blue", 2022);` create two instances of the `Car` class with different colors and years.
11. **Method Calls**: In the main method, the `startEngine`, `setColor`, `getColor`, `printCarInfo`, and `accelerate` methods are called on the `myCar` and `anotherCar` objects to perform various actions.
12. **Accessing Static Variable and Method**: In the main method, `Car.increaseCount();` is called twice, increasing the count of `Car` objects and `System.out.println("Number of cars: " + Car.numberOfCars);` is printing the total number of `Car` objects “created”.

## Further Readings

- [Java Class Object Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Object.html)
