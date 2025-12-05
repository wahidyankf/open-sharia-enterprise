---
title: "Class: Intermediate"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/java-cookbook/java-cookbook-primary/src/main/java/com/ayokoding/cookbook/class_intermediate).

</aside>

## Inheritance

Inheritance is a mechanism in Java that allows one class to inherit the properties and methods of another class. The class that inherits is called the subclass or derived class, and the class from which it inherits is called the superclass or base class. Inheritance promotes code reusability and allows for the creation of a hierarchical structure of classes.

### **Example**

```java
class Animal {
    protected String name;

    public Animal(String name) {
        this.name = name;
    }

    public void eat() {
        System.out.println(name + " is eating.");
    }
}

class Dog extends Animal {
    public Dog(String name) {
        super(name);
    }

    public void bark() {
        System.out.println(name + " is barking.");
    }
}

public class Main {
    public static void main(String[] args) {
        Dog dog = new Dog("Buddy");
        dog.eat(); // Output: Buddy is eating.
        dog.bark(); // Output: Buddy is barking.
    }
}
```

Here’s the breakdown of the code:

1. The program has a class named `Animal`. This class is likely a superclass for different types of animals.
   1. The `Animal` class has a single protected instance variable `name` of type `String`. The keyword `protected` means the variable is accessible within its own class, to subclasses, and also to classes in the same package.
   2. The `Animal` class has a constructor which takes a `String` argument. This constructor is used to initialize the `name` attribute of an `Animal` object.
   3. The `Animal` class has a method called `eat()`. This method, when called, will print to the console the `name` of the `Animal` object and a message stating that it is eating.
2. The program has another class named `Dog`. This class is a subclass of `Animal`, inheriting all of the `Animal` class's properties and methods.
   1. The `Dog` class has a constructor which also takes a `String` argument. This constructor calls the superclass constructor (`super(name)`) to initialize the `name` of the `Dog` object.
   2. The `Dog` class has a method called `bark()`. This method, when called, will print to the console the `name` of the `Dog` object and a message stating that it is barking.
3. There is a `Main` class that contains the `main` method, the entry point of the program.
   1. Inside the `main` method, a `Dog` object is created named "Buddy".
   2. The `eat()` method is called on the `Dog` object, which prints "Buddy is eating." to the console.
   3. The `bark()` method is then called on the `Dog` object, which prints "Buddy is barking." to the console.

## Polymorphism and Method Overloading

Polymorphism is the ability of an object to take on many forms. In Java, polymorphism allows a single method or class to have multiple implementations. There are two types of polymorphism in Java: compile-time polymorphism (method overloading) and runtime polymorphism (method overriding).

### **Example**

```java
class Animal {
    public void makeSound() {
        System.out.println("Animal is making a sound.");
    }
}

class Dog extends Animal {
    @Override
    public void makeSound() {
        System.out.println("Dog is barking.");
    }
}

public class Main {
    public static void main(String[] args) {
        Animal animal = new Animal();
        Animal dog = new Dog();

        animal.makeSound(); // Output: Animal is making a sound.
        dog.makeSound(); // Output: Dog is barking.
    }
}
```

Here's a breakdown of the code:

1. **Animal class definition**: The code starts with a definition of a class named "Animal". This class has a public method called `makeSound()`, which when called, prints "Animal is making a sound." to the console.
2. **Dog class definition**: Then we have a class named "Dog" which is a subclass of the "Animal" class. This is signified by the `extends Animal` in the class definition. The Dog class overrides the `makeSound()` method from the Animal class. This means that, when a Dog object calls the `makeSound()` method, it will print "Dog is barking." instead of "Animal is making a sound.".
3. **Main class and main method**: The Main class contains the `main` method, which is the entry point to the program.
4. **Creating Animal instance**: Inside the `main` method, an object named `animal` of class Animal is instantiated.
5. **Creating Dog instance**: After that, an object named `dog` of class Dog is instantiated, but it's interesting to note that it is declared as an Animal type. This is known as "upcasting" - an instance of a subclass is treated as an instance of the superclass. This is possible because every Dog is an Animal (due to inheritance).
6. **Calling the makeSound() method**: Finally, the `makeSound()` method is called on both `animal` and `dog` instances. For `animal`, it calls the `makeSound()` method defined in the Animal class, while for `dog`, it calls the `makeSound()` method defined in the Dog class. This is an example of method overriding and polymorphism in Java. Even though the `dog` object is declared as an Animal type, it still uses the Dog's version of `makeSound()` because the actual object is a Dog.

The output of the code is as follows:

- "Animal is making a sound." is printed when the `makeSound()` method is called on the `animal` object.
- "Dog is barking." is printed when the `makeSound()` method is called on the `dog` object.

## Instance Initializers (Non-static initialization blocks)

Instance initializers, also known as non-static initialization blocks, are used to initialize the instance data members of a class. They are executed each time an object of the class is created before the constructor is called. Instance initializers are helpful when performing some initialization logic familiar to all class constructors.

### **Example**

```java
package com.ayokoding.cookbook.java_class.intermediate.instance_initializers;

class Animal {
  private String species;

  {
    // Instance initializer block
    species = "Unknown";
    System.out.println("Instance initializer block executed.");
  }

  public Animal() {
    System.out.println("Constructor executed. Species: " + species);
  }

  public String getSpecies() {
    return species;
  }
}

public class Main {
  public static void main(String[] args) {
    Animal animal = new Animal();
    // Output:
    // Instance initializer block executed.
    // Constructor executed. Species: Unknown

    System.out.println(animal.getSpecies()); // Output: Unknown
  }
}
```

Here's the explanation of this Java code:

1. **Class Declaration**: The `Animal` class is declared. This class represents a certain type of object in Java.
2. **Private Variable**: Inside the `Animal` class, there's a private variable `species` of type String. This variable is meant to hold the species of the animal object. Being private, it is only accessible within the `Animal` class.
3. **Instance Initializer Block**: This is a block of code that's executed every time an instance of the `Animal` class is created. It is defined by the braces `{}` without any keyword. In this case, it sets the `species` variable to "Unknown" and then prints a message indicating it has been executed.
4. **Constructor**: This is the `Animal()` constructor, called when a new `Animal` object is created. It prints a message showing that it has been executed and shows the species of the animal, which at this point would be "Unknown" as set by the instance initializer block.
5. **Getter Method**: The `getSpecies()` method is a getter method for the `species` variable. It returns the current value of `species`.
6. **Main Class and Method**: The `Main` class is defined with a `main` method. The `main` method is the entry point for the program.
7. **Animal Object Creation**: Inside the `main` method, a new `Animal` object is created. This triggers the instance initializer block and the constructor, resulting in the output "Instance initializer block executed" followed by "Constructor executed. Species: Unknown".
8. **Getter Method Call**: Finally, `animal.getSpecies()` is called to get the species of the animal, which is then printed out. At this point, the species is still "Unknown", so "Unknown" is printed.

The specific order of events in this program is important to understand. When a new `Animal` object is created, the instance initializer block is executed first, setting the species to "Unknown". After that, the constructor is called, which prints the species "Unknown" at this point.

## Static Initializers

Static initializers are used to initialize the static data members of a class. They are executed only once when the class is loaded into memory. Static initializers are helpful when performing some one-time initialization for static variables or setting up static resources.

### **Example**

```java
class Animal {
    private static int count;

    static {
        // Static initializer block
        count = 0;
        System.out.println("Static initializer block executed.");
    }

    public Animal() {
        count++;
        System.out.println("Constructor executed. Count: " + count);
    }

		public static int getCount() {
	    return count;
	  }
}

public class Main {
  public static void main(String[] args) {
    new Animal();
    new Animal();
    // Output:
    // Static initializer block executed.
    // Constructor executed. Count: 1
    // Constructor executed. Count: 2

    System.out.println(Animal.getCount()); // Output: 2
  }
}
```

Here's the explanation of this Java code:

1. **`class Animal`**: This is the class definition for an `Animal`. This class will have methods and attributes that define the properties and behavior of an animal in the code.
2. **`private static int count;`**: This line declares a `private` `static` integer named `count`. As a `static` variable, it belongs to the class itself, not to individual instances of the class. This means all instances of the `Animal` class share this single `count` variable. The `private` keyword means it can only be accessed within this class.
3. **`static { ... }` block**: This is a static initializer block. It is executed once when the class is loaded into the memory. This code initializes the `count` variable to `0` and prints a message "Static initializer block executed." to the console.
4. **`public Animal() { ... }`**: This is the constructor for the `Animal` class. This constructor is called every time a new instance of `Animal` is created. It increments the `count` by one and prints the message "Constructor executed. Count: " followed by the current value of `count`.
5. **`public static int getCount()`**: This is a static method which returns the current value of `count`. As a `static` method, it can be called on the class itself, not on class instances. The `public` keyword means it can be accessed from outside this class.
6. **`public class Main { ... }`**: This is the `Main` class where the program starts execution.
7. **`public static void main(String[] args) { ... }`**: This is the `main` method. It is the entry point of any Java application. The JVM starts execution from this method.
8. **`new Animal(); new Animal();`**: These lines are creating two new instances of the `Animal` class. For each instance, the constructor of `Animal` class is invoked which increments the `count` by one and prints the message. Therefore, the `count` becomes `2` and the messages "Constructor executed. Count: 1" and "Constructor executed. Count: 2" are printed to the console.
9. **`System.out.println(Animal.getCount());`**: This line calls the static method `getCount()` of the `Animal` class and prints the returned value. Since `count` is `2`, it prints `2` to the console.

## Inner/Nested Classes

In Java, a class can be defined within another class, called a nested class or inner class. Inner classes have access to the members of the enclosing class, including private members. Java has four types of nested classes: static nested classes, non-static nested classes (inner classes), local classes, and anonymous classes.

### **Example**

```java
class Animal {
    private String name;

    public Animal(String name) {
        this.name = name;
    }

    public void eat() {
        System.out.println(name + " is eating.");
    }

    class Inner {
        public void innerMethod() {
            System.out.println("Inner method. Name: " + name);
        }
    }
}

public class Main {
    public static void main(String[] args) {
        Animal animal = new Animal("Lion");
        Animal.Inner inner = animal.new Inner();

        inner.innerMethod(); // Output: Inner method. Name: Lion
    }
}
```

Here's a breakdown of the Java code:

1. `class Animal`: This is the declaration of a class named `Animal`.
2. `private String name;`: A private instance variable `name` of type `String` is declared in the `Animal` class. Private means this variable can only be accessed within the `Animal` class itself.
3. `public Animal(String name)`: This is a constructor for the `Animal` class. It takes a string argument and assigns it to the instance variable `name`. The keyword `this` is used to reference the current instance of the class.
4. `public void eat()`: This is an instance method named `eat`. It does not return any value (`void`). When this method is called, it prints the name of the animal along with the string "is eating".
5. `class Inner`: This is an inner class within the `Animal` class. An inner class is a class declared inside another class, giving it access to the outer class's private variables and methods.
6. `public void innerMethod()`: This is a method in the inner class `Inner`. When this method is called, it prints the string "Inner method. Name: " followed by the value of `name`. Since `Inner` is an inner class, it has access to `Animal`'s private variable `name`.
7. `public class Main`: This is the declaration of another class named `Main`. This class contains the `main` method, which is the entry point of the Java program.
8. `public static void main(String[] args)`: This is the `main` method. The JVM starts executing the program from this method.
9. `Animal animal = new Animal("Lion");`: An instance of `Animal` is created with the name "Lion", and the reference to this instance is stored in the `animal` variable.
10. `Animal.Inner inner = animal.new Inner();`: An instance of the inner class `Inner` is created using the `animal` instance. A reference to this inner class instance is stored in the `inner` variable.
11. `inner.innerMethod();`: The `innerMethod` of the `Inner` class is called using the `inner` instance. The output is "Inner method. Name: Lion" since `name` was set to "Lion" when the `Animal` instance was created.

## Further Readings

1. [Oracle Java Tutorials: Inheritance](https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
2. [GeeksforGeeks: Inheritance in Java](https://www.geeksforgeeks.org/inheritance-in-java/)
3. [Oracle Java Tutorials: Polymorphism](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
4. [Baeldung: Polymorphism in Java](https://www.baeldung.com/java-polymorphism)
5. [Oracle Java Tutorials: Overriding and Hiding Methods](https://docs.oracle.com/javase/tutorial/java/IandI/override.html)
6. [Oracle Java Tutorials: Initialization Blocks](https://docs.oracle.com/javase/tutorial/java/javaOO/initial.html)
7. [Oracle Java Tutorials: Initialization Blocks](https://docs.oracle.com/javase/tutorial/java/javaOO/initial.html)
8. [Oracle Java Tutorials: Nested Classes](https://docs.oracle.com/javase/tutorial/java/javaOO/nested.html)
