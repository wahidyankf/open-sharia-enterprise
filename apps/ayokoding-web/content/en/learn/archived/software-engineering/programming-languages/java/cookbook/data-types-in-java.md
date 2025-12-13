---
title: "Data Types in Java"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

Java is a strongly-typed language, meaning every variable and expression has a type known at compile time. Java has two main categories of data types: primitive and non-primitive data types (also known as reference types).

## Primitive Data Types

Primitive data types are the most basic data types in Java. They are built into the language and are not objects. Here are some characteristics of primitive data types:

- **Size**: Primitive data types have a fixed size determined by the type. For example, a `byte` is always 8 bits, an `int` is always 32 bits, and a `double` is always 64 bits.
- **Value**: Primitive data types hold a value of the specified type. For example, a `boolean` can hold either `true` or `false`, an `int` can hold any integer value within its range, and a `double` can hold any floating-point value within its range.
- **Memory Allocation**: Primitive data types are allocated memory on the stack, a region of memory used for temporary storage.
- **Performance**: Primitive data types are generally faster and more efficient than non-primitive data types because they are not objects and do not require the overhead of object creation and garbage collection.

There are eight primitive data types in Java:

1. **boolean**: This data type represents a boolean value, which can be `true` or `false`.
2. **byte**: This data type represents an 8-bit signed two's complement integer. It has a minimum value of -128 and a maximum value of 127.
3. **char**: This data type represents a single 16-bit Unicode character. It has a minimum value of '\u0000' (or 0) and a maximum value of '\uffff' (or 65,535).
4. **short**: This data type represents a 16-bit signed two's complement integer. It has a minimum value of -32,768 and a maximum value of 32,767.
5. **int**: This data type represents a 32-bit signed two's complement integer. It has a minimum value of -2,147,483,648 and a maximum value of 2,147,483,647.
6. **long**: This data type represents a 64-bit signed two's complement integer. It has a minimum value of -9,223,372,036,854,775,808 and a maximum value of 9,223,372,036,854,775,807.
7. **float**: This data type represents a single-precision 32-bit IEEE 754 floating point. It should not be used for precise values such as currency.
8. **double**: This data type represents a double-precision 64-bit IEEE 754 floating point. It should be used for precise values such as currency.

## Non-Primitive Data Types

Non-primitive data types are also known as reference types because they refer to objects. They are created using defined classes or interfaces. Here are some characteristics of non-primitive data types:

- **Size**: Non-primitive data types do not have a fixed size because they can vary depending on the data they hold.
- **Value**: Non-primitive data types hold a reference to an object rather than the object itself. The reference points to a memory location where the object is stored.
- **Memory Allocation**: Non-primitive data types are allocated memory on the heap, a region of memory used for long-term storage.
- **Performance**: Non-primitive data types are generally slower and less efficient than primitive data types because they require the overhead of object creation and garbage collection.

These are examples of non-primitive data types in Java:

1. **Arrays**: An array is a collection of elements of the same type. It can hold a fixed number of values.
2. **Classes**: A class is a blueprint for creating objects. It defines a set of properties and methods that an object of that class will have.
3. **Interfaces**: An interface is a collection of abstract methods a class can implement. It defines a set of methods that a class must implement.
4. **Strings**: A string is a sequence of characters. In Java, strings are objects of the `String` class.

## Further Reading

- [Java Primitive Data Types](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/datatypes.html) - Oracle's official documentation on primitive data types in Java.
- [Java Non-Primitive Data Types](https://docs.oracle.com/javase/tutorial/java/javaOO/objectcreation.html) - Oracle's official documentation on non-primitive data types in Java.
