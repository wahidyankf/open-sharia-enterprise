---
title: "Data Types: Primitive"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code in this section [here](https://github.com/organiclever/ayokoding/tree/main/contents/java-cookbook/java-cookbook-primary/src/main/java/com/ayokoding/cookbook/data_types_primitive).

</aside>

## List of Primitive’s Data Type

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class Primitive {

  public static void main(String[] args) {
    byte b = 100;
    System.out.println(b); // 100

    short s = 1000;
    System.out.println(s); // 1000
    int i = 100000;
    System.out.println(i); // 100000
    long l = 1000000000L;
    System.out.println(l); // 1000000000

    float f = 3.14f;
    System.out.println(f); // 3.14
    double d = 3.141592653589793;
    System.out.println(d); // 3.141592653589793

    char c = 'A';
    System.out.println(c); // A

    boolean bool = true;
    System.out.println(bool); // true
  }
}
```

### Explanation

This Java code demonstrates the use of the primitive data types in Java. It declares variables of each type, assigns them a value, and then prints each value to the console. Let's break it down:

- `byte b = 100;`: Here, a byte (a data type that can hold values from -128 to 127) variable named `b` is declared and assigned the value 100.
- `short s = 1000;`: A short (which can hold values from -32768 to 32767) variable named `s` is declared and assigned the value 1000.
- `int i = 100000;`: An int (which can hold values from -2,147,483,648 to 2,147,483,647) variable named `i` is declared and assigned the value 100000.
- `long l = 1000000000L;`: A long (which can hold values from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807) variable named `l` is declared and assigned the value 1000000000. The `L` at the end denotes that the number is a long literal.
- `float f = 3.14f;`: A float (which can hold decimal values and has a precision of about 6-7 decimal digits) variable named `f` is declared and assigned the value 3.14. The `f` at the end denotes that the number is a float literal.
- `double d = 3.141592653589793;`: A double (which can hold decimal values, and has precision of about 15-17 decimal digits) variable named `d` is declared and assigned the value 3.141592653589793.
- `char c = 'A';`: A char (which can hold a single character) variable named `c` is declared and assigned the value 'A'.
- `boolean bool = true;`: A boolean (which can hold a `true` or `false` value) variable named `bool` is declared and assigned the value `true`.
- The `System.out.println(var);` lines are used to print the value of each variable to the console. The value inside the parentheses is the name of the variable whose value you want to print.

So, if you run this code, it will print the values of the variables in order, each on a new line: 100, 1000, 100000, 1000000000, 3.14, 3.141592653589793, 'A', true.

## `byte` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveByteOperations {
  public static void main(String[] args) {
    // Arithmetic Operations
    byte a = 10;
    byte b = 20;
    byte c;

    c = (byte) (a + b);
    System.out.println(c); // 30

    c = (byte) (b - a);
    System.out.println(c); // 10

    c = (byte) (a * b);
    System.out.println(c); // -56

    c = (byte) (b / a);
    System.out.println(c); // 2

    c = (byte) (b % a);
    System.out.println(c); // 0

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Bitwise Operations
    c = (byte) (a & b);
    System.out.println(c); // 0

    c = (byte) (a | b);
    System.out.println(c); // 30

    c = (byte) (a ^ b);
    System.out.println(c); // 30

    c = (byte) ~a;
    System.out.println(c); // -11

    c = (byte) (a << 2);
    System.out.println(c); // 40

    c = (byte) (a >> 2);
    System.out.println(c); // 2

    c = (byte) (a >>> 2);
    System.out.println(c); // 2

    // Assignment Operations
    a = 10;
    System.out.println(a); // 10

    a += 2;
    System.out.println(a); // 12

    a -= 2;
    System.out.println(a); // 10

    a *= 2;
    System.out.println(a); // 20

    a /= 2;
    System.out.println(a); // 10

    a %= 2;
    System.out.println(a); // 0

    a = 10;
    a &= 2;
    System.out.println(a); // 2

    a = 10;
    a |= 2;
    System.out.println(a); // 10

    a = 10;
    a ^= 2;
    System.out.println(a); // 8

    a = 10;
    a <<= 2;
    System.out.println(a); // 40

    a = 10;
    a >>= 2;
    System.out.println(a); // 2

    a = 10;
    a >>>= 2;
    System.out.println(a); // 2

    // Unary Operations
    a = 10;
    c = (byte) +a;
    System.out.println(c); // 10

    c = (byte) -a;
    System.out.println(c); // -10

    a++;
    System.out.println(a); // 11

    a--;
    System.out.println(a); // 10
  }
}
```

### Explanation

This Java code demonstrates the use of various byte operations in Java. These operations include arithmetic, relational, bitwise, assignment, and unary. Let's break down each of these operations and explain them individually.

1. Arithmetic Operations:
   These operations include addition (+), subtraction (-), multiplication (\*), division (/), and modulus (%). The code performs these operations between the byte variables 'a' and 'b'. Each result is explicitly cast to a byte because Java promotes the operands to int before performing the operations.
2. Relational Operations:
   These operations compare two values and return a boolean result (true or false). The operations include equals to (==), not equals to (!=), greater than (>), less than (<), greater than or equal to (>=), and less than or equal to (<=).
3. Bitwise Operations:
   Bitwise operations are used to perform manipulation of individual bits of a number. They include bitwise AND (&), OR (|), XOR (^), complement (~), left shift (<<), right shift (>>), and unsigned right shift (>>>). Each operation is performed on the variables 'a' and 'b'. The bitwise complement operation is a unary operation and thus performed on 'a' only.
4. Assignment Operations:
   These are combined operations that perform an operation and an assignment at the same time. For example, the "+=" operator adds the right operand to the left operand and assigns the result to the left operand. These operations are performed on the variable 'a' itself.
5. Unary Operations:
   Unary operations are those that require only a single operand. This includes unary plus (+), unary minus (-), increment (++), and decrement (--). These operations are performed on the variable 'a' only.

It's important to note that the byte data type in Java is 8-bit signed two's complement integer. The minimum value is -128 and the maximum value is 127 (inclusive). So when byte overflows (for example, when a multiplication operation exceeds the maximum value of a byte), it goes back to the minimum value and continues from there. This is why the multiplication operation (`a * b`) results in -56 even though `10 * 20` equals `200`, beyond the maximum byte value.

## `short` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveShortOperations {
  public static void main(String[] args) {

    // Arithmetic Operations
    short a = 1000;
    short b = 2000;
    short c;

    c = (short) (a + b);
    System.out.println(c); // 3000

    c = (short) (b - a);
    System.out.println(c); // 1000

    c = (short) (a * b);
    System.out.println(c); // -31616 (overflow occurred)

    c = (short) (b / a);
    System.out.println(c); // 2

    c = (short) (b % a);
    System.out.println(c); // 0

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Bitwise Operations
    c = (short) (a & b);
    System.out.println(c); // 960

    c = (short) (a | b);
    System.out.println(c); // 2040

    c = (short) (a ^ b);
    System.out.println(c); // 1080

    c = (short) ~a;
    System.out.println(c); // -1001

    c = (short) (a << 2);
    System.out.println(c); // 4000

    c = (short) (a >> 2);
    System.out.println(c); // 250

    c = (short) (a >>> 2);
    System.out.println(c); // 250

    // Assignment Operations
    a = 1000;
    System.out.println(a); // 1000

    a += 200;
    System.out.println(a); // 1200

    a -= 200;
    System.out.println(a); // 1000

    a *= 2;
    System.out.println(a); // 2000

    a /= 2;
    System.out.println(a); // 1000

    a %= 200;
    System.out.println(a); // 0

    a = 1000;
    a &= 200;
    System.out.println(a); // 200

    a = 1000;
    a |= 200;
    System.out.println(a); // 1000

    a = 1000;
    a ^= 200;
    System.out.println(a); // 800

    a = 1000;
    a <<= 2;
    System.out.println(a); // 4000

    a = 1000;
    a >>= 2;
    System.out.println(a); // 250

    a = 1000;
    a >>>= 2;
    System.out.println(a); // 250

    // Unary Operations
    a = 1000;
    c = (short) +a;
    System.out.println(c); // 1000

    c = (short) -a;
    System.out.println(c); // -1000

    a++;
    System.out.println(a); // 1001

    a--;
    System.out.println(a); // 1000
  }
}
```

### Explanation

This simple Java class `PrimitiveShortOperations` contains a `main` method demonstrating various operations performed on `short` data type variables in Java.

It starts by declaring two `short` variables `a` and `b` with values `1000` and `2000` respectively, and another `short` variable `c` which will be used to store results.

The operations demonstrated are as follows:

1. **Arithmetic Operations**: Addition (`+`), subtraction (`), multiplication (`), division (`/`), and modulus (`%`) operations are performed on `a` and `b`. Results are casted to `short` because in Java, arithmetic operations result in `int` by default. For instance, `c = (short) (a + b);` adds `a` and `b`, and assigns the result to `c`.
2. **Relational Operations**: Comparison operations are performed such as equal to (`==`), not equal to (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`).
3. **Bitwise Operations**: Bitwise AND (`&`), OR (`|`), XOR (`^`), NOT (`~`), left shift (`<<`), right shift (`>>`), and unsigned right shift (`>>>`) operations are demonstrated. For example, `c = (short) (a & b);` performs a bitwise AND operation on `a` and `b`.
4. **Assignment Operations**: Various assignment operations are performed, such as simple assignment (`=`), addition and assignment (`+=`), subtraction and assignment (`=`), multiplication and assignment (`=`), division and assignment (`/=`), modulus and assignment (`%=`), bitwise AND and assignment (`&=`), bitwise OR and assignment (`|=`), bitwise XOR and assignment (`^=`), left shift and assignment (`<<=`), right shift and assignment (`>>=`), and unsigned right shift and assignment (`>>>=`).
5. **Unary Operations**: Unary plus (`+`), unary minus (``), increment (`++`), and decrement (`-`) operations are demonstrated—for example, `a++;`increments the value of`a` by one.

One thing to note is the line `c = (short) (a * b);`. The result is `-31616` due to an overflow. The maximum value a `short` can store in Java is `32767`, and multiplying `1000 * 2000 = 2000000` which is far beyond the limit of `short`, causing an overflow and resulting in a negative number. This is a good demonstration of why it's important to choose the correct data types for operations to prevent such issues.

## `int` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveIntOperations {
  public static void main(String[] args) {

    // Arithmetic Operations
    int a = 1000;
    int b = 2000;
    int c;

    c = a + b;
    System.out.println(c); // 3000

    c = b - a;
    System.out.println(c); // 1000

    c = a * b;
    System.out.println(c); // 2000000

    c = b / a;
    System.out.println(c); // 2

    c = b % a;
    System.out.println(c); // 0

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Bitwise Operations
    c = a & b;
    System.out.println(c); // 960

    c = a | b;
    System.out.println(c); // 2040

    c = a ^ b;
    System.out.println(c); // 1080

    c = ~a;
    System.out.println(c); // -1001

    c = a << 2;
    System.out.println(c); // 4000

    c = a >> 2;
    System.out.println(c); // 250

    c = a >>> 2;
    System.out.println(c); // 250

    // Assignment Operations
    a = 1000;
    System.out.println(a); // 1000

    a += 200;
    System.out.println(a); // 1200

    a -= 200;
    System.out.println(a); // 1000

    a *= 2;
    System.out.println(a); // 2000

    a /= 2;
    System.out.println(a); // 1000

    a %= 200;
    System.out.println(a); // 0

    a = 1000;
    a &= 200;
    System.out.println(a); // 200

    a = 1000;
    a |= 200;
    System.out.println(a); // 1000

    a = 1000;
    a ^= 200;
    System.out.println(a); // 800

    a = 1000;
    a <<= 2;
    System.out.println(a); // 4000

    a = 1000;
    a >>= 2;
    System.out.println(a); // 250

    a = 1000;
    a >>>= 2;
    System.out.println(a); // 250

    // Unary Operations
    a = 1000;
    c = +a;
    System.out.println(c); // 1000

    c = -a;
    System.out.println(c); // -1000

    a++;
    System.out.println(a); // 1001

    a--;
    System.out.println(a); // 1000
  }
}
```

### Explanation

This Java class `PrimitiveIntOperations` contains a `main` method that demonstrates various operations performed on `int` data type variables in Java.

It starts by declaring two `int` variables `a` and `b` with values `1000` and `2000` respectively, and another `int` variable `c` which will be used to store results.

The operations demonstrated are as follows:

1. **Arithmetic Operations**: Addition (`+`), subtraction (`), multiplication (`), division (`/`), and modulus (`%`) operations are performed on `a` and `b`. For instance, `c = a + b;` adds `a` and `b`, and assigns the result to `c`.
2. **Relational Operations**: Comparison operations are performed such as equal to (`==`), not equal to (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`).
3. **Bitwise Operations**: Bitwise AND (`&`), OR (`|`), XOR (`^`), NOT (`~`), left shift (`<<`), right shift (`>>`), and unsigned right shift (`>>>`) operations are demonstrated. For example, `c = a & b;` performs a bitwise AND operation on `a` and `b`.
4. **Assignment Operations**: Various assignment operations are performed such as simple assignment (`=`), addition and assignment (`+=`), subtraction and assignment (`=`), multiplication and assignment (`=`), division and assignment (`/=`), modulus and assignment (`%=`), bitwise AND and assignment (`&=`), bitwise OR and assignment (`|=`), bitwise XOR and assignment (`^=`), left shift and assignment (`<<=`), right shift and assignment (`>>=`), and unsigned right shift and assignment (`>>>=`).
5. **Unary Operations**: Unary plus (`+`), unary minus (``), increment (`++`), and decrement (`-`) operations are demonstrated. For example, `a++;`increments the value of`a` by one.

In this class, the operations are almost identical to those in the previous `ShortOperations` class. The key difference is that `int` variables are used instead of `short`, which gives a larger range of valid values. For instance, the line `c = a * b;` correctly outputs `2000000` without an overflow, which wasn't the case with the `short` data type in the previous class.

## `long` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveLongOperations {
  public static void main(String[] args) {
    // Arithmetic Operations
    long a = 10L;
    long b = 20L;
    long c;

    c = a + b;
    System.out.println(c); // 30

    c = b - a;
    System.out.println(c); // 10

    c = a * b;
    System.out.println(c); // 200

    c = b / a;
    System.out.println(c); // 2

    c = b % a;
    System.out.println(c); // 0

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Bitwise Operations
    c = a & b;
    System.out.println(c); // 0

    c = a | b;
    System.out.println(c); // 30

    c = a ^ b;
    System.out.println(c); // 30

    c = ~a;
    System.out.println(c); // -11

    c = a << 2;
    System.out.println(c); // 40

    c = a >> 2;
    System.out.println(c); // 2

    c = a >>> 2;
    System.out.println(c); // 2

    // Assignment Operations
    a = 10;
    System.out.println(a); // 10

    a += 2;
    System.out.println(a); // 12

    a -= 2;
    System.out.println(a); // 10

    a *= 2;
    System.out.println(a); // 20

    a /= 2;
    System.out.println(a); // 10

    a %= 2;
    System.out.println(a); // 0

    a = 10;
    a &= 2;
    System.out.println(a); // 2

    a = 10;
    a |= 2;
    System.out.println(a); // 10

    a = 10;
    a ^= 2;
    System.out.println(a); // 8

    a = 10;
    a <<= 2;
    System.out.println(a); // 40

    a = 10;
    a >>= 2;
    System.out.println(a); // 2

    a = 10;
    a >>>= 2;
    System.out.println(a); // 2

    // Unary Operations
    a = 10;
    c = +a;
    System.out.println(c); // 10

    c = -a;
    System.out.println(c); // -10

    a++;
    System.out.println(a); // 11

    a--;
    System.out.println(a); // 10
  }
}
```

### Explanation

This Java class `PrimitiveLongOperations` contains a `main` method demonstrating various operations performed on `long` data type variables in Java.

It starts by declaring two `long` variables `a` and `b` with values `10L` and `20L` respectively, and another `long` variable `c` which will be used to store results.

The operations demonstrated are as follows:

1. **Arithmetic Operations**: Addition (`+`), subtraction (`), multiplication (`), division (`/`), and modulus (`%`) operations are performed on `a` and `b`. For instance, `c = a + b;` adds `a` and `b`, and assigns the result to `c`.
2. **Relational Operations**: Comparison operations are performed such as equal to (`==`), not equal to (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`).
3. **Bitwise Operations**: Bitwise AND (`&`), OR (`|`), XOR (`^`), NOT (`~`), left shift (`<<`), right shift (`>>`), and unsigned right shift (`>>>`) operations are demonstrated. For example, `c = a & b;` performs a bitwise AND operation on `a` and `b`.
4. **Assignment Operations**: Various assignment operations are performed such as simple assignment (`=`), addition and assignment (`+=`), subtraction and assignment (`=`), multiplication and assignment (`=`), division and assignment (`/=`), modulus and assignment (`%=`), bitwise AND and assignment (`&=`), bitwise OR and assignment (`|=`), bitwise XOR and assignment (`^=`), left shift and assignment (`<<=`), right shift and assignment (`>>=`), and unsigned right shift and assignment (`>>>=`).
5. **Unary Operations**: Unary plus (`+`), unary minus (``), increment (`++`), and decrement (`-`) operations are demonstrated. For example, `a++;`increments the value of`a` by one.

In this class, the operations are very similar to those in the previous `PrimitiveIntOperations` class, but with `long` variables instead of `int`. The key difference is that `long` data type can store larger numbers, so it's used when `int` is not sufficient. One minor point to note is that `long` literals in Java are followed by an 'L' or 'l' to denote them as `long` (e.g., `10L` or `10l`).

## `float` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveFloatOperations {
  public static void main(String[] args) {
    // Arithmetic Operations
    float a = 10.5f;
    float b = 20.5f;
    float c;

    c = a + b;
    System.out.println(c); // 31.0

    c = b - a;
    System.out.println(c); // 10.0

    c = a * b;
    System.out.println(c); // 215.25

    c = b / a;
    System.out.println(c); // 1.9523809

    c = b % a;
    System.out.println(c); // 10.0

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Assignment Operations
    a = 10.5f;
    System.out.println(a); // 10.5

    a += 2.5f;
    System.out.println(a); // 13.0

    a -= 2.5f;
    System.out.println(a); // 10.5

    a *= 2.5f;
    System.out.println(a); // 26.25

    a /= 2.5f;
    System.out.println(a); // 10.5

    a %= 2.5f;
    System.out.println(a); // 0.5

    // Unary Operations
    a = 10.5f;
    c = +a;
    System.out.println(c); // 10.5

    c = -a;
    System.out.println(c); // -10.5
  }
}
```

### Explanation

This Java class `PrimitiveFloatOperations` contains a `main` method demonstrating various operations performed on `float` data type variables in Java.

It starts by declaring two `float` variables `a` and `b` with values `10.5f` and `20.5f` respectively, and another `float` variable `c` which will be used to store results.

The operations demonstrated are as follows:

1. **Arithmetic Operations**: Addition (`+`), subtraction (`), multiplication (`), division (`/`), and modulus (`%`) operations are performed on `a` and `b`. For instance, `c = a + b;` adds `a` and `b`, and assigns the result to `c`.
2. **Relational Operations**: Comparison operations are performed such as equal to (`==`), not equal to (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`).
3. **Assignment Operations**: Various assignment operations are performed such as simple assignment (`=`), addition and assignment (`+=`), subtraction and assignment (`=`), multiplication and assignment (`=`), division and assignment (`/=`), and modulus and assignment (`%=`).
4. **Unary Operations**: Unary plus (`+`), unary minus (``) operations are demonstrated. For example, `c = -a;`gives the negation of`a`.

Unlike in the previous examples with `int` and `long` data types, the bitwise operations are not applicable for `float` data types, hence they're not demonstrated in this class.

Another point to note is that `float` literals in Java are followed by an 'F' or 'f' to denote them as `float` (e.g., `10.5F` or `10.5f`). Float is a single-precision 32-bit IEEE 754 floating point, used to save memory in large arrays of floating point numbers and it's more than enough for most purposes.

## `double` Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveDoubleOperations {
  public static void main(String[] args) {
    // Arithmetic Operations
    double a = 10.0;
    double b = 20.0;
    double c;

    c = a + b;
    System.out.println(c); // 30.0

    c = b - a;
    System.out.println(c); // 10.0

    c = a * b;
    System.out.println(c); // 200.0

    c = b / a;
    System.out.println(c); // 2.0

    c = b % a;
    System.out.println(c); // 0.0

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Assignment Operations
    a = 10.0;
    System.out.println(a); // 10.0

    a += 2.0;
    System.out.println(a); // 12.0

    a -= 2.0;
    System.out.println(a); // 10.0

    a *= 2.0;
    System.out.println(a); // 20.0

    a /= 2.0;
    System.out.println(a); // 10.0

    a %= 2.0;
    System.out.println(a); // 0.0

    // Unary Operations
    a = 10.0;
    c = +a;
    System.out.println(c); // 10.0

    c = -a;
    System.out.println(c); // -10.0
  }
}
```

### Explanation

This Java class `PrimitiveDoubleOperations` contains a `main` method demonstrating various operations performed on `double` data type variables in Java.

It starts by declaring two `double` variables `a` and `b` with values `10.0` and `20.0` respectively, and another `double` variable `c` which will be used to store results.

The operations demonstrated are as follows:

1. **Arithmetic Operations**: Addition (`+`), subtraction (`), multiplication (`), division (`/`), and modulus (`%`) operations are performed on `a` and `b`. For instance, `c = a + b;` adds `a` and `b`, and assigns the result to `c`.
2. **Relational Operations**: Comparison operations are performed such as equal to (`==`), not equal to (`!=`), greater than (`>`), less than (`<`), greater than or equal to (`>=`), and less than or equal to (`<=`).
3. **Assignment Operations**: Various assignment operations are performed such as simple assignment (`=`), addition and assignment (`+=`), subtraction and assignment (`=`), multiplication and assignment (`=`), division and assignment (`/=`), and modulus and assignment (`%=`).
4. **Unary Operations**: Unary plus (`+`), unary minus (``) operations are demonstrated. For example, `c = -a;`gives the negation of`a`.

Unlike in the previous examples with `int` and `long` data types, the bitwise operations are not applicable for `double` data types; hence they're not demonstrated in this class.

Double is a double-precision 64-bit IEEE 754 floating point. It is generally used for decimal values and is the default choice for floating-point operations in Java.

## `char` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveCharOperations {
  public static void main(String[] args) {
    // Arithmetic Operations
    char a = 'A';
    char b = 'B';
    int c;

    c = a + b;
    System.out.println(c); // 131

    c = b - a;
    System.out.println(c); // 1

    c = a * b;
    System.out.println(c); // 4290

    c = b / a;
    System.out.println(c); // 1

    c = b % a;
    System.out.println(c); // 1

    // Relational Operations
    System.out.println(a == b); // false
    System.out.println(a != b); // true
    System.out.println(a > b); // false
    System.out.println(a < b); // true
    System.out.println(a >= b); // false
    System.out.println(a <= b); // true

    // Bitwise Operations
    c = a & b;
    System.out.println(c); // 64

    c = a | b;
    System.out.println(c); // 67

    c = a ^ b;
    System.out.println(c); // 3

    c = ~a;
    System.out.println(c); // -66

    c = a << 2;
    System.out.println(c); // 260

    c = a >> 2;
    System.out.println(c); // 16

    c = a >>> 2;
    System.out.println(c); // 16

    // Assignment Operations
    a = 'A';
    System.out.println(a); // A

    a += 2;
    System.out.println(a); // C

    a -= 2;
    System.out.println(a); // A

    a *= 2;
    System.out.println(a); // blank => ?

    a /= 2;
    System.out.println(a); // A

    a %= 2;
    System.out.println(a); // blank => ?

    a = 'A';
    a &= 'B';
    System.out.println(a); // @

    a = 'A';
    a |= 'B';
    System.out.println(a); // C

    a = 'A';
    a ^= 'B';
    System.out.println(a); // blank => ?

    a = 'A';
    a <<= 2;
    System.out.println(a); // Ą

    a = 'Ä';
    a >>= 2;
    System.out.println(a); // 1

    a = 'Ä';
    a >>>= 2;
    System.out.println(a); // 1

    // Unary Operations
    a = 'A';
    c = +a;
    System.out.println(c); // 65

    a++;
    System.out.println(a); // B

    a--;
    System.out.println(a); // A
  }
}
```

### Explanation

This Java class **`PrimitiveCharOperations`** contains a **`main`** method demonstrating various operations performed on **`char`** data type variables in Java. The **`char`** data type in Java stores a single 16-bit Unicode character. It has a minimum value of '\u0000' (or 0) and a maximum value of '\uffff' (or 65,535 inclusive). The class starts by declaring two **`char`** variables, **`a`** and **`b`**, with values 'A' and 'B', respectively.

Here are the explanations for the operations:

**Arithmetic Operations**

1. `c = a + b;`: It adds the ASCII values of 'A' (65) and 'B' (66), resulting in 131.
2. `c = b - a;`: It subtracts the ASCII value of 'A' (65) from 'B' (66), resulting in 1.
3. `c = a * b;`: It multiplies the ASCII values of 'A' (65) and 'B' (66), resulting in 4290.
4. `c = b / a;`: It divides the ASCII value of 'B' (66) by 'A' (65), resulting in 1.
5. `c = b % a;`: It computes the remainder when the ASCII value of 'B' (66) is divided by 'A' (65), resulting in 1.

**Relational Operations**

1. `a == b`: It checks whether 'A' is equal to 'B', resulting in `false`.
2. `a != b`: It checks whether 'A' is not equal to 'B', resulting in `true`.
3. `a > b`: It checks whether 'A' is greater than 'B', resulting in `false`.
4. `a < b`: It checks whether 'A' is less than 'B', resulting in `true`.
5. `a >= b`: It checks whether 'A' is greater than or equal to 'B', resulting in `false`.
6. `a <= b`: It checks whether 'A' is less than or equal to 'B', resulting in `true`.

**Bitwise Operations**

1. `c = a & b;`: It performs a bitwise AND operation on the ASCII values of 'A' (65) and 'B' (66), resulting in 64.
2. `c = a | b;`: It performs a bitwise OR operation on the ASCII values of 'A' (65) and 'B' (66), resulting in 67.
3. `c = a ^ b;`: It performs a bitwise XOR operation on the ASCII values of 'A' (65) and 'B' (66), resulting in 3.
4. `c = ~a;`: It performs a bitwise NOT operation on the ASCII value of 'A' (65), resulting in -66.
5. `c = a << 2;`: It performs a left shift operation on the ASCII value of 'A' (65), moving bits to the left by 2 places, resulting in 260.
6. `c = a >> 2;`: It performs a right shift operation on the ASCII value of 'A' (65), moving bits to the right by 2 places, resulting in 16.
7. `c = a >>> 2;`: It performs an unsigned right shift operation on the ASCII value of 'A' (65), moving bits to the right by 2 places without considering sign, resulting in 16.

**Assignment Operations**

1. `a = 'A';`: It assigns the character 'A' to the variable `a`.
2. `a += 2;`: It adds 2 to the ASCII value of 'A' (65), resulting in the character 'C' (67).
3. `a -= 2;`: It subtracts 2 from the ASCII value of 'C' (67), resulting in the character 'A' (65).
4. `a *= 2;`: It multiplies the ASCII value of 'A' (65) by 2, resulting

in a non-printable character with ASCII value 130. This may appear as blank or a special character in the console.

1. `a /= 2;`: It divides the ASCII value of the non-printable character (130) by 2, resulting in the character 'A' (65).
2. `a %= 2;`: It computes the remainder when the ASCII value of 'A' (65) is divided by 2, resulting in a non-printable character with ASCII value 1. This may appear as blank in the console.
3. `a &= 'B';`: It performs a bitwise AND assignment operation on the ASCII values of 'A' (65) and 'B' (66), resulting in the character '@'.
4. `a |= 'B';`: It performs a bitwise OR assignment operation on the ASCII values of '@' (64) and 'B' (66), resulting in the character 'C'.
5. `a ^= 'B';`: It performs a bitwise XOR assignment operation on the ASCII values of 'C' (67) and 'B' (66), resulting in a non-printable character with ASCII value 3. This may appear as blank in the console.
6. `a <<= 2;`: It performs a left shift assignment operation on the ASCII value of 'A' (65), moving bits to the left by 2 places, resulting in a special character with ASCII value 260.
7. `a >>= 2;`: It performs a right shift assignment operation on the ASCII value of 'Ä', moving bits to the right by 2 places, resulting in the character '1'.
8. `a >>>= 2;`: It performs an unsigned right shift assignment operation on the ASCII value of 'Ä', moving bits to the right by 2 places without considering sign, resulting in the character '1'.

**Unary Operations**

1. `c = +a;`: It gives the ASCII value of 'A', which is 65.
2. `a++;`: It increments the ASCII value of 'A' (65) by 1, resulting in the character 'B' (66).
3. `a--;`: It decrements the ASCII value of 'B' (66) by 1, resulting in the character 'A' (65).

## `boolean` Data Type Operations

### Code

```java
package com.ayokoding.cookbook.data_types_primitive;

public class PrimitiveBooleanOperations {
  public static void main(String[] args) {

    // boolean values
    boolean bool1 = true;
    boolean bool2 = false;

    // Logical AND
    boolean result = bool1 && bool2;
    System.out.println(result); // false

    // Logical OR
    result = bool1 || bool2;
    System.out.println(result); // true

    // Logical XOR (exclusive OR)
    result = bool1 ^ bool2;
    System.out.println(result); // true

    // Logical NOT
    result = !bool1;
    System.out.println(result); // false

    // Equality Check
    result = (bool1 == bool2);
    System.out.println(result); // false

    // Inequality Check
    result = (bool1 != bool2);
    System.out.println(result); // true

    // Assignment
    bool1 = false;
    System.out.println(bool1); // false

    bool1 = true;
    System.out.println(bool1); // true

    // Conditional (Ternary) Operator
    result = (bool1 == bool2) ? bool1 : bool2;
    System.out.println(result); // false
  }
}
```

### Explanation

This Java class `PrimitiveBooleanOperations` contains a `main` method demonstrating various operations performed on `boolean` data type variables in Java.

The `boolean` data type has only two possible values: `true` and `false`. This class starts by declaring two `boolean` variables, `bool1` and `bool2`, with values `true` and `false` respectively.

Here are the operations demonstrated:

1. **Logical AND (`&&`)**: If both `bool1` and `bool2` are true, then the result is true. Otherwise, the result is false. In this case, the result is false because `bool2` is false.
2. **Logical OR (`||`)**: If either `bool1` or `bool2` (or both) is true, then the result is true. Otherwise, the result is false. Here, the result is true because `bool1` is true.
3. **Logical XOR (`^`)**: If exactly one of `bool1` and `bool2` is true (but not both), then the result is true. Otherwise, the result is false. In this case, the result is true because `bool1` is true and `bool2` is false.
4. **Logical NOT (`!`)**: This operation inverts the value of `bool1`. Since `bool1` is true, the result of `!bool1` is false.
5. **Equality (`==`) and Inequality (`!=`)**: These operations check whether `bool1` and `bool2` are equal or not. Since `bool1` is true and `bool2` is false, `bool1 == bool2` returns false and `bool1 != bool2` returns true.
6. **Assignment (`=`)**: This simply assigns a new value to `bool1`.
7. **Conditional (Ternary) Operator (`? :`)**: This operation checks the condition `bool1 == bool2`. If the condition is true, it returns `bool1`; otherwise, it returns `bool2`. In this case, since `bool1` and `bool2` are not equal, it returns `bool2`, which is false.

This code effectively demonstrates how `boolean` variables can be used in Java and the logical operations that can be performed on them.
