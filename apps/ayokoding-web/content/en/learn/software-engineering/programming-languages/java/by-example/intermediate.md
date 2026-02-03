---
title: "Intermediate"
date: 2026-01-02T05:01:35+07:00
draft: false
weight: 10000002
description: "Master intermediate Java through 20 examples: advanced OOP, generics, functional programming, streams, file I/O, testing, and concurrency patterns"
tags:
  [
    "java",
    "tutorial",
    "by-example",
    "intermediate",
    "generics",
    "streams",
    "concurrency",
    "testing",
    "functional-programming",
  ]
---

Master intermediate Java concepts through 20 annotated code examples. Each example builds on beginner foundations, introducing advanced OOP, generics, functional programming, and concurrency patterns.

## Example 31: Abstract Classes and Template Method Pattern

Abstract classes provide partial implementations with abstract methods that subclasses must implement. The template method pattern defines algorithm skeletons in abstract classes with customizable steps.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Abstract["Abstract Class<br/>template method#40;#41;"] --> Concrete1["Concrete Class A<br/>implements abstract methods"]
    Abstract --> Concrete2["Concrete Class B<br/>implements abstract methods"]

    style Abstract fill:#0173B2,color:#fff
    style Concrete1 fill:#DE8F05,color:#fff
    style Concrete2 fill:#029E73,color:#fff
```

**Code**:

```java
// ABSTRACT CLASS - cannot be instantiated
abstract class DataProcessor {
    // => Cannot create: new DataProcessor() - compile error
    // => Must extend to use

    // TEMPLATE METHOD - defines algorithm skeleton
    public final void process() {
        // => Final keyword prevents subclass override
        // => Ensures all subclasses execute same workflow
        loadData();                  // => Step 1: subclass-specific loading
                                     // => Calls overridden loadData() in CSVProcessor/JSONProcessor
        transform();                 // => Step 2: subclass-specific transformation
                                     // => Polymorphic call: runtime type determines implementation
        validate();                  // => Step 3: shared validation logic
                                     // => Calls concrete method (same for all subclasses)
        save();                      // => Step 4: subclass-specific persistence
                                     // => Storage destination varies by subclass
    }

    // ABSTRACT METHODS - subclasses must implement
    protected abstract void loadData();
                                     // => No method body: subclass MUST override
    protected abstract void transform();
                                     // => Customization point for data transformation
    protected abstract void save();  // => Each subclass chooses storage mechanism

    // CONCRETE METHOD - shared implementation
    protected void validate() {      // => All subclasses inherit this
                                     // => Can be overridden (not final)
        System.out.println("Validating data");
                                     // => Output: Validating data
    }
}

// CONCRETE SUBCLASS
class CSVProcessor extends DataProcessor {
                                     // => Inherits template method process()
                                     // => Must implement 3 abstract methods

    @Override
    protected void loadData() {      // => Implements abstract method
        System.out.println("Loading CSV file");
                                     // => Output: Loading CSV file
    }

    @Override
    protected void transform() {     // => Implements abstract method
        System.out.println("Parsing CSV to objects");
                                     // => Output: Parsing CSV to objects
    }

    @Override
    protected void save() {          // => Implements abstract method
        System.out.println("Saving to database");
                                     // => Output: Saving to database
    }
}

class JSONProcessor extends DataProcessor {
                                     // => Different implementations, same interface

    @Override
    protected void loadData() {
        System.out.println("Loading JSON file");
                                     // => Output: Loading JSON file
    }

    @Override
    protected void transform() {
        System.out.println("Parsing JSON to objects");
                                     // => Output: Parsing JSON to objects
    }

    @Override
    protected void save() {
        System.out.println("Saving to cache");
                                     // => Output: Saving to cache
    }
}

// USAGE
DataProcessor csv = new CSVProcessor();
                                     // => Creates concrete instance
csv.process();                       // => Executes 4-step pipeline
                                     // => Calls CSVProcessor's overridden methods
                                     // => Output: Loading CSV file
                                     // =>         Parsing CSV to objects
                                     // =>         Validating data
                                     // =>         Saving to database

DataProcessor json = new JSONProcessor();
// => Different processor, same workflow structure
json.process();
// => Uses JSONProcessor implementations
// => Output: Loading JSON file
// =>         Parsing JSON to objects
// =>         Validating data
// =>         Saving to cache
```

**Key Takeaway**: Abstract classes combine concrete methods (shared implementation) with abstract methods (enforced customization). The template method pattern uses a final concrete method defining the algorithm structure, delegating steps to abstract methods implemented by subclasses—ensuring consistent workflow while allowing customization.

**Why It Matters**: Abstract classes solve the code duplication vs. flexibility tradeoff that plagued early OOP—without them, shared logic required copy-paste across subclasses or awkward empty method implementations in interfaces. The template method pattern enabled frameworks like Spring and servlets to define processing pipelines (request handling, transaction management) while letting developers customize specific steps. Modern Java's default methods in interfaces (Java 8+) provide similar capabilities, but abstract classes remain essential when state (fields) must be shared. The pattern appears throughout the JDK: InputStream.read() templates byte reading, AbstractList provides skeletal List implementation, and Thread.run() enables custom thread behavior.

---

## Example 32: Composition Over Inheritance

Composition builds objects from reusable components rather than inheriting from parent classes. It provides flexibility by assembling behaviors dynamically rather than being locked into inheritance hierarchies.

**Code**:

```java
// COMPOSITION - building objects from components
// => HAS-A relationships instead of IS-A (inheritance)

// Component interfaces
interface Engine {                   // => Contract: all engines must implement start()
                                     // => Enables polymorphism across engine types
    void start();                    // => No body: abstract by default in interfaces
}

interface Transmission {             // => Contract: all transmissions must implement shift()
                                     // => Allows different shifting strategies
    void shift(int gear);            // => Parameter: target gear number
}

// Component implementations
class ElectricEngine implements Engine {
    // => Concrete implementation of Engine interface
    // => Electric motor behavior
    public void start() {
        System.out.println("Electric motor starting silently");
        // => Electric-specific: quiet startup
        // => Output: "Electric motor starting silently"
    }
}

class GasEngine implements Engine {
    // => Concrete implementation of Engine interface
    // => Combustion engine behavior
    public void start() {
        System.out.println("Gas engine roaring to life");
        // => Gas-specific: loud startup with ignition
        // => Output: "Gas engine roaring to life"
    }
}

class ManualTransmission implements Transmission {
    // => Concrete implementation of Transmission interface
    // => Manual gear control strategy
    public void shift(int gear) {
        System.out.println("Manual shift to gear " + gear);
        // => Driver controls each gear change
        // => Output: "Manual shift to gear N"
    }
}

class AutomaticTransmission implements Transmission {
    // => Concrete implementation of Transmission interface
    // => Automatic gear control strategy
    public void shift(int gear) {
        System.out.println("Automatic shift to gear " + gear);
        // => System determines shift timing
        // => Output: "Automatic shift to gear N"
    }
}

// COMPOSED CLASS - has-a relationship
class Car {                          // => Car contains components (composition)
    private final Engine engine;     // => HAS-A Engine (can be any Engine implementation)
                                     // => Final: cannot be reassigned after construction
    private final Transmission transmission;
                                     // => HAS-A Transmission (can be any Transmission type)

    public Car(Engine engine, Transmission transmission) {
                                     // => Dependency injection pattern
        this.engine = engine;        // => Store engine reference
        this.transmission = transmission;
                                     // => Store transmission reference
    }

    public void drive() {            // => Coordinate components
        engine.start();              // => Delegate to composed engine
        transmission.shift(1);       // => Shift to 1st gear
        transmission.shift(2);       // => Shift to 2nd gear
    }
}

// USAGE - flexible assembly of behaviors
Car electricAuto = new Car(new ElectricEngine(), new AutomaticTransmission());
// => Mix electric engine + automatic transmission
electricAuto.drive();
// => Output: Electric motor starting silently
// =>         Automatic shift to gear 1
// =>         Automatic shift to gear 2

Car gasManual = new Car(new GasEngine(), new ManualTransmission());
// => Mix gas engine + manual transmission
gasManual.drive();
// => Output: Gas engine roaring to life
// =>         Manual shift to gear 1
// =>         Manual shift to gear 2

// CONTRAST: Inheritance approach (rigid, explosive class hierarchy)
// => Would need: ElectricAutoCar, ElectricManualCar, GasAutoCar, GasManualCar
// => 2 engines × 2 transmissions = 4 classes (composition uses 2 components)
// => Adding diesel: 2 more classes (composition: 1 component)
// => Composition grows linearly, inheritance grows exponentially
```

**Key Takeaway**: Composition assembles objects from independent components (has-a relationships), providing runtime flexibility to mix and match behaviors. Prefer composition over inheritance to avoid rigid class hierarchies and the "diamond problem" where multiple inheritance paths create ambiguity.

**Why It Matters**: "Favor composition over inheritance" became a core OOP principle after decades of inheritance abuse created unmaintainable class hierarchies. Inheritance couples subclasses to superclass implementation details—changing a parent class breaks all children (fragile base class problem). Composition enables dependency injection frameworks like Spring to wire objects at runtime, testing frameworks to inject mocks, and strategy patterns to swap algorithms dynamically. The Java Collections Framework uses composition extensively: ArrayList composes an Object[] array, LinkedList composes Node objects, and decorator classes like Collections.synchronizedList() wrap existing lists with thread-safety behavior without inheritance.

---

## Example 33: Nested and Inner Classes

Java supports nested classes (static) and inner classes (non-static) that provide encapsulation and logical grouping. Inner classes access outer class instance members, while nested classes are independent.

**Code**:

```java
public class OuterClass {            // => Outer class containing nested/inner classes
    private String outerField = "Outer field";
                                     // => Instance field accessible to inner classes
    private static String staticField = "Static field";
                                     // => Static field accessible to all nested classes

    // STATIC NESTED CLASS - independent of outer instance
    public static class StaticNested {
                                     // => Static: no implicit reference to outer instance
                                     // => Can instantiate without OuterClass instance
        public void display() {      // => Method in static nested class
            System.out.println(staticField);
                                     // => ✅ Can access static outer members
                                     // => Output: "Static field"
            // System.out.println(outerField);
                                     // => ❌ ERROR: cannot access instance members
        }
    }

    // INNER CLASS (non-static) - tied to outer instance
    public class Inner {             // => Non-static: holds implicit outer reference
                                     // => Cannot exist without OuterClass instance
        private String innerField = "Inner field";
                                     // => Inner class can have own fields

        public void display() {
            System.out.println(outerField);
                                     // => ✅ Accesses outer instance field
                                     // => Implicit: OuterClass.this.outerField
            System.out.println(staticField);
                                     // => ✅ Also accesses static members
            System.out.println(innerField);
                                     // => Accesses own field
        }

        public void accessOuter() {
            OuterClass.this.outerField = "Modified";
                                     // => Explicit outer reference syntax
                                     // => OuterClass.this = the outer instance
        }
    }

    // METHOD LOCAL INNER CLASS - defined inside method
    public void methodWithLocalClass() {
                                     // => Local class scope: only in this method
        final String localVar = "Local variable";
                                     // => Must be final/effectively final (Java 8+)

        class LocalInner {           // => Class defined inside method body
                                     // => Can access method's local variables
            public void display() {
                System.out.println(localVar);
                                     // => Accesses enclosing method's final variable
                System.out.println(outerField);
                                     // => Also accesses outer instance fields
            }
        }

        LocalInner local = new LocalInner();
                                     // => Instantiate local class within method
        local.display();             // => Output: "Local variable", "Outer field"
    }

    // ANONYMOUS INNER CLASS - one-time implementation
    public Runnable createRunnable() {
        // => Returns interface instance without named class
        return new Runnable() {
            // => Anonymous class: no class name
            // => Implements Runnable on the fly
            @Override
            public void run() {
                System.out.println("Anonymous inner class: " + outerField);
                // => Can access outer instance members
                // => Output: "Anonymous inner class: Outer field"
            }
        };
        // => Before Java 8 lambdas, this was common pattern
    }
}

// USAGE
OuterClass.StaticNested nested = new OuterClass.StaticNested();
// => Create static nested class: no outer instance needed
// => Syntax: OuterClass.NestedClass
nested.display();                    // => Calls display() on static nested instance
                                     // => Output: "Static field"

OuterClass outer = new OuterClass();
// => Create outer instance first
OuterClass.Inner inner = outer.new Inner();
// => Create inner class: requires outer instance
// => Syntax: outerInstance.new InnerClass()
// => Inner holds reference to 'outer'
inner.display();
// => Output: "Outer field"
// =>         "Static field"
// =>         "Inner field"

outer.methodWithLocalClass();
// => Executes method containing local inner class
// => Output: "Local variable"
// =>         "Outer field"

Runnable r = outer.createRunnable();
// => Returns anonymous Runnable instance
r.run();
// => Output: "Anonymous inner class: Outer field"
```

**Key Takeaway**: Static nested classes are independent of outer instances and can only access outer static members. Inner classes (non-static) are tied to outer instances and can access all outer members. Use nested classes for logical grouping and inner classes when tight coupling with outer state is needed.

**Why It Matters**: Inner classes enable event handling patterns in GUI frameworks (Swing, JavaFX) where listeners need access to surrounding component state without passing references manually. Anonymous inner classes powered Java's callback mechanisms before lambdas (Java 8), though lambdas are now preferred for functional interfaces. Static nested classes organize helper classes without polluting package namespace—Map.Entry, LinkedList.Node, and Builder patterns use this extensively. The outer class reference overhead (inner classes store implicit reference to outer instance) can cause memory leaks if inner instances outlive outer instances, a common pitfall in Android Activity listeners.

---

## Example 34: Reflection API - Runtime Introspection

Reflection allows runtime inspection and manipulation of classes, methods, and fields. It enables frameworks to work with user-defined classes without compile-time knowledge.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Class["Class Object"] --> Methods["Methods<br/>getMethods#40;#41;"]
    Class --> Fields["Fields<br/>getFields#40;#41;"]
    Class --> Constructors["Constructors<br/>getConstructors#40;#41;"]
    Methods --> Invoke["Invoke<br/>method.invoke#40;#41;"]

    style Class fill:#0173B2,color:#fff
    style Methods fill:#DE8F05,color:#fff
    style Fields fill:#029E73,color:#fff
    style Constructors fill:#CC78BC,color:#fff
    style Invoke fill:#CA9161,color:#fff
```

**Code**:

```java
import java.lang.reflect.*;
// => Import reflection API classes

class Person {
    // => Simple class for reflection demonstration
    private String name;
    // => Private field: not accessible without setAccessible()
    public int age;
    // => Public field: directly accessible via reflection

    public Person() {}
    // => Default constructor

    public Person(String name, int age) {
        // => Parameterized constructor
        this.name = name;
        this.age = age;
    }

    private void secretMethod() {
        // => Private method: needs setAccessible() to invoke
        System.out.println("Secret: " + name);
    }

    public String getName() {
        // => Public getter method
        return name;
    }
}

// REFLECTION USAGE
Class<?> clazz = Person.class;
                                 // => Obtain Class metadata object for Person
                                 // => .class literal retrieves compile-time Class reference
                                 // => <?> wildcard: type-safe reflection (unknown generic type)
                                 // => Alternative: Class.forName("Person") loads by string name
                                 // => Class object contains all runtime type information

// GET CLASS INFORMATION
String className = clazz.getName();
                                 // => Returns fully qualified name (package + class)
                                 // => Result: "Person" (or "com.example.Person" if in package)
                                 // => Includes package prefix for uniqueness
String simpleName = clazz.getSimpleName();
                                 // => Returns class name without package prefix
                                 // => Result: "Person" (just class name)
                                 // => Used for display/logging purposes

// INSTANTIATE via reflection
Constructor<?> constructor = clazz.getConstructor(String.class, int.class);
                                 // => Finds public constructor matching parameter types
                                 // => getConstructor() searches public constructors only
                                 // => Parameter types: String.class, int.class (varargs)
                                 // => Throws NoSuchMethodException if no matching constructor
                                 // => Constructor<?> holds reference to constructor metadata
Object instance = constructor.newInstance("Alice", 30);
                                 // => Invokes constructor with arguments
                                 // => newInstance() calls Constructor with varargs Object[]
                                 // => Creates instance: new Person("Alice", 30)
                                 // => Returns Object (generic type, needs casting for type safety)
                                 // => Throws InstantiationException, IllegalAccessException, InvocationTargetException

// ACCESS FIELDS
Field ageField = clazz.getField("age");
                                 // => Gets PUBLIC field named "age" by string lookup
                                 // => getField() searches public fields only (not private)
                                 // => Throws NoSuchFieldException if field not found or not public
                                 // => Field object wraps field metadata
int ageValue = (int) ageField.get(instance);
                                 // => Reads field value from specific instance
                                 // => ageField.get() returns Object, requires cast to int
                                 // => Retrieves value from Person object's age field
                                 // => Result: 30 (initial value from constructor)
                                 // => Autoboxing: int → Integer → Object → (int) unbox
ageField.set(instance, 31);      // => Modifies field value reflectively
                                 // => ageField.set(object, value) mutates field on instance
                                 // => Changes age from 30 to 31
                                 // => Now age = 31 (modified)
                                 // => Value autoboxed: 31 (int) → Integer → Object

// ACCESS PRIVATE FIELDS
Field nameField = clazz.getDeclaredField("name");
                                 // => Gets any field (public or private) by name
                                 // => getDeclaredField() finds private members (bypasses normal access)
                                 // => Searches only this class (not inherited fields)
                                 // => "name" is private, so getField("name") would fail
nameField.setAccessible(true);   // => Disables Java access control checks
                                 // => Allows reading/writing private field
                                 // => ⚠️ Security risk: breaks encapsulation (violates OOP)
                                 // => SecurityManager may block this if installed
                                 // => Equivalent to making field public at runtime
String nameValue = (String) nameField.get(instance);
                                 // => Reads private field value via reflection
                                 // => nameField.get() returns Object, cast to String
                                 // => Result: "Alice" (value set in constructor)
                                 // => Without setAccessible(true): IllegalAccessException

// INVOKE METHODS
Method getNameMethod = clazz.getMethod("getName");
                                 // => Finds public method by name and parameter types
                                 // => getMethod() searches public methods (including inherited)
                                 // => Empty parameter array: no arguments for getName()
                                 // => Returns Method object wrapping method metadata
String name = (String) getNameMethod.invoke(instance);
                                 // => Invokes method on instance reflectively
                                 // => invoke(object, args...) calls method with varargs
                                 // => Equivalent to: instance.getName() (direct call)
                                 // => Returns Object (generic), cast to String
                                 // => Result: "Alice" (method return value)
                                 // => ~3x slower than direct invocation

// INVOKE PRIVATE METHODS
Method secretMethod = clazz.getDeclaredMethod("secretMethod");
                                 // => Finds private method by name
                                 // => getDeclaredMethod() searches private methods
                                 // => getMethod() would fail (private not accessible)
secretMethod.setAccessible(true);// => Bypasses private access modifier
                                 // => Allows invoking private method
                                 // => ⚠️ Breaks encapsulation (testing/framework use)
secretMethod.invoke(instance);   // => Invokes private method reflectively
                                 // => Calls secretMethod() on instance
                                 // => Method executes: prints "Secret: Alice"
                                 // => Output: "Secret: Alice"
                                 // => Without setAccessible(true): IllegalAccessException

// LIST ALL METHODS
for (Method method : clazz.getDeclaredMethods()) {
    // => Iterates all declared methods (public and private)
    System.out.println(method.getName());
    // => Prints method names
}
// => Output: secretMethod
// =>         getName
```

**Key Takeaway**: Reflection provides runtime access to class metadata, allowing inspection of fields, methods, and constructors. Use `setAccessible(true)` to bypass access controls for private members. Reflection enables frameworks and libraries to work with arbitrary user classes without compile-time knowledge.

**Why It Matters**: Reflection powers Java's entire framework ecosystem—Spring uses it for dependency injection (scanning for @Component, @Autowired), JPA for entity mapping (@Entity, @Column), JUnit for test discovery (@Test), and serialization frameworks (Gson, Jackson) for JSON mapping. Without reflection, these frameworks would require manual registration or code generation for every class. However, reflection has costs: performance overhead (method.invoke() is ~3x slower than direct calls), type safety loss (casts and string-based lookups), and security risks (accessing private members breaks encapsulation). Modern Java introduced MethodHandles (Java 7) as faster, type-safe alternatives, while annotation processors enable compile-time code generation replacing some reflection use cases.

---

## Example 35: Annotations and Custom Metadata

Annotations add metadata to code that can be processed at compile-time or runtime. Custom annotations enable declarative programming patterns used extensively in frameworks.

**Code**:

```java
import java.lang.annotation.*;
import java.lang.reflect.*;

// DEFINE CUSTOM ANNOTATION
@Retention(RetentionPolicy.RUNTIME)  // => Available at runtime via reflection
                                     // => Enables runtime inspection of annotation data
@Target(ElementType.METHOD)          // => Can only be applied to methods
                                     // => Compile error if used on class/field
public @interface Test {             // => @interface keyword defines annotation type
    String description() default "";  // => Annotation parameter with default value
    int timeout() default 0;         // => Optional timeout parameter (milliseconds)
}

// ANOTHER ANNOTATION
@Retention(RetentionPolicy.RUNTIME)  // => Available at runtime
@Target(ElementType.FIELD)           // => Restricts to field declarations
public @interface Inject {           // => Marks fields for dependency injection
}

// USE ANNOTATIONS
class TestSuite {                    // => Class containing test methods
    @Inject                          // => Annotation marks field for injection
    private String dependency;       // => Framework will populate this field

    @Test(description = "Adds two numbers", timeout = 1000)
                                     // => Annotation with parameters
    public void testAddition() {     // => Test method (has @Test)
        System.out.println("Testing addition");
    }

    @Test(description = "Divides by zero")
                                     // => Annotation with description only
    public void testDivision() {     // => Test method (has @Test)
        System.out.println("Testing division");
    }

    public void helperMethod() {     // => No @Test annotation
        System.out.println("Not a test");
    }
}

// PROCESS ANNOTATIONS via reflection
Class<?> clazz = TestSuite.class;
                                 // => Get Class object representing TestSuite type
                                 // => clazz is java.lang.Class<TestSuite> (type metadata)
                                 // => Class object contains all runtime information about TestSuite
                                 // => Enables introspection of methods, fields, annotations

// Find and execute @Test methods
for (Method method : clazz.getDeclaredMethods()) {
                                 // => getDeclaredMethods() returns all methods (public + private)
                                 // => Returns Method[] array: testAddition, testDivision, helperMethod
                                 // => Method object wraps method metadata (name, params, annotations)
                                 // => Each Method represents one method in the class
    if (method.isAnnotationPresent(Test.class)) {
                                 // => Check if method has @Test annotation at runtime
                                 // => Uses reflection to inspect method's annotation metadata
                                 // => testAddition: true (has @Test), testDivision: true, helperMethod: false
                                 // => Annotation must have RUNTIME retention to be visible
                                 // => RetentionPolicy.RUNTIME enables runtime discovery
                                 // => SOURCE/CLASS retention annotations discarded before runtime
        Test testAnnotation = method.getAnnotation(Test.class);
                                 // => Retrieve @Test annotation instance from method
                                 // => Returns proxy implementing Test interface
                                 // => testAnnotation contains description and timeout parameters
                                 // => Parameters accessible as method calls (description(), timeout())
        System.out.println("Running test: " + testAnnotation.description());
                                 // => Access annotation parameter value via method call
                                 // => testAnnotation.description() returns String from annotation
                                 // => Output: "Running test: Adds two numbers" (for testAddition)
                                 // => Output: "Running test: Divides by zero" (for testDivision)

        try {
            Object instance = clazz.getDeclaredConstructor().newInstance();
                                 // => Create new TestSuite instance via reflection
                                 // => getDeclaredConstructor() finds no-arg constructor
                                 // => newInstance() calls constructor, returns new object
            method.invoke(instance);  // => Execute test method on instance
                                 // => invoke() calls method dynamically (equivalent to instance.testAddition())
        } catch (Exception e) {
            System.out.println("Test failed: " + e.getMessage());
                                 // => Catch exceptions from method invocation
        }
    }
}
// => Output: Running test: Adds two numbers, Testing addition
//           Running test: Divides by zero, Testing division

// BUILT-IN ANNOTATIONS
class Example {
    @Override                    // => Compile-time check for overriding
                                 // => Compiler error if not actually overriding
    public String toString() {   // => Overrides Object.toString()
        return "Example";
    }

    @Deprecated                  // => Marks method as deprecated
                                 // => Compiler warning when called
    public void oldMethod() {}   // => Legacy method

    @SuppressWarnings("unchecked") // => Suppresses specific compiler warnings
                                   // => Useful for legacy code with raw types
    public void rawTypeMethod() {  // => Method using raw types
        java.util.List list = new java.util.ArrayList();
                                   // => Raw List (no generic type)
    }
}
```

**Key Takeaway**: Annotations add metadata to code without affecting execution. Use `@Retention` to specify when annotations are available (SOURCE, CLASS, RUNTIME) and `@Target` to restrict where annotations can be applied. Process runtime annotations via reflection to implement custom behaviors.

**Why It Matters**: Annotations revolutionized Java framework design by enabling declarative programming—compare Spring's `@Autowired` to manual factory.getBean() calls, or JPA's `@Entity @Column` to XML configuration files. Annotations shift complexity from user code to framework code, improving readability (@Transactional vs. manual transaction.begin/commit). However, overuse creates "magic" behavior invisible in code (Spring's @Async spawning threads invisibly), making debugging difficult. Annotation processing (compile-time, via javax.annotation.processing) generates code during compilation (Lombok, Dagger), avoiding reflection overhead while keeping declarative syntax. The combination of annotations + reflection enabled the "convention over configuration" movement that made frameworks like Spring Boot possible.

---

## Example 36: Generic Methods and Bounded Type Parameters

Generic methods enable type-safe method implementations that work with any type. Bounded type parameters restrict generic types to subclasses of a bound, enabling access to bound class methods.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Method["Generic Method<br/><T> void print#40;T item#41;"] --> AnyType["Accepts any type<br/>String, Integer, etc."]

    Bounded["Bounded Method<br/><T extends Number>"] --> Restrict["Only Number subclasses<br/>Integer, Double, etc."]
    Restrict --> Access["Can call Number methods<br/>doubleValue#40;#41;"]

    MultiBound["Multiple Bounds<br/><T extends A & B>"] --> Class["Extends class A"]
    MultiBound --> Interface["Implements interface B"]

    style Method fill:#0173B2,color:#fff
    style Bounded fill:#DE8F05,color:#fff
    style MultiBound fill:#029E73,color:#fff
    style Access fill:#CC78BC,color:#fff
    style Class fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.*;

// GENERIC METHOD - type parameter before return type
public static <T> void printArray(T[] array) {
                                 // => <T> declares type parameter for this method
                                 // => T can be any type (inferred from argument)
    for (T element : array) {    // => Enhanced for-loop iterates array elements
                                 // => Each element has type T (type-safe)
        System.out.print(element + " ");
                                 // => Prints element followed by space
    }
    System.out.println();        // => Newline after all elements printed
}

// BOUNDED TYPE PARAMETER - restricts to Number subclasses
public static <T extends Number> double sum(List<T> numbers) {
                                 // => T must be Number or subclass (Integer, Double, etc.)
    double total = 0;
    for (T num : numbers) {
        total += num.doubleValue();  // => Can call Number methods because of bound
    }
    return total;
}

// MULTIPLE BOUNDS - must extend class AND implement interfaces
interface Measurable {
    double getMeasurement();
}

public static <T extends Number & Measurable> double average(List<T> items) {
                                 // => T must extend Number AND implement Measurable
    double total = 0;
    for (T item : items) {
        total += item.getMeasurement();
    }
    return total / items.size();
}

// USAGE
Integer[] ints = {1, 2, 3};      // => Array of Integer wrapper objects
String[] strs = {"a", "b", "c"};  // => Array of String objects
printArray(ints);                // => Compiler infers T = Integer from argument type
                                 // => Calls printArray<Integer>(Integer[])
                                 // => Output: 1 2 3
printArray(strs);                // => Compiler infers T = String from argument type
                                 // => Calls printArray<String>(String[])
                                 // => Output: a b c

List<Integer> intList = Arrays.asList(1, 2, 3, 4);
                                 // => Creates fixed-size List<Integer> from varargs
                                 // => List contains [1, 2, 3, 4]
double sum = sum(intList);       // => Calls sum<Integer>(List<Integer>)
                                 // => Iterates list: 1+2+3+4 = 10
                                 // => sum is 10.0 (converted to double)

// GENERIC CLASS with type parameter
class Box<T> {
                                 // => Generic class with type parameter T
                                 // => T is placeholder, replaced at instantiation
    private T content;           // => Field of type T (unknown until instantiation)

    public void set(T content) {
                                 // => Method parameter type T
                                 // => Enforces type safety at compile time
        this.content = content;  // => Stores value of type T
    }

    public T get() {
                                 // => Return type T (same type as set)
        return content;          // => Returns stored value (type T)
    }
}

Box<String> stringBox = new Box<>();
                                 // => Diamond operator <> infers type from left side
                                 // => T = String for this instance
                                 // => Creates Box<String>
stringBox.set("Hello");          // => Type-safe: only String allowed
                                 // => Compiler enforces: set(String)
                                 // => Calling set(123) would be compile error
String value = stringBox.get();  // => No cast needed (compiler knows returns String)
                                 // => value is "Hello" (type: String)
```

**Key Takeaway**: Generic methods use `<T>` before return type to declare type parameters. Bounded type parameters (`<T extends Class>`) restrict acceptable types and enable calling methods of the bound class. Multiple bounds require `<T extends Class & Interface1 & Interface2>` syntax with class first.

**Why It Matters**: Bounded type parameters solve the "how do I operate on generic types?" problem—without bounds, generic `<T>` treats everything as Object, preventing method calls beyond Object's methods. Bounds enable generic algorithms: Collections.sort() requires `<T extends Comparable<T>>` to call compareTo(), Stream.max() requires Comparator, and numeric operations require `<T extends Number>`. Multiple bounds enable rich generic constraints in libraries like Hibernate (entities must extend BaseEntity AND implement Serializable). The class-first rule in multiple bounds reflects Java's single-inheritance, multiple-interfaces model, though rarely needed in practice.

---

## Example 37: Wildcards and Type Variance

Wildcards (`?`) represent unknown types in generics. Upper-bounded wildcards (`? extends Type`) enable reading, lower-bounded wildcards (`? super Type`) enable writing, following covariance and contravariance rules.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Unbounded["Unbounded<br/>List<?>"] --> ReadObj["Read as Object"]
    Unbounded --> NoWrite["Cannot write"]

    Upper["Upper bound<br/>List<? extends Number>"] --> ReadNum["Read as Number"]
    Upper --> NoWriteNum["Cannot write<br/>#40;unknown exact type#41;"]

    Lower["Lower bound<br/>List<? super Integer>"] --> WriteInt["Write Integer"]
    Lower --> ReadObjLower["Read as Object only"]

    style Unbounded fill:#0173B2,color:#fff
    style Upper fill:#DE8F05,color:#fff
    style Lower fill:#029E73,color:#fff
    style ReadNum fill:#CC78BC,color:#fff
    style WriteInt fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.*;

// UNBOUNDED WILDCARD - unknown type
public static void printList(List<?> list) {
                                 // => Accepts List of any type (?)
    for (Object elem : list) {   // => Can only read as Object (safest supertype)
        System.out.print(elem + " ");
    }
    System.out.println();
    // list.add("x");            // => ERROR: cannot write to List<?>
}

// UPPER-BOUNDED WILDCARD - covariance (reading)
public static double sumNumbers(List<? extends Number> numbers) {
                                 // => ? extends Number: accepts List<Integer>, List<Double>, etc.
    double sum = 0;
    for (Number num : numbers) { // => Can read as Number (upper bound)
        sum += num.doubleValue();// => Calls Number.doubleValue()
    }
    return sum;
    // numbers.add(1);           // => ERROR: cannot write (exact type unknown)
}

// LOWER-BOUNDED WILDCARD - contravariance (writing)
public static void addIntegers(List<? super Integer> list) {
                                 // => ? super Integer: accepts List<Integer>, List<Number>, List<Object>
    list.add(1);                 // => Can write Integer (safe for any supertype)
    list.add(2);
    // Integer val = list.get(0); // => ERROR: can only read as Object
                                 // => Could be List<Number> or List<Object>
}

// USAGE
List<Integer> ints = Arrays.asList(1, 2, 3);
List<Double> doubles = Arrays.asList(1.0, 2.0, 3.0);

printList(ints);                 // => Output: 1 2 3
printList(doubles);              // => Output: 1.0 2.0 3.0

double sum1 = sumNumbers(ints);  // => sum1 is 6.0 (Integer extends Number)
double sum2 = sumNumbers(doubles); // => sum2 is 6.0 (Double extends Number)

List<Number> numbers = new ArrayList<>();
addIntegers(numbers);            // => Can add Integers (Number super Integer)
                                 // => numbers becomes [1, 2]
System.out.println(numbers);     // => Output: [1, 2]

// PECS RULE: Producer Extends, Consumer Super
// Producer (reading): use <? extends T>
// Consumer (writing): use <? super T>

public static <T> void copy(List<? extends T> source, List<? super T> dest) {
                                 // => Generic method with two wildcard parameters
                                 // => source produces T (extends: covariance)
                                 // => dest consumes T (super: contravariance)
                                 // => Enables copying from subtype to supertype
    for (T item : source) {
                                 // => Reads from source (produces T)
                                 // => item has type T
        dest.add(item);          // => Writes to dest (consumes T)
                                 // => Safe: dest accepts T or supertype
    }
}

List<Integer> src = Arrays.asList(1, 2, 3);
                                 // => Source: List<Integer>
List<Number> dst = new ArrayList<>();
                                 // => Destination: List<Number> (supertype of Integer)
copy(src, dst);                  // => T = Integer (inferred)
                                 // => source: List<? extends Integer> (List<Integer> matches)
                                 // => dest: List<? super Integer> (List<Number> matches)
                                 // => Copies [1, 2, 3] from src to dst
                                 // => dst now contains [1, 2, 3]
```

**Key Takeaway**: Use `? extends Type` for reading (covariance), `? super Type` for writing (contravariance), and `?` for both reading (as Object) and no writing. Follow PECS rule: "Producer Extends, Consumer Super"—if a method produces values from a collection, use `extends`; if it consumes values into a collection, use `super`.

**Why It Matters**: Wildcards enable flexible generic APIs that accept related types without requiring exact matches—before wildcards, List<Integer> and List<Number> were completely unrelated despite inheritance. The PECS rule guides API design: Collections.addAll(Collection<? extends E>) is producer (extends), Collections.sort(List<T>, Comparator<? super T>) is consumer (super). Understanding variance prevents type safety violations: allowing writes to List<? extends Number> would permit adding Double to List<Integer>. This complexity drives some developers to avoid wildcards entirely, but mastering them enables elegant generic APIs like Stream.collect(Collector<? super T>).

---

## Example 38: Collections Framework Deep Dive

The Collections Framework provides algorithms for sorting, searching, and transforming collections. Understanding time complexities and choosing appropriate implementations is critical for performance.

**Code**:

```java
import java.util.*;
                                 // => Import collections framework classes

// CHOOSING COLLECTION TYPES based on requirements

// ArrayList - indexed access, dynamic size
List<String> arrayList = new ArrayList<>();
arrayList.add("A");              // => O(1) amortized, arrayList is ["A"]
String value = arrayList.get(0); // => O(1) random access, value is "A"
arrayList.remove(0);             // => O(n) due to shifting, arrayList is []

// LinkedList - efficient insertion/deletion
List<String> linkedList = new LinkedList<>();
linkedList.add("A");             // => O(1) append
linkedList.add(0, "B");          // => O(1) prepend, linkedList is ["B", "A"]
String first = linkedList.get(0); // => O(n) traversal, first is "B"

// HashSet - unique elements, O(1) operations
Set<String> hashSet = new HashSet<>();
hashSet.add("A");                // => O(1) insert, hashSet is {"A"}
boolean contains = hashSet.contains("A");
                                 // => O(1) lookup, contains is true

// TreeSet - sorted, O(log n) operations
Set<Integer> treeSet = new TreeSet<>();
treeSet.add(3);
treeSet.add(1);                  // => Auto-sorted: treeSet is {1, 3}
treeSet.add(2);                  // => treeSet is {1, 2, 3}
System.out.println(treeSet);     // => Output: [1, 2, 3]

// HashMap - key-value pairs, O(1) operations
Map<String, Integer> hashMap = new HashMap<>();
hashMap.put("Alice", 30);        // => O(1) insert
Integer age = hashMap.get("Alice"); // => O(1) lookup, age is 30

// TreeMap - sorted by keys, O(log n) operations
Map<String, Integer> treeMap = new TreeMap<>();
treeMap.put("Charlie", 25);
treeMap.put("Alice", 30);
treeMap.put("Bob", 28);          // => Maintains sorted order by key
System.out.println(treeMap);     // => Output: {Alice=30, Bob=28, Charlie=25}

// COLLECTIONS UTILITY METHODS
List<Integer> numbers = Arrays.asList(3, 1, 4, 1, 5, 9);
                                 // => numbers is [3, 1, 4, 1, 5, 9] (fixed-size list)

Collections.sort(numbers);       // => In-place sort (mutates original)
                                 // => numbers is [1, 1, 3, 4, 5, 9]
Collections.reverse(numbers);    // => In-place reversal
                                 // => numbers is [9, 5, 4, 3, 1, 1]
Collections.shuffle(numbers);    // => Randomizes order (uses Random)
                                 // => numbers is [unpredictable order]

int max = Collections.max(numbers); // => Finds maximum element
                                 // => max is 9
int min = Collections.min(numbers); // => Finds minimum element
                                 // => min is 1

int frequency = Collections.frequency(numbers, 1);
                                 // => Counts occurrences of 1
                                 // => frequency is 2 (appears twice)

// IMMUTABLE COLLECTIONS (Java 9+)
List<String> immutable = List.of("A", "B", "C");
                                 // => immutable is ["A", "B", "C"] (cannot modify)
// immutable.add("D");           // => UnsupportedOperationException (immutable)

Map<String, Integer> immutableMap = Map.of("A", 1, "B", 2);
                                 // => immutableMap is {"A"=1, "B"=2} (cannot modify)
// immutableMap.put("C", 3);     // => UnsupportedOperationException (immutable)
```

**Key Takeaway**: Choose collection types based on access patterns: ArrayList for indexed access, LinkedList for frequent insertions/deletions, HashSet for uniqueness with O(1) operations, TreeSet for sorted uniqueness, HashMap for key-value O(1) lookups, TreeMap for sorted keys. Understand time complexity tradeoffs to avoid performance pitfalls.

**Why It Matters**: Choosing the wrong collection type causes severe performance issues at scale—using ArrayList.contains() in a loop creates O(n²) complexity vs. HashSet's O(n). A common pitfall: using LinkedList thinking "linked list is always better for insertions," but ArrayList's cache locality makes it faster for small-to-medium lists. TreeSet/TreeMap's O(log n) seems close to HashMap's O(1), but with millions of entries, the difference is dramatic (20 operations vs. 1). Java 9's List.of() and Map.of() factory methods create space-efficient immutable collections (no separate unmodifiable wrappers), essential for functional programming patterns. The Collections Framework standardized algorithms that previously required manual implementation, eliminating bugs from manual binary search or sort implementations.

---

## Example 39: Concurrent Collections for Thread Safety

Concurrent collections provide thread-safe operations without external synchronization. They use lock-free algorithms and fine-grained locking for better concurrency than synchronized collections.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Legacy["Legacy Collections<br/>ArrayList, HashMap"] --> Sync["Synchronized Wrappers<br/>Collections.synchronized*"]
    Legacy --> Concurrent["Concurrent Collections<br/>ConcurrentHashMap"]
    Sync --> Problem["Single Lock<br/>Poor Concurrency"]
    Concurrent --> Solution["Fine-Grained Locking<br/>High Concurrency"]

    style Legacy fill:#0173B2,color:#fff
    style Sync fill:#DE8F05,color:#fff
    style Concurrent fill:#029E73,color:#fff
    style Problem fill:#CC78BC,color:#fff
    style Solution fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;
import java.util.*;

// CONCURRENTHASHMAP - thread-safe without single lock
ConcurrentHashMap<String, Integer> concurrentMap = new ConcurrentHashMap<>();
                                 // => concurrentMap is {} (empty, thread-safe map)
                                 // => Uses segment-based locking (high concurrency)

// Thread-safe operations
concurrentMap.put("key", 1);     // => Thread-safe put without blocking all threads
                                 // => concurrentMap is {"key"=1}
Integer value = concurrentMap.get("key");
                                 // => Thread-safe get (lock-free read in Java 8+)
                                 // => value is 1

// ATOMIC OPERATIONS
concurrentMap.putIfAbsent("key", 2);
                                 // => Only puts if key doesn't exist (atomic check-and-set)
                                 // => Returns 1 (key exists, not inserted)
                                 // => concurrentMap is {"key"=1} (unchanged)

concurrentMap.computeIfAbsent("newKey", k -> k.length());
                                 // => Computes value atomically if key absent
                                 // => Lambda: "newKey".length() returns 6
                                 // => concurrentMap is {"key"=1, "newKey"=6}

concurrentMap.merge("key", 1, Integer::sum);
                                 // => Atomically updates: old value + new value
                                 // => Merges 1 (existing) + 1 (new) = 2
                                 // => concurrentMap is {"key"=2, "newKey"=6}

// COPYONWRITEARRAYLIST - reads without locking, writes copy entire array
CopyOnWriteArrayList<String> cowList = new CopyOnWriteArrayList<>();
                                 // => cowList is [] (empty, optimized for reads)
cowList.add("A");                // => Creates new array copy with "A"
                                 // => cowList is ["A"] (write expensive, read cheap)
cowList.add("B");                // => Every write copies array (O(n) write cost)
                                 // => cowList is ["A", "B"]

// Safe iteration during concurrent modifications
for (String item : cowList) {    // => Iteration uses snapshot (no ConcurrentModificationException)
                                 // => Iterates over: ["A", "B"]
    cowList.add("C");            // => Modifications don't affect ongoing iteration
                                 // => Creates new array but iterator sees old snapshot
}                                // => After loop: cowList is ["A", "B", "C", "C"]

// BLOCKINGQUEUE - producer-consumer pattern
BlockingQueue<String> queue = new LinkedBlockingQueue<>(10);
                                 // => queue capacity is 10 (blocks when full)
                                 // => Uses locks and conditions (thread coordination)

// Producer thread
new Thread(() -> {               // => Lambda creates producer thread
    try {
        queue.put("item");       // => Blocks if queue full (waits for space)
                                 // => Wakes up consumers waiting for items
    } catch (InterruptedException e) {
        e.printStackTrace();     // => Handle thread interruption
    }
}).start();                      // => Start producer thread immediately

// Consumer thread
new Thread(() -> {               // => Lambda creates consumer thread
    try {
        String item = queue.take(); // => Blocks if queue empty (waits for item)
                                 // => item is "item" (from producer)
        System.out.println("Consumed: " + item);
                                 // => Output: Consumed: item
    } catch (InterruptedException e) {
        e.printStackTrace();     // => Handle thread interruption
    }
}).start();                      // => Start consumer thread immediately

// CONCURRENTSKIPLISTMAP - sorted, concurrent alternative to TreeMap
ConcurrentSkipListMap<Integer, String> skipListMap = new ConcurrentSkipListMap<>();
                                 // => skipListMap is {} (empty skip list, sorted + thread-safe)
skipListMap.put(3, "three");     // => skipListMap is {3="three"}
skipListMap.put(1, "one");       // => skipListMap is {1="one", 3="three"} (auto-sorted)
skipListMap.put(2, "two");       // => skipListMap is {1="one", 2="two", 3="three"}
System.out.println(skipListMap);  // => Output: {1=one, 2=two, 3=three} (sorted, thread-safe)

// CONTRAST: Synchronized wrapper (poor concurrency)
Map<String, Integer> syncMap = Collections.synchronizedMap(new HashMap<>());
                                 // => syncMap is {} (wrapped HashMap)
                                 // => Single lock for all operations (severe bottleneck)
                                 // => All threads block each other (no concurrency)
```

**Key Takeaway**: Use ConcurrentHashMap for high-concurrency key-value access with fine-grained locking. CopyOnWriteArrayList suits read-heavy workloads where writes are rare. BlockingQueue enables producer-consumer patterns with thread-safe blocking operations. Avoid synchronized wrappers (Collections.synchronizedMap()) which use coarse-grained locking.

**Why It Matters**: ConcurrentHashMap revolutionized Java concurrency by replacing Hashtable's single-lock design with segment-based locking (Java 7) and later lock-free reads with CAS operations (Java 8+), achieving 10-100x throughput in multi-threaded scenarios. Before concurrent collections, developers used synchronized blocks or Collections.synchronizedMap(), creating severe bottlenecks—every operation acquired the same lock, serializing all threads. BlockingQueue enables elegant producer-consumer patterns without manual wait/notify complexity, powering thread pools (Executors use BlockingQueue internally) and message queues. CopyOnWriteArrayList's write-amplification (copying entire array) makes it unsuitable for write-heavy workloads, but perfect for listener lists in event systems where reads vastly outnumber writes.

---

## Example 40: Stream Pipeline Optimization

Stream operations are lazy (intermediate) or eager (terminal). Understanding laziness enables building efficient pipelines that short-circuit and minimize iterations.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Source["Collection<br/>[1,2,3,4,5]"] --> Stream["stream#40;#41;"]
    Stream --> Filter["filter<br/>#40;intermediate/lazy#41;"]
    Filter --> Map["map<br/>#40;intermediate/lazy#41;"]
    Map --> Terminal["collect<br/>#40;terminal/eager#41;"]
    Terminal --> Execute["Execute pipeline<br/>process elements"]

    Execute --> Result["Result: List"]

    style Source fill:#0173B2,color:#fff
    style Filter fill:#DE8F05,color:#fff
    style Map fill:#DE8F05,color:#fff
    style Terminal fill:#029E73,color:#fff
    style Execute fill:#CC78BC,color:#fff
    style Result fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.*;
import java.util.stream.*;

List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
                                 // => numbers is [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// LAZY EVALUATION - intermediate operations don't execute until terminal operation
Stream<Integer> stream = numbers.stream()
                                 // => Creates stream from list (no processing yet)
                                 // => Pipeline defined but not executed
    .filter(n -> {
        System.out.println("Filter: " + n);
        return n % 2 == 0;       // => NOT executed yet (lazy intermediate operation)
                                 // => Defines filtering logic, doesn't apply it
    })
    .map(n -> {
        System.out.println("Map: " + n);
        return n * 2;            // => NOT executed yet (lazy intermediate operation)
                                 // => Defines mapping logic, doesn't apply it
    });
// => No output yet (no terminal operation)
// => Stream pipeline defined but not executed
// => No System.out.println calls made

List<Integer> result = stream.collect(Collectors.toList());
                                 // => Terminal operation triggers pipeline execution
                                 // => Now executes filter and map for each element
// => Output: Filter: 1, Filter: 2, Map: 2, Filter: 3, Filter: 4, Map: 4, ...
// => result is [4, 8, 12, 16, 20] (even numbers doubled)

// SHORT-CIRCUITING - stops processing when result determined
Optional<Integer> first = numbers.stream()
                                 // => Creates new stream from numbers
    .filter(n -> {
        System.out.println("Checking: " + n);
        return n > 5;            // => Filter for n > 5
    })
    .findFirst();                // => Short-circuits after first match
                                 // => Stops immediately when match found
// => Output: Checking: 1, Checking: 2, ..., Checking: 6
// => Stops at 6 (first n > 5), doesn't process 7, 8, 9, 10
// => first is Optional[6]

// PARALLEL STREAMS - splits work across threads
long count = numbers.parallelStream()
                                 // => Splits collection across ForkJoinPool threads
    .filter(n -> n % 2 == 0)     // => Parallel filtering (each thread processes subset)
                                 // => Filters: 2, 4, 6, 8, 10
    .count();                    // => Terminal operation: counts matches
                                 // => Parallel execution (order not guaranteed)
                                 // => count is 5

// OPTIMIZATION: limit() short-circuits infinite streams
Stream.iterate(0, n -> n + 1)    // => Infinite stream: 0, 1, 2, 3, ... (unbounded)
                                 // => Without limit(), would run forever
    .filter(n -> n % 2 == 0)     // => Even numbers: 0, 2, 4, 6, 8, ...
    .limit(5)                    // => Take first 5: stops after 5 elements
                                 // => Short-circuits infinite stream
    .forEach(System.out::println);
                                 // => Terminal operation: prints each element
// => Output: 0, 2, 4, 6, 8 (stops, doesn't run forever)

// PRIMITIVE STREAMS - avoid autoboxing overhead
IntStream.range(1, 1000000)      // => Primitive int stream (no Integer objects)
                                 // => range is [1, 2, ..., 999999]
    .filter(n -> n % 2 == 0)     // => Filters even numbers (no boxing)
    .sum();                      // => sum() on IntStream (no boxing/unboxing)
                                 // => Returns int sum directly (efficient)

// STATELESS vs STATEFUL operations
numbers.stream()                 // => Creates stream from numbers
    .filter(n -> n > 5)          // => Stateless: each element processed independently
                                 // => Filters: 6, 7, 8, 9, 10
    .map(n -> n * 2)             // => Stateless: independent transformation
                                 // => Maps to: 12, 14, 16, 18, 20
    .sorted()                    // => STATEFUL: requires all elements (breaks streaming)
                                 // => Collects all before sorting (memory overhead)
    .collect(Collectors.toList());
                                 // => Terminal: collects to List
                                 // => Result is [12, 14, 16, 18, 20] (sorted)
```

**Key Takeaway**: Intermediate operations (filter, map) are lazy—they don't execute until a terminal operation (collect, forEach, count) triggers the pipeline. Short-circuiting operations (findFirst, limit, anyMatch) stop processing early. Use primitive streams (IntStream, LongStream, DoubleStream) to avoid autoboxing overhead for numeric operations.

**Why It Matters**: Lazy evaluation enables efficient stream pipelines—filter then map then findFirst only processes elements until the first match, potentially touching 1% of a million-element collection vs. processing all elements eagerly. Before streams, achieving this required manual short-circuit logic in loops. However, stateful operations (sorted, distinct) break streaming by requiring all elements in memory, negating laziness benefits—sorting a billion-element stream still loads everything. Parallel streams promise automatic parallelization but have overhead: Collections.parallelStream() splits work across ForkJoinPool threads, beneficial only for CPU-intensive operations on large collections (>10,000 elements). Primitive streams eliminate autoboxing that creates millions of temporary Integer/Double objects, critical for numeric computations but often forgotten.

---

## Example 41: Collectors and Stream Reduction

Collectors transform stream results into collections, maps, or aggregated values. Custom collectors enable complex reductions beyond built-in options.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Stream["Stream<String>"] --> Collect["collect#40;Collector#41;"]
    Collect --> ToList["toList#40;#41;<br/>→ List"]
    Collect --> GroupBy["groupingBy#40;#41;<br/>→ Map"]
    Collect --> Joining["joining#40;#41;<br/>→ String"]
    Collect --> Custom["Custom Collector<br/>→ Any type"]

    style Stream fill:#0173B2,color:#fff
    style Collect fill:#DE8F05,color:#fff
    style ToList fill:#029E73,color:#fff
    style GroupBy fill:#029E73,color:#fff
    style Joining fill:#029E73,color:#fff
    style Custom fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.*;
import java.util.stream.*;

List<String> words = Arrays.asList("apple", "banana", "apricot", "blueberry", "avocado");

// COLLECTING TO COLLECTIONS
List<String> list = words.stream()
                                 // => Creates stream from words list
    .filter(w -> w.startsWith("a"))
                                 // => Keeps: apple, apricot, avocado
    .collect(Collectors.toList());
                                 // => Collects to ArrayList
                                 // => list is [apple, apricot, avocado]

Set<String> set = words.stream()
    .collect(Collectors.toSet());
                                 // => Collects to HashSet (unordered)

// JOINING STRINGS
String joined = words.stream()
    .collect(Collectors.joining(", "));
                                 // => joined is "apple, banana, apricot, blueberry, avocado"

String prefixed = words.stream()
    .collect(Collectors.joining(", ", "[", "]"));
                                 // => joining(delimiter, prefix, suffix)
                                 // => prefixed is "[apple, banana, apricot, blueberry, avocado]"

// GROUPING BY
Map<Character, List<String>> grouped = words.stream()
    .collect(Collectors.groupingBy(w -> w.charAt(0)));
                                 // => Groups by first character
                                 // => {a=[apple, apricot, avocado], b=[banana, blueberry]}

// COUNTING
Map<Character, Long> counts = words.stream()
    .collect(Collectors.groupingBy(w -> w.charAt(0), Collectors.counting()));
                                 // => Groups by first char, then counts each group
                                 // => {a=3, b=2}

// PARTITIONING (boolean predicate)
Map<Boolean, List<String>> partitioned = words.stream()
    .collect(Collectors.partitioningBy(w -> w.length() > 6));
                                 // => Splits into true/false groups by length
                                 // => {false=[apple, banana], true=[apricot, blueberry, avocado]}

// MAPPING WITHIN GROUPING
Map<Character, List<Integer>> lengths = words.stream()
    .collect(Collectors.groupingBy(
        w -> w.charAt(0),        // => Groups by first character
        Collectors.mapping(String::length, Collectors.toList())
                                 // => Transforms to word lengths
    ));
                                 // => {a=[5, 7, 7], b=[6, 9]}

// CUSTOM COLLECTOR - joining with custom logic
String custom = words.stream()
    .collect(Collector.of(
                                 // => Creates custom collector (4 components)
        StringBuilder::new,      // => Supplier: creates StringBuilder container
        (sb, s) -> sb.append(s).append(" "),
                                 // => Accumulator: appends word + space
        (sb1, sb2) -> sb1.append(sb2),
                                 // => Combiner: merges parallel results
        StringBuilder::toString  // => Finisher: final transformation
                                 // => Function<StringBuilder, String>
                                 // => Converts mutable StringBuilder to immutable String
    ));
                                 // => custom is "apple banana apricot blueberry avocado "
                                 // => Note: trailing space from append logic

// REDUCING
int totalLength = words.stream()
                                 // => Stream<String> from words
    .collect(Collectors.summingInt(String::length));
                                 // => summingInt() converts to int and sums
                                 // => String::length method reference
                                 // => apple(5) + banana(6) + apricot(7) + blueberry(9) + avocado(7)
                                 // => totalLength is 34 (sum of all word lengths)

Optional<String> longest = words.stream()
                                 // => Stream<String> from words
    .collect(Collectors.maxBy(Comparator.comparing(String::length)));
                                 // => maxBy() finds maximum by comparator
                                 // => Comparator.comparing(String::length) compares by length
                                 // => Finds word with maximum length
                                 // => blueberry has 9 characters (longest)
                                 // => longest is Optional[blueberry]
                                 // => Optional in case stream was empty

// TEEING (Java 12+) - apply two collectors and merge results
Map<String, Object> stats = words.stream()
                                 // => Stream<String> from words
    .collect(Collectors.teeing(
                                 // => teeing() applies 2 collectors, merges results
                                 // => Solves "one stream, two collectors" problem
        Collectors.counting(),   // => First collector: counts elements
                                 // => Result: 5L (Long)
        Collectors.joining(","), // => Second collector: joins with comma
                                 // => Result: "apple,banana,apricot,blueberry,avocado"
        (count, joined) -> Map.of("count", count, "words", joined)
                                 // => Merger: BiFunction<Long, String, Map>
                                 // => Combines both collector results into single Map
                                 // => Creates Map with 2 entries
    ));
                                 // => {count=5, words=apple,banana,apricot,blueberry,avocado}
                                 // => Single pass through stream produces both results
```

**Key Takeaway**: Use built-in Collectors for common operations: toList(), toSet(), groupingBy(), partitioningBy(), joining(). Compose collectors with mapping(), counting(), summingInt() for complex aggregations. Create custom collectors via Collector.of() for specialized reduction logic.

**Why It Matters**: Collectors enable declarative data transformations replacing imperative loops—groupingBy() replaces manual Map population with iteration, joining() replaces StringBuilder loops. The composition pattern (groupingBy + counting) creates multi-level aggregations in single expressions, impossible in traditional loops without nested logic. Custom collectors extend the system: statistical frameworks collect custom metrics, test libraries collect assertion results, and data pipelines collect into custom data structures. However, complex collector chains become unreadable—groupingBy(x, mapping(y, filtering(z, ...))) creates nested generics and confusing semantics. The teeing() collector (Java 12) elegantly solves the "one stream, multiple collectors" problem that previously required collect() duplication or stream re-creation.

---

## Example 42: Method References and Function Composition

Method references provide shorthand for lambdas that delegate to existing methods. Composing functions creates reusable transformation pipelines.

**Code**:

```java
import java.util.*;
import java.util.function.*;

// METHOD REFERENCE TYPES

// 1. STATIC METHOD REFERENCE
Function<String, Integer> parser1 = Integer::parseInt;
                                 // => Equivalent to: s -> Integer.parseInt(s)
int value = parser1.apply("123"); // => value is 123

// 2. INSTANCE METHOD REFERENCE (on particular object)
String prefix = "Hello, ";
Function<String, String> greeter = prefix::concat;
                                 // => Equivalent to: s -> prefix.concat(s)
String greeting = greeter.apply("World");
                                 // => greeting is "Hello, World"

// 3. INSTANCE METHOD REFERENCE (on arbitrary object)
Function<String, Integer> lengthGetter = String::length;
                                 // => Equivalent to: s -> s.length()
                                 // => Calls length() on parameter (not specific object)
                                 // => Type: Function<String, Integer>
int length = lengthGetter.apply("test");
                                 // => Applies function to "test" string
                                 // => "test".length() returns 4
                                 // => length is 4

// 4. CONSTRUCTOR REFERENCE
Supplier<List<String>> listMaker = ArrayList::new;
                                 // => Equivalent to: () -> new ArrayList<>()
                                 // => References no-arg constructor
                                 // => Type: Supplier<List<String>>
List<String> list = listMaker.get();
                                 // => Calls ArrayList::new (creates new ArrayList)
                                 // => list is empty ArrayList<String>

Function<String, Person> personMaker = Person::new;
                                 // => References Person constructor taking String
                                 // => Equivalent to: name -> new Person(name)
                                 // => Calls Person(String name) constructor

// FUNCTION COMPOSITION - chaining transformations
Function<String, String> trim = String::trim;
                                 // => Function: removes leading/trailing whitespace
Function<String, String> upper = String::toUpperCase;
                                 // => Function: converts to uppercase
Function<String, Integer> length = String::length;
                                 // => Function: returns string length

Function<String, Integer> pipeline = trim.andThen(upper).andThen(length);
                                 // => andThen() chains functions left-to-right
                                 // => trim first, then upper, then length
                                 // => Type: Function<String, Integer>
                                 // => Execution order: trim → upper → length
int result = pipeline.apply("  hello  ");
                                 // => Step 1: trim("  hello  ") → "hello"
                                 // => Step 2: upper("hello") → "HELLO"
                                 // => Step 3: length("HELLO") → 5
                                 // => result is 5

Function<String, Integer> composed = length.compose(upper).compose(trim);
                                 // => compose() chains right-to-left (reverse order)
                                 // => trim first, then upper, then length
                                 // => Same result as andThen, different syntax

// PREDICATE COMPOSITION - combining conditions
Predicate<String> startsWithA = s -> s.startsWith("a");
                                 // => Predicate: tests if string starts with 'a'
Predicate<String> longerThan5 = s -> s.length() > 5;
                                 // => Predicate: tests if string length > 5

Predicate<String> combined = startsWithA.and(longerThan5);
                                 // => and() creates logical AND predicate
                                 // => Both conditions must be true
                                 // => Type: Predicate<String>
boolean test1 = combined.test("apple");
                                 // => "apple" starts with 'a': true
                                 // => "apple".length() is 5, not > 5: false
                                 // => AND result: false
boolean test2 = combined.test("apricot");
                                 // => "apricot" starts with 'a': true
                                 // => "apricot".length() is 7 > 5: true
                                 // => AND result: true

Predicate<String> either = startsWithA.or(longerThan5);
                                 // => or() creates logical OR predicate
                                 // => At least one condition must be true
                                 // => Type: Predicate<String>

Predicate<String> negated = startsWithA.negate();
                                 // => negate() inverts predicate result
                                 // => Returns true if string does NOT start with 'a'

// PRACTICAL EXAMPLE - reusable transformations
List<String> inputs = Arrays.asList("  apple  ", "  BANANA  ", "  cherry  ");
                                 // => Input list with whitespace and mixed case

List<String> processed = inputs.stream()
                                 // => Creates Stream<String> from list
    .map(String::trim)           // => Method reference: removes whitespace
                                 // => "  apple  " → "apple"
                                 // => "  BANANA  " → "BANANA"
                                 // => "  cherry  " → "cherry"
    .map(String::toLowerCase)    // => Method reference: converts to lowercase
                                 // => "apple" → "apple" (no change)
                                 // => "BANANA" → "banana"
                                 // => "cherry" → "cherry" (no change)
    .filter(s -> s.length() > 5)
                                 // => Lambda predicate: keeps strings with length > 5
                                 // => "apple" (5 chars): false (filtered out)
                                 // => "banana" (6 chars): true (kept)
                                 // => "cherry" (6 chars): true (kept)
    .collect(Collectors.toList());
                                 // => Collects remaining elements to List
                                 // => processed is ["banana", "cherry"]
```

**Key Takeaway**: Method references (`Class::method`) provide concise alternatives to lambdas that just call a method. Use `andThen()` for left-to-right function composition and `compose()` for right-to-left. Compose predicates with `and()`, `or()`, and `negate()` to build complex conditions from simple ones.

**Why It Matters**: Method references eliminate lambda boilerplate for common cases—`list.forEach(System.out::println)` is clearer than `list.forEach(s -> System.out.println(s))`. Function composition enables point-free style (defining transformations without intermediate variables), common in functional programming but rare in Java. However, excessive composition creates unreadable pipelines: `f.andThen(g).andThen(h).compose(i).compose(j)` obscures logic. The real power emerges in frameworks: Spring's @Transactional uses method references for AOP pointcuts, testing frameworks use them for method handles, and reactive libraries (Reactor, RxJava) chain transformations via function composition. Constructor references (`ArrayList::new`) enable supplier patterns and factory methods without lambda wrappers.

---

## Example 43: NIO.2 File Operations and Path API

NIO.2 (java.nio.file) provides modern file I/O with Path abstraction, symbolic link support, and directory traversal. It replaces legacy java.io.File with clearer semantics.

**Code**:

```java
import java.nio.file.*;
import java.io.IOException;
import java.util.stream.Stream;

// PATH OPERATIONS - modern file path abstraction
Path path = Paths.get("data", "file.txt");
                                 // => Factory method creates Path object
                                 // => Joins path components with system separator
                                 // => Creates path to data/file.txt
                                 // => Type: Path (interface, implementation varies by OS)
Path absolute = path.toAbsolutePath();
                                 // => Converts relative path to absolute
                                 // => Prepends current working directory
                                 // => Example: /home/user/project/data/file.txt
Path parent = path.getParent();
                                 // => Extracts parent directory path
                                 // => parent is "data"
Path filename = path.getFileName();
                                 // => Extracts file name component
                                 // => filename is "file.txt"

// FILE OPERATIONS
try {
    // Create file
    Files.createFile(path);      // => Creates empty file at path
                                 // => Throws FileAlreadyExistsException if file exists
                                 // => Atomic operation (guaranteed by OS)

    // Write content
    String content = "Hello, NIO.2!";
                                 // => Content string to write
    Files.writeString(path, content);
                                 // => Writes string to file using UTF-8
                                 // => Overwrites existing content completely
                                 // => Convenient for small text files

    // Read content
    String read = Files.readString(path);
                                 // => Reads entire file into String
                                 // => Uses UTF-8 encoding by default
                                 // => read is "Hello, NIO.2!"
                                 // => Suitable for small files (loads all into memory)

    // Append content
    Files.writeString(path, "\nNew line", StandardOpenOption.APPEND);
                                 // => StandardOpenOption.APPEND: adds to end
                                 // => "\n" creates new line before text
                                 // => Preserves existing content
                                 // => File now contains 2 lines

    // Read all lines
    List<String> lines = Files.readAllLines(path);
                                 // => Reads file, splits by line breaks
                                 // => Returns List<String> with each line
                                 // => lines is ["Hello, NIO.2!", "New line"]
                                 // => Entire file loaded into memory

    // STREAMING LINES (for large files)
    try (Stream<String> stream = Files.lines(path)) {
                                 // => Creates Stream<String> lazily reading lines
                                 // => Efficient for large files (doesn't load all into memory)
                                 // => try-with-resources ensures stream closed
        stream.filter(line -> line.startsWith("Hello"))
                                 // => Filters lines starting with "Hello"
                                 // => Keeps only: "Hello, NIO.2!"
              .forEach(System.out::println);
                                 // => Prints each matching line
                                 // => Output: Hello, NIO.2!
    }                            // => Stream auto-closed here

    // FILE METADATA
    boolean exists = Files.exists(path);
                                 // => Checks if file exists on filesystem
                                 // => exists is true (we just created it)
    boolean isReadable = Files.isReadable(path);
                                 // => Checks read permission
                                 // => isReadable likely true (depends on permissions)
    boolean isDirectory = Files.isDirectory(path);
                                 // => Tests if path is directory
                                 // => isDirectory is false (it's a file)
    long size = Files.size(path);
                                 // => Returns file size in bytes
                                 // => size depends on content written

    // COPY and MOVE
    Path backup = Paths.get("data", "backup.txt");
                                 // => Target path for copy operation
    Files.copy(path, backup, StandardCopyOption.REPLACE_EXISTING);
                                 // => Copies file to backup location
                                 // => REPLACE_EXISTING: overwrites if backup.txt exists
                                 // => Creates new file with same content

    Path moved = Paths.get("data", "moved.txt");
                                 // => Target path for move operation
    Files.move(backup, moved, StandardCopyOption.ATOMIC_MOVE);
                                 // => Moves backup.txt to moved.txt
                                 // => ATOMIC_MOVE: guaranteed atomic on same filesystem
                                 // => backup.txt deleted, moved.txt created
                                 // => Original path (backup) no longer exists

    // DELETE
    Files.delete(moved);         // => Deletes moved.txt file
                                 // => Throws NoSuchFileException if file doesn't exist
                                 // => Throws DirectoryNotEmptyException if directory with contents
    Files.deleteIfExists(path);  // => Deletes file.txt if it exists
                                 // => Returns boolean: true if deleted, false if didn't exist
                                 // => No exception if file not found

} catch (IOException e) {
    e.printStackTrace();
}

// DIRECTORY OPERATIONS
try {
    Path dir = Paths.get("mydir");
                                 // => Creates Path to directory "mydir"
    Files.createDirectory(dir);  // => Creates single directory
                                 // => Throws if parent doesn't exist
                                 // => Throws if directory already exists

    Path nested = Paths.get("my/nested/dir");
                                 // => Path to deeply nested directory
    Files.createDirectories(nested);
                                 // => Creates all parent directories if needed
                                 // => Creates "my", "my/nested", "my/nested/dir"
                                 // => No error if directory already exists

    // LIST DIRECTORY CONTENTS
    try (Stream<Path> paths = Files.list(dir)) {
                                 // => Returns Stream<Path> of direct children
                                 // => NOT recursive (only immediate children)
                                 // => try-with-resources closes stream
        paths.forEach(System.out::println);
                                 // => Prints each child path
                                 // => Output: one line per file/directory in mydir
    }

    // WALK DIRECTORY TREE (recursive)
    try (Stream<Path> paths = Files.walk(nested)) {
                                 // => Returns Stream<Path> recursively traversing tree
                                 // => Includes all descendants (files and directories)
                                 // => Depth-first traversal
        paths.filter(Files::isRegularFile)
                                 // => Filters to only regular files (not directories)
                                 // => Method reference to Files.isRegularFile(Path)
             .forEach(System.out::println);
                                 // => Prints path of each file in tree
                                 // => Output: all files in nested and subdirectories
    }

} catch (IOException e) {
                                 // => Handles I/O exceptions from directory operations
    e.printStackTrace();
}
```

**Key Takeaway**: Use NIO.2's Path and Files classes for modern file I/O. Files.readString() and Files.writeString() handle simple text operations. Use Files.lines() to stream large files efficiently. Files.walk() recursively traverses directories, returning Stream<Path> for functional processing.

**Why It Matters**: NIO.2 fixed java.io.File's numerous flaws: ambiguous error handling (boolean returns vs. exceptions), poor symbolic link support, and platform-dependent behavior. The Path interface abstracts filesystem differences, enabling identical code for local files, network shares, and ZIP filesystem providers. Files.lines() streams enable processing gigabyte files without loading into memory—critical for log analysis and data processing. The switch from blocking I/O to asynchronous I/O (AsynchronousFileChannel) enables high-throughput servers, though most applications stick with simpler synchronous Files methods. Modern Java's Files API is so superior that java.io.File is essentially deprecated in new code, though legacy APIs still require it.

---

## Example 44: JSON Processing with Jackson

Jackson is Java's de facto JSON library, providing object mapping, streaming, and tree model APIs. It enables serialization/deserialization between Java objects and JSON.

**Code**:

```java
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.annotation.*;
import java.util.*;

// POJO for JSON mapping
class Person {
    private String name;
    private int age;

    @JsonProperty("email_address")  // => Maps to different JSON field name
    private String email;

    @JsonIgnore                  // => Excludes from JSON serialization
    private String password;

    // Constructors, getters, setters...
    public Person() {}

    public Person(String name, int age, String email) {
        this.name = name;
        this.age = age;
        this.email = email;
    }

    // Getters/setters omitted for brevity
}

// OBJECT MAPPER - main Jackson entry point
ObjectMapper mapper = new ObjectMapper();
                                 // => Central Jackson component for conversions
                                 // => Handles serialization and deserialization
                                 // => Reusable (thread-safe after configuration)

// SERIALIZE (Java object → JSON)
Person person = new Person("Alice", 30, "alice@example.com");
                                 // => Creates Person object with 3 fields
String json = mapper.writeValueAsString(person);
                                 // => Serializes object to JSON string
                                 // => Uses reflection to read fields
                                 // => Calls getters for field values
                                 // => json is {"name":"Alice","age":30,"email_address":"alice@example.com"}
                                 // => Note: "email_address" from @JsonProperty annotation

// DESERIALIZE (JSON → Java object)
String jsonInput = "{\"name\":\"Bob\",\"age\":25,\"email_address\":\"bob@example.com\"}";
                                 // => JSON string with 3 fields
Person deserializedPerson = mapper.readValue(jsonInput, Person.class);
                                 // => Parses JSON to Person object
                                 // => Calls default constructor Person()
                                 // => Uses setters to populate fields
                                 // => Maps "email_address" to email field
                                 // => deserializedPerson.name is "Bob"
                                 // => deserializedPerson.age is 25
                                 // => deserializedPerson.email is "bob@example.com"

// WORKING WITH COLLECTIONS
List<Person> people = Arrays.asList(
                                 // => Creates list of 2 Person objects
    new Person("Alice", 30, "alice@example.com"),
    new Person("Bob", 25, "bob@example.com")
);

String jsonArray = mapper.writeValueAsString(people);
                                 // => Serializes entire list to JSON array
                                 // => Each Person becomes JSON object
                                 // => jsonArray is [{"name":"Alice",...},{"name":"Bob",...}]
                                 // => Square brackets indicate JSON array

// DESERIALIZE TO LIST
List<Person> deserializedList = mapper.readValue(
    jsonArray,               // => JSON array string to parse
    mapper.getTypeFactory().constructCollectionType(List.class, Person.class)
                                 // => TypeFactory handles generic type erasure
                                 // => Specifies: List<Person> (not just List)
                                 // => Needed because generics erased at runtime
);
                                 // => deserializedList contains 2 Person objects
                                 // => Fully populated from JSON

// TREE MODEL - for dynamic JSON
JsonNode root = mapper.readTree(jsonInput);
                                 // => Parses JSON to tree structure
                                 // => JsonNode is abstract representation
                                 // => No POJO class required
String name = root.get("name").asText();
                                 // => Navigates to "name" field
                                 // => Extracts as text/String
                                 // => name is "Bob"
int age = root.get("age").asInt();
                                 // => Navigates to "age" field
                                 // => Extracts as integer
                                 // => age is 25

// CREATE JSON TREE
ObjectNode node = mapper.createObjectNode();
                                 // => Creates new JSON object node
                                 // => Mutable tree structure
node.put("name", "Charlie");     // => Adds string field
                                 // => Key: "name", value: "Charlie"
node.put("age", 35);             // => Adds integer field
                                 // => Key: "age", value: 35
String createdJson = mapper.writeValueAsString(node);
                                 // => Serializes tree to JSON string
                                 // => createdJson is {"name":"Charlie","age":35}
```

**Key Takeaway**: Use ObjectMapper for JSON serialization (writeValueAsString) and deserialization (readValue). Annotate POJOs with @JsonProperty for field mapping and @JsonIgnore to exclude fields. Use JsonNode tree model for dynamic JSON without predefined Java classes.

**Why It Matters**: JSON processing powers REST APIs, configuration files, and data interchange in modern Java applications. Jackson's annotation-based mapping eliminates manual JSON parsing that plagued early Java (manual JSONObject.getString() calls). The library's performance (faster than Gson through bytecode generation) makes it standard in Spring Boot, JAX-RS, and most Java REST frameworks. However, Jackson's reflection-based approach has limitations: it requires default constructors and getters/setters (violating immutability), and deserialization can instantiate arbitrary classes (security risk). Modern alternatives like kotlinx.serialization use compile-time code generation, but Jackson's ecosystem and Spring integration keep it dominant.

---

## Example 45: JUnit 5 Testing Fundamentals

JUnit 5 provides annotations, assertions, and lifecycle methods for unit testing. It enables test-driven development and ensures code correctness through automated verification.

**Code**:

```java
import org.junit.jupiter.api.*;  // => JUnit 5 core annotations (@Test, @BeforeEach, etc.)
import static org.junit.jupiter.api.Assertions.*;
                                 // => Static import for assertion methods (assertEquals, assertTrue, etc.)
import java.util.*;              // => Java utilities (not used here, common in tests)

class Calculator {               // => Class under test (production code)
    public int add(int a, int b) {
                                 // => Method to test: addition operation
        return a + b;            // => Returns sum of two integers
    }

    public int divide(int a, int b) {
                                 // => Method to test: division operation with validation
        if (b == 0) throw new ArithmeticException("Division by zero");
                                 // => Guards against division by zero
                                 // => Throws ArithmeticException with message
        return a / b;            // => Returns quotient if divisor non-zero
    }
}

// TEST CLASS
class CalculatorTest {          // => Test class following JUnit 5 naming convention (*Test)
    private Calculator calculator;
                                 // => Instance field holding object under test
                                 // => Recreated before each test via @BeforeEach

    // LIFECYCLE METHODS
    @BeforeAll                   // => Runs once before all tests (must be static)
                                 // => Use for expensive one-time setup (DB connections, etc.)
    static void initAll() {      // => Must be static (runs before any instance created)
        System.out.println("Initializing test suite");
                                 // => Output: Initializing test suite (once at start)
    }

    @BeforeEach                  // => Runs before each test
                                 // => Use to reset test state (fresh object per test)
    void init() {                // => Instance method (new instance per test)
        calculator = new Calculator();
                                 // => Creates fresh Calculator for each test
                                 // => Ensures test isolation (no shared state)
    }

    @AfterEach                   // => Runs after each test
                                 // => Use for cleanup (close files, release resources)
    void tearDown() {            // => Instance method (runs after each test)
        calculator = null;       // => Clears reference (helps garbage collection)
                                 // => Not strictly needed here (automatic cleanup)
    }

    @AfterAll                    // => Runs once after all tests
                                 // => Use for expensive cleanup (close DB, shutdown servers)
    static void tearDownAll() {  // => Must be static (runs after all instances destroyed)
        System.out.println("Test suite complete");
                                 // => Output: Test suite complete (once at end)
    }

    // BASIC TEST
    @Test                        // => Marks method as test
                                 // => JUnit discovers and runs methods with @Test annotation
    void testAddition() {        // => Test method name should describe what's being tested
        int result = calculator.add(2, 3);
                                 // => Calls add with arguments 2, 3
                                 // => result is 5
        assertEquals(5, result);  // => Assertion: expected vs actual
                                 // => Test passes if result == 5, fails otherwise
    }

    // MULTIPLE ASSERTIONS
    @Test
    void testMultipleAssertions() {
                                 // => Tests multiple scenarios in one test method
        assertAll(               // => Groups assertions (all executed even if one fails)
                                 // => Without assertAll, first failure stops execution
            () -> assertEquals(5, calculator.add(2, 3)),
                                 // => Lambda assertion: 2 + 3 should equal 5
            () -> assertEquals(0, calculator.add(-2, 2)),
                                 // => Lambda assertion: -2 + 2 should equal 0
            () -> assertTrue(calculator.add(1, 1) > 0)
                                 // => Lambda assertion: 1 + 1 should be positive
        );                       // => All three assertions run, failure report shows all issues
    }

    // EXCEPTION TESTING
    @Test
    void testDivisionByZero() {  // => Tests that exception is thrown correctly
        Exception exception = assertThrows(
                                 // => Captures thrown exception for further assertions
            ArithmeticException.class,
                                 // => Expected exception type
            () -> calculator.divide(10, 0)
                                 // => Lambda that should throw exception
        );                       // => Asserts exception is thrown
                                 // => Test fails if no exception or wrong type thrown
        assertEquals("Division by zero", exception.getMessage());
                                 // => Verifies exception message is correct
                                 // => Ensures error messages are user-friendly
    }

    // TIMEOUT TESTING
    @Test
    @Timeout(1)                  // => Test must complete within 1 second
                                 // => Fails if test takes longer (prevents hanging tests)
    void testPerformance() {     // => Tests that method completes quickly
        calculator.add(1, 1);    // => Simple addition should be instant
                                 // => Test fails if takes more than 1 second
    }

    // DISABLED TEST
    @Disabled("Not implemented yet")
                                 // => Temporarily disables test (not run during test suite)
                                 // => Use for incomplete tests or known failures
    @Test
    void testNotReady() {        // => Test method that's disabled
        // Skipped during test run
                                 // => JUnit shows this as "skipped" in results
                                 // => Reason appears in test report
    }

    // PARAMETERIZED TEST
    @ParameterizedTest           // => Runs same test with different inputs
                                 // => More concise than writing multiple @Test methods
    @ValueSource(ints = {1, 2, 3, 4, 5})
                                 // => Provides input values (test runs 5 times)
                                 // => Each int becomes parameter to test method
    void testMultipleInputs(int number) {
                                 // => number takes values 1, 2, 3, 4, 5 across 5 runs
        assertTrue(calculator.add(number, 1) > number);
                                 // => Asserts: number + 1 > number (should always be true)
                                 // => Runs 5 assertions: (1+1>1), (2+1>2), (3+1>3), (4+1>4), (5+1>5)
    }

    // DISPLAY NAME
    @DisplayName("Test division with valid inputs")
                                 // => Custom display name for test reports
                                 // => More readable than method name in test output
    @Test
    void testDivision() {        // => Actual method name (less important with @DisplayName)
        assertEquals(2, calculator.divide(10, 5));
                                 // => Asserts: 10 / 5 == 2
                                 // => Test report shows: "Test division with valid inputs ✓"
    }
}
```

**Key Takeaway**: Use @Test to mark test methods, assertions (assertEquals, assertTrue, assertThrows) to verify behavior, and lifecycle annotations (@BeforeEach, @AfterEach) to set up and tear down test fixtures. Use @ParameterizedTest for testing multiple inputs with one test method.

**Why It Matters**: JUnit revolutionized Java development by enabling automated testing that catches regressions before deployment. Before JUnit, testing required manual execution and verification—error-prone and time-consuming. The @Test annotation transformed testing from comment-marked methods to discoverable, runnable tests via reflection. JUnit 5's assertAll() solves the "first failure stops execution" problem of traditional assertions, reporting all failures in one run. However, unit tests have limitations: they verify logic in isolation but miss integration issues (database, network, external APIs). The "test pyramid" recommends many unit tests, fewer integration tests, and minimal UI tests—but achieving this requires disciplined test design and mocking dependencies (enter Mockito).

---

## Example 46: Mockito for Dependency Mocking

Mockito creates mock objects for testing, isolating units from dependencies. It enables testing code that depends on databases, external APIs, or complex objects without actual implementations.

**Code**:

```java
import org.junit.jupiter.api.Test;
                                 // => JUnit 5 test annotation
import org.mockito.*;            // => Mockito core classes (Mock, InjectMocks, etc.)
import static org.mockito.Mockito.*;
                                 // => Static import for Mockito methods (when, verify, etc.)
import static org.junit.jupiter.api.Assertions.*;
                                 // => Static import for assertion methods
import java.util.*;              // => Java utilities

// DEPENDENCIES TO MOCK
interface UserRepository {       // => External dependency that will be mocked
                                 // => Interface makes mocking easier (no concrete class needed)
    User findById(String id);    // => Method that reads from data source
                                 // => In real code, would query database
    void save(User user);        // => Method that writes to data source
                                 // => In real code, would persist to database
}

class User {                     // => Domain object (data class)
    private String id;           // => User identifier
    private String name;         // => User name

    public User(String id, String name) {
                                 // => Constructor for creating User objects
        this.id = id;            // => Sets user ID
        this.name = name;        // => Sets user name
    }

    // Getters/setters...
                                 // => getName() needed for UserService logic
}

// SERVICE CLASS UNDER TEST
class UserService {              // => Business logic class we want to test
                                 // => Depends on UserRepository (will be mocked)
    private final UserRepository repository;
                                 // => Dependency injected via constructor
                                 // => Final ensures immutability

    public UserService(UserRepository repository) {
                                 // => Constructor injection (testable design)
        this.repository = repository;
                                 // => Stores repository reference
    }

    public String getUserName(String id) {
                                 // => Business method to test
        User user = repository.findById(id);
                                 // => Calls repository (will call mock in tests)
        return user != null ? user.getName() : "Unknown";
                                 // => Returns name if user found, "Unknown" if null
    }

    public void updateUser(User user) {
                                 // => Business method for updating users
        repository.save(user);   // => Delegates to repository (will call mock)
                                 // => No return value (void method)
    }
}

// TESTS WITH MOCKS
class UserServiceTest {          // => Test class using Mockito mocks
    @Mock                        // => Mockito creates mock implementation
                                 // => mockRepository doesn't need real implementation
    private UserRepository mockRepository;
                                 // => Mock object (all methods return null by default)

    @InjectMocks                 // => Mockito injects mocks into this object
                                 // => userService will be created with mockRepository injected
    private UserService userService;
                                 // => Object under test (uses mocked dependencies)

    @BeforeEach                  // => Runs before each test method
    void setUp() {               // => Initializes Mockito annotations
        MockitoAnnotations.openMocks(this);
                                 // => Initialize mocks
                                 // => Creates mock for @Mock fields and injects into @InjectMocks
    }

    @Test
    void testGetUserName() {     // => Tests happy path: user exists
        // STUBBING - define mock behavior
        User mockUser = new User("123", "Alice");
                                 // => Creates test User object
                                 // => mockUser has id="123", name="Alice"
        when(mockRepository.findById("123")).thenReturn(mockUser);
                                 // => When findById("123") called, return mockUser
                                 // => Stubbing: defines what mock should do
                                 // => mockRepository is programmed to return mockUser

        // EXECUTE
        String name = userService.getUserName("123");
                                 // => Calls method under test
                                 // => Internally calls mockRepository.findById("123")
                                 // => name is "Alice" (from mockUser)

        // VERIFY
        assertEquals("Alice", name);
                                 // => Asserts returned name is correct
        verify(mockRepository, times(1)).findById("123");
                                 // => Verify findById was called exactly once
                                 // => Ensures service uses repository correctly
    }

    @Test
    void testGetUserNameNotFound() {
                                 // => Tests edge case: user not found
        when(mockRepository.findById("999")).thenReturn(null);
                                 // => Stub to return null (user doesn't exist)
                                 // => mockRepository.findById("999") returns null

        String name = userService.getUserName("999");
                                 // => Calls method with non-existent ID
                                 // => name should be "Unknown"

        assertEquals("Unknown", name);
                                 // => Asserts fallback value returned
                                 // => Verifies null-safety logic works
    }

    @Test
    void testUpdateUser() {      // => Tests void method using verification
        User user = new User("123", "Bob");
                                 // => Creates test user to save
                                 // => user has id="123", name="Bob"

        userService.updateUser(user);
                                 // => Calls update method
                                 // => Internally calls mockRepository.save(user)

        // VERIFY method called with specific argument
        verify(mockRepository).save(user);
                                 // => Verify save was called with user
                                 // => times(1) is implicit default
                                 // => Ensures service delegates to repository correctly
    }

    @Test
    void testExceptionHandling() {
                                 // => Tests that service propagates exceptions
        // STUB TO THROW EXCEPTION
        when(mockRepository.findById(anyString()))
                                 // => anyString() matches any String argument
            .thenThrow(new RuntimeException("Database error"));
                                 // => Stub throws exception when called
                                 // => Simulates database failure

        assertThrows(RuntimeException.class, () -> {
                                 // => Asserts that lambda throws RuntimeException
            userService.getUserName("123");
                                 // => Calls service (should propagate exception)
        });                      // => Test passes if RuntimeException thrown
    }

    @Test
    void testArgumentMatchers() {
                                 // => Tests flexible argument matching
        // ARGUMENT MATCHERS - flexible matching
        when(mockRepository.findById(anyString()))
                                 // => anyString() matches ANY String (not specific value)
                                 // => More flexible than exact matching
            .thenReturn(new User("any", "AnyUser"));
                                 // => Returns same user regardless of ID

        String name = userService.getUserName("anything");
                                 // => Calls with "anything" (matches anyString())
                                 // => name is "AnyUser"
        assertEquals("AnyUser", name);
                                 // => Asserts matcher worked correctly

        // Verify with matchers
        verify(mockRepository).findById(startsWith("any"));
                                 // => Verifies findById called with String starting with "any"
                                 // => "anything" starts with "any" (verification passes)
    }
}
```

**Key Takeaway**: Use Mockito's @Mock to create mock objects and @InjectMocks to inject them into test subjects. Stub behavior with when().thenReturn() and verify interactions with verify(). Use argument matchers (anyString(), startsWith()) for flexible verification.

**Why It Matters**: Mockito enables unit testing code with external dependencies (databases, REST APIs, file systems) without requiring actual infrastructure. Before mocking frameworks, tests required test databases or in-memory implementations—slow and brittle. Mockito's when/thenReturn syntax provides readable test setup compared to hand-written stub classes. However, over-mocking creates "test smells": mocking everything tests implementation details rather than behavior, making tests fragile to refactoring. The "mock vs. stub vs. fake" debate continues: mocks verify interactions (verify calls), stubs provide canned responses, and fakes are working implementations (H2 in-memory database). Mockito handles mocks and stubs; fakes require manual implementation or test containers.

---

## Example 47: Thread Basics and Runnable

Threads enable concurrent execution. Java provides Runnable interface for defining thread tasks and Thread class for execution management.

**Code**:

```java
// RUNNABLE - task to execute in thread
class PrintTask implements Runnable {
                                 // => Implements Runnable functional interface
    private final String message;
                                 // => Immutable message field (thread-safe)

    public PrintTask(String message) {
                                 // => Constructor initializes task
        this.message = message;  // => Stores message for run() method
    }

    @Override
    public void run() {          // => Executed when thread starts
                                 // => This code runs in separate thread
        for (int i = 0; i < 5; i++) {
                                 // => Loop 5 times
            System.out.println(message + " " + i);
                                 // => Output: message + iteration number
            try {
                Thread.sleep(100); // => Pause 100ms (yields CPU to other threads)
                                 // => Allows context switching
            } catch (InterruptedException e) {
                                 // => Thrown if thread interrupted during sleep
                e.printStackTrace();
                                 // => Print stack trace
            }
        }
    }
}

// CREATE AND START THREADS
Thread thread1 = new Thread(new PrintTask("Thread-1"));
                                 // => Creates Thread with Runnable task
                                 // => PrintTask implements Runnable interface
                                 // => Thread not started yet (NEW state)
Thread thread2 = new Thread(new PrintTask("Thread-2"));
                                 // => Second independent thread
                                 // => Each has separate execution context

thread1.start();                 // => Starts thread (calls run() in new thread)
                                 // => Creates OS thread, enters RUNNABLE state
                                 // => run() executes concurrently with main thread
thread2.start();                 // => Starts second thread concurrently
                                 // => Both threads execute independently
                                 // => Output interleaved (non-deterministic order)
                                 // => Example: Thread-1 0, Thread-2 0, Thread-1 1...

// WAIT FOR COMPLETION
try {
    thread1.join();              // => Blocks until thread1 completes
                                 // => Main thread waits for thread1 to finish
                                 // => Ensures thread1 done before proceeding
    thread2.join();              // => Blocks until thread2 completes
                                 // => Main thread waits for thread2 to finish
                                 // => Both threads guaranteed finished after this
} catch (InterruptedException e) {
                                 // => Thrown if waiting thread interrupted
    e.printStackTrace();         // => Print exception details
}

// LAMBDA SYNTAX (Java 8+)
Thread thread3 = new Thread(() -> {
                                 // => Lambda implements Runnable.run()
                                 // => Concise syntax for simple tasks
    System.out.println("Lambda thread running");
                                 // => Output: Lambda thread running
});
thread3.start();                 // => Starts thread executing lambda
                                 // => Lambda code runs in new thread

// THREAD PROPERTIES
Thread current = Thread.currentThread();
                                 // => Gets reference to currently executing thread
                                 // => In main: returns main thread
String name = current.getName();
                                 // => Thread name (default: Thread-N)
                                 // => name might be "main" if in main thread
long id = current.getId();       // => Unique thread ID (positive long)
                                 // => Assigned by JVM, never reused
                                 // => Example: 1 (main thread), 2, 3, ...
int priority = current.getPriority();
                                 // => Priority hint for scheduler (1-10)
                                 // => Default: 5 (Thread.NORM_PRIORITY)
                                 // => Higher priority MAY get more CPU time (not guaranteed)
```

**Key Takeaway**: Implement Runnable to define thread tasks, create Thread objects wrapping Runnable, and call start() to begin execution. Use join() to wait for thread completion. Never call run() directly—it executes in current thread without concurrency.

**Why It Matters**: Threads enable responsive UIs (Swing event thread vs. background workers), parallel processing (multi-core CPU utilization), and asynchronous I/O (network servers handling thousands of connections). However, raw threads have issues: no return values (run() returns void), manual lifecycle management (start/join/interrupt), and no exception handling (exceptions die silently in threads). These limitations drove ExecutorService creation (thread pools with Future return values) and eventually CompletableFuture (composable async operations). Modern Java rarely uses Thread directly—Executors and higher-level abstractions are preferred—but understanding threads is essential for debugging concurrency issues.

---

## Example 48: Synchronization and Thread Safety

Shared mutable state requires synchronization to prevent race conditions. Java provides synchronized keyword, locks, and atomic classes for thread safety.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Unsafe["Unsynchronized<br/>count++"] --> Race["Race Condition<br/>Lost updates"]

    Sync["synchronized method"] --> Lock["Acquire intrinsic lock"]
    Lock --> Execute["Execute method<br/>#40;exclusive access#41;"]
    Execute --> Unlock["Release lock"]

    Atomic["AtomicInteger<br/>incrementAndGet"] --> CAS["Compare-And-Swap<br/>#40;lock-free#41;"]

    style Unsafe fill:#0173B2,color:#fff
    style Race fill:#DE8F05,color:#fff
    style Sync fill:#029E73,color:#fff
    style Execute fill:#CC78BC,color:#fff
    style Atomic fill:#CA9161,color:#fff
```

**Code**:

```java
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;

// RACE CONDITION - unsafe shared state
class UnsafeCounter {
    private int count = 0;

    public void increment() {
        count++;                 // => NOT thread-safe (read-modify-write race)
    }

    public int getCount() {
        return count;
    }
}

// SYNCHRONIZED METHOD - thread-safe via intrinsic lock
class SynchronizedCounter {
    private int count = 0;       // => Shared mutable state (requires synchronization)

    public synchronized void increment() {
                                 // => synchronized keyword acquires intrinsic lock
                                 // => Only one thread can execute this method at a time
                                 // => Other threads block until lock released
        count++;                 // => Thread-safe increment (read-modify-write)
                                 // => Lock ensures atomicity of this operation
    }                            // => Lock automatically released on method exit

    public synchronized int getCount() {
                                 // => synchronized ensures visibility of count changes
        return count;            // => Returns current value (consistent read)
    }
}

// SYNCHRONIZED BLOCK - finer-grained locking
class BlockCounter {
    private int count = 0;       // => Shared mutable state
    private final Object lock = new Object();
                                 // => Explicit lock object (any Object works)
                                 // => final ensures lock reference never changes

    public void increment() {
        // Non-critical code here executes without lock
        synchronized(lock) {     // => Acquires lock on specific object
                                 // => Only locks critical section (not entire method)
                                 // => Allows better concurrency vs method-level sync
            count++;             // => Critical section: thread-safe increment
        }                        // => Lock automatically released here
        // Non-critical code here executes without lock
    }
}

// EXPLICIT LOCK - ReentrantLock for advanced control
class LockCounter {
    private int count = 0;       // => Shared mutable state
    private final Lock lock = new ReentrantLock();
                                 // => Explicit Lock interface (more flexible than synchronized)
                                 // => "Reentrant" means same thread can acquire multiple times

    public void increment() {
        lock.lock();             // => Explicitly acquires lock
                                 // => Blocks if another thread holds lock
                                 // => Must manually unlock (unlike synchronized)
        try {
            count++;             // => Critical section
                                 // => Thread-safe modification
        } finally {
            lock.unlock();       // => MUST unlock in finally block
                                 // => Ensures unlock even if exception thrown
                                 // => Failure to unlock causes deadlock
        }
    }
}

// ATOMIC CLASSES - lock-free thread safety
class AtomicCounter {
    private final AtomicInteger count = new AtomicInteger(0);
                                 // => AtomicInteger provides thread-safe operations
                                 // => Uses compare-and-swap (CAS) hardware instructions
                                 // => Lock-free (no blocking, better performance)
                                 // => Initialized to 0

    public void increment() {
        count.incrementAndGet(); // => Atomically increments and returns new value
                                 // => Thread-safe without synchronized/locks
                                 // => Equivalent to: ++count in thread-safe way
                                 // => Uses CAS loop internally
    }

    public int getCount() {
        return count.get();      // => Atomically reads current value
                                 // => Thread-safe read operation
                                 // => Returns int value
    }
}

// DEMONSTRATION
SynchronizedCounter counter = new SynchronizedCounter();
                                 // => Creates shared counter object
                                 // => Will be accessed by multiple threads

// Multiple threads incrementing
Thread t1 = new Thread(() -> {
                                 // => Lambda defines thread task
    for (int i = 0; i < 1000; i++) {
                                 // => Loop 1000 times
        counter.increment();     // => Each call acquires lock, increments, releases lock
                                 // => Synchronized method ensures thread safety
    }
});

Thread t2 = new Thread(() -> {
                                 // => Second thread with same task
    for (int i = 0; i < 1000; i++) {
                                 // => Also loops 1000 times
        counter.increment();     // => Competes with t1 for lock
                                 // => May block waiting for t1 to release lock
    }
});

t1.start();                      // => Starts first thread
                                 // => Begins executing loop
t2.start();                      // => Starts second thread concurrently
                                 // => Both threads run simultaneously

try {
    t1.join();                   // => Main thread waits for t1 to finish
                                 // => Blocks until t1 completes all 1000 increments
    t2.join();                   // => Main thread waits for t2 to finish
                                 // => Blocks until t2 completes all 1000 increments
} catch (InterruptedException e) {
    e.printStackTrace();
}

System.out.println("Final count: " + counter.getCount());
                                 // => Both threads finished, reads final value
                                 // => t1 did 1000 increments, t2 did 1000 increments
                                 // => Output: Final count: 2000 (correct with synchronization)
                                 // => Without synchronization: value would be < 2000 (lost updates)
```

**Key Takeaway**: Use synchronized methods or blocks to protect shared mutable state. Explicit locks (ReentrantLock) provide more control (try-lock, timed lock). Atomic classes (AtomicInteger) offer lock-free thread safety for simple operations. Without synchronization, concurrent access causes race conditions leading to incorrect results.

**Why It Matters**: Synchronization prevents data corruption from concurrent access—without it, count++ (read-modify-write) allows lost updates where two threads read same value and both write incremented result (both write 1 instead of 1 and 2). The synchronized keyword uses intrinsic locks (every Java object has one), simple but coarse-grained (locks entire method/block). ReentrantLock enables fairness (FIFO waiting), try-lock (non-blocking), and lock conditions (await/signal), essential for complex synchronization. Atomic classes use CPU compare-and-swap (CAS) instructions for lock-free updates, faster than locks for simple operations but limited to single-variable updates. Modern Java emphasizes immutability and concurrent collections over manual synchronization, reserving locks for complex state machines and atomic operations for counters/flags.

---

## Example 49: ExecutorService and Thread Pools

ExecutorService manages thread pools for executing tasks without manual Thread creation. It provides lifecycle management, Future results, and efficient thread reuse.

**Code**:

```java
import java.util.concurrent.*;
import java.util.*;

// FIXED THREAD POOL - reuses fixed number of threads
ExecutorService executor = Executors.newFixedThreadPool(3);
                                 // => Factory method creates thread pool
                                 // => Pool contains exactly 3 worker threads
                                 // => Threads reused across tasks (efficient)
                                 // => Tasks queue if all 3 threads busy

// SUBMIT RUNNABLE tasks
executor.submit(() -> {
                                 // => Lambda implements Runnable
                                 // => submit() returns Future<?> (no result)
    System.out.println("Task 1 running in " + Thread.currentThread().getName());
                                 // => Output: Task 1 running in pool-1-thread-1
                                 // => Thread name shows pool and thread number
});

executor.submit(() -> {
                                 // => Second task submitted to same pool
                                 // => Executed by any available thread
    System.out.println("Task 2 running in " + Thread.currentThread().getName());
                                 // => Output: Task 2 running in pool-1-thread-2 (or thread-1 if reused)
                                 // => Thread allocation non-deterministic
});

// SUBMIT CALLABLE tasks (return values)
Future<Integer> future = executor.submit(() -> {
                                 // => Lambda implements Callable<Integer>
                                 // => Callable differs from Runnable: returns value
                                 // => submit() returns Future<Integer> (represents async result)
    Thread.sleep(1000);          // => Simulates long-running computation
                                 // => Sleeps 1 second
    return 42;                   // => Returns value from task
                                 // => Type: Integer (Callable<Integer> return type)
});

try {
    Integer result = future.get();
                                 // => Blocks current thread until task completes
                                 // => Retrieves result from Future
                                 // => Waits up to 1 second (task's sleep time)
                                 // => result is 42
    System.out.println("Result: " + result);
                                 // => Output: Result: 42
} catch (InterruptedException | ExecutionException e) {
                                 // => InterruptedException: if waiting interrupted
                                 // => ExecutionException: if task threw exception
    e.printStackTrace();
}

// SUBMIT MULTIPLE TASKS
List<Callable<Integer>> tasks = Arrays.asList(
                                 // => Creates list of 3 Callable tasks
    () -> { Thread.sleep(500); return 1; },
                                 // => Task 1: sleeps 500ms, returns 1
    () -> { Thread.sleep(300); return 2; },
                                 // => Task 2: sleeps 300ms, returns 2 (faster)
    () -> { Thread.sleep(100); return 3; }
                                 // => Task 3: sleeps 100ms, returns 3 (fastest)
);

try {
    // invokeAll - waits for all tasks to complete
    List<Future<Integer>> futures = executor.invokeAll(tasks);
                                 // => Submits all 3 tasks to pool
                                 // => Blocks until ALL tasks complete
                                 // => Returns List<Future<Integer>> with results
                                 // => futures.size() is 3
    for (Future<Integer> f : futures) {
                                 // => Iterates over completed futures
        System.out.println("Result: " + f.get());
                                 // => f.get() doesn't block (already complete)
                                 // => Output: Result: 1
                                 // =>         Result: 2
                                 // =>         Result: 3
                                 // => Order matches submission order, not completion order
    }

    // invokeAny - returns first completed result
    Integer first = executor.invokeAny(tasks);
                                 // => Submits all tasks, returns result of first to complete
                                 // => Task 3 completes first (100ms sleep)
                                 // => Cancels remaining tasks after first completes
                                 // => Blocks until at least one task finishes
    System.out.println("First result: " + first);
                                 // => first is 3 (fastest task)
                                 // => Output: First result: 3
} catch (InterruptedException | ExecutionException e) {
    e.printStackTrace();
}

// SCHEDULED EXECUTOR - delayed/periodic tasks
ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(2);
                                 // => Creates pool with 2 threads for scheduled tasks
                                 // => Supports delays and periodic execution

// Execute after delay
scheduler.schedule(() -> {
                                 // => schedule() runs task once after delay
    System.out.println("Delayed task");
                                 // => Output: Delayed task (after 2 seconds)
}, 2, TimeUnit.SECONDS);         // => Initial delay: 2 seconds
                                 // => TimeUnit.SECONDS specifies unit
                                 // => Task runs once, not repeated

// Execute periodically
scheduler.scheduleAtFixedRate(() -> {
                                 // => scheduleAtFixedRate() repeats task at fixed intervals
    System.out.println("Periodic task");
                                 // => Output: Periodic task (every 1 second)
}, 0, 1, TimeUnit.SECONDS);      // => Initial delay: 0 (starts immediately)
                                 // => Period: 1 second between executions
                                 // => Runs: 0s, 1s, 2s, 3s, ... (until shutdown)

// SHUTDOWN
executor.shutdown();             // => Initiates graceful shutdown
                                 // => No new tasks accepted after this call
                                 // => Previously submitted tasks continue executing
                                 // => Doesn't block (returns immediately)
try {
    if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                                 // => Waits up to 60 seconds for tasks to complete
                                 // => Returns true if all tasks finished
                                 // => Returns false if timeout occurred
        executor.shutdownNow();  // => Force shutdown if timeout
                                 // => Attempts to stop executing tasks
                                 // => Interrupts threads running tasks
                                 // => Returns list of tasks that never started
    }
} catch (InterruptedException e) {
                                 // => If current thread interrupted while waiting
    executor.shutdownNow();      // => Force shutdown on interruption
                                 // => Propagates interrupt signal
}
```

**Key Takeaway**: Use ExecutorService for thread pool management. Submit Runnable (no return value) or Callable (returns Future) tasks. Use shutdown() to gracefully stop accepting new tasks and awaitTermination() to wait for completion. ScheduledExecutorService enables delayed and periodic task execution.

**Why It Matters**: Thread pools eliminate the overhead of creating/destroying threads for each task—thread creation costs ~1ms and 1MB stack memory, prohibitive for servers handling thousands of requests. Executors.newFixedThreadPool() reuses threads across tasks, reducing overhead to microseconds. Future<T> provides the "return value from asynchronous operation" capability missing from raw threads, enabling functional-style async code (flatMap over futures, compose operations). However, thread pools have limits: fixed-size pools can deadlock if tasks wait for other tasks (all threads blocked), and unbounded queues consume infinite memory if tasks arrive faster than processing. The fork/join pool (Java 7) and virtual threads (Java 21) address these issues with work-stealing and lightweight threads, but ExecutorService remains the standard for bounded concurrency.

---

## Example 50: CompletableFuture for Async Programming

CompletableFuture enables composable asynchronous operations with functional-style transformations. It replaces callback hell with declarative async pipelines.

**Code**:

```java
import java.util.concurrent.*;

// BASIC COMPLETABLEFUTURE
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
                                 // => supplyAsync() runs lambda in background thread
                                 // => Returns CompletableFuture<String>
                                 // => Executes in ForkJoinPool.commonPool() by default
    try {
        Thread.sleep(1000);      // => Simulates long-running task
                                 // => Sleeps 1 second
    } catch (InterruptedException e) {
        throw new RuntimeException(e);
    }
    return "Hello";              // => Returns value when task completes
                                 // => Result available after 1 second
});

// CHAINING OPERATIONS
CompletableFuture<String> result = future
                                 // => Start with CompletableFuture<String> from above
    .thenApply(s -> s + " World")
                                 // => thenApply() transforms result when available
                                 // => Lambda: s -> s + " World"
                                 // => Input: "Hello" (from previous stage)
                                 // => Output: "Hello World"
                                 // => Returns CompletableFuture<String>
    .thenApply(String::toUpperCase);
                                 // => Method reference: transforms to uppercase
                                 // => Input: "Hello World"
                                 // => Output: "HELLO WORLD"
                                 // => Returns CompletableFuture<String>

result.thenAccept(s -> System.out.println("Result: " + s));
                                 // => thenAccept() consumes result (no return value)
                                 // => Lambda executes when result available
                                 // => s is "HELLO WORLD"
                                 // => Output: Result: HELLO WORLD (after 1+ seconds)
                                 // => Returns CompletableFuture<Void>

// COMBINING FUTURES
CompletableFuture<Integer> future1 = CompletableFuture.supplyAsync(() -> 10);
                                 // => Creates async task returning 10
                                 // => Runs in background thread
CompletableFuture<Integer> future2 = CompletableFuture.supplyAsync(() -> 20);
                                 // => Creates second independent async task returning 20
                                 // => Both futures execute concurrently

CompletableFuture<Integer> combined = future1.thenCombine(future2, (a, b) -> a + b);
                                 // => thenCombine() waits for BOTH futures to complete
                                 // => BiFunction: (a, b) -> a + b combines results
                                 // => a = 10 (from future1)
                                 // => b = 20 (from future2)
                                 // => Result: 30
                                 // => Returns CompletableFuture<Integer>
combined.thenAccept(sum -> System.out.println("Sum: " + sum));
                                 // => sum is 30 (combined result)
                                 // => Output: Sum: 30

// SEQUENTIAL COMPOSITION
CompletableFuture<String> sequential = CompletableFuture.supplyAsync(() -> "user123")
                                 // => First async operation: returns "user123"
    .thenCompose(userId -> CompletableFuture.supplyAsync(() -> {
                                 // => Second async operation depends on first result
                                 // => userId = "user123" from previous stage
        // Fetch user details based on userId
        return "User: " + userId;// => Returns "User: user123"
    }));                         // => thenCompose flattens nested CompletableFuture<CompletableFuture<String>> → CompletableFuture<String>
                                 // => Like flatMap for futures (avoids nesting)

// EXCEPTION HANDLING
CompletableFuture<String> withError = CompletableFuture.supplyAsync(() -> {
                                 // => Async task that will throw exception
    if (true) throw new RuntimeException("Error!");
                                 // => Always throws exception
    return "Success";            // => Never reached
})
.exceptionally(ex -> {
                                 // => exceptionally() handles exceptions
                                 // => Called if previous stage threw exception
                                 // => ex is the thrown exception
    System.out.println("Caught: " + ex.getMessage());
                                 // => Output: Caught: Error!
    return "Fallback value";     // => Returns fallback on exception
                                 // => Replaces exception with normal value
})
.thenApply(s -> s.toUpperCase());
                                 // => Continues pipeline with fallback value
                                 // => s is "Fallback value"
                                 // => Result: "FALLBACK VALUE"

// TIMEOUT (Java 9+)
CompletableFuture<String> withTimeout = CompletableFuture.supplyAsync(() -> {
                                 // => Async task that takes too long
    try {
        Thread.sleep(5000);      // => Sleeps 5 seconds (too slow)
    } catch (InterruptedException e) {}
    return "Slow result";        // => Would return after 5 seconds
})
.orTimeout(2, TimeUnit.SECONDS);
                                 // => orTimeout() fails future if exceeds 2 seconds
                                 // => Completes exceptionally with TimeoutException
                                 // => Task cancelled if times out

// ALLOF - wait for all futures
CompletableFuture<Void> allDone = CompletableFuture.allOf(future1, future2, sequential);
                                 // => allOf() waits for ALL futures to complete
                                 // => Returns CompletableFuture<Void> (no combined result)
                                 // => Completes when all inputs complete
allDone.thenRun(() -> System.out.println("All futures complete"));
                                 // => thenRun() executes when all done
                                 // => Output: All futures complete

// ANYOF - wait for any future
CompletableFuture<Object> anyDone = CompletableFuture.anyOf(future1, future2);
                                 // => anyOf() completes when ANY future completes
                                 // => Returns CompletableFuture<Object> (type-unsafe)
                                 // => Result is value from first completed future
anyDone.thenAccept(result -> System.out.println("First result: " + result));
                                 // => result is whichever completes first
                                 // => Output: First result: 10 or First result: 20

// BLOCKING WAIT (use sparingly)
try {
    String finalResult = result.get();
                                 // => Blocks current thread until future completes
                                 // => Returns result value ("HELLO WORLD")
                                 // => Defeats async purpose (use thenAccept instead)
                                 // => May wait indefinitely if future never completes
    String timeoutResult = result.get(1, TimeUnit.SECONDS);
                                 // => Blocks with timeout (maximum 1 second)
                                 // => Returns result if completes within timeout
                                 // => Throws TimeoutException if exceeds 1 second
                                 // => Better than get() for potentially long operations
} catch (InterruptedException | ExecutionException | TimeoutException e) {
                                 // => InterruptedException: if thread interrupted while waiting
                                 // => ExecutionException: if future completed with exception
                                 // => TimeoutException: if timeout exceeded
    e.printStackTrace();
}
```

**Key Takeaway**: CompletableFuture enables async operations with functional composition. Use thenApply() for transformations, thenCombine() to merge results, thenCompose() for sequential async operations, and exceptionally() for error handling. Avoid blocking with get()—prefer reactive chaining with thenAccept() and thenRun().

**Why It Matters**: CompletableFuture brought reactive programming to standard Java, eliminating callback hell (nested callbacks for async operations). Before CompletableFuture, async code required manual thread management or libraries like Guava's ListenableFuture. The functional composition model (thenApply, thenCompose) matches modern async patterns from JavaScript Promises, Kotlin coroutines, and Scala futures. However, CompletableFuture has limitations: no cancellation propagation (cancelling parent doesn't cancel children), complex error handling (exceptions don't propagate automatically), and verbose syntax compared to async/await. Virtual threads (Java 21) enable blocking-style async code with structured concurrency, potentially replacing CompletableFuture for many use cases, though CompletableFuture remains essential for composing independent async operations.

---

## Modern Java Idioms (Java 17+)

Modern Java idioms leverage features from Java 17+ (records, sealed classes, pattern matching, text blocks) and Java 21+ (virtual threads, enhanced pattern matching). These idioms emphasize immutability, type safety, and conciseness.

---

## Example 51: Records for Immutable Data

Records provide concise syntax for immutable data carriers, automatically generating constructors, getters, equals(), hashCode(), and toString().

```mermaid
graph TD
    Traditional["Traditional Class<br/>99 lines: constructor, getters,<br/>equals, hashCode, toString"] --> Record["Record<br/>19 lines: compact constructor<br/>+ validation only"]

    Record --> Auto["Auto-generated:<br/>getters, equals,<br/>hashCode, toString"]

    style Traditional fill:#DE8F05,color:#fff
    style Record fill:#029E73,color:#fff
    style Auto fill:#0173B2,color:#fff
```

**Code**:

```java
// TRADITIONAL CLASS - verbose
public final class Payment {
    private final BigDecimal amount;
    private final LocalDate date;

    public Payment(BigDecimal amount, LocalDate date) {
        this.amount = amount;
        this.date = date;
    }

    public BigDecimal amount() { return amount; }
    public LocalDate date() { return date; }

    @Override
    public boolean equals(Object o) {
        // => 15 lines of boilerplate equality logic
        // => Compares all fields for structural equality
        // => Handles null and type checking
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Payment payment = (Payment) o;
        return Objects.equals(amount, payment.amount) &&
               Objects.equals(date, payment.date);
    }

    @Override
    public int hashCode() {
        // => Generates hash from all fields
        return Objects.hash(amount, date);
    }

    @Override
    public String toString() {
        // => String representation for debugging
        return "Payment{amount=" + amount + ", date=" + date + '}';
    }
}

// RECORD - concise (Java 17+)
public record PaymentRecord(
    BigDecimal amount,       // => Final field, auto-getter: amount()
                             // => Immutable: no setter generated
    LocalDate date           // => Final field, auto-getter: date()
) {
    // COMPACT CONSTRUCTOR - validation only
    public PaymentRecord {   // => No parameter list (uses record components)
                             // => Runs BEFORE field initialization
                             // => Validates before assignment (fail-fast)
        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
                             // => Business rule: amount must be positive
            throw new IllegalArgumentException("Amount must be positive");
                             // => Throws immediately (object never created)
        }
        Objects.requireNonNull(date, "Date required");
                             // => Null check with custom message
                             // => Prevents null dates
        // => After this block: fields initialized to params
        // => No explicit this.amount = amount needed
    }

    // DERIVED METHODS - business logic
    public boolean isRecent() {
        // => Custom method beyond auto-generated ones
        return date.isAfter(LocalDate.now().minusDays(30));
                             // => Returns true if within last 30 days
    }
}

// USAGE
PaymentRecord payment = new PaymentRecord(
    new BigDecimal("150.00"),// => amount field
    LocalDate.of(2026, 1, 15)// => date field
);                           // => Compact constructor runs validation
                             // => Fields initialized after validation passes

System.out.println(payment.amount());
                             // => Output: 150.00
                             // => Auto-generated getter (no get prefix)
System.out.println(payment.date());
                             // => Output: 2026-01-15
System.out.println(payment);
                             // => Output: PaymentRecord[amount=150.00, date=2026-01-15]
                             // => Auto-generated toString with field names

PaymentRecord copy = new PaymentRecord(payment.amount(), payment.date());
                             // => Records have no setters (immutable)
                             // => Create new instance to "modify"
System.out.println(payment.equals(copy));
                             // => Output: true
                             // => Auto-generated structural equality
```

**Key Takeaway**: Use records for immutable data classes (DTOs, value objects, API responses). Records reduce boilerplate by 80%, enforce immutability by default, and provide structural equality automatically. Validation goes in compact constructor.

**Why It Matters**: Before records (pre-Java 14), creating immutable data classes required 50-100 lines of boilerplate for constructors, getters, equals(), hashCode(), and toString(). Tools like Lombok reduced boilerplate but added external dependencies and IDE-specific issues. Records are language-native (no dependencies), compile-time verified, and optimized by JVM. Records model domain concepts clearly: `PaymentRecord` is obviously data (no behavior), unlike traditional classes where intent is unclear. Records integrate with pattern matching (Java 21+), enabling destructuring in switch expressions and instanceof checks. Use records for 90% of immutable data classes—fallback to traditional classes only when inheritance, custom serialization, or mutable fields required.

---

## Example 52: Sealed Classes for Closed Hierarchies

Sealed classes restrict which classes can extend or implement them, enabling exhaustive pattern matching and domain modeling.

```mermaid
graph TD
    Sealed["sealed interface<br/>TransactionType"] --> Cash["final class<br/>CashTransaction"]
    Sealed --> Card["final class<br/>CardTransaction"]
    Sealed --> Bank["final class<br/>BankTransfer"]

    Note["Compiler enforces:<br/>No other classes<br/>can implement"] --> Sealed

    style Sealed fill:#0173B2,color:#fff
    style Cash fill:#DE8F05,color:#fff
    style Card fill:#029E73,color:#fff
    style Bank fill:#CC78BC,color:#fff
```

**Code**:

```java
// SEALED INTERFACE - restricts implementors
public sealed interface TransactionType
    permits CashTransaction, CardTransaction, BankTransfer {
                             // => Only these 3 classes can implement
                             // => Compiler prevents other implementations
                             // => Enables exhaustive pattern matching

    BigDecimal amount();     // => All subtypes must provide amount
    String description();    // => Common interface method
}

// PERMITTED IMPLEMENTATIONS
public final class CashTransaction implements TransactionType {
                             // => final prevents further subclassing
                             // => Must be in permits clause above
    private final BigDecimal amount;
    private final String currency;

    public CashTransaction(BigDecimal amount, String currency) {
        this.amount = amount;
        this.currency = currency;
    }

    @Override
    public BigDecimal amount() { return amount; }

    @Override
    public String description() {
        return "Cash: " + amount + " " + currency;
                             // => Returns: "Cash: 100.00 USD"
    }
}

public final class CardTransaction implements TransactionType {
                             // => Second permitted subtype
    private final BigDecimal amount;
    private final String last4Digits;

    public CardTransaction(BigDecimal amount, String last4Digits) {
        this.amount = amount;
        this.last4Digits = last4Digits;
    }

    @Override
    public BigDecimal amount() { return amount; }

    @Override
    public String description() {
        return "Card ending " + last4Digits + ": " + amount;
                             // => Returns: "Card ending 1234: 50.00"
    }
}

public final class BankTransfer implements TransactionType {
                             // => Third permitted subtype
    private final BigDecimal amount;
    private final String accountNumber;

    public BankTransfer(BigDecimal amount, String accountNumber) {
        this.amount = amount;
        this.accountNumber = accountNumber;
    }

    @Override
    public BigDecimal amount() { return amount; }

    @Override
    public String description() {
        return "Bank transfer to " + accountNumber + ": " + amount;
                             // => Returns: "Bank transfer to 123456: 200.00"
    }
}

// EXHAUSTIVE PATTERN MATCHING (Java 21+)
public String processTransaction(TransactionType txn) {
    return switch (txn) {    // => Compiler checks ALL permitted types covered
                             // => No default case needed (exhaustiveness guaranteed)
        case CashTransaction c ->
                             // => Matches CashTransaction, binds to 'c'
                             // => No casting needed (pattern variable)
            "Processing cash: " + c.description();
                             // => Returns: "Processing cash: Cash: 100.00 USD"

        case CardTransaction card ->
                             // => Matches CardTransaction, binds to 'card'
            "Processing card: " + card.description();
                             // => Returns: "Processing card: Card ending 1234: 50.00"

        case BankTransfer bank ->
                             // => Matches BankTransfer, binds to 'bank'
            "Processing bank: " + bank.description();
                             // => Returns: "Processing bank: Bank transfer to 123456: 200.00"
        // => No default needed: compiler knows these are ALL types
        // => If permits clause changes, compiler errors here (catches bugs)
    };
}

// USAGE
TransactionType txn1 = new CashTransaction(
    new BigDecimal("100.00"),// => amount
    "USD"                    // => currency
);
System.out.println(processTransaction(txn1));
                             // => Output: Processing cash: Cash: 100.00 USD

TransactionType txn2 = new CardTransaction(
    new BigDecimal("50.00"), // => amount
    "1234"                   // => last 4 digits
);
System.out.println(processTransaction(txn2));
                             // => Output: Processing card: Card ending 1234: 50.00

// COMPILE ERROR if trying to add new implementation
// public final class CheckTransaction implements TransactionType {
//                               // => Compiler error: not in permits clause
//                               // => Cannot implement sealed interface
// }
```

**Key Takeaway**: Use sealed classes/interfaces to model closed domain hierarchies (payment types, status enums, result types). Compiler enforces exhaustiveness in pattern matching, eliminating runtime errors from missing cases. All permitted types must be final, sealed, or non-sealed.

**Why It Matters**: Sealed classes solve the "expression problem": adding new operations (methods) without modifying existing code, while controlling type extensions. Traditional interfaces allow anyone to implement (open hierarchy), making exhaustive pattern matching impossible—you always need a default case that might hide bugs. Sealed types enable algebraic data types (sum types) common in functional languages: `TransactionType = Cash | Card | Bank`. This powers type-safe state machines, command patterns, and domain models. When adding new permitted type, compiler errors at all switch expressions, forcing updates—prevents forgetting to handle new cases. Use sealed types for domain models with known, fixed variants (HTTP methods, database operations, workflow states). Use traditional interfaces when extensibility required (plugin systems, third-party implementations).

---

## Example 53: Pattern Matching for Switch

Pattern matching for switch combines type checking, casting, and conditional logic in concise syntax.

```mermaid
graph TD
    Switch["switch#40;object#41;"] --> Type1["case String s"]
    Switch --> Type2["case Integer i"]
    Switch --> Type3["case List list"]
    Switch --> Null["case null"]

    Type1 --> Guard1["when s.length#40;#41; > 10"]
    Type2 --> Guard2["when i > 100"]

    style Switch fill:#0173B2,color:#fff
    style Type1 fill:#DE8F05,color:#fff
    style Type2 fill:#029E73,color:#fff
    style Type3 fill:#CC78BC,color:#fff
    style Null fill:#CA9161,color:#000
```

**Code**:

```java
// TRADITIONAL INSTANCEOF CASCADE
public String formatOld(Object obj) {
    if (obj == null) {       // => Manual null check
        return "null";
    } else if (obj instanceof String) {
        String s = (String) obj;
                             // => Manual cast after check
        return "String: " + s;
    } else if (obj instanceof Integer) {
        Integer i = (Integer) obj;
                             // => Duplicate casting boilerplate
        return "Integer: " + i;
    } else if (obj instanceof List<?>) {
        List<?> list = (List<?>) obj;
                             // => Verbose and error-prone
        return "List of " + list.size() + " items";
    } else {
        return "Unknown";
    }
}

// PATTERN MATCHING FOR SWITCH (Java 21+)
public String formatNew(Object obj) {
    return switch (obj) {    // => Switch expression (returns value)
                             // => Exhaustive: covers all cases or uses default

        case null ->         // => Explicit null handling (no NullPointerException)
                             // => Pattern matching supports null case
            "null";          // => Returns: "null"

        case String s ->     // => Type pattern: matches String, binds to 's'
                             // => No explicit cast needed (automatic)
                             // => 's' scoped to this case only
            "String: " + s;  // => Returns: "String: hello"

        case Integer i when i > 100 ->
                             // => Type pattern with GUARD condition
                             // => 'when' adds boolean condition
                             // => Matches Integer AND i > 100
            "Large Integer: " + i;
                             // => Returns: "Large Integer: 150"

        case Integer i ->    // => Fallback for smaller integers
                             // => Ordered after guarded case (guards checked first)
            "Small Integer: " + i;
                             // => Returns: "Small Integer: 42"

        case List<?> list when !list.isEmpty() ->
                             // => Generic type pattern with guard
                             // => Matches non-empty lists only
            "List of " + list.size() + " items";
                             // => Returns: "List of 3 items"

        case List<?> list -> // => Matches empty lists
            "Empty list";    // => Returns: "Empty list"

        default ->           // => Catches all unmatched types
                             // => Required unless cases exhaustive
            "Unknown: " + obj.getClass().getSimpleName();
                             // => Returns: "Unknown: Double" (for 3.14)
    };
}

// RECORD PATTERNS (Java 21+)
record Point(int x, int y) {}
                             // => Simple coordinate record

public String describePoint(Object obj) {
    return switch (obj) {
        case Point(int x, int y) when x == 0 && y == 0 ->
                             // => RECORD PATTERN: destructures Point into x, y
                             // => Extracts fields in one step (no getters)
                             // => Guard checks origin (0, 0)
            "Origin";        // => Returns: "Origin"

        case Point(int x, int y) when x == y ->
                             // => Destructures and checks diagonal
            "Diagonal at (" + x + ", " + y + ")";
                             // => Returns: "Diagonal at (5, 5)"

        case Point(int x, int y) when y == 0 ->
                             // => Checks horizontal axis
            "X-axis at x=" + x;
                             // => Returns: "X-axis at x=10"

        case Point(int x, int y) ->
                             // => General point
            "Point(" + x + ", " + y + ")";
                             // => Returns: "Point(3, 7)"

        default -> "Not a point";
    };
}

// USAGE
System.out.println(formatNew("hello"));
                             // => Output: String: hello
System.out.println(formatNew(150));
                             // => Output: Large Integer: 150
System.out.println(formatNew(42));
                             // => Output: Small Integer: 42
System.out.println(formatNew(List.of(1, 2, 3)));
                             // => Output: List of 3 items
System.out.println(formatNew(null));
                             // => Output: null
System.out.println(formatNew(3.14));
                             // => Output: Unknown: Double

Point origin = new Point(0, 0);
System.out.println(describePoint(origin));
                             // => Output: Origin
Point diagonal = new Point(5, 5);
System.out.println(describePoint(diagonal));
                             // => Output: Diagonal at (5, 5)
```

**Key Takeaway**: Pattern matching for switch eliminates instanceof-cast boilerplate, supports guards (when clauses) for complex conditions, and enables record destructuring. Use null case to avoid NullPointerException, guards to combine type and logic checks, and sealed types for exhaustive matching.

**Why It Matters**: Traditional instanceof chains required 3 steps per branch: type check (instanceof), cast, then use. Pattern matching reduces this to one step, eliminating 66% of boilerplate and cast errors (forgetting cast after check). Guards (when clauses) avoid nested ifs, making complex conditions readable: `case Integer i when i > 100` vs `if (obj instanceof Integer) { Integer i = (Integer) obj; if (i > 100) {...}}`. Record patterns enable destructuring (extracting fields directly): `case Point(int x, int y)` replaces manual `point.x()`, `point.y()` calls—critical for nested data structures. Sealed types + pattern matching = compiler-verified exhaustiveness: adding new permitted type forces updating all switch expressions (catches bugs at compile-time). This combination brings algebraic data types and exhaustive pattern matching from functional languages (Scala, Haskell) to Java, transforming type-based dispatching from error-prone runtime checks to compile-time guarantees.

---

## Example 54: Optional for Null Safety

Optional explicitly models presence/absence of values, eliminating NullPointerException through functional composition.

**Code**:

```java
// NULL-BASED APPROACH - error-prone
public String findUserEmailOld(String userId) {
    User user = database.findUser(userId);
                             // => Returns null if not found
                             // => Caller must remember to check
    if (user == null) {
        return null;         // => Null propagation continues
                             // => Caller must check again
    }

    String email = user.getEmail();
                             // => getEmail() might also return null
    if (email == null) {
        return null;         // => Nested null checks proliferate
    }

    return email.toLowerCase();
                             // => Risk: NullPointerException if checks missed
}

// OPTIONAL APPROACH - null-safe
public Optional<String> findUserEmail(String userId) {
    return database.findUserOptional(userId)
                             // => Returns Optional<User> (empty if not found)
                             // => Explicit: caller knows value might be absent
        .map(User::getEmail) // => map() transforms Optional<User> → Optional<String>
                             // => If user absent: returns Optional.empty() (short-circuits)
                             // => If user present: extracts email, wraps in Optional
                             // => Handles null email: returns Optional.empty()
        .map(String::toLowerCase);
                             // => Chains transformations safely
                             // => Optional<String> → Optional<String> (lowercase)
                             // => If email absent: returns Optional.empty()
}

// CREATION
Optional<String> present = Optional.of("value");
                             // => Creates Optional with non-null value
                             // => Throws NullPointerException if "value" is null
Optional<String> nullable = Optional.ofNullable(getValue());
                             // => Creates Optional from potentially null value
                             // => Returns Optional.empty() if getValue() returns null
Optional<String> empty = Optional.empty();
                             // => Creates empty Optional explicitly

// TRANSFORMATION - map()
Optional<String> upper = Optional.of("hello")
    .map(String::toUpperCase);
                             // => map() applies function if value present
                             // => Returns Optional<String> with "HELLO"
                             // => If empty: returns Optional.empty() (no function call)

// FLAT MAPPING - flatMap()
public Optional<String> getUserCity(String userId) {
    return database.findUserOptional(userId)
                             // => Optional<User>
        .flatMap(user -> user.getAddress())
                             // => getAddress() returns Optional<Address>
                             // => flatMap() prevents Optional<Optional<Address>>
                             // => Flattens to Optional<Address>
        .flatMap(address -> address.getCity());
                             // => getCity() returns Optional<String>
                             // => Final result: Optional<String> (city)
}

// FILTERING - filter()
Optional<String> longEmail = Optional.of("user@example.com")
    .filter(email -> email.length() > 10);
                             // => filter() keeps value if predicate true
                             // => email.length() is 16 (> 10), so kept
                             // => Returns Optional<String> with "user@example.com"

Optional<String> shortEmail = Optional.of("me@co")
    .filter(email -> email.length() > 10);
                             // => email.length() is 5 (< 10), so filtered out
                             // => Returns Optional.empty()

// TERMINAL OPERATIONS
String result1 = Optional.of("value").orElse("default");
                             // => orElse() returns value if present, else default
                             // => Returns: "value"
                             // => CAVEAT: "default" is ALWAYS evaluated (even if not used)

String result2 = Optional.empty().orElse("default");
                             // => Optional is empty, returns default
                             // => Returns: "default"

String result3 = Optional.of("value").orElseGet(() -> expensiveDefault());
                             // => orElseGet() takes Supplier (lazy evaluation)
                             // => Only calls expensiveDefault() if Optional empty
                             // => Returns: "value" (supplier not called)

String result4 = Optional.empty()
    .orElseThrow(() -> new IllegalStateException("Not found"));
                             // => orElseThrow() throws exception if empty
                             // => Throws: IllegalStateException("Not found")

// PRESENCE CHECKS
Optional<String> opt = Optional.of("test");
if (opt.isPresent()) {      // => isPresent() returns true if value present
    String value = opt.get();// => get() returns value if present
                             // => DANGER: get() throws NoSuchElementException if empty
                             // => Prefer ifPresent() or orElse() instead
    System.out.println(value);
}

// SAFER PRESENCE HANDLING
Optional.of("test").ifPresent(value -> {
                             // => ifPresent() executes lambda if value present
                             // => No exception risk (lambda not called if empty)
    System.out.println("Value: " + value);
                             // => Output: Value: test
});

Optional.empty().ifPresent(value -> {
                             // => Lambda not executed (Optional empty)
    System.out.println("Value: " + value);
                             // => No output
});

// CONDITIONAL ACTIONS (Java 9+)
Optional.of("test").ifPresentOrElse(
    value -> System.out.println("Found: " + value),
                             // => Action if value present
                             // => Output: Found: test
    () -> System.out.println("Not found")
                             // => Action if value absent
                             // => Not executed (value present)
);

Optional.empty().ifPresentOrElse(
    value -> System.out.println("Found: " + value),
                             // => Not executed (value absent)
    () -> System.out.println("Not found")
                             // => Executed when empty
                             // => Output: Not found
);

// OR OPERATOR (Java 9+)
Optional<String> primary = Optional.empty();
Optional<String> secondary = Optional.of("fallback");

Optional<String> result5 = primary.or(() -> secondary);
                             // => or() returns this Optional if present
                             // => Otherwise returns alternative Optional
                             // => Returns: Optional with "fallback"
                             // => Lazy: supplier only called if primary empty

// STREAM CONVERSION (Java 9+)
List<String> emails = users.stream()
    .map(User::getEmailOptional)
                             // => Stream<Optional<String>>
    .flatMap(Optional::stream)
                             // => Optional.stream() converts Optional → Stream
                             // => Empty optionals become empty streams (filtered out)
                             // => Present optionals become 1-element streams
                             // => flatMap flattens: Stream<Stream<String>> → Stream<String>
    .collect(Collectors.toList());
                             // => Collects only present emails (no nulls)
```

**Key Takeaway**: Use Optional for return types when absence is expected and valid. Chain transformations with map()/flatMap(), provide defaults with orElse()/orElseGet(), and avoid get() (prefer ifPresent()/orElseThrow()). Never use Optional for fields or parameters (use null checks instead).

**Why It Matters**: NullPointerException is Java's most common runtime error, often from forgetting null checks or null propagating through method chains. Optional makes absence explicit in type signatures: `Optional<User> findUser()` vs `User findUser()` clearly signals "might not exist." This shifts null handling from implicit (remember to check) to explicit (compiler/IDE prompts). Functional composition (map, flatMap, filter) enables null-safe chaining without nested ifs: `findUser().map(User::getEmail).map(String::toLowerCase).orElse("unknown")` replaces 10+ lines of null checks. However, Optional has overhead (wrapper object allocation) and should NOT be used everywhere: avoid Optional fields (breaks serialization), Optional parameters (caller burden), or Optional collections (use Collections.emptyList()). Optional excels for return types in queries, configuration lookups, and parsers where absence is meaningful. Java 9+ enhancements (ifPresentOrElse, or, stream) make Optional more powerful, approaching monadic error handling from functional languages.

---

## Example 55: Stream API Collectors

Collectors transform streams into collections, maps, or aggregate values through terminal operations.

**Code**:

```java
record Employee(String name, String department, int salary) {}
                             // => Simple employee record

List<Employee> employees = List.of(
    new Employee("Alice", "Engineering", 80000),
    new Employee("Bob", "Sales", 60000),
    new Employee("Charlie", "Engineering", 90000),
    new Employee("Diana", "HR", 55000),
    new Employee("Eve", "Sales", 65000)
);                           // => Sample employee data

// BASIC COLLECTION
List<String> names = employees.stream()
    .map(Employee::name)     // => Extracts names: Stream<String>
    .collect(Collectors.toList());
                             // => Collects to List<String>
                             // => Returns: ["Alice", "Bob", "Charlie", "Diana", "Eve"]

Set<String> uniqueDepts = employees.stream()
    .map(Employee::department)
                             // => Extracts departments: Stream<String>
    .collect(Collectors.toSet());
                             // => Collects to Set<String> (removes duplicates)
                             // => Returns: ["Engineering", "Sales", "HR"]

// GROUPING BY
Map<String, List<Employee>> byDepartment = employees.stream()
    .collect(Collectors.groupingBy(Employee::department));
                             // => groupingBy() creates Map<String, List<Employee>>
                             // => Key: department name
                             // => Value: List of employees in that department
                             // => Returns: {
                             //   "Engineering": [Alice, Charlie],
                             //   "Sales": [Bob, Eve],
                             //   "HR": [Diana]
                             // }

// GROUPING WITH DOWNSTREAM COLLECTOR
Map<String, Long> employeeCountByDept = employees.stream()
    .collect(Collectors.groupingBy(
        Employee::department,// => Classifier: groups by department
        Collectors.counting()// => Downstream collector: counts employees
    ));                      // => Returns Map<String, Long>
                             // => Returns: {
                             //   "Engineering": 2,
                             //   "Sales": 2,
                             //   "HR": 1
                             // }

Map<String, Integer> totalSalaryByDept = employees.stream()
    .collect(Collectors.groupingBy(
        Employee::department,// => Groups by department
        Collectors.summingInt(Employee::salary)
                             // => Downstream: sums salaries in each group
    ));                      // => Returns Map<String, Integer>
                             // => Returns: {
                             //   "Engineering": 170000,
                             //   "Sales": 125000,
                             //   "HR": 55000
                             // }

Map<String, Optional<Employee>> highestPaidByDept = employees.stream()
    .collect(Collectors.groupingBy(
        Employee::department,
        Collectors.maxBy(Comparator.comparingInt(Employee::salary))
                             // => Downstream: finds employee with max salary
    ));                      // => Returns Map<String, Optional<Employee>>
                             // => Optional because group might be empty
                             // => Returns: {
                             //   "Engineering": Optional[Charlie(90000)],
                             //   "Sales": Optional[Eve(65000)],
                             //   "HR": Optional[Diana(55000)]
                             // }

// PARTITIONING BY (boolean predicate)
Map<Boolean, List<Employee>> partitionedBySalary = employees.stream()
    .collect(Collectors.partitioningBy(e -> e.salary() > 65000));
                             // => partitioningBy() splits into 2 groups: true/false
                             // => Predicate: salary > 65000
                             // => Returns Map<Boolean, List<Employee>>
                             // => Returns: {
                             //   true: [Alice(80000), Charlie(90000)],
                             //   false: [Bob(60000), Diana(55000), Eve(65000)]
                             // }

// MAPPING DOWNSTREAM
Map<String, List<String>> namesByDept = employees.stream()
    .collect(Collectors.groupingBy(
        Employee::department,
        Collectors.mapping(
            Employee::name,  // => Extracts name from each employee
            Collectors.toList()
                             // => Collects mapped names to List
        )
    ));                      // => Returns Map<String, List<String>>
                             // => Returns: {
                             //   "Engineering": ["Alice", "Charlie"],
                             //   "Sales": ["Bob", "Eve"],
                             //   "HR": ["Diana"]
                             // }

// JOINING STRINGS
String allNames = employees.stream()
    .map(Employee::name)
    .collect(Collectors.joining(", "));
                             // => joining() concatenates with delimiter
                             // => Returns: "Alice, Bob, Charlie, Diana, Eve"

String formattedNames = employees.stream()
    .map(Employee::name)
    .collect(Collectors.joining(
        ", ",                // => Delimiter between elements
        "Employees: ",       // => Prefix before first element
        "."                  // => Suffix after last element
    ));                      // => Returns: "Employees: Alice, Bob, Charlie, Diana, Eve."

// REDUCING
int totalSalaries = employees.stream()
    .collect(Collectors.summingInt(Employee::salary));
                             // => summingInt() sums integer values
                             // => Returns: 350000

double avgSalary = employees.stream()
    .collect(Collectors.averagingInt(Employee::salary));
                             // => averagingInt() calculates average
                             // => Returns: 70000.0

Optional<Employee> highestPaid = employees.stream()
    .collect(Collectors.maxBy(Comparator.comparingInt(Employee::salary)));
                             // => maxBy() finds maximum by comparator
                             // => Returns: Optional[Charlie(90000)]

// STATISTICS
IntSummaryStatistics stats = employees.stream()
    .collect(Collectors.summarizingInt(Employee::salary));
                             // => summarizingInt() computes count, sum, min, avg, max
System.out.println(stats.getCount());
                             // => Output: 5
System.out.println(stats.getSum());
                             // => Output: 350000
System.out.println(stats.getMin());
                             // => Output: 55000
System.out.println(stats.getMax());
                             // => Output: 90000
System.out.println(stats.getAverage());
                             // => Output: 70000.0

// TO MAP
Map<String, Integer> salaryByName = employees.stream()
    .collect(Collectors.toMap(
        Employee::name,      // => Key mapper: employee name
        Employee::salary     // => Value mapper: salary
    ));                      // => Returns Map<String, Integer>
                             // => Returns: {
                             //   "Alice": 80000,
                             //   "Bob": 60000,
                             //   "Charlie": 90000,
                             //   "Diana": 55000,
                             //   "Eve": 65000
                             // }

// TO MAP WITH MERGE FUNCTION
Map<String, Integer> salaryByDeptMerged = employees.stream()
    .collect(Collectors.toMap(
        Employee::department,// => Key mapper (duplicates exist)
        Employee::salary,    // => Value mapper
        Integer::sum         // => Merge function: handles key collisions
                             // => Sums salaries for same department
    ));                      // => Returns: {
                             //   "Engineering": 170000,
                             //   "Sales": 125000,
                             //   "HR": 55000
                             // }

// FILTERING (Java 9+)
Map<String, List<Employee>> highEarnersbyDept = employees.stream()
    .collect(Collectors.groupingBy(
        Employee::department,
        Collectors.filtering(
            e -> e.salary() > 60000,
                             // => Filter predicate
            Collectors.toList()
                             // => Downstream collector after filtering
        )
    ));                      // => Returns: {
                             //   "Engineering": [Alice, Charlie],
                             //   "Sales": [Eve],
                             //   "HR": []
                             // }

// FLAT MAPPING (Java 9+)
record Department(String name, List<String> teams) {}
List<Department> departments = List.of(
    new Department("Engineering", List.of("Backend", "Frontend")),
    new Department("Sales", List.of("Direct", "Enterprise"))
);

Map<String, List<String>> teamsByDept = departments.stream()
    .collect(Collectors.toMap(
        Department::name,
        Department::teams
    ));
                             // => Regular approach: Map<String, List<String>>

List<String> allTeams = departments.stream()
    .flatMap(d -> d.teams().stream())
                             // => flatMap flattens nested lists
    .collect(Collectors.toList());
                             // => Returns: ["Backend", "Frontend", "Direct", "Enterprise"]
```

**Key Takeaway**: Use Collectors for terminal stream operations: toList()/toSet() for collections, groupingBy() for multi-level grouping, partitioningBy() for boolean splits, joining() for string concatenation, and summarizingInt() for statistics. Compose collectors with downstream collectors for complex aggregations.

**Why It Matters**: Stream collectors enable declarative data transformation, replacing imperative loops with functional pipelines. Before streams (pre-Java 8), grouping required manual map creation and loop-based aggregation (error-prone, verbose). Collectors provide optimized, reusable aggregation strategies: `groupingBy()` handles concurrent collection, `toMap()` detects duplicate keys, `summarizingInt()` computes multiple statistics in one pass. Downstream collectors enable compositional aggregation: group → filter → count in single pipeline. This matches SQL-like expressiveness (GROUP BY, COUNT, SUM) but with type safety and IDE support. Advanced collectors (filtering, flatMapping in Java 9+) eliminate intermediate stream operations, improving performance. Use collectors for ETL pipelines, report generation, and data analysis—prefer imperative loops only when: early termination needed (findFirst), stateful accumulation required, or parallel execution harmful (sequential guarantees).

---

## Example 56: Text Blocks for Multi-Line Strings

Text blocks (Java 17+) provide clean syntax for multi-line strings without escape sequences or concatenation.

**Code**:

```java
// TRADITIONAL STRING CONCATENATION - verbose
String sqlOld = "SELECT users.id, users.name, users.email\n" +
                "FROM users\n" +
                "JOIN orders ON users.id = orders.user_id\n" +
                "WHERE orders.status = 'ACTIVE'\n" +
                "ORDER BY users.name";
                             // => Requires \n for newlines
                             // => Requires + for concatenation
                             // => Unreadable formatting
                             // => Easy to forget \n or +

// TEXT BLOCK (Java 17+)
String sqlNew = """
    SELECT users.id, users.name, users.email
    FROM users
    JOIN orders ON users.id = orders.user_id
    WHERE orders.status = 'ACTIVE'
    ORDER BY users.name
    """;                     // => Triple quotes delimit text block
                             // => Automatic newline preservation
                             // => No escape sequences needed
                             // => Closing """ determines indentation level
                             // => All lines dedented to match closing """

// FORMATTING WITH TEXT BLOCKS
String userId = "user123";
String status = "ACTIVE";

String formattedSql = """
    SELECT users.id, users.name, users.email
    FROM users
    WHERE users.id = '%s'
    AND users.status = '%s'
    """.formatted(userId, status);
                             // => formatted() replaces %s placeholders
                             // => Returns: SELECT users.id, users.name, users.email
                             //             FROM users
                             //             WHERE users.id = 'user123'
                             //             AND users.status = 'ACTIVE'

// JSON TEMPLATE
String jsonTemplate = """
    {
      "user": {
        "id": "%s",
        "name": "%s",
        "email": "%s",
        "active": %b
      }
    }
    """.formatted("user123", "Alice", "alice@example.com", true);
                             // => No escaping " needed inside text block
                             // => Returns valid JSON:
                             // {
                             //   "user": {
                             //     "id": "user123",
                             //     "name": "Alice",
                             //     "email": "alice@example.com",
                             //     "active": true
                             //   }
                             // }

// HTML TEMPLATE
String htmlTemplate = """
    <!DOCTYPE html>
    <html>
      <head>
        <title>%s</title>
      </head>
      <body>
        <h1>Welcome, %s!</h1>
        <p>Email: %s</p>
      </body>
    </html>
    """.formatted("User Profile", "Alice", "alice@example.com");
                             // => Natural HTML formatting
                             // => No escape sequences for quotes

// ESCAPE SEQUENCES IN TEXT BLOCKS
String withEscapes = """
    Line 1
    Line 2 with "quoted text"
    Line 3 with \\backslash
    Line 4 with \ttab
    """;                     // => " doesn't need escaping in text block
                             // => \\ escapes backslash
                             // => \t for tab still works
                             // => Returns:
                             // Line 1
                             // Line 2 with "quoted text"
                             // Line 3 with \backslash
                             // Line 4 with    tab

// INDENTATION CONTROL
String indented = """
        Indented by 8 spaces
        Also indented by 8
    """;                     // => Closing """ at column 4
                             // => Content dedented by 4 (minimum indentation)
                             // => Each line keeps 4 extra spaces
                             // => Returns:
                             //     Indented by 8 spaces
                             //     Also indented by 8

String noIndent = """
    Line 1
    Line 2
""";                         // => Closing """ at column 0
                             // => Content dedented by 0 (no common indent)
                             // => Returns:
                             //     Line 1
                             //     Line 2

// TRAILING WHITESPACE (Java 15+)
String preserveTrailing = """
    Line 1   \s
    Line 2\s
    """;                     // => \s preserves trailing space
                             // => Prevents IDE auto-trim
                             // => Returns:
                             // Line 1
                             // Line 2

// LINE CONTINUATION (Java 15+)
String singleLine = """
    This is a very long line that \
    continues on the next line without a newline
    """;                     // => \ at end of line continues to next
                             // => No newline inserted
                             // => Returns: This is a very long line that continues on the next line without a newline

// SCRIPT TEMPLATES
String bashScript = """
    #!/bin/bash
    set -e

    echo "Starting deployment..."
    docker build -t myapp:%s .
    docker push myapp:%s
    kubectl apply -f deployment.yaml
    echo "Deployment complete"
    """.formatted("v1.2.3", "v1.2.3");
                             // => Shell script with version substitution
                             // => Preserves script structure

// REGEX PATTERNS
String emailRegex = """
    ^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$
    """.strip();             // => strip() removes leading/trailing whitespace
                             // => Single backslash for regex (not double)
                             // => Returns: ^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$

// COMPARISON: OLD vs NEW
// OLD: 8 lines, 6 escaped newlines, 5 concatenations
String oldWay = "{\n" +
                "  \"status\": \"success\",\n" +
                "  \"data\": {\n" +
                "    \"id\": \"" + id + "\",\n" +
                "    \"value\": " + value + "\n" +
                "  }\n" +
                "}";

// NEW: 8 lines, 0 escapes, 0 concatenations
String newWay = """
    {
      "status": "success",
      "data": {
        "id": "%s",
        "value": %d
      }
    }
    """.formatted(id, value);
                             // => Cleaner, more maintainable
```

**Key Takeaway**: Use text blocks for SQL queries, JSON/XML templates, HTML, scripts, and documentation. Closing `"""` position controls indentation level. Use `.formatted()` for variable substitution, `\s` to preserve trailing spaces, and `\` for line continuation.

**Why It Matters**: Multi-line strings in Java were notoriously painful before text blocks (Java 15), requiring manual newline escapes (`\n`), string concatenation (`+`), and quote escaping (`\"`). This made SQL queries, JSON templates, and HTML generation error-prone and unreadable. Text blocks solve this with: (1) Automatic newline preservation (no `\n`), (2) Quote preservation (no escaping `"` inside block), (3) Smart indentation (dedents to closing `"""`), and (4) Integration with formatted() for variable substitution. This matches multi-line string syntax from Python (`"""`), JavaScript (backticks), and Kotlin (`"""`). Text blocks improve code clarity by making structured text (SQL, JSON, YAML) look like structured text—not concatenated line fragments. Use for: database queries (readable SQL), API responses (JSON/XML templates), documentation (embedded examples), and scripts (shell/Python snippets). Avoid for: single-line strings (use regular `"..."`), dynamic content (prefer template engines for complex HTML), and security-sensitive contexts (sanitize inputs before `formatted()`).

---

## Example 57: Local Variable Type Inference (var)

The `var` keyword (Java 10+) infers local variable types from initializers, reducing verbosity while maintaining type safety.

**Code**:

```java
// TRADITIONAL TYPE DECLARATION - verbose
Map<String, List<Employee>> employeesByDepartment =
    new HashMap<String, List<Employee>>();
                             // => Type repeated 3 times
                             // => Redundant: right side has all type info

// VAR TYPE INFERENCE (Java 10+)
var employeesByDept = new HashMap<String, List<Employee>>();
                             // => Compiler infers Map<String, List<Employee>>
                             // => Type appears once (right side)
                             // => Still strongly typed (not dynamic typing)

// GOOD USAGE - type obvious from initializer
var message = "Hello";       // => String (obvious)
var count = 42;              // => int (obvious)
var price = 19.99;           // => double (obvious)
var names = new ArrayList<String>();
                             // => ArrayList<String> (obvious)
var numbers = List.of(1, 2, 3);
                             // => List<Integer> (obvious from List.of)

// GOOD USAGE - diamond operator
var employees = new ArrayList<Employee>();
                             // => ArrayList<Employee> inferred from <>
                             // => Before var: ArrayList<Employee> employees = new ArrayList<>();

// GOOD USAGE - complex generic types
var mapper = new HashMap<String, Function<Employee, String>>();
                             // => HashMap<String, Function<Employee, String>>
                             // => Saves typing nested generics

// GOOD USAGE - streams
var activeEmployees = employees.stream()
    .filter(Employee::isActive)
    .collect(Collectors.toList());
                             // => List<Employee> inferred from stream operations
                             // => Clear from context

// GOOD USAGE - try-with-resources
try (var reader = new BufferedReader(new FileReader("data.txt"))) {
                             // => BufferedReader inferred
                             // => Shorter than BufferedReader reader = ...
    var line = reader.readLine();
                             // => String inferred from readLine()
}

// GOOD USAGE - loops
for (var employee : employees) {
                             // => Employee inferred from employees collection
    System.out.println(employee.name());
}

var entries = map.entrySet();
for (var entry : entries) {  // => Map.Entry<String, Integer> inferred
    System.out.println(entry.getKey() + ": " + entry.getValue());
}

// BAD USAGE - type not obvious
var result = process();      // => What type is result?
                             // => Reader must check process() signature
                             // => Prefer explicit type for clarity

var data = getData();        // => What is data?
                             // => Generic method name gives no hint

// GOOD ALTERNATIVE - explicit type
ProcessedData result = process();
                             // => Clear: result is ProcessedData
UserData data = getData();   // => Clear: data is UserData

// BAD USAGE - null or generic initializers
var value = null;            // => COMPILE ERROR: cannot infer from null
var list = new ArrayList<>();// => COMPILE ERROR: cannot infer generic type
                             // => Must use: new ArrayList<String>()

// BAD USAGE - method parameters (not allowed)
// public void printMessage(var message) {
//                               // => COMPILE ERROR: var only for local variables
// }

// BAD USAGE - fields (not allowed)
// class MyClass {
//     var field = "value";  // => COMPILE ERROR: var only for local variables
// }

// LAMBDAS WITH VAR (Java 11+)
BiFunction<Integer, Integer, Integer> add = (var x, var y) -> x + y;
                             // => var in lambda parameters (all or none)
                             // => Allows annotations: (@NotNull var x, @NotNull var y)
                             // => Without var: (x, y) -> x + y (shorter)
                             // => Use var if annotations needed

// CHAIN READABILITY
// BAD - too much var
var a = getA();
var b = a.getB();
var c = b.getC();
var d = c.getD();            // => Unclear: what are a, b, c, d?

// GOOD - selective var
UserAccount account = getAccount();
var transactions = account.getTransactions();
                             // => List<Transaction> inferred (obvious)
var recent = transactions.stream()
    .filter(t -> t.isRecent())
    .collect(Collectors.toList());
                             // => Type clear from stream operations

// COMPARISON
// BEFORE var
Map<String, List<Transaction>> transactionsByUser =
    new HashMap<String, List<Transaction>>();
                             // => 83 characters
                             // => Type repeated

// AFTER var
var transactionsByUser = new HashMap<String, List<Transaction>>();
                             // => 67 characters
                             // => Type appears once
                             // => Same type safety
```

**Key Takeaway**: Use `var` when type is obvious from right side (constructors, method calls, literals). Avoid `var` when type is unclear (generic method names, complex return types). Never use for fields, parameters, or when initializer lacks type info (null, raw generics). `var` reduces verbosity, not type safety.

**Why It Matters**: Java's verbosity (repeating types) was a long-standing criticism, especially with generics: `Map<String, List<Employee>> map = new HashMap<String, List<Employee>>()` repeats the type declaration twice. `var` reduces this without sacrificing type safety—Java remains statically typed, compiler infers exact type from initializer. This isn't dynamic typing (like JavaScript `var`): once inferred, type is fixed and compile-time checked. `var` shines with: (1) Complex generics (nested types), (2) Diamond operator (avoid repeating `<Type>`), (3) Streams (type obvious from operations), (4) Try-with-resources (resource type clear). However, overusing `var` harms readability: `var result = process()` forces reader to check `process()` signature, while `ProcessedData result = process()` is self-documenting. Use `var` as "don't repeat yourself" (DRY) for types, not "hide the type." Modern IDEs show inferred types on hover, but code reviews and diffs lack this—prioritize readability. `var` adoption guideline: if deleting left-hand type makes reader pause, keep explicit type; if right-hand side clearly shows type, use `var`.

---

## Example 58: Try-With-Resources for Resource Management

Try-with-resources (Java 7+) automatically closes resources implementing AutoCloseable, eliminating finally-block boilerplate and resource leaks.

**Code**:

```java
// MANUAL RESOURCE MANAGEMENT - error-prone
public void processFileOld(String path) throws IOException {
    BufferedReader reader = null;
                             // => Initialize to null for finally block
    try {
        reader = new BufferedReader(new FileReader(path));
                             // => Opens file resource
                             // => Resource leak if exception before assignment
        String line = reader.readLine();
                             // => Read first line from file
        System.out.println(line);
                             // => Output: first line content
    } finally {
        if (reader != null) {// => Manual null check required
            try {
                reader.close();
                             // => Manual close in finally
                             // => Another try-catch for close() exceptions
            } catch (IOException e) {
                             // => Swallows close exception (bad)
                e.printStackTrace();
            }
        }
    }                        // => 15 lines for simple file read
}

// TRY-WITH-RESOURCES (Java 7+)
public void processFile(String path) throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
                             // => Resource declared in try(...) parentheses
                             // => Implements AutoCloseable (close() called automatically)
                             // => Guaranteed close even if exception thrown
        String line = reader.readLine();
                             // => Read file content
        System.out.println(line);
                             // => Output: first line of file
    }                        // => reader.close() called automatically here
                             // => If exception in try body: close() still called
                             // => If close() throws: exception suppressed, original thrown
}                            // => 6 lines (60% less code)

// MULTIPLE RESOURCES
public void copyFile(String source, String dest) throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(source));
                             // => First resource: source file reader
         BufferedWriter writer = new BufferedWriter(new FileWriter(dest))) {
                             // => Second resource: destination file writer
                             // => Multiple resources separated by semicolon
                             // => Closed in REVERSE order: writer, then reader
                             // => Guaranteed cleanup even if reader/writer throws

        String line;         // => Variable to hold each line
        while ((line = reader.readLine()) != null) {
                             // => Read each line until EOF
            writer.write(line);
                             // => Write to destination
            writer.newLine();// => Add newline
        }                    // => Automatic flush and close for both resources
    }
}

// EFFECTIVELY FINAL RESOURCES (Java 9+)
public void processExistingResource(BufferedReader reader) throws IOException {
                             // => reader parameter passed from caller
    // Java 7-8: must declare inside try
    // try (BufferedReader r = reader) { ... }

    // Java 9+: can use effectively final variable
    try (reader) {           // => reader must be final or effectively final
                             // => No reassignment needed
                             // => Cleaner for resources from parameters
        String line = reader.readLine();
                             // => Read first line
        System.out.println(line);
                             // => Output: line content
    }                        // => reader.close() called automatically
}

// CUSTOM AUTOCLOSEABLE RESOURCE
class DatabaseConnection implements AutoCloseable {
                             // => Custom resource implementing AutoCloseable
    private Connection conn;

    public DatabaseConnection(String url) throws SQLException {
        this.conn = DriverManager.getConnection(url);
                             // => Opens database connection
        System.out.println("Database connected");
    }

    public void executeQuery(String sql) throws SQLException {
        try (Statement stmt = conn.createStatement()) {
                             // => Nested try-with-resources
                             // => Statement auto-closed
            stmt.execute(sql);
                             // => Execute SQL query
        }
    }

    @Override
    public void close() throws SQLException {
                             // => Called automatically by try-with-resources
        if (conn != null && !conn.isClosed()) {
                             // => Check connection is valid and open
            conn.close();    // => Close underlying connection
            System.out.println("Database disconnected");
                             // => Output: Database disconnected
        }
    }
}

// USAGE OF CUSTOM RESOURCE
public void queryDatabase(String sql) throws SQLException {
    try (DatabaseConnection db = new DatabaseConnection("jdbc:...")) {
                             // => Output: Database connected
                             // => db.close() will be called automatically
        db.executeQuery(sql);
    }                        // => Output: Database disconnected
                             // => Guaranteed cleanup
}

// SUPPRESSED EXCEPTIONS
public void demonstrateSuppressedExceptions() {
    try (var resource = new AutoCloseable() {
        @Override
        public void close() throws Exception {
            throw new Exception("Error during close");
                             // => close() throws exception
        }
    }) {
        throw new Exception("Error in try block");
                             // => try block throws exception FIRST
    } catch (Exception e) {
        System.out.println("Main exception: " + e.getMessage());
                             // => Output: Main exception: Error in try block
                             // => Original exception preserved

        Throwable[] suppressed = e.getSuppressed();
                             // => getSuppressed() returns close() exceptions
        System.out.println("Suppressed exceptions: " + suppressed.length);
                             // => Output: Suppressed exceptions: 1
        System.out.println("Suppressed: " + suppressed[0].getMessage());
                             // => Output: Suppressed: Error during close
    }                        // => Close exceptions don't hide original exception
}

// NESTED TRY-WITH-RESOURCES
public void processNestedResources() throws IOException {
    try (var outer = new BufferedReader(new FileReader("outer.txt"))) {
                             // => Outer resource
        String config = outer.readLine();

        try (var inner = new BufferedReader(new FileReader(config))) {
                             // => Inner resource (nested try)
            String data = inner.readLine();
            System.out.println(data);
        }                    // => inner.close() called here
    }                        // => outer.close() called here
                             // => Clean separation of resource lifetimes
}

// COMPARISON: OLD vs NEW
// OLD: 20+ lines, manual null checks, error-prone
FileInputStream fis = null;
try {
    fis = new FileInputStream("data.txt");
    // ... use fis
} finally {
    if (fis != null) {
        try {
            fis.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

// NEW: 3 lines, automatic cleanup, exception-safe
try (var fis = new FileInputStream("data.txt")) {
    // ... use fis
}                            // => fis.close() guaranteed
```

**Key Takeaway**: Always use try-with-resources for AutoCloseable resources (files, streams, connections). Resources close in reverse declaration order. Close exceptions are suppressed (not lost) if try block throws. Java 9+ allows effectively final variables in try(...).

**Why It Matters**: Resource leaks (unclosed files, connections, streams) were a major source of bugs in Java before try-with-resources (Java 7). Traditional finally blocks required: (1) Null checks (resource might not initialize), (2) Nested try-catch for close() exceptions, (3) Manual exception handling that often swallowed close() errors. Try-with-resources automates this: compiler generates finally block that calls close() in reverse order, handles null automatically, and preserves exceptions (close() exceptions suppressed, not lost). This prevents: file descriptor exhaustion (OS limit on open files), connection pool leaks (database connections not returned), memory leaks (streams holding references). Try-with-resources works with any AutoCloseable: files (BufferedReader, FileWriter), streams (InputStream, OutputStream), network (Socket, ServerSocket), database (Connection, Statement, ResultSet), and custom resources. Java 9+ improvement (effectively final resources) enables passing resources as parameters without re-wrapping. Exception handling improved: try block exception is primary, close() exceptions available via getSuppressed()—critical for debugging resource cleanup failures. Modern Java code should use try-with-resources for 100% of AutoCloseable resources—manual finally blocks are code smell indicating pre-Java 7 patterns.

---

## Example 59: Builder Pattern for Complex Objects

Builder pattern creates complex objects step-by-step, providing readable construction with validation and optional parameters.

**Code**:

```java
// PROBLEM - complex constructor
public class LoanAgreement {
    private final String id;
    private final String borrower;
    private final String lender;
    private final BigDecimal principal;
    private final BigDecimal interestRate;
    private final int termMonths;
    private final LocalDate startDate;
    private final String purpose;
    private final boolean secured;
    private final List<String> collateral;

    // BAD: telescoping constructor
    public LoanAgreement(String id, String borrower, String lender,
                        BigDecimal principal, BigDecimal interestRate,
                        int termMonths, LocalDate startDate) {
        this(id, borrower, lender, principal, interestRate,
             termMonths, startDate, null, false, List.of());
                             // => Calls overloaded constructor with defaults
                             // => Hard to read: which parameter is which?
    }

    public LoanAgreement(String id, String borrower, String lender,
                        BigDecimal principal, BigDecimal interestRate,
                        int termMonths, LocalDate startDate, String purpose,
                        boolean secured, List<String> collateral) {
                             // => 10 parameters (unreadable)
                             // => Parameter order easy to mix up
                             // => No named parameters in Java
        this.id = id;
        this.borrower = borrower;
        this.lender = lender;
        this.principal = principal;
        this.interestRate = interestRate;
        this.termMonths = termMonths;
        this.startDate = startDate;
        this.purpose = purpose;
        this.secured = secured;
        this.collateral = List.copyOf(collateral);
    }
}

// SOLUTION - Builder Pattern
public class LoanAgreement {
    // Immutable fields
    private final String id;
    private final String borrower;
    private final String lender;
    private final BigDecimal principal;
    private final BigDecimal interestRate;
    private final int termMonths;
    private final LocalDate startDate;
    private final String purpose;
    private final boolean secured;
    private final List<String> collateral;

    // PRIVATE CONSTRUCTOR - only builder can create
    private LoanAgreement(Builder builder) {
                             // => Accepts Builder, not individual fields
                             // => Validation in one place
        this.id = builder.id;
        this.borrower = builder.borrower;
        this.lender = builder.lender;
        this.principal = builder.principal;
        this.interestRate = builder.interestRate;
        this.termMonths = builder.termMonths;
        this.startDate = builder.startDate;
        this.purpose = builder.purpose;
        this.secured = builder.secured;
        this.collateral = builder.collateral;

        validate();          // => Centralized validation after construction
    }

    private void validate() {
        if (borrower == null || borrower.isBlank()) {
            throw new IllegalArgumentException("Borrower required");
        }
        if (principal.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Principal must be positive");
        }
        if (termMonths <= 0) {
            throw new IllegalArgumentException("Term must be positive");
        }
                             // => Business rule validation
                             // => Fails fast before object fully constructed
    }

    // Getters
    public String id() { return id; }
    public String borrower() { return borrower; }
    public BigDecimal principal() { return principal; }
    // ... other getters

    // BUILDER CLASS
    public static class Builder {
                             // => Static nested class
                             // => Mutable fields (before building)
        // Required fields
        private String id;
        private String borrower;
        private String lender;
        private BigDecimal principal;
        private BigDecimal interestRate;
        private int termMonths;
        private LocalDate startDate;

        // Optional fields with defaults
        private String purpose = "";
        private boolean secured = false;
        private List<String> collateral = List.of();

        // REQUIRED FIELD SETTERS - return this for chaining
        public Builder id(String id) {
            this.id = id;    // => Sets id field
            return this;     // => Returns builder for method chaining
        }

        public Builder borrower(String borrower) {
            this.borrower = borrower;
            return this;
        }

        public Builder lender(String lender) {
            this.lender = lender;
            return this;
        }

        public Builder principal(BigDecimal principal) {
            this.principal = principal;
            return this;
        }

        public Builder interestRate(BigDecimal interestRate) {
            this.interestRate = interestRate;
            return this;
        }

        public Builder termMonths(int termMonths) {
            this.termMonths = termMonths;
            return this;
        }

        public Builder startDate(LocalDate startDate) {
            this.startDate = startDate;
            return this;
        }

        // OPTIONAL FIELD SETTERS
        public Builder purpose(String purpose) {
            this.purpose = purpose;
            return this;     // => Optional: caller can skip this
        }

        public Builder secured(boolean secured) {
            this.secured = secured;
            return this;
        }

        public Builder collateral(List<String> collateral) {
            this.collateral = List.copyOf(collateral);
                             // => Defensive copy (immutability)
            return this;
        }

        // BUILD METHOD - creates LoanAgreement
        public LoanAgreement build() {
                             // => Terminal operation
            return new LoanAgreement(this);
                             // => Calls private constructor
                             // => Validation happens in constructor
        }
    }

    // FACTORY METHOD - creates builder
    public static Builder builder() {
                             // => Static factory method
        return new Builder();// => Returns new builder instance
    }
}

// USAGE - fluent API
LoanAgreement loan = LoanAgreement.builder()
                             // => Creates builder
    .id("LOAN-2024-001")    // => Sets id, returns builder
    .borrower("John Doe")   // => Sets borrower, returns builder
    .lender("Bank Corp")    // => Method chaining (fluent interface)
    .principal(new BigDecimal("50000"))
    .interestRate(new BigDecimal("0.065"))
    .termMonths(36)
    .startDate(LocalDate.of(2024, 1, 1))
    .purpose("Home renovation")
                             // => Optional field (can be omitted)
    .secured(true)          // => Another optional field
    .collateral(List.of("Property deed"))
                             // => Optional list parameter
    .build();               // => Builds LoanAgreement instance
                             // => Validation runs here

// USAGE - minimal required fields only
LoanAgreement minimalLoan = LoanAgreement.builder()
    .id("LOAN-2024-002")
    .borrower("Jane Smith")
    .lender("Credit Union")
    .principal(new BigDecimal("25000"))
    .interestRate(new BigDecimal("0.055"))
    .termMonths(24)
    .startDate(LocalDate.now())
    .build();               // => Optional fields use defaults
                             // => purpose = "", secured = false, collateral = []

// VALIDATION FAILURE
try {
    LoanAgreement invalid = LoanAgreement.builder()
        .id("LOAN-2024-003")
        .borrower("")       // => Invalid: blank borrower
        .lender("Bank")
        .principal(new BigDecimal("-1000"))
                             // => Invalid: negative principal
        .interestRate(new BigDecimal("0.05"))
        .termMonths(-12)    // => Invalid: negative term
        .startDate(LocalDate.now())
        .build();           // => Throws IllegalArgumentException
} catch (IllegalArgumentException e) {
    System.out.println("Validation error: " + e.getMessage());
                             // => Output: Validation error: Borrower required
}

// ALTERNATIVE: RECORD WITH BUILDER (Java 17+)
// For simple cases, use record with static factory
public record SimpleLoan(String id, String borrower, BigDecimal amount) {
    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private String id;
        private String borrower;
        private BigDecimal amount;

        public Builder id(String id) {
            this.id = id;
            return this;
        }

        public Builder borrower(String borrower) {
            this.borrower = borrower;
            return this;
        }

        public Builder amount(BigDecimal amount) {
            this.amount = amount;
            return this;
        }

        public SimpleLoan build() {
            return new SimpleLoan(id, borrower, amount);
                             // => Record constructor handles immutability
        }
    }
}

SimpleLoan simple = SimpleLoan.builder()
    .id("SIMPLE-001")
    .borrower("Alice")
    .amount(new BigDecimal("10000"))
    .build();               // => Combines record + builder
```

**Key Takeaway**: Use builder pattern for classes with 4+ parameters, optional fields, or complex validation. Builder provides: fluent API (method chaining), named parameters (readability), immutability (fields set once), and centralized validation (in build() or constructor). For simple cases, records reduce builder boilerplate.

**Why It Matters**: Java lacks named parameters and default arguments (unlike Python, Kotlin), making constructors with many parameters unreadable: `new LoanAgreement("id", "borrower", "lender", principal, rate, term, date, purpose, true, collateral)` (which parameter is which?). Telescoping constructors (multiple overloads for optional params) explode combinatorially: 3 optional params = 8 constructors. Builder pattern solves this with: (1) Named setters (`.borrower("John")` self-documents), (2) Optional params (skip `.purpose()` if not needed), (3) Immutability (final fields after build()), (4) Validation (centralized in build()). The fluent API (returning `this`) enables method chaining, reading like natural language: `builder().id("x").borrower("y").build()`. Builders shine for: configuration objects (many optional settings), test data (readable test setup), DSLs (domain-specific languages), and API responses (flexible field sets). Downsides: boilerplate (builder class doubles code size) and memory (temporary builder object). Java 14+ records reduce builder boilerplate for simple cases (no custom validation). Consider Lombok's `@Builder` annotation to auto-generate builders, or Kotlin's data classes with default params (eliminates builder need). Modern Java projects use builders for 90% of complex domain objects—telescoping constructors are anti-pattern post-Java 8.

---

## Example 60: Immutability Patterns with Records

Immutability ensures objects never change after creation, providing thread safety and predictable behavior. Records (Java 17+) enforce immutability by default.

**Code**:

```java
// MUTABLE CLASS - problematic
public class MutableAccount {
    private BigDecimal balance;
                             // => Not final: can be changed after construction
    private String owner;

    public MutableAccount(BigDecimal balance, String owner) {
        this.balance = balance;
        this.owner = owner;
    }

    public void setBalance(BigDecimal balance) {
                             // => Setter allows mutation
                             // => Thread-unsafe: concurrent modifications
        this.balance = balance;
    }

    public BigDecimal getBalance() {
        return balance;      // => Returns current balance
    }

    // PROBLEM: shared mutable state
    MutableAccount account = new MutableAccount(new BigDecimal("1000"), "Alice");
    processPayment(account); // => Modifies account.balance
    generateReport(account); // => Sees modified balance (unexpected)
                             // => Hard to reason about state changes
}

// IMMUTABLE CLASS - traditional approach
public final class ImmutableAccount {
                             // => final class: prevents subclassing
                             // => Subclasses could add mutable fields
    private final BigDecimal balance;
                             // => final field: cannot be reassigned
    private final String owner;
                             // => All fields final (deeply immutable)

    public ImmutableAccount(BigDecimal balance, String owner) {
        this.balance = balance;
        this.owner = owner;  // => Fields set once in constructor
    }

    // NO SETTERS - only getters
    public BigDecimal balance() { return balance; }
    public String owner() { return owner; }

    // MODIFIED COPY - returns new instance
    public ImmutableAccount withBalance(BigDecimal newBalance) {
                             // => Functional update: returns NEW object
                             // => Original object unchanged
        return new ImmutableAccount(newBalance, this.owner);
                             // => Creates copy with updated field
    }

    public ImmutableAccount deposit(BigDecimal amount) {
        return new ImmutableAccount(
            this.balance.add(amount),
                             // => Calculates new balance
            this.owner
        );                   // => Returns new ImmutableAccount
                             // => Original account unchanged
    }
}

// USAGE - immutable operations
ImmutableAccount account = new ImmutableAccount(new BigDecimal("1000"), "Alice");
                             // => Original account: balance = 1000
ImmutableAccount updated = account.deposit(new BigDecimal("500"));
                             // => New account: balance = 1500
System.out.println(account.balance());
                             // => Output: 1000 (original unchanged)
System.out.println(updated.balance());
                             // => Output: 1500 (new instance)

// RECORD - immutable by default (Java 17+)
public record Account(
    BigDecimal balance,      // => Implicitly final
    String owner             // => Implicitly final
) {                          // => No boilerplate: equals, hashCode, toString auto-generated

    // COMPACT CONSTRUCTOR - validation
    public Account {         // => Runs before field initialization
        if (balance.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Balance cannot be negative");
        }
        Objects.requireNonNull(owner, "Owner required");
    }

    // FUNCTIONAL UPDATES - return new instances
    public Account deposit(BigDecimal amount) {
        return new Account(balance.add(amount), owner);
                             // => Creates new record with updated balance
    }

    public Account withdraw(BigDecimal amount) {
        if (balance.compareTo(amount) < 0) {
            throw new IllegalArgumentException("Insufficient funds");
        }
        return new Account(balance.subtract(amount), owner);
    }

    public Account transferOwner(String newOwner) {
        return new Account(balance, newOwner);
                             // => Creates new record with different owner
    }
}

// USAGE - record immutability
Account account1 = new Account(new BigDecimal("1000"), "Alice");
Account account2 = account1.deposit(new BigDecimal("500"));
Account account3 = account2.withdraw(new BigDecimal("200"));
                             // => Chained functional updates
                             // => Each operation returns new instance
System.out.println(account1.balance());
                             // => Output: 1000 (original unchanged)
System.out.println(account2.balance());
                             // => Output: 1500
System.out.println(account3.balance());
                             // => Output: 1300

// DEFENSIVE COPYING - for mutable field types
public record MutableFieldRecord(List<String> items) {
                             // => List is mutable type (not String/BigDecimal)
                             // => DANGER: external code can modify list

    // BAD: exposes mutable internal state
    // Caller can do: record.items().add("hack");

    // GOOD: defensive copy in compact constructor
    public MutableFieldRecord {
        items = List.copyOf(items);
                             // => Creates immutable copy
                             // => List.copyOf() returns unmodifiable list
                             // => External changes don't affect record
    }

    // ALTERNATIVE: return copy from accessor
    @Override
    public List<String> items() {
        return List.copyOf(items);
                             // => Returns immutable copy
                             // => Caller cannot modify internal state
    }
}

// USAGE - defensive copying
List<String> original = new ArrayList<>(List.of("A", "B", "C"));
MutableFieldRecord record = new MutableFieldRecord(original);
original.add("D");           // => Modifies original list
System.out.println(record.items());
                             // => Output: [A, B, C] (record unaffected)
                             // => Defensive copy protected internal state

// NESTED IMMUTABILITY
public record Address(String street, String city, String zip) {}
                             // => Immutable nested record

public record Person(String name, Address address) {
                             // => Nested immutable structure

    public Person withAddress(Address newAddress) {
        return new Person(name, newAddress);
                             // => Functional update
    }

    public Person withCity(String newCity) {
        return new Person(
            name,
            new Address(address.street(), newCity, address.zip())
                             // => Creates new Address with updated city
        );                   // => Deep immutability: all levels immutable
    }
}

// USAGE - nested immutability
Address addr1 = new Address("123 Main St", "Boston", "02101");
Person person1 = new Person("Alice", addr1);
Person person2 = person1.withCity("Cambridge");
                             // => Creates new Person with new Address
System.out.println(person1.address().city());
                             // => Output: Boston (original unchanged)
System.out.println(person2.address().city());
                             // => Output: Cambridge

// IMMUTABLE COLLECTIONS
public record Portfolio(Map<String, BigDecimal> holdings) {
                             // => Map is mutable type

    public Portfolio {
        holdings = Map.copyOf(holdings);
                             // => Defensive copy: immutable map
                             // => Prevents external modification
    }

    public Portfolio addHolding(String symbol, BigDecimal shares) {
        var updated = new HashMap<>(holdings);
                             // => Creates mutable copy of immutable map
        updated.put(symbol, shares);
                             // => Adds new holding
        return new Portfolio(updated);
                             // => Returns new Portfolio with updated holdings
    }

    public Portfolio removeHolding(String symbol) {
        var updated = new HashMap<>(holdings);
        updated.remove(symbol);
        return new Portfolio(updated);
    }
}

// COMPARISON: MUTABLE vs IMMUTABLE
// MUTABLE - risky
MutableAccount mut = new MutableAccount(new BigDecimal("1000"), "Alice");
processPayment(mut);         // => SIDE EFFECT: modifies mut.balance
System.out.println(mut.getBalance());
                             // => Output: ??? (depends on processPayment implementation)
                             // => Hard to track state changes

// IMMUTABLE - safe
Account imm = new Account(new BigDecimal("1000"), "Alice");
Account result = processPaymentImmutable(imm);
                             // => Returns NEW account
                             // => Original account unchanged
System.out.println(imm.balance());
                             // => Output: 1000 (guaranteed)
System.out.println(result.balance());
                             // => Output: [calculated balance]
                             // => Easy to reason about: no hidden state changes
```

**Key Takeaway**: Use records for immutability by default. All record fields are implicitly final (no setters). For updates, create new instances with modified values (functional updates). Use defensive copying (List.copyOf(), Map.copyOf()) for mutable field types. Immutability provides thread safety, predictable behavior, and hashcode stability.

**Why It Matters**: Mutable objects are a primary source of bugs in concurrent programs: race conditions (simultaneous modifications), inconsistent state (partial updates), and unpredictable behavior (methods change object state). Immutability eliminates these: immutable objects are thread-safe by default (no synchronization needed), can be safely shared across threads, and have stable hash codes (safe for HashMap/HashSet keys). Before records (pre-Java 14), creating immutable classes required: final class declaration, final fields, no setters, defensive copying in constructors/getters, manual equals/hashCode/toString—50+ lines of boilerplate. Records reduce this to one line: `record Account(BigDecimal balance, String owner) {}`. Functional updates (`withBalance()`, `deposit()`) replace setters, returning new instances instead of modifying existing ones—this matches functional programming patterns (Haskell, Clojure) and modern JavaScript (React immutability). Immutability enables: safe caching (values never change), optimistic concurrency (compare-and-swap), and event sourcing (state history preserved). Use immutability for: value objects (Money, Address), domain events (OrderPlaced, PaymentReceived), configuration (AppConfig), and API responses. Avoid for: large data structures (copying overhead), UI state (frequent updates), or performance-critical hot paths (profiling required). Modern Java embraces immutability as default—mutable classes require justification, not vice versa.

---

## Functional Programming in Java

Functional programming in Java enables writing cleaner, more predictable code through pure functions, immutability, and function composition. This section covers functional interfaces, lambda expressions, method references, streams, and functional error handling patterns.

## Example 61: Pure Functions and Referential Transparency

Pure functions always return the same output for the same input with no side effects. Referentially transparent expressions can be replaced with their values without changing program behavior.

```mermaid
graph LR
    Input["Input: x, y"]
    Pure["Pure Function<br/>add#40;x, y#41;"]
    Output["Output: x + y"]

    Input2["Input: x, y"]
    Impure["Impure Function<br/>addAndLog#40;x, y#41;"]
    Output2["Output: x + y"]
    SideEffect["Side Effect: Console output"]

    Input --> Pure
    Pure --> Output

    Input2 --> Impure
    Impure --> Output2
    Impure -.->|produces| SideEffect

    style Input fill:#0173B2,stroke:#000,color:#fff
    style Pure fill:#029E73,stroke:#000,color:#fff
    style Output fill:#0173B2,stroke:#000,color:#fff
    style Input2 fill:#0173B2,stroke:#000,color:#fff
    style Impure fill:#DE8F05,stroke:#000,color:#000
    style Output2 fill:#0173B2,stroke:#000,color:#fff
    style SideEffect fill:#DE8F05,stroke:#000,color:#000
```

**Code**:

```java
// PURE FUNCTION - referentially transparent
public class Calculator {
    public static int add(int x, int y) {
                                     // => Takes 2 inputs, returns 1 output
                                     // => No external dependencies
        return x + y;                // => Same inputs always produce same output
                                     // => add(2, 3) always returns 5
    }

    public static int multiply(int x, int y) {
        return x * y;                // => Pure: no side effects
                                     // => multiply(4, 5) always returns 20
    }
}

// USAGE - pure functions
int result1 = Calculator.add(2, 3);
                                     // => result1 is 5
int result2 = Calculator.add(2, 3);
                                     // => result2 is 5 (same inputs, same output)
                                     // => Can replace Calculator.add(2, 3) with 5 everywhere

// IMPURE FUNCTION - NOT referentially transparent
public class ImpureCalculator {
    private static int callCount = 0;
                                     // => Mutable state!

    public static int addImpure(int x, int y) {
        callCount++;                 // => SIDE EFFECT: modifies external state
        System.out.println("Adding " + x + " + " + y);
                                     // => SIDE EFFECT: I/O operation
        return x + y;                // => Returns value, but has side effects
    }

    public static int getNextId() {
        return callCount++;          // => Different result each call!
                                     // => NOT referentially transparent
    }
}

// USAGE - impure functions
int result3 = ImpureCalculator.addImpure(2, 3);
                                     // => Output: Adding 2 + 3
                                     // => result3 is 5
                                     // => callCount is now 1
int result4 = ImpureCalculator.addImpure(2, 3);
                                     // => Output: Adding 2 + 3
                                     // => result4 is 5
                                     // => callCount is now 2
                                     // => Cannot replace with 5 (side effects differ)

int id1 = ImpureCalculator.getNextId();
                                     // => id1 is 2
int id2 = ImpureCalculator.getNextId();
                                     // => id2 is 3
                                     // => Same inputs (none), different outputs!

// PURE FUNCTION - explicit dependencies
public static int addWithContext(int x, int y, int multiplier) {
                                     // => All dependencies in parameters
                                     // => No hidden state
    return (x + y) * multiplier;     // => Fully deterministic
}

int result5 = addWithContext(2, 3, 10);
                                     // => result5 is 50: (2 + 3) * 10
                                     // => Referentially transparent: can replace with 50
```

**Key Takeaway**: Pure functions have deterministic outputs (same inputs → same outputs) and no side effects (no I/O, no state mutation, no exceptions). They enable referential transparency: replacing function calls with their values doesn't change program behavior.

**Why It Matters**: Pure functions are trivial to test (no setup, no mocks), safe to cache (memoization: store result for inputs, return cached value on repeat calls), and safe to parallelize (no shared state = no race conditions). Testing impure functions requires: mocking external dependencies, resetting state between tests, and handling non-deterministic behavior. Testing pure functions: `assertEquals(5, Calculator.add(2, 3))` - no setup needed. Referential transparency enables: compiler optimizations (constant folding, common subexpression elimination), mathematical reasoning (equational reasoning: substitute equals for equals), and parallel execution (no coordination needed). Example: `add(add(1, 2), add(3, 4))` can be rewritten as `add(3, 7)` or `10` because `add(1, 2)` and `add(3, 4)` are referentially transparent. This powers functional programming, declarative code, and reliable systems.

## Example 62: Function Interface - Single Abstract Method

Function is a functional interface representing a function that takes one argument and returns a result. It's the foundation for lambda expressions and method references.

```java
import java.util.function.Function;

// FUNCTION INTERFACE - takes T, returns R
Function<String, Integer> stringLength = s -> s.length();
                                     // => Lambda: input String s, return int length
                                     // => Type: Function<String, Integer>
String input = "Hello";              // => input is "Hello"
Integer length = stringLength.apply(input);
                                     // => Calls lambda with "Hello"
                                     // => length is 5

// FUNCTION - parsing
Function<String, Integer> parseNumber = s -> Integer.parseInt(s);
                                     // => Converts String to Integer
Integer num = parseNumber.apply("42");
                                     // => num is 42

// FUNCTION - transformation
Function<Integer, String> formatNumber = n -> String.format("Value: %d", n);
                                     // => Converts Integer to formatted String
String formatted = formatNumber.apply(100);
                                     // => formatted is "Value: 100"

// FUNCTION COMPOSITION - andThen (left to right)
Function<String, Integer> parse = s -> Integer.parseInt(s);
                                     // => Step 1: String → Integer
Function<Integer, Integer> doubleValue = n -> n * 2;
                                     // => Step 2: Integer → Integer
Function<Integer, String> format = n -> "Result: " + n;
                                     // => Step 3: Integer → String

Function<String, String> pipeline = parse.andThen(doubleValue).andThen(format);
                                     // => Compose: parse THEN doubleValue THEN format
                                     // => Type: Function<String, String>

String result = pipeline.apply("10");
                                     // => Step 1: "10" → 10 (parse)
                                     // => Step 2: 10 → 20 (doubleValue)
                                     // => Step 3: 20 → "Result: 20" (format)
                                     // => result is "Result: 20"

// FUNCTION COMPOSITION - compose (right to left)
Function<Integer, Integer> addFive = n -> n + 5;
                                     // => Step 2: add 5
Function<Integer, Integer> multiplyByTwo = n -> n * 2;
                                     // => Step 1: multiply by 2

Function<Integer, Integer> composeExample = addFive.compose(multiplyByTwo);
                                     // => Compose: multiplyByTwo THEN addFive
                                     // => Reads right-to-left (mathematical notation)

Integer result2 = composeExample.apply(10);
                                     // => Step 1: 10 * 2 = 20 (multiplyByTwo)
                                     // => Step 2: 20 + 5 = 25 (addFive)
                                     // => result2 is 25

// IDENTITY FUNCTION - returns input unchanged
Function<String, String> identity = Function.identity();
                                     // => Equivalent to: s -> s
String same = identity.apply("test");
                                     // => same is "test" (unchanged)

// PRACTICAL EXAMPLE - data transformation pipeline
Function<String, String> trim = String::trim;
                                     // => Method reference: removes whitespace
Function<String, String> uppercase = String::toUpperCase;
                                     // => Method reference: converts to uppercase
Function<String, Integer> wordCount = s -> s.split("\\s+").length;
                                     // => Lambda: counts words

Function<String, Integer> processText = trim.andThen(uppercase).andThen(wordCount);
                                     // => Pipeline: trim → uppercase → count

Integer words = processText.apply("  Hello World  ");
                                     // => Step 1: "  Hello World  " → "Hello World" (trim)
                                     // => Step 2: "Hello World" → "HELLO WORLD" (uppercase)
                                     // => Step 3: "HELLO WORLD" → 2 (wordCount)
                                     // => words is 2
```

**Key Takeaway**: Function<T, R> represents T → R transformations. Use `andThen()` for left-to-right composition (f.andThen(g) = g(f(x))) and `compose()` for right-to-left composition (f.compose(g) = f(g(x))). Function.identity() returns input unchanged.

**Why It Matters**: Function interface is the foundation of functional programming in Java: it powers Stream.map(), Optional.map(), CompletableFuture.thenApply(), and all transformation operations. Before Java 8, you needed anonymous inner classes: `new Function<String, Integer>() { public Integer apply(String s) { return s.length(); } }`—10 lines for one operation. Lambda expressions reduce this to `s -> s.length()`—concise, readable, type-inferred. Function composition builds complex transformations from simple building blocks: `trim.andThen(uppercase).andThen(wordCount)` is more readable than nested calls: `wordCount.apply(uppercase.apply(trim.apply(input)))`. Real-world use cases: ETL pipelines (extract → transform → load), validation chains (sanitize → validate → normalize), data processing (parse → enrich → format), and middleware (logging → authentication → business logic). Functions are first-class values: pass as parameters, return from methods, store in variables—enabling higher-order functions and functional composition patterns throughout your codebase.

## Example 63: Predicate Interface - Boolean Testing

Predicate is a functional interface representing a boolean-valued function for testing conditions. It's the foundation for filtering operations.

```mermaid
graph LR
    Input["Input: value"]
    Predicate["Predicate<br/>test#40;value#41;"]
    OutputTrue["Output: true"]
    OutputFalse["Output: false"]

    Input --> Predicate
    Predicate -->|condition met| OutputTrue
    Predicate -->|condition not met| OutputFalse

    style Input fill:#0173B2,stroke:#000,color:#fff
    style Predicate fill:#029E73,stroke:#000,color:#fff
    style OutputTrue fill:#0173B2,stroke:#000,color:#fff
    style OutputFalse fill:#DE8F05,stroke:#000,color:#000
```

**Code**:

```java
import java.util.function.Predicate;

// PREDICATE INTERFACE - takes T, returns boolean
Predicate<Integer> isPositive = n -> n > 0;
                                     // => Lambda: test if n > 0
                                     // => Type: Predicate<Integer>
boolean result1 = isPositive.test(10);
                                     // => Calls lambda with 10
                                     // => result1 is true (10 > 0)
boolean result2 = isPositive.test(-5);
                                     // => result2 is false (-5 not > 0)

// PREDICATE - string validation
Predicate<String> isNotEmpty = s -> s != null && !s.isEmpty();
                                     // => Tests: not null AND not empty
boolean valid1 = isNotEmpty.test("Hello");
                                     // => valid1 is true
boolean valid2 = isNotEmpty.test("");
                                     // => valid2 is false (empty string)
boolean valid3 = isNotEmpty.test(null);
                                     // => valid3 is false (null)

// PREDICATE COMPOSITION - and (logical AND)
Predicate<Integer> isEven = n -> n % 2 == 0;
                                     // => Tests if n divisible by 2
Predicate<Integer> isGreaterThan10 = n -> n > 10;
                                     // => Tests if n > 10

Predicate<Integer> isEvenAndGreaterThan10 = isEven.and(isGreaterThan10);
                                     // => Combines: both must be true
                                     // => Equivalent to: n -> isEven.test(n) && isGreaterThan10.test(n)

boolean test1 = isEvenAndGreaterThan10.test(12);
                                     // => 12 is even: true
                                     // => 12 > 10: true
                                     // => test1 is true (both conditions met)
boolean test2 = isEvenAndGreaterThan10.test(8);
                                     // => 8 is even: true
                                     // => 8 > 10: false
                                     // => test2 is false (second condition failed)

// PREDICATE COMPOSITION - or (logical OR)
Predicate<String> startsWithA = s -> s.startsWith("A");
                                     // => Tests first character is 'A'
Predicate<String> endsWithZ = s -> s.endsWith("Z");
                                     // => Tests last character is 'Z'

Predicate<String> startsWithAorEndsWithZ = startsWithA.or(endsWithZ);
                                     // => Combines: either can be true
                                     // => Equivalent to: s -> startsWithA.test(s) || endsWithZ.test(s)

boolean test3 = startsWithAorEndsWithZ.test("Apple");
                                     // => Starts with 'A': true
                                     // => test3 is true (first condition met)
boolean test4 = startsWithAorEndsWithZ.test("Buzz");
                                     // => Ends with 'Z': true
                                     // => test4 is true (second condition met)
boolean test5 = startsWithAorEndsWithZ.test("Hello");
                                     // => Starts with 'A': false
                                     // => Ends with 'Z': false
                                     // => test5 is false (no conditions met)

// PREDICATE COMPOSITION - negate (logical NOT)
Predicate<Integer> isNegative = isPositive.negate();
                                     // => Inverts: NOT isPositive
                                     // => Equivalent to: n -> !isPositive.test(n)

boolean test6 = isNegative.test(-10);
                                     // => -10 > 0: false
                                     // => negate false: true
                                     // => test6 is true

// PREDICATE - filtering lists
import java.util.List;
List<Integer> numbers = List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
                                     // => numbers is [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

List<Integer> evenNumbers = numbers.stream()
    .filter(isEven)              // => Keeps only even numbers
                                     // => Uses Predicate.test() for each element
    .toList();                   // => Collects results to list
                                     // => evenNumbers is [2, 4, 6, 8, 10]

List<Integer> evenAndLarge = numbers.stream()
    .filter(isEvenAndGreaterThan10.negate())
                                     // => Keeps numbers that are NOT (even AND > 10)
                                     // => Equivalent to: odd OR <= 10
    .toList();                   // => evenAndLarge is [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                                     // => (no numbers are both even AND > 10 in range)

// COMPLEX PREDICATE - business rules
record Product(String name, double price, String category, boolean inStock) {}

Predicate<Product> isAffordable = p -> p.price() < 100;
                                     // => Price under $100
Predicate<Product> isElectronics = p -> "Electronics".equals(p.category());
                                     // => Category is Electronics
Predicate<Product> isAvailable = Product::inStock;
                                     // => Method reference: in stock

Predicate<Product> shouldDisplay = isAffordable.and(isElectronics).and(isAvailable);
                                     // => All three must be true
                                     // => Affordable AND Electronics AND In Stock

Product laptop = new Product("Laptop", 1200, "Electronics", true);
Product mouse = new Product("Mouse", 25, "Electronics", true);
Product book = new Product("Book", 15, "Books", true);

boolean display1 = shouldDisplay.test(laptop);
                                     // => price < 100: false
                                     // => display1 is false (too expensive)
boolean display2 = shouldDisplay.test(mouse);
                                     // => price < 100: true, Electronics: true, inStock: true
                                     // => display2 is true (all conditions met)
boolean display3 = shouldDisplay.test(book);
                                     // => Electronics: false
                                     // => display3 is false (wrong category)
```

**Key Takeaway**: Predicate<T> represents T → boolean tests. Use `and()` for logical AND (both must be true), `or()` for logical OR (either can be true), and `negate()` for logical NOT (invert result). Predicates power Stream.filter(), Collection.removeIf(), and conditional logic.

**Why It Matters**: Predicates separate filtering logic from iteration: `numbers.stream().filter(isEven)` is more declarative than imperative loops: `for (int n : numbers) { if (n % 2 == 0) { ... } }`. Predicate composition enables readable business rules: `shouldDisplay = isAffordable.and(isElectronics).and(isAvailable)` clearly expresses "affordable AND electronics AND available" without nested if statements. Real-world applications: validation chains (input sanitization → format validation → business rules), dynamic filtering (user-selected filters combined with AND/OR), authorization (role checks → permission checks → resource ownership), and query builders (SQL WHERE clause construction). Predicates are reusable test building blocks: define once (`isPositive`, `isEven`, `isGreaterThan10`), compose many ways (`isEvenAndGreaterThan10`, `isPositive.and(isEven)`), and use across streams, conditionals, and loops. Predicate composition eliminates boolean flag proliferation: instead of `if (isEven && isGreaterThan10 && isPositive)`, use `filter(isEven.and(isGreaterThan10).and(isPositive))`—self-documenting, testable in isolation, and composable.

## Example 64: Consumer and Supplier Interfaces

Consumer accepts input and performs side effects (no return value). Supplier takes no input and returns a value (lazy evaluation).

```java
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.Random;

// CONSUMER INTERFACE - takes T, returns void
Consumer<String> printer = s -> System.out.println(s);
                                     // => Lambda: prints string
                                     // => Type: Consumer<String>
                                     // => Side effect: console output
printer.accept("Hello World");       // => Output: Hello World
                                     // => No return value (void)

// CONSUMER - multiple side effects
Consumer<Integer> logger = n -> System.out.println("Processing: " + n);
                                     // => Logs number being processed
Consumer<Integer> validator = n -> {
    if (n < 0) throw new IllegalArgumentException("Negative number");
                                     // => Validates input (side effect: exception)
};

// CONSUMER COMPOSITION - andThen (sequential execution)
Consumer<Integer> process = validator.andThen(logger);
                                     // => Executes validator THEN logger
                                     // => Both side effects occur in order
process.accept(10);                  // => Output: Processing: 10
                                     // => validator passes (10 >= 0)
                                     // => logger prints message
// process.accept(-5);               // => Throws IllegalArgumentException
                                     // => validator fails before logger runs

// CONSUMER - list iteration
import java.util.List;
List<String> names = List.of("Alice", "Bob", "Charlie");
names.forEach(printer);              // => Applies Consumer to each element
                                     // => Output: Alice
                                     // =>         Bob
                                     // =>         Charlie

// CONSUMER - method reference
Consumer<String> upperPrinter = System.out::println;
                                     // => Method reference: equivalent to s -> System.out.println(s)
List<String> words = List.of("hello", "world");
words.stream()
    .map(String::toUpperCase)        // => Transforms to uppercase
    .forEach(upperPrinter);          // => Prints each transformed value
                                     // => Output: HELLO
                                     // =>         WORLD

// SUPPLIER INTERFACE - takes nothing, returns T
Supplier<Double> randomValue = () -> Math.random();
                                     // => Lambda: generates random double
                                     // => Type: Supplier<Double>
                                     // => No input parameters
Double value1 = randomValue.get();   // => Calls lambda, returns random value
                                     // => value1 is random double [0.0, 1.0)
Double value2 = randomValue.get();   // => Different value each call
                                     // => value2 is different random double

// SUPPLIER - lazy evaluation
Supplier<String> expensiveOperation = () -> {
    System.out.println("Computing...");
                                     // => Side effect: logged when called
    return "Result";                 // => Returns computed value
};
// => Nothing printed yet (lambda not called)

System.out.println("Before calling supplier");
                                     // => Output: Before calling supplier
String result = expensiveOperation.get();
                                     // => NOW lambda executes
                                     // => Output: Computing...
                                     // => result is "Result"
System.out.println("After calling supplier");
                                     // => Output: After calling supplier

// SUPPLIER - factory pattern
Supplier<Random> randomFactory = Random::new;
                                     // => Constructor reference: creates new Random
Random r1 = randomFactory.get();     // => r1 is new Random instance
Random r2 = randomFactory.get();     // => r2 is different Random instance
                                     // => Each get() creates NEW object

// SUPPLIER - default value generation
record Config(String host, int port) {
    static Supplier<Config> defaultConfig = () ->
        new Config("localhost", 8080);
                                     // => Default configuration supplier
}

Config cfg1 = Config.defaultConfig.get();
                                     // => cfg1 is Config[host=localhost, port=8080]

// SUPPLIER - Optional default value
import java.util.Optional;
Optional<String> maybeName = Optional.empty();
                                     // => maybeName is empty
String name = maybeName.orElseGet(() -> "Unknown");
                                     // => Supplier called only if Optional empty
                                     // => name is "Unknown"
                                     // => Lambda executed (empty Optional)

Optional<String> presentName = Optional.of("Alice");
String name2 = presentName.orElseGet(() -> {
    System.out.println("Generating default");
    return "Unknown";
});                                  // => Lambda NOT executed (Optional present)
                                     // => No output printed
                                     // => name2 is "Alice"

// COMPARISON: orElse vs orElseGet
String default1 = maybeName.orElse(expensiveComputation());
                                     // => expensiveComputation() ALWAYS called
                                     // => Even if Optional is present
                                     // => Eager evaluation

String default2 = maybeName.orElseGet(() -> expensiveComputation());
                                     // => Supplier called ONLY if Optional empty
                                     // => Lazy evaluation
                                     // => Preferred for expensive operations

// PRACTICAL EXAMPLE - logging with Consumer
record Transaction(String id, double amount) {}

Consumer<Transaction> logTransaction = t ->
    System.out.println("[LOG] Transaction " + t.id() + ": $" + t.amount());
Consumer<Transaction> auditTransaction = t ->
    System.out.println("[AUDIT] Recording transaction " + t.id());
Consumer<Transaction> notifyTransaction = t ->
    System.out.println("[NOTIFY] Sending notification for " + t.id());

Consumer<Transaction> processTransaction =
    logTransaction.andThen(auditTransaction).andThen(notifyTransaction);
                                     // => Pipeline: log → audit → notify

Transaction tx = new Transaction("TX-001", 1500.0);
processTransaction.accept(tx);       // => Output: [LOG] Transaction TX-001: $1500.0
                                     // =>         [AUDIT] Recording transaction TX-001
                                     // =>         [NOTIFY] Sending notification for TX-001
```

**Key Takeaway**: Consumer<T> takes input T and performs side effects (void return). Use `andThen()` to chain consumers sequentially. Supplier<T> takes no input and returns T (lazy evaluation). Use Supplier for: factory methods, default value generation, and deferred computation.

**Why It Matters**: Consumer separates side effects from transformations: Stream.forEach(consumer) applies side effects after processing, while Stream.map(function) transforms values without side effects. Consumer.andThen() builds processing pipelines: `log.andThen(audit).andThen(notify)` clearly expresses ordered operations. Before Java 8, you needed: `void process(Transaction t) { log(t); audit(t); notify(t); }`—tightly coupled. With consumers: `processTransaction.accept(tx)`—each step is isolated, testable, and reusable. Supplier enables lazy evaluation: `Optional.orElseGet(() -> expensiveOperation())` defers computation until needed. Contrast with `Optional.orElse(expensiveOperation())`—always executes, even if value present. Real-world use cases: Consumer for event handlers (onClick, onSave, onError), logging (audit trails, analytics), notifications (email, SMS, webhooks), and batch processing. Supplier for configuration defaults, random value generation, object factories, and expensive resource initialization (database connections, file handles). Consumer + Supplier combo: `Supplier<List<Data>> loader → Consumer<Data> processor`—loader generates data lazily, processor handles each item. Functional interfaces enable dependency injection: instead of hardcoded logging, pass `Consumer<Transaction> logger` as parameter—testable with mock consumers, swappable implementations without code changes.

## Example 65: Method References - Concise Lambda Syntax

Method references provide shorthand syntax for lambda expressions that call existing methods. Four types: static, instance (bound), instance (unbound), and constructor references.

```java
import java.util.List;
import java.util.function.*;

// STATIC METHOD REFERENCE - ClassName::staticMethod
Function<String, Integer> parseMethod = Integer::parseInt;
                                     // => Equivalent lambda: s -> Integer.parseInt(s)
                                     // => Calls static method parseInt
Integer num = parseMethod.apply("42");
                                     // => num is 42

BiFunction<Integer, Integer, Integer> maxMethod = Math::max;
                                     // => Equivalent lambda: (a, b) -> Math.max(a, b)
                                     // => Takes 2 ints, returns max
Integer maximum = maxMethod.apply(10, 20);
                                     // => maximum is 20

// INSTANCE METHOD REFERENCE (bound) - instance::instanceMethod
String text = "Hello World";         // => Specific instance
Supplier<Integer> lengthMethod = text::length;
                                     // => Equivalent lambda: () -> text.length()
                                     // => Bound to specific "Hello World" instance
Integer len = lengthMethod.get();    // => len is 11

Supplier<String> upperMethod = text::toUpperCase;
                                     // => Equivalent lambda: () -> text.toUpperCase()
String upper = upperMethod.get();    // => upper is "HELLO WORLD"

// INSTANCE METHOD REFERENCE (unbound) - ClassName::instanceMethod
Function<String, Integer> lengthUnbound = String::length;
                                     // => Equivalent lambda: s -> s.length()
                                     // => Calls length on parameter
Integer len2 = lengthUnbound.apply("Test");
                                     // => len2 is 4

Function<String, String> trimUnbound = String::trim;
                                     // => Equivalent lambda: s -> s.trim()
String trimmed = trimUnbound.apply("  spaces  ");
                                     // => trimmed is "spaces"

BiFunction<String, String, Boolean> startsWithUnbound = String::startsWith;
                                     // => Equivalent lambda: (s, prefix) -> s.startsWith(prefix)
                                     // => First parameter is instance, second is method argument
Boolean starts = startsWithUnbound.apply("Hello", "Hel");
                                     // => starts is true

// CONSTRUCTOR REFERENCE - ClassName::new
Supplier<StringBuilder> builderConstructor = StringBuilder::new;
                                     // => Equivalent lambda: () -> new StringBuilder()
                                     // => No-arg constructor
StringBuilder sb1 = builderConstructor.get();
                                     // => sb1 is new empty StringBuilder

Function<String, StringBuilder> builderWithString = StringBuilder::new;
                                     // => Equivalent lambda: s -> new StringBuilder(s)
                                     // => Constructor with String parameter
StringBuilder sb2 = builderWithString.apply("Initial");
                                     // => sb2 is StringBuilder containing "Initial"

BiFunction<Integer, Integer, Point> pointConstructor = Point::new;
                                     // => Equivalent lambda: (x, y) -> new Point(x, y)
                                     // => Constructor with 2 int parameters
record Point(int x, int y) {}
Point p = pointConstructor.apply(10, 20);
                                     // => p is Point[x=10, y=20]

// ARRAY CONSTRUCTOR REFERENCE - Type[]::new
IntFunction<String[]> arrayConstructor = String[]::new;
                                     // => Equivalent lambda: size -> new String[size]
String[] arr = arrayConstructor.apply(5);
                                     // => arr is String[5] (5 empty slots)

// METHOD REFERENCES IN STREAMS
List<String> words = List.of("apple", "banana", "cherry");

// Static method reference
List<Integer> lengths = words.stream()
    .map(String::length)             // => Instance method reference (unbound)
                                     // => Equivalent: w -> w.length()
    .toList();                       // => lengths is [5, 6, 6]

// Instance method reference (unbound) with multiple parameters
List<Boolean> startsWithA = words.stream()
    .map(w -> w.startsWith("a"))     // => Lambda version
    .toList();
                                     // => startsWithA is [true, false, false]

// Could use: (not method reference, but shows pattern)
Predicate<String> startsWithAPred = "a"::startsWith;
                                     // => Bound instance reference
                                     // => Equivalent: s -> "a".startsWith(s)
                                     // => WRONG: checks if "a" starts with parameter

// CORRECT: unbound for filtering
List<String> aWords = words.stream()
    .filter(w -> w.startsWith("a"))  // => Lambda: each word starts with "a"
    .toList();                       // => aWords is ["apple"]

// Constructor reference for object creation
record Person(String name) {}
List<Person> people = words.stream()
    .map(Person::new)                // => Constructor reference
                                     // => Equivalent: name -> new Person(name)
    .toList();                       // => people is [Person[name=apple], Person[name=banana], Person[name=cherry]]

// COMPLEX EXAMPLE - chaining method references
List<String> raw = List.of("  HELLO  ", "  world  ", "  Java  ");

List<String> processed = raw.stream()
    .map(String::trim)               // => Remove whitespace
                                     // => ["HELLO", "world", "Java"]
    .map(String::toLowerCase)        // => Convert to lowercase
                                     // => ["hello", "world", "java"]
    .map(String::toUpperCase)        // => Convert to uppercase
                                     // => ["HELLO", "WORLD", "JAVA"]
    .toList();                       // => processed is ["HELLO", "WORLD", "JAVA"]

// COMPARISON: Lambda vs Method Reference
// Lambda
Function<String, Integer> lambda = s -> Integer.parseInt(s);
// Method reference
Function<String, Integer> methodRef = Integer::parseInt;
                                     // => More concise
                                     // => Same functionality

// Lambda with multiple statements (CANNOT use method reference)
Function<String, Integer> complex = s -> {
    String trimmed = s.trim();       // => Preprocessing
    return Integer.parseInt(trimmed);// => Parsing
};
                                     // => No method reference equivalent (multiple steps)

// METHOD REFERENCE GUIDELINES
// ✓ Use method reference: String::length (concise, clear)
// ✓ Use lambda when logic is simple: s -> s.startsWith("A")
// ✓ Use lambda when multiple statements needed
// ✗ Avoid method reference if less clear than lambda

// PRACTICAL EXAMPLE - error handling with method references
List<String> numbers = List.of("1", "2", "invalid", "4");

List<Integer> parsed = numbers.stream()
    .map(s -> {
        try {
            return Integer.parseInt(s);
                                     // => Parse successful
        } catch (NumberFormatException e) {
            return 0;                // => Parse failed, default to 0
        }
    })
    .toList();                       // => parsed is [1, 2, 0, 4]
                                     // => Cannot use Integer::parseInt directly
                                     // => (would throw exception on "invalid")
```

**Key Takeaway**: Method references simplify lambda syntax: static (Integer::parseInt), instance bound (text::length), instance unbound (String::trim), and constructor (StringBuilder::new). Use when lambda just calls one method without additional logic.

**Why It Matters**: Method references reduce boilerplate: `map(String::length)` is more concise than `map(s -> s.length())`—fewer characters, same clarity. They improve readability by highlighting what operation is performed, not how: `filter(String::isEmpty)` clearly states "filter empty strings," while `filter(s -> s.isEmpty())` adds noise with parameter name. Performance is identical (javac compiles both to same bytecode via invokedynamic). When to use method references: single method call without arguments modification, no additional logic needed, method matches functional interface signature. When to use lambdas: multiple statements (`s -> { trim(s); return parse(s); }`), argument transformation (`s -> parseInt(s.trim())`), or clearer than method reference (`s -> s.startsWith("A")` vs creating separate method). Real-world patterns: Stream transformations (`.map(String::toUpperCase)`), filtering (`.filter(Objects::nonNull)`), sorting (`.sorted(String::compareTo)`), constructor references for object factories (`.map(Person::new)`), and method chaining (`.map(String::trim).map(String::toLowerCase)`). Modern Java code favors method references where possible—concise, functional style.

## Example 66: Streams API - Declarative Collection Processing

Streams enable declarative, functional-style collection processing with filter, map, and reduce operations. Streams are lazy (only evaluated when terminal operation called) and can be parallelized.

```mermaid
graph LR
    Source["Collection<br/>[1,2,3,4,5]"]
    Filter["filter#40;#41;<br/>n > 2"]
    Map["map#40;#41;<br/>n * 2"]
    Collect["collect#40;#41;<br/>toList"]
    Result["Result<br/>[6,8,10]"]

    Source --> Filter
    Filter --> Map
    Map --> Collect
    Collect --> Result

    style Source fill:#0173B2,stroke:#000,color:#fff
    style Filter fill:#DE8F05,stroke:#000,color:#000
    style Map fill:#029E73,stroke:#000,color:#fff
    style Collect fill:#CC78BC,stroke:#000,color:#000
    style Result fill:#0173B2,stroke:#000,color:#fff
```

**Code**:

```java
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// STREAM CREATION - from collection
List<Integer> numbers = List.of(1, 2, 3, 4, 5);
Stream<Integer> stream = numbers.stream();
                                     // => Creates stream from list
                                     // => No processing yet (lazy)

// FILTER - keeps elements matching predicate
List<Integer> evens = numbers.stream()
    .filter(n -> n % 2 == 0)         // => Tests each element
                                     // => Keeps: 2, 4
    .toList();                       // => Terminal operation: triggers processing
                                     // => evens is [2, 4]

// MAP - transforms each element
List<Integer> doubled = numbers.stream()
    .map(n -> n * 2)                 // => Transforms: 1→2, 2→4, 3→6, 4→8, 5→10
    .toList();                       // => doubled is [2, 4, 6, 8, 10]

// CHAINING - filter then map
List<Integer> evenDoubled = numbers.stream()
    .filter(n -> n % 2 == 0)         // => Filter: keeps [2, 4]
    .map(n -> n * 2)                 // => Map: transforms [2, 4] → [4, 8]
    .toList();                       // => evenDoubled is [4, 8]

// REDUCE - combines elements to single value
int sum = numbers.stream()
    .reduce(0, (a, b) -> a + b);     // => Accumulator: starts at 0
                                     // => Step 1: 0 + 1 = 1
                                     // => Step 2: 1 + 2 = 3
                                     // => Step 3: 3 + 3 = 6
                                     // => Step 4: 6 + 4 = 10
                                     // => Step 5: 10 + 5 = 15
                                     // => sum is 15

// REDUCE - using method reference
int sum2 = numbers.stream()
    .reduce(0, Integer::sum);        // => Equivalent to (a, b) -> a + b
                                     // => sum2 is 15

// REDUCE - finding maximum
int max = numbers.stream()
    .reduce(Integer.MIN_VALUE, Math::max);
                                     // => Starts at Integer.MIN_VALUE
                                     // => Step 1: max(-2147483648, 1) = 1
                                     // => Step 2: max(1, 2) = 2
                                     // => Step 3: max(2, 3) = 3
                                     // => Step 4: max(3, 4) = 4
                                     // => Step 5: max(4, 5) = 5
                                     // => max is 5

// REDUCE - optional result (no identity value)
import java.util.Optional;
Optional<Integer> optMax = numbers.stream()
    .reduce(Math::max);              // => No starting value
                                     // => Returns Optional (empty if stream empty)
                                     // => optMax is Optional[5]

// FLATMAP - flattens nested collections
List<List<Integer>> nested = List.of(
    List.of(1, 2),
    List.of(3, 4),
    List.of(5, 6)
);                                   // => nested is [[1,2], [3,4], [5,6]]

List<Integer> flattened = nested.stream()
    .flatMap(List::stream)           // => Converts each inner list to stream
                                     // => Stream<Integer> Stream<Integer> Stream<Integer>
                                     // => Flattens to single Stream<Integer>
    .toList();                       // => flattened is [1, 2, 3, 4, 5, 6]

// FLATMAP - practical example
record Order(String id, List<String> items) {}

List<Order> orders = List.of(
    new Order("O1", List.of("Apple", "Banana")),
    new Order("O2", List.of("Cherry")),
    new Order("O3", List.of("Date", "Elderberry", "Fig"))
);

List<String> allItems = orders.stream()
    .flatMap(order -> order.items().stream())
                                     // => Extracts items from each order
                                     // => Flattens to single stream of items
    .toList();                       // => allItems is ["Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig"]

// DISTINCT - removes duplicates
List<Integer> duplicates = List.of(1, 2, 2, 3, 3, 3, 4, 4, 4, 4);
List<Integer> unique = duplicates.stream()
    .distinct()                      // => Removes duplicate elements
                                     // => Uses equals() for comparison
    .toList();                       // => unique is [1, 2, 3, 4]

// SORTED - orders elements
List<String> words = List.of("banana", "apple", "cherry");
List<String> sorted = words.stream()
    .sorted()                        // => Natural ordering (alphabetical)
    .toList();                       // => sorted is ["apple", "banana", "cherry"]

List<String> reverseSorted = words.stream()
    .sorted((a, b) -> b.compareTo(a))// => Custom comparator (reverse)
    .toList();                       // => reverseSorted is ["cherry", "banana", "apple"]

// LIMIT - takes first N elements
List<Integer> firstThree = numbers.stream()
    .limit(3)                        // => Takes first 3 elements
    .toList();                       // => firstThree is [1, 2, 3]

// SKIP - skips first N elements
List<Integer> afterTwo = numbers.stream()
    .skip(2)                         // => Skips first 2 elements
    .toList();                       // => afterTwo is [3, 4, 5]

// PEEK - performs action without consuming (for debugging)
List<Integer> debugged = numbers.stream()
    .peek(n -> System.out.println("Processing: " + n))
                                     // => Side effect: prints each element
                                     // => Output: Processing: 1
                                     // =>         Processing: 2
                                     // =>         Processing: 3
                                     // =>         Processing: 4
                                     // =>         Processing: 5
    .map(n -> n * 2)
    .peek(n -> System.out.println("After map: " + n))
                                     // => Output: After map: 2
                                     // =>         After map: 4
                                     // =>         After map: 6
                                     // =>         After map: 8
                                     // =>         After map: 10
    .toList();                       // => debugged is [2, 4, 6, 8, 10]

// COLLECTORS - advanced terminal operations
import java.util.Map;

List<String> items = List.of("apple", "banana", "apricot", "cherry", "avocado");

// Grouping by first letter
Map<Character, List<String>> grouped = items.stream()
    .collect(Collectors.groupingBy(s -> s.charAt(0)));
                                     // => Groups by first character
                                     // => grouped is {a=[apple, apricot, avocado], b=[banana], c=[cherry]}

// Counting elements
long count = numbers.stream()
    .filter(n -> n > 2)
    .count();                        // => Terminal operation: counts elements
                                     // => count is 3 (numbers 3, 4, 5)

// Joining strings
String joined = words.stream()
    .collect(Collectors.joining(", "));
                                     // => Concatenates with delimiter
                                     // => joined is "banana, apple, cherry"

String joinedWithPrefixSuffix = words.stream()
    .collect(Collectors.joining(", ", "[", "]"));
                                     // => delimiter=", ", prefix="[", suffix="]"
                                     // => joinedWithPrefixSuffix is "[banana, apple, cherry]"

// LAZY EVALUATION - operations not executed until terminal operation
Stream<Integer> lazyStream = numbers.stream()
    .filter(n -> {
        System.out.println("Filtering: " + n);
        return n > 2;
    })
    .map(n -> {
        System.out.println("Mapping: " + n);
        return n * 2;
    });                              // => Nothing printed yet!
                                     // => No terminal operation

System.out.println("About to collect");
                                     // => Output: About to collect
List<Integer> result = lazyStream.toList();
                                     // => NOW operations execute
                                     // => Output: Filtering: 1
                                     // =>         Filtering: 2
                                     // =>         Filtering: 3
                                     // =>         Mapping: 3
                                     // =>         Filtering: 4
                                     // =>         Mapping: 4
                                     // =>         Filtering: 5
                                     // =>         Mapping: 5
                                     // => result is [6, 8, 10]

// PARALLEL STREAMS - utilize multiple cores
List<Integer> largeList = Stream.iterate(1, n -> n + 1)
    .limit(1000000)
    .toList();                       // => 1 million elements

long parallelSum = largeList.parallelStream()
    .mapToInt(Integer::intValue)    // => Convert to IntStream (primitive specialization)
    .sum();                          // => Parallel reduction
                                     // => Splits work across multiple threads
                                     // => Faster for large datasets

// INFINITE STREAMS - generate() and iterate()
Stream<Double> randomStream = Stream.generate(Math::random);
                                     // => Infinite stream of random doubles
                                     // => Must use limit() to avoid infinite loop

List<Double> tenRandom = randomStream.limit(10).toList();
                                     // => Takes first 10 random values

Stream<Integer> infiniteCounter = Stream.iterate(0, n -> n + 1);
                                     // => 0, 1, 2, 3, 4, 5, ...
                                     // => Infinite sequence

List<Integer> firstTen = infiniteCounter.limit(10).toList();
                                     // => firstTen is [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

// STREAM CLOSING - for resources (files, database connections)
// try (Stream<String> lines = Files.lines(Path.of("data.txt"))) {
//     lines.forEach(System.out::println);
// }                                 // => Stream auto-closed (implements AutoCloseable)
```

**Key Takeaway**: Streams enable declarative collection processing: filter (keep matching elements), map (transform elements), reduce (combine to single value), flatMap (flatten nested collections). Operations are lazy (evaluated only when terminal operation called) and can be parallelized (parallelStream()).

**Why It Matters**: Streams eliminate imperative loops: `numbers.stream().filter(n -> n > 2).map(n -> n * 2).toList()` is more readable than traditional loops: `List<Integer> result = new ArrayList<>(); for (int n : numbers) { if (n > 2) result.add(n * 2); }`—declarative vs imperative. Streams compose operations: `.filter().map().reduce()` clearly expresses data pipeline (filter → transform → aggregate), while loops obscure intent with nested if statements and mutable accumulators. Lazy evaluation optimizes performance: stream operations don't execute until terminal operation (toList(), count(), reduce())—enables short-circuiting (findFirst() stops after finding match) and fusion (combining multiple operations into single pass). Parallel streams utilize multiple CPU cores: `parallelStream()` automatically splits work across threads—performance gains for CPU-intensive operations on large datasets (avoid for small datasets: parallelization overhead exceeds benefits). Real-world applications: ETL pipelines (extract → transform → load), data aggregation (grouping, summing, counting), filtering/searching (SQL-like operations), and batch processing. Stream API design principles: intermediate operations return streams (enable chaining), terminal operations return concrete values (trigger execution), and stateless operations enable parallelization (no shared mutable state). Before Java 8, functional-style collection processing required third-party libraries (Google Guava, Apache Commons) or verbose loops. Streams bring functional programming patterns to Java: higher-order functions (map, filter, reduce), lazy evaluation, immutability (streams don't modify source collections), and composability. Modern Java code favors streams for collection processing—concise, readable, and optimizable.

## Example 67: Optional - Null-Safe Value Handling

Optional is a container representing presence or absence of a value, eliminating NullPointerException through explicit null handling. It forces developers to handle missing values intentionally.

```java
import java.util.Optional;
import java.util.List;

// CREATING OPTIONAL - of() for non-null values
Optional<String> present = Optional.of("Hello");
                                     // => present is Optional["Hello"]
                                     // => Optional.of(null) throws NullPointerException
                                     // => Use only when value is guaranteed non-null

// CREATING OPTIONAL - ofNullable() for potentially null values
String nullable = null;
Optional<String> maybePresent = Optional.ofNullable(nullable);
                                     // => maybePresent is Optional.empty
                                     // => Handles null safely

String nonNull = "World";
Optional<String> definitelyPresent = Optional.ofNullable(nonNull);
                                     // => definitelyPresent is Optional["World"]

// CREATING OPTIONAL - empty() for absent values
Optional<String> empty = Optional.empty();
                                     // => empty is Optional.empty

// CHECKING PRESENCE - isPresent() and isEmpty()
boolean hasValue = present.isPresent();
                                     // => hasValue is true
boolean noValue = empty.isPresent();
                                     // => noValue is false

boolean hasNoValue = present.isEmpty();
                                     // => hasNoValue is false (Java 11+)
boolean hasNoValue2 = empty.isEmpty();
                                     // => hasNoValue2 is true

// GETTING VALUE - get() (unsafe, throws if empty)
String value = present.get();        // => value is "Hello"
                                     // => Retrieves value if present
// String value2 = empty.get();      // => Throws NoSuchElementException!
                                     // => AVOID get() - use orElse/orElseGet instead

// SAFE RETRIEVAL - orElse() (eager default)
String value3 = maybePresent.orElse("Default");
                                     // => If empty: returns "Default"
                                     // => value3 is "Default" (maybePresent is empty)

String value4 = definitelyPresent.orElse("Default");
                                     // => If present: returns wrapped value
                                     // => value4 is "World" (definitelyPresent has value)

// SAFE RETRIEVAL - orElseGet() (lazy default with Supplier)
String value5 = maybePresent.orElseGet(() -> "Computed Default");
                                     // => Supplier called only if empty
                                     // => value5 is "Computed Default"

String value6 = definitelyPresent.orElseGet(() -> {
    System.out.println("Computing default");
    return "Expensive Default";
});                                  // => Supplier NOT called (Optional has value)
                                     // => No output printed
                                     // => value6 is "World"

// COMPARISON: orElse vs orElseGet
String expensive = expensiveOperation();
String result1 = maybePresent.orElse(expensive);
                                     // => expensiveOperation() ALWAYS called
                                     // => Even if Optional present (eager)

String result2 = maybePresent.orElseGet(() -> expensiveOperation());
                                     // => expensiveOperation() called ONLY if empty (lazy)
                                     // => Preferred for expensive defaults

// THROWING EXCEPTION - orElseThrow()
String value7 = definitelyPresent.orElseThrow();
                                     // => Returns value if present
                                     // => value7 is "World"
// String value8 = empty.orElseThrow();
                                     // => Throws NoSuchElementException if empty

String value9 = maybePresent.orElseThrow(() ->
    new IllegalStateException("Value required"));
                                     // => Custom exception if empty
                                     // => Throws IllegalStateException

// TRANSFORMATION - map()
Optional<Integer> length = present.map(String::length);
                                     // => Transforms "Hello" → 5
                                     // => length is Optional[5]

Optional<Integer> emptyLength = empty.map(String::length);
                                     // => empty.map() returns empty
                                     // => emptyLength is Optional.empty

// CHAINING TRANSFORMATIONS - map().map()
Optional<String> upperFirst = present
    .map(String::toUpperCase)        // => "Hello" → "HELLO"
    .map(s -> s.substring(0, 1));    // => "HELLO" → "H"
                                     // => upperFirst is Optional["H"]

// FLATMAP - prevents nested Optionals
record Person(String name) {
    Optional<String> getEmail() {    // => Returns Optional
        return Optional.ofNullable(null);
    }
}

Optional<Person> maybePerson = Optional.of(new Person("Alice"));

Optional<Optional<String>> nestedEmail = maybePerson.map(Person::getEmail);
                                     // => map() wraps result in Optional
                                     // => nestedEmail is Optional[Optional.empty]
                                     // => Type: Optional<Optional<String>>

Optional<String> flatEmail = maybePerson.flatMap(Person::getEmail);
                                     // => flatMap() unwraps inner Optional
                                     // => flatEmail is Optional.empty
                                     // => Type: Optional<String>

// FILTERING - filter()
Optional<Integer> number = Optional.of(42);

Optional<Integer> even = number.filter(n -> n % 2 == 0);
                                     // => Tests: 42 % 2 == 0 (true)
                                     // => even is Optional[42]

Optional<Integer> odd = number.filter(n -> n % 2 != 0);
                                     // => Tests: 42 % 2 != 0 (false)
                                     // => odd is Optional.empty

// CONDITIONAL ACTION - ifPresent()
present.ifPresent(s -> System.out.println("Value: " + s));
                                     // => Consumer called if present
                                     // => Output: Value: Hello

empty.ifPresent(s -> System.out.println("Value: " + s));
                                     // => Consumer NOT called (empty)
                                     // => No output

// CONDITIONAL ACTION - ifPresentOrElse() (Java 9+)
present.ifPresentOrElse(
    s -> System.out.println("Found: " + s),
                                     // => Consumer if present
    () -> System.out.println("Not found")
                                     // => Runnable if empty
);                                   // => Output: Found: Hello

empty.ifPresentOrElse(
    s -> System.out.println("Found: " + s),
    () -> System.out.println("Not found")
);                                   // => Output: Not found

// ALTERNATIVE - or() (Java 9+)
Optional<String> firstChoice = Optional.empty();
Optional<String> secondChoice = Optional.of("Fallback");

Optional<String> selected = firstChoice.or(() -> secondChoice);
                                     // => Returns secondChoice if firstChoice empty
                                     // => selected is Optional["Fallback"]

// STREAM INTEGRATION - stream() (Java 9+)
Optional<Integer> optNum = Optional.of(10);
List<Integer> list = optNum.stream()
    .map(n -> n * 2)
    .toList();                       // => list is [20]

Optional<Integer> optEmpty = Optional.empty();
List<Integer> emptyList = optEmpty.stream().toList();
                                     // => emptyList is []

// PRACTICAL EXAMPLE - user lookup
record User(String id, String name, String email) {}

class UserRepository {
    Optional<User> findById(String id) {
        if ("123".equals(id)) {
            return Optional.of(new User("123", "Alice", "alice@example.com"));
        }
        return Optional.empty();     // => User not found
    }
}

UserRepository repo = new UserRepository();

// IMPERATIVE (without Optional)
User user = null;
try {
    user = repo.findById("123").orElseThrow();
                                     // => Retrieves user or throws
} catch (Exception e) {
    System.out.println("User not found");
}
if (user != null) {
    System.out.println(user.name());
}

// FUNCTIONAL (with Optional)
String userName = repo.findById("123")
    .map(User::name)                 // => Extracts name if present
    .orElse("Guest");                // => Default if absent
                                     // => userName is "Alice"

String missingUser = repo.findById("999")
    .map(User::name)
    .orElse("Guest");                // => missingUser is "Guest"

// CHAINING OPERATIONS
repo.findById("123")
    .filter(u -> u.email().contains("@"))
                                     // => Validates email format
    .map(User::email)
    .map(String::toUpperCase)
    .ifPresent(email -> System.out.println("Email: " + email));
                                     // => Output: Email: ALICE@EXAMPLE.COM

// AVOIDING COMMON MISTAKES
// ✗ WRONG: isPresent() + get()
if (present.isPresent()) {
    String val = present.get();      // => Verbose, defeats Optional purpose
}

// ✓ RIGHT: orElse/orElseGet
String val = present.orElse("Default");
                                     // => Concise, null-safe

// ✗ WRONG: returning null
Optional<String> findUser(String id) {
    return null;                     // => Defeats Optional purpose!
}

// ✓ RIGHT: returning Optional.empty()
Optional<String> findUser2(String id) {
    return Optional.empty();         // => Explicit absence
}
```

**Key Takeaway**: Optional explicitly represents presence/absence of values. Use `orElse()` for eager defaults, `orElseGet()` for lazy/expensive defaults, `map()` for transformations, `flatMap()` for chaining Optional-returning methods, and `filter()` for conditional presence. Avoid `get()`—use `orElse()`/`orElseThrow()` instead.

**Why It Matters**: NullPointerException is the most common runtime exception in Java—Tony Hoare (null inventor) called it his "billion-dollar mistake." Before Optional, null checking was manual: `if (value != null) { ... }`—easy to forget, verbose, error-prone. Optional makes null handling explicit: method signatures declare Optional return types, forcing callers to handle absence intentionally. Compare: `User getUser(String id)` (might return null—caller must guess) vs `Optional<User> findUser(String id)` (clearly states "may be absent"—caller must handle). Optional enables functional composition: `findUser(id).map(User::email).filter(e -> e.contains("@")).orElse("no-email")` chains operations safely—no nested if-null checks. Real-world applications: database queries (findById → Optional<Entity>), configuration properties (getProperty → Optional<String>), parsing (parseInt → Optional<Integer>), and API responses (optional fields). When to use Optional: method return types (explicit absence), optional parameters (avoid method overloads), and stream operations (filter → Optional result). When NOT to use Optional: class fields (use null or default value), method parameters (use overloads or @Nullable annotation), or collections (use empty list instead of Optional<List>). Performance overhead is negligible: Optional is a lightweight wrapper (single object allocation). Modern Java APIs embrace Optional: Stream.findFirst(), Map.get() equivalents, and Optional-returning methods throughout standard library. Pattern: Optional represents computational context (presence/absence), enabling monadic operations (map, flatMap, filter)—same pattern as Stream (sequence), CompletableFuture (asynchrony), and Try (error handling).

## Example 68: Function Composition and Currying

Function composition combines simple functions into complex operations. Currying transforms multi-parameter functions into chains of single-parameter functions, enabling partial application.

```mermaid
graph LR
    Input["Input: x"]
    F1["Function f<br/>x → f#40;x#41;"]
    F2["Function g<br/>f#40;x#41; → g#40;f#40;x#41;#41;"]
    Output["Output: g#40;f#40;x#41;#41;"]

    Input --> F1
    F1 --> F2
    F2 --> Output

    style Input fill:#0173B2,stroke:#000,color:#fff
    style F1 fill:#029E73,stroke:#000,color:#fff
    style F2 fill:#DE8F05,stroke:#000,color:#000
    style Output fill:#0173B2,stroke:#000,color:#fff
```

**Code**:

```java
import java.util.function.*;
import java.math.BigDecimal;

// FUNCTION COMPOSITION - andThen (left to right)
Function<Integer, Integer> addTwo = n -> n + 2;
                                     // => Step 1: add 2
Function<Integer, Integer> multiplyThree = n -> n * 3;
                                     // => Step 2: multiply by 3

Function<Integer, Integer> composed = addTwo.andThen(multiplyThree);
                                     // => Execute: addTwo THEN multiplyThree
                                     // => Equivalent to: x -> multiplyThree(addTwo(x))

Integer result1 = composed.apply(5);
                                     // => Step 1: 5 + 2 = 7 (addTwo)
                                     // => Step 2: 7 * 3 = 21 (multiplyThree)
                                     // => result1 is 21

// FUNCTION COMPOSITION - compose (right to left)
Function<Integer, Integer> composed2 = multiplyThree.compose(addTwo);
                                     // => Execute: addTwo THEN multiplyThree
                                     // => Equivalent to: x -> multiplyThree(addTwo(x))
                                     // => Same as andThen but reversed order

Integer result2 = composed2.apply(5);
                                     // => Step 1: 5 + 2 = 7 (addTwo, executed first)
                                     // => Step 2: 7 * 3 = 21 (multiplyThree)
                                     // => result2 is 21

// COMPARISON: andThen vs compose
Function<Integer, Integer> a = x -> x + 10;
Function<Integer, Integer> b = x -> x * 2;

Integer andThenResult = a.andThen(b).apply(5);
                                     // => (5 + 10) * 2 = 30
                                     // => a first, then b

Integer composeResult = a.compose(b).apply(5);
                                     // => (5 * 2) + 10 = 20
                                     // => b first, then a

// CHAINING COMPOSITION - pipeline of operations
Function<String, String> trim = String::trim;
                                     // => Removes whitespace
Function<String, String> uppercase = String::toUpperCase;
                                     // => Converts to uppercase
Function<String, Integer> length = String::length;
                                     // => Gets string length

Function<String, Integer> pipeline = trim.andThen(uppercase).andThen(length);
                                     // => Compose: trim → uppercase → length

Integer len = pipeline.apply("  hello  ");
                                     // => Step 1: "  hello  " → "hello" (trim)
                                     // => Step 2: "hello" → "HELLO" (uppercase)
                                     // => Step 3: "HELLO" → 5 (length)
                                     // => len is 5

// PREDICATE COMPOSITION - complex business rules
Predicate<Integer> isPositive = n -> n > 0;
Predicate<Integer> isEven = n -> n % 2 == 0;
Predicate<Integer> lessThan100 = n -> n < 100;

Predicate<Integer> complexRule = isPositive
    .and(isEven)
    .and(lessThan100);               // => All three must be true
                                     // => Positive AND even AND < 100

boolean valid1 = complexRule.test(42);
                                     // => 42 > 0: true, 42 % 2 == 0: true, 42 < 100: true
                                     // => valid1 is true
boolean valid2 = complexRule.test(-10);
                                     // => -10 > 0: false (fails first check)
                                     // => valid2 is false (short-circuits)
boolean valid3 = complexRule.test(150);
                                     // => 150 > 0: true, 150 % 2 == 0: true, 150 < 100: false
                                     // => valid3 is false

// CURRYING - transform multi-parameter function into chain
// BEFORE CURRYING: BiFunction<A, B, C>
BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;
                                     // => Takes 2 parameters
Integer sum = add.apply(5, 3);       // => sum is 8

// AFTER CURRYING: Function<A, Function<B, C>>
Function<Integer, Function<Integer, Integer>> addCurried =
    a -> b -> a + b;                 // => Returns function that takes second parameter
                                     // => Type: Integer → (Integer → Integer)

Function<Integer, Integer> addFive = addCurried.apply(5);
                                     // => Partially applies first parameter: a = 5
                                     // => Returns: b -> 5 + b
Integer sum2 = addFive.apply(3);     // => sum2 is 8 (5 + 3)
Integer sum3 = addFive.apply(10);    // => sum3 is 15 (5 + 10)

// CURRYING - practical example (tax calculation)
// Non-curried version
BiFunction<BigDecimal, BigDecimal, BigDecimal> calculateTax =
    (amount, rate) -> amount.multiply(rate);

BigDecimal tax1 = calculateTax.apply(
    new BigDecimal("1000"),
    new BigDecimal("0.15")
);                                   // => tax1 is 150.00

// Curried version
Function<BigDecimal, Function<BigDecimal, BigDecimal>> calculateTaxCurried =
    rate -> amount -> amount.multiply(rate);
                                     // => Curry: rate first, then amount

Function<BigDecimal, BigDecimal> salesTax = calculateTaxCurried.apply(
    new BigDecimal("0.08")
);                                   // => Partially apply 8% rate
                                     // => Returns: amount -> amount * 0.08

Function<BigDecimal, BigDecimal> incomeTax = calculateTaxCurried.apply(
    new BigDecimal("0.25")
);                                   // => Partially apply 25% rate

BigDecimal salesTaxAmount = salesTax.apply(new BigDecimal("1000"));
                                     // => salesTaxAmount is 80.00 (1000 * 0.08)
BigDecimal incomeTaxAmount = incomeTax.apply(new BigDecimal("5000"));
                                     // => incomeTaxAmount is 1250.00 (5000 * 0.25)

// PARTIAL APPLICATION - fixing some parameters
interface TriFunction<A, B, C, R> {
    R apply(A a, B b, C c);
}

TriFunction<String, String, String, String> concat =
    (a, b, c) -> a + b + c;
                                     // => Takes 3 parameters

// Partial application: fix first parameter
BiFunction<String, String, String> greet = (name, punctuation) ->
    concat.apply("Hello, ", name, punctuation);
                                     // => First parameter fixed to "Hello, "

String greeting1 = greet.apply("Alice", "!");
                                     // => greeting1 is "Hello, Alice!"
String greeting2 = greet.apply("Bob", ".");
                                     // => greeting2 is "Hello, Bob."

// COMPOSING PREDICATES AND FUNCTIONS
record Product(String name, double price, String category) {}

Function<Product, String> getName = Product::name;
Function<Product, Double> getPrice = Product::price;
Predicate<Double> isAffordable = price -> price < 100;

Predicate<Product> isAffordableProduct = getPrice.andThen(isAffordable::test)::apply;
                                     // => Compose: extract price → test if affordable
                                     // => Alternative: p -> isAffordable.test(getPrice.apply(p))

Product laptop = new Product("Laptop", 1200, "Electronics");
Product mouse = new Product("Mouse", 25, "Electronics");

boolean laptopAffordable = isAffordableProduct.test(laptop);
                                     // => Extract 1200 → test 1200 < 100 → false
boolean mouseAffordable = isAffordableProduct.test(mouse);
                                     // => Extract 25 → test 25 < 100 → true

// PRACTICAL COMPOSITION - data validation pipeline
Function<String, String> removeWhitespace = s -> s.replaceAll("\\s+", "");
Function<String, String> removeDashes = s -> s.replace("-", "");
Predicate<String> isNumeric = s -> s.matches("\\d+");
Predicate<String> hasValidLength = s -> s.length() >= 10 && s.length() <= 15;

// Compose transformations
Function<String, String> normalizePhone = removeWhitespace.andThen(removeDashes);

// Compose validations
Predicate<String> isValidPhone = normalizePhone.andThen(s ->
    isNumeric.test(s) && hasValidLength.test(s)
)::apply;                            // => Transform → validate

boolean valid4 = isValidPhone.test("123-456-7890");
                                     // => Step 1: "123-456-7890" → "123-456-7890" (removeWhitespace)
                                     // => Step 2: "123-456-7890" → "1234567890" (removeDashes)
                                     // => Step 3: isNumeric("1234567890") → true
                                     // => Step 4: hasValidLength("1234567890") → true (length 10)
                                     // => valid4 is true

boolean valid5 = isValidPhone.test("123-ABC");
                                     // => Normalized: "123ABC"
                                     // => isNumeric fails (contains letters)
                                     // => valid5 is false

// HIGHER-ORDER FUNCTION - returns configured function
Function<String, Predicate<String>> startsWithPredicate = prefix ->
    s -> s.startsWith(prefix);       // => Returns Predicate testing prefix

Predicate<String> startsWithA = startsWithPredicate.apply("A");
Predicate<String> startsWithB = startsWithPredicate.apply("B");

boolean testA = startsWithA.test("Apple");
                                     // => testA is true
boolean testB = startsWithB.test("Apple");
                                     // => testB is false

// FUNCTION BUILDER PATTERN
class FunctionBuilder<T, R> {
    private Function<T, R> function;

    public FunctionBuilder(Function<T, R> initial) {
        this.function = initial;
    }

    public FunctionBuilder<T, R> andThen(Function<R, R> next) {
        this.function = this.function.andThen(next);
        return this;                 // => Enables method chaining
    }

    public Function<T, R> build() {
        return this.function;
    }
}

Function<String, Integer> complexPipeline = new FunctionBuilder<>(String::trim)
    .andThen(String::toUpperCase)
    .andThen(s -> s.replace("HELLO", "HI"))
    .andThen(String::length)
    .build();                        // => Builds composed function

Integer result = complexPipeline.apply("  hello world  ");
                                     // => Step 1: trim → "hello world"
                                     // => Step 2: uppercase → "HELLO WORLD"
                                     // => Step 3: replace → "HI WORLD"
                                     // => Step 4: length → 8
                                     // => result is 8
```

**Key Takeaway**: Function composition combines simple functions into complex operations using `andThen()` (left-to-right) or `compose()` (right-to-left). Currying transforms multi-parameter functions into chains of single-parameter functions, enabling partial application (fixing some parameters while leaving others open).

**Why It Matters**: Function composition eliminates nested function calls: `f.andThen(g).andThen(h).apply(x)` is more readable than `h(g(f(x)))`—left-to-right execution matches reading order. Composition builds complex operations from simple, testable building blocks: test `trim`, `uppercase`, `length` independently, then compose into `pipeline`—easier to debug (isolate failing step) and maintain (replace/reorder steps). Currying enables configuration through partial application: define `calculateTaxCurried` once, create specialized functions `salesTax`, `incomeTax` by fixing rate parameter—eliminates code duplication and centralizes logic. Real-world applications: validation pipelines (sanitize → format → business rules), data transformations (parse → enrich → format → serialize), middleware chains (authentication → authorization → rate limiting → business logic), and query builders (filters composed from user selections). Before functional interfaces (Java 8), composition required wrapper classes or anonymous inner classes—verbose, unreadable. Modern Java enables concise composition: `Function.andThen()`, `Predicate.and()`, method references—declarative, type-safe. Mathematical foundations: function composition is associative (`f.andThen(g).andThen(h)` == `f.andThen(g.andThen(h))`) but not commutative (`f.andThen(g)` != `g.andThen(f)`)—order matters. Currying originates from Haskell Curry (mathematician): all multi-parameter functions are curried by default in Haskell/ML. Java requires explicit currying (verbose): `a -> b -> c -> ...`—but enables powerful patterns like partial application and function factories. Performance: composition has zero runtime overhead (inlined by JIT compiler), but creates intermediate Function objects (negligible allocation cost). Use composition for: readability (declarative pipelines), testability (isolated steps), and flexibility (configurable operations).

## Example 69: Immutable Collections and Persistent Data Structures

Java provides several ways to create immutable collections (List.of(), Set.of(), Map.of()). Immutable collections prevent modification after creation, ensuring thread safety and predictable behavior.

```java
import java.util.*;

// IMMUTABLE LIST - List.of() (Java 9+)
List<String> immutableList = List.of("Apple", "Banana", "Cherry");
                                     // => immutableList is ["Apple", "Banana", "Cherry"]
                                     // => Cannot modify (UnsupportedOperationException)

// immutableList.add("Date");        // => UnsupportedOperationException
// immutableList.remove(0);          // => UnsupportedOperationException
// immutableList.set(0, "Apricot");  // => UnsupportedOperationException

// IMMUTABLE SET - Set.of()
Set<Integer> immutableSet = Set.of(1, 2, 3, 4, 5);
                                     // => immutableSet is [1, 2, 3, 4, 5] (order undefined)
                                     // => No duplicates allowed
                                     // => Set.of(1, 2, 2) throws IllegalArgumentException

// immutableSet.add(6);              // => UnsupportedOperationException

// IMMUTABLE MAP - Map.of()
Map<String, Integer> immutableMap = Map.of(
    "Alice", 30,
    "Bob", 25,
    "Charlie", 35
);                                   // => immutableMap is {Alice=30, Bob=25, Charlie=35}
                                     // => Keys must be unique
                                     // => Map.of("A", 1, "A", 2) throws IllegalArgumentException

// immutableMap.put("David", 40);    // => UnsupportedOperationException
// immutableMap.remove("Alice");     // => UnsupportedOperationException

// IMMUTABLE MAP - Map.ofEntries() (for > 10 entries)
import static java.util.Map.entry;

Map<String, String> largeMap = Map.ofEntries(
    entry("key1", "value1"),
    entry("key2", "value2"),
    entry("key3", "value3"),
    entry("key4", "value4"),
    entry("key5", "value5"),
    entry("key6", "value6"),
    entry("key7", "value7"),
    entry("key8", "value8"),
    entry("key9", "value9"),
    entry("key10", "value10"),
    entry("key11", "value11")
);                                   // => Map.of() supports up to 10 entries
                                     // => Use Map.ofEntries() for more

// UNMODIFIABLE VIEW - Collections.unmodifiableList()
List<String> mutableList = new ArrayList<>(List.of("A", "B", "C"));
                                     // => mutableList is mutable ArrayList
List<String> unmodifiableView = Collections.unmodifiableList(mutableList);
                                     // => unmodifiableView wraps mutableList
                                     // => Modifications throw UnsupportedOperationException

// unmodifiableView.add("D");        // => UnsupportedOperationException

// BUT: Original list can still be modified!
mutableList.add("D");                // => Modifies underlying list
System.out.println(unmodifiableView);// => Output: [A, B, C, D]
                                     // => unmodifiableView reflects change!
                                     // => DANGER: not truly immutable

// TRUE IMMUTABILITY - List.copyOf() (Java 10+)
List<String> mutableSource = new ArrayList<>(List.of("X", "Y", "Z"));
List<String> immutableCopy = List.copyOf(mutableSource);
                                     // => Creates defensive copy
                                     // => True immutability

mutableSource.add("W");              // => Modifies original
System.out.println(immutableCopy);   // => Output: [X, Y, Z]
                                     // => immutableCopy unchanged (true immutability)

// DEFENSIVE COPYING - preventing external mutation
class Portfolio {
    private final List<String> stocks;

    // BAD: exposes mutable reference
    public Portfolio(List<String> stocks) {
        this.stocks = stocks;        // => Stores reference to mutable list
                                     // => External code can modify!
    }

    public List<String> getStocks() {
        return stocks;               // => Exposes internal mutable state
    }
}

List<String> myStocks = new ArrayList<>(List.of("AAPL", "GOOGL"));
Portfolio portfolio = new Portfolio(myStocks);
myStocks.add("MSFT");                // => Modifies Portfolio's internal state!
                                     // => portfolio.getStocks() now contains "MSFT"

// GOOD: defensive copying
class SafePortfolio {
    private final List<String> stocks;

    public SafePortfolio(List<String> stocks) {
        this.stocks = List.copyOf(stocks);
                                     // => Defensive copy in constructor
                                     // => External changes don't affect Portfolio
    }

    public List<String> getStocks() {
        return stocks;               // => Returns immutable list (List.copyOf result)
                                     // => Caller cannot modify
    }
}

List<String> safeStocks = new ArrayList<>(List.of("AAPL", "GOOGL"));
SafePortfolio safePortfolio = new SafePortfolio(safeStocks);
safeStocks.add("MSFT");              // => Modifies original list
System.out.println(safePortfolio.getStocks());
                                     // => Output: [AAPL, GOOGL]
                                     // => safePortfolio unchanged (defensive copy)

// IMMUTABLE COLLECTION OPERATIONS - return new collections
List<String> original = List.of("A", "B", "C");
                                     // => original is immutable

// Adding element (returns new list)
List<String> withD = new ArrayList<>(original);
withD.add("D");
List<String> newList = List.copyOf(withD);
                                     // => newList is ["A", "B", "C", "D"]
                                     // => original unchanged

// Simpler with Stream
List<String> withE = Stream.concat(
    original.stream(),
    Stream.of("E")
).toList();                          // => withE is ["A", "B", "C", "E"]
                                     // => original unchanged

// Removing element
List<String> withoutB = original.stream()
    .filter(s -> !"B".equals(s))
    .toList();                       // => withoutB is ["A", "C"]
                                     // => original unchanged

// Updating element
List<String> withLowercase = original.stream()
    .map(String::toLowerCase)
    .toList();                       // => withLowercase is ["a", "b", "c"]
                                     // => original unchanged

// NESTED IMMUTABILITY - immutable collections of immutable objects
record Point(int x, int y) {}        // => Immutable record

List<Point> points = List.of(
    new Point(1, 2),
    new Point(3, 4),
    new Point(5, 6)
);                                   // => Immutable list of immutable records
                                     // => Deep immutability

// Cannot modify list
// points.add(new Point(7, 8));      // => UnsupportedOperationException

// Cannot modify points (records are immutable)
Point first = points.get(0);         // => first is Point[x=1, y=2]
// first.x = 10;                     // => Compile error (no setters in records)

// MUTABLE COLLECTIONS OF IMMUTABLE OBJECTS - shallow immutability
List<Point> mutablePoints = new ArrayList<>(points);
mutablePoints.add(new Point(7, 8));  // => Can add to list
                                     // => List is mutable, but Point objects are immutable

// COMPARISON: Mutable vs Immutable approach
// MUTABLE
class MutableCounter {
    private int count = 0;

    public void increment() {
        count++;                     // => Modifies state
    }

    public int getCount() {
        return count;
    }
}

MutableCounter mut = new MutableCounter();
mut.increment();                     // => count is now 1
mut.increment();                     // => count is now 2
                                     // => Hard to track state changes
                                     // => NOT thread-safe (requires synchronization)

// IMMUTABLE
record ImmutableCounter(int count) {
    public ImmutableCounter increment() {
        return new ImmutableCounter(count + 1);
                                     // => Returns NEW instance
                                     // => Original unchanged
    }
}

ImmutableCounter imm1 = new ImmutableCounter(0);
ImmutableCounter imm2 = imm1.increment();
                                     // => imm2 is ImmutableCounter[count=1]
ImmutableCounter imm3 = imm2.increment();
                                     // => imm3 is ImmutableCounter[count=2]
System.out.println(imm1.count());    // => Output: 0 (original unchanged)
                                     // => Easy to reason about (no hidden state)
                                     // => Thread-safe by default (no mutation)

// PRACTICAL EXAMPLE - event sourcing with immutable collections
record Event(String type, String data, long timestamp) {}

class EventStore {
    private List<Event> events = List.of();
                                     // => Start with empty immutable list

    public void addEvent(Event event) {
        events = Stream.concat(
            events.stream(),
            Stream.of(event)
        ).toList();                  // => Create new list with added event
                                     // => Previous state discarded
    }

    public List<Event> getEvents() {
        return events;               // => Already immutable (List.of/toList result)
    }

    public List<Event> getEventsByType(String type) {
        return events.stream()
            .filter(e -> e.type().equals(type))
            .toList();               // => Returns new immutable filtered list
    }
}

EventStore store = new EventStore();
store.addEvent(new Event("CREATED", "User created", System.currentTimeMillis()));
store.addEvent(new Event("UPDATED", "Email updated", System.currentTimeMillis()));
store.addEvent(new Event("DELETED", "User deleted", System.currentTimeMillis()));

List<Event> allEvents = store.getEvents();
                                     // => allEvents is immutable
List<Event> createdEvents = store.getEventsByType("CREATED");
                                     // => createdEvents is immutable filtered view
```

**Key Takeaway**: Use List.of(), Set.of(), Map.of() for truly immutable collections (Java 9+). List.copyOf() creates defensive copies. Collections.unmodifiableList() creates views that reflect underlying changes. Immutable collections prevent modification (UnsupportedOperationException), ensuring thread safety and predictable behavior. "Updates" return new collections, preserving originals.

**Why It Matters**: Immutable collections eliminate entire categories of bugs: race conditions (multiple threads can't modify simultaneously), unexpected side effects (called methods can't change your collections), and defensive copying overhead (safe to share references). Thread safety without synchronization: immutable collections are inherently thread-safe—no locks, no volatile variables, no coordination needed. Safe sharing: pass immutable collections to untrusted code without fear of modification—no defensive copying required. Before Java 9, creating immutable collections was verbose: `Collections.unmodifiableList(Arrays.asList("A", "B", "C"))`—error-prone (original Arrays.asList list is mutable). Modern syntax: `List.of("A", "B", "C")`—concise, guaranteed immutable. Defensive copying prevents bugs: storing mutable collections exposes internal state to external modification. Pattern: copy on input (constructor), copy on output (getter)—but List.of()/List.copyOf() removes output copying need (already immutable). Real-world usage: configuration (immutable settings), domain events (event sourcing: append-only), API responses (prevent client modification), caches (stable keys/values), and concurrent processing (share data across threads). Performance: immutable collections are memory-efficient (no capacity overhead like ArrayList), cache-friendly (stable references), but "updates" create new collections (copy-on-write)—acceptable for infrequent updates, problematic for frequent mutations. When to use: default to immutable (explicit mutability when needed), data structures with infrequent changes, thread-shared data, public APIs (prevent misuse). When to avoid: frequent mutations (use mutable internally, expose immutable copies), performance-critical hot paths (profile first), large collections with many updates (consider persistent data structures like Vavr). Modern Java trend: immutability by default—records (immutable by design), text blocks (immutable strings), and sealed classes (immutable hierarchies).

## Example 70: Functional Error Handling with Try and Either

Functional error handling uses types (Try, Either) to represent success/failure explicitly, avoiding exceptions in functional pipelines. Try encapsulates computations that may throw exceptions. Either represents values with two possibilities (Left for errors, Right for success).

```mermaid
graph TD
    Input["Input: risky operation"]
    Try["Try"]
    Success["Success<br/>Result wrapped"]
    Failure["Failure<br/>Exception wrapped"]

    Input --> Try
    Try -->|no exception| Success
    Try -->|exception thrown| Failure

    style Input fill:#0173B2,stroke:#000,color:#fff
    style Try fill:#029E73,stroke:#000,color:#fff
    style Success fill:#0173B2,stroke:#000,color:#fff
    style Failure fill:#DE8F05,stroke:#000,color:#000
```

**Code**:

```java
import java.util.function.*;
import java.util.Optional;

// TRY MONAD - encapsulates computation that may fail
// Note: Not in Java standard library, shown conceptually
// (Use Vavr library in production: io.vavr.control.Try)

sealed interface Try<T> {
    boolean isSuccess();
    boolean isFailure();
    T get();
    Throwable getError();

    static <T> Try<T> success(T value) {
        return new Success<>(value);
    }

    static <T> Try<T> failure(Throwable error) {
        return new Failure<>(error);
    }

    static <T> Try<T> of(Supplier<T> supplier) {
        try {
            return success(supplier.get());
                                     // => Executes supplier
                                     // => Wraps result in Success
        } catch (Exception e) {
            return failure(e);       // => Catches exception
                                     // => Wraps in Failure
        }
    }

    <U> Try<U> map(Function<T, U> mapper) {
        if (isSuccess()) {
            try {
                return success(mapper.apply(get()));
                                     // => Transform value if success
            } catch (Exception e) {
                return failure(e);   // => Exception during mapping
            }
        }
        return (Try<U>) this;        // => Propagate failure
    }

    <U> Try<U> flatMap(Function<T, Try<U>> mapper) {
        if (isSuccess()) {
            try {
                return mapper.apply(get());
                                     // => Unwraps nested Try
            } catch (Exception e) {
                return failure(e);
            }
        }
        return (Try<U>) this;
    }

    T orElse(T defaultValue) {
        return isSuccess() ? get() : defaultValue;
                                     // => Returns value or default
    }

    record Success<T>(T value) implements Try<T> {
        public boolean isSuccess() { return true; }
        public boolean isFailure() { return false; }
        public T get() { return value; }
        public Throwable getError() { throw new UnsupportedOperationException("Success has no error"); }
    }

    record Failure<T>(Throwable error) implements Try<T> {
        public boolean isSuccess() { return false; }
        public boolean isFailure() { return true; }
        public T get() { throw new RuntimeException(error); }
        public Throwable getError() { return error; }
    }
}

// TRY - basic usage
Try<Integer> tryParse = Try.of(() -> Integer.parseInt("42"));
                                     // => Attempts parsing
                                     // => tryParse is Success[42]

Try<Integer> tryParseFail = Try.of(() -> Integer.parseInt("invalid"));
                                     // => NumberFormatException caught
                                     // => tryParseFail is Failure[NumberFormatException]

// TRY - retrieving values
if (tryParse.isSuccess()) {
    Integer value = tryParse.get();  // => value is 42
    System.out.println("Parsed: " + value);
                                     // => Output: Parsed: 42
}

if (tryParseFail.isFailure()) {
    Throwable error = tryParseFail.getError();
    System.out.println("Error: " + error.getMessage());
                                     // => Output: Error: For input string: "invalid"
}

// TRY - with default value
Integer value1 = tryParse.orElse(0);
                                     // => value1 is 42 (success)
Integer value2 = tryParseFail.orElse(0);
                                     // => value2 is 0 (failure, uses default)

// TRY - mapping transformations
Try<Integer> doubled = tryParse.map(n -> n * 2);
                                     // => Success[42] → Success[84]
                                     // => doubled is Success[84]

Try<Integer> doubledFail = tryParseFail.map(n -> n * 2);
                                     // => Failure propagates (no mapping)
                                     // => doubledFail is Failure[NumberFormatException]

// TRY - chaining operations
Try<String> result = Try.of(() -> Integer.parseInt("100"))
    .map(n -> n * 2)                 // => 100 → 200
    .map(n -> n + 50)                // => 200 → 250
    .map(Object::toString);          // => 250 → "250"
                                     // => result is Success["250"]

Try<String> resultFail = Try.of(() -> Integer.parseInt("bad"))
    .map(n -> n * 2)                 // => Failure, skips mapping
    .map(n -> n + 50)                // => Skipped
    .map(Object::toString);          // => Skipped
                                     // => resultFail is Failure[NumberFormatException]

// TRY - flatMap for nested Tries
Try<Integer> divideByTwo(Integer n) {
    return Try.of(() -> {
        if (n % 2 != 0) throw new IllegalArgumentException("Not divisible by 2");
        return n / 2;
    });
}

Try<Integer> chainedTry = Try.of(() -> Integer.parseInt("20"))
    .flatMap(this::divideByTwo)      // => 20 / 2 = 10
    .flatMap(this::divideByTwo);     // => 10 / 2 = 5
                                     // => chainedTry is Success[5]

Try<Integer> chainedFail = Try.of(() -> Integer.parseInt("21"))
    .flatMap(this::divideByTwo);     // => 21 not divisible by 2
                                     // => chainedFail is Failure[IllegalArgumentException]

// EITHER MONAD - represents two possible values
// Left = error/failure, Right = success
sealed interface Either<L, R> {
    boolean isLeft();
    boolean isRight();
    L getLeft();
    R getRight();

    static <L, R> Either<L, R> left(L value) {
        return new Left<>(value);
    }

    static <L, R> Either<L, R> right(R value) {
        return new Right<>(value);
    }

    <U> Either<L, U> map(Function<R, U> mapper) {
        if (isRight()) {
            return right(mapper.apply(getRight()));
                                     // => Transform right value
        }
        return (Either<L, U>) this;  // => Propagate left
    }

    <U> Either<L, U> flatMap(Function<R, Either<L, U>> mapper) {
        if (isRight()) {
            return mapper.apply(getRight());
                                     // => Unwraps nested Either
        }
        return (Either<L, U>) this;
    }

    R orElse(R defaultValue) {
        return isRight() ? getRight() : defaultValue;
    }

    record Left<L, R>(L value) implements Either<L, R> {
        public boolean isLeft() { return true; }
        public boolean isRight() { return false; }
        public L getLeft() { return value; }
        public R getRight() { throw new UnsupportedOperationException("Left has no right value"); }
    }

    record Right<L, R>(R value) implements Either<L, R> {
        public boolean isLeft() { return false; }
        public boolean isRight() { return true; }
        public L getLeft() { throw new UnsupportedOperationException("Right has no left value"); }
        public R getRight() { return value; }
    }
}

// EITHER - validation example
Either<String, Integer> validateAge(String input) {
    try {
        int age = Integer.parseInt(input);
        if (age < 0) {
            return Either.left("Age cannot be negative");
                                     // => Validation error (left)
        }
        if (age > 150) {
            return Either.left("Age too large");
                                     // => Validation error
        }
        return Either.right(age);    // => Valid age (right)
    } catch (NumberFormatException e) {
        return Either.left("Invalid number format");
                                     // => Parse error
    }
}

// EITHER - usage
Either<String, Integer> validAge = validateAge("25");
                                     // => validAge is Right[25]

Either<String, Integer> negativeAge = validateAge("-5");
                                     // => negativeAge is Left["Age cannot be negative"]

Either<String, Integer> invalidAge = validateAge("abc");
                                     // => invalidAge is Left["Invalid number format"]

// EITHER - extracting values
if (validAge.isRight()) {
    Integer age = validAge.getRight();
    System.out.println("Valid age: " + age);
                                     // => Output: Valid age: 25
}

if (negativeAge.isLeft()) {
    String error = negativeAge.getLeft();
    System.out.println("Error: " + error);
                                     // => Output: Error: Age cannot be negative
}

// EITHER - mapping transformations
Either<String, Integer> doubled = validAge.map(age -> age * 2);
                                     // => Right[25] → Right[50]

Either<String, Integer> doubledError = negativeAge.map(age -> age * 2);
                                     // => Left propagates (no mapping)
                                     // => doubledError is Left["Age cannot be negative"]

// EITHER - chaining validations
Either<String, String> formatAge(Integer age) {
    return Either.right(age + " years old");
}

Either<String, String> result2 = validateAge("30")
    .flatMap(age -> formatAge(age));
                                     // => validateAge → Right[30]
                                     // => formatAge → Right["30 years old"]
                                     // => result2 is Right["30 years old"]

Either<String, String> result3 = validateAge("-10")
    .flatMap(age -> formatAge(age));
                                     // => validateAge → Left["Age cannot be negative"]
                                     // => formatAge skipped
                                     // => result3 is Left["Age cannot be negative"]

// PRACTICAL EXAMPLE - user registration with Either
record User(String email, int age, String country) {}

Either<String, String> validateEmail(String email) {
    if (email == null || !email.contains("@")) {
        return Either.left("Invalid email format");
    }
    return Either.right(email);
}

Either<String, Integer> validateUserAge(String ageStr) {
    return validateAge(ageStr);      // => Reuses previous validation
}

Either<String, String> validateCountry(String country) {
    if (country == null || country.isBlank()) {
        return Either.left("Country required");
    }
    return Either.right(country);
}

Either<String, User> registerUser(String email, String ageStr, String country) {
    return validateEmail(email)
        .flatMap(validEmail ->
            validateUserAge(ageStr)
                .flatMap(validAge ->
                    validateCountry(country)
                        .map(validCountry ->
                            new User(validEmail, validAge, validCountry)
                        )
                )
        );                           // => Chains all validations
                                     // => Returns Left on first error
                                     // => Returns Right[User] if all valid
}

// EITHER - registration examples
Either<String, User> success = registerUser("alice@example.com", "25", "USA");
                                     // => All validations pass
                                     // => success is Right[User[email=alice@example.com, age=25, country=USA]]

Either<String, User> emailFail = registerUser("invalid-email", "25", "USA");
                                     // => Email validation fails
                                     // => emailFail is Left["Invalid email format"]

Either<String, User> ageFail = registerUser("alice@example.com", "-5", "USA");
                                     // => Age validation fails
                                     // => ageFail is Left["Age cannot be negative"]

// EITHER - extracting results with pattern matching (conceptual)
String message = switch (success) {
    case Either.Right<String, User>(User user) ->
        "User registered: " + user.email();
    case Either.Left<String, User>(String error) ->
        "Registration failed: " + error;
};                                   // => message is "User registered: alice@example.com"

// COMPARISON: Exceptions vs Functional Error Handling
// EXCEPTIONS - imperative
public User registerUserExceptions(String email, String ageStr, String country)
        throws IllegalArgumentException {
    if (email == null || !email.contains("@")) {
        throw new IllegalArgumentException("Invalid email");
                                     // => Exception thrown (non-local control flow)
    }
    int age = Integer.parseInt(ageStr);
                                     // => May throw NumberFormatException
    if (age < 0) {
        throw new IllegalArgumentException("Negative age");
    }
    if (country == null || country.isBlank()) {
        throw new IllegalArgumentException("Country required");
    }
    return new User(email, age, country);
}

// Caller must handle exceptions
try {
    User user = registerUserExceptions("alice@example.com", "25", "USA");
    System.out.println("Success: " + user);
} catch (IllegalArgumentException | NumberFormatException e) {
    System.out.println("Error: " + e.getMessage());
}                                    // => Verbose, error-prone (easy to miss exceptions)

// FUNCTIONAL - declarative
Either<String, User> user = registerUser("alice@example.com", "25", "USA");
String result4 = user.isRight()
    ? "Success: " + user.getRight()
    : "Error: " + user.getLeft();
                                     // => Concise, type-safe
                                     // => Errors explicit in type signature

// ADVANTAGES OF FUNCTIONAL ERROR HANDLING
// 1. Type safety - errors explicit in return type
// 2. No exceptions - pure functional pipeline
// 3. Composable - chain operations with map/flatMap
// 4. Explicit - caller must handle both cases
// 5. Testable - pure functions, no side effects
```

**Key Takeaway**: Try encapsulates computations that may throw exceptions, wrapping results in Success or Failure. Either represents values with two possibilities: Left (error/failure) and Right (success). Both enable functional error handling without exceptions, maintaining type safety and composability. Use map() for transformations and flatMap() for chaining operations that return Try/Either.

**Why It Matters**: Exceptions break functional pipelines: they use non-local control flow (throwing traverses stack), are invisible in type signatures (callers don't know what exceptions to expect), and force imperative error handling (try-catch blocks). Try and Either make errors explicit: `Either<String, User>` clearly states "returns error String OR User"—compiler enforces handling both cases. Functional error handling enables composition: `validateEmail().flatMap(validateAge).flatMap(validateCountry).map(createUser)`—declarative pipeline without try-catch nesting. Contrast with exceptions: `try { validateEmail(); try { validateAge(); try { validateCountry(); return createUser(); } catch ... } catch ... } catch ...`—deeply nested, error-prone. Real-world applications: validation chains (form processing: field1 → field2 → field3 → submit), API responses (parse → validate → transform → encode), batch processing (process each item, collect successes/failures separately), and parsing (parse → extract → transform → validate). Performance: Try/Either create wrapper objects (negligible overhead), but avoid exception throwing cost (stack trace generation is expensive). Libraries providing Try/Either: Vavr (io.vavr.control.Try, io.vavr.control.Either), Functional Java (fj.data.Either), and custom implementations. Pattern origins: Haskell Either, Scala Try/Either, Rust Result<T, E>—functional languages embrace error-as-value pattern. When to use: business logic (validation, transformation), pure functional code (no side effects), composable pipelines (chained operations), and type-safe error handling (compiler-verified). When to avoid: exceptional conditions (truly unexpected failures warrant exceptions), I/O operations (exceptions appropriate for network/file errors), framework integration (Spring/Jakarta require exception-based handling), and legacy code (mixing paradigms confusing). Modern trend: functional error handling gaining adoption—Kotlin Result, Swift Result, Rust Result are mainstream. Java lacks standard Try/Either (consider Vavr), but pattern is powerful: explicit errors, type-safe composition, and pure functional pipelines.

## Type Safety in Java

Master compile-time type safety through modern Java features: sealed classes for exhaustive handling, Optional for null safety, JSpecify annotations for null-checking, records for immutable value objects, and type-safe patterns that prevent runtime errors.

## Example 71: Records for Immutable Value Objects

Records provide automatic immutability with validation in compact constructors, eliminating boilerplate while ensuring type safety through invariant enforcement at construction time.

```java
// RECORD - immutable data carrier
public record AccountId(String value) {
    // => Automatically generates: constructor, getters, equals, hashCode, toString
    // => All fields are final and immutable

    // COMPACT CONSTRUCTOR - validates invariants
    public AccountId {
        // => Runs before field assignment
        // => 'value' parameter is in scope, not field yet
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Account ID cannot be blank");
            // => Prevents construction of invalid instances
        }
        if (!value.matches("ACC-\\d{10}")) {
            // => Regex validation: ACC- prefix + 10 digits
            throw new IllegalArgumentException("Invalid account ID format");
            // => Type system guarantees: if AccountId exists, it's valid
        }
    }
}

// USAGE - compile-time type safety
public void processPayment(AccountId fromAccount, AccountId toAccount, BigDecimal amount) {
    // => Type system prevents passing wrong parameters
    // => Cannot pass String as AccountId
    // => Cannot pass BigDecimal as AccountId
    System.out.println("Transfer " + amount + " from " + fromAccount.value()
        + " to " + toAccount.value());
    // => Output: Transfer 1000 from ACC-1234567890 to ACC-9876543210
}

AccountId validAccount = new AccountId("ACC-1234567890");
// => validAccount.value() is "ACC-1234567890" (guaranteed valid format)

// COMPILE ERROR - cannot mutate
// validAccount.value = "different"; // => final field

// RUNTIME ERROR - validation fails
try {
    AccountId invalid = new AccountId("INVALID");
} catch (IllegalArgumentException e) {
    System.out.println(e.getMessage());
    // => Output: Invalid account ID format
}
```

**Key Takeaway**: Records combine immutability (all fields final), validation (compact constructor), and boilerplate elimination (auto-generated methods) to create type-safe value objects. Compact constructors run before field assignment, enabling invariant validation that prevents invalid instances from existing.

**Why It Matters**: Traditional Java value objects require 50+ lines of boilerplate: private final fields, constructor with validation, getters, equals(), hashCode(), toString(). Records compress this to ~10 lines while maintaining type safety. Validation in compact constructors establishes class invariants: if an AccountId object exists, it's guaranteed valid—no need to re-validate in every method. This "parse, don't validate" pattern shifts errors left: invalid data rejected at construction (compile-time proximity) rather than during business logic (runtime). Type-safe value objects prevent primitive obsession: `processPayment(String from, String to)` allows passing unvalidated strings, mixing up parameter order, or forgetting validation. Records with validation ensure: `processPayment(AccountId from, AccountId to)` - compiler prevents type mismatches, parameter order errors caught at compile-time, and validation guaranteed by type system. Real-world applications: domain-driven design (entity IDs, value objects, domain primitives), API boundaries (request/response DTOs with validation), configuration (type-safe config values), and event sourcing (immutable event payloads). Performance: records have minimal overhead vs manual classes—JVM optimizes compact constructors, final fields enable JIT optimizations, and zero-cost abstraction for value objects. Immutability benefits: thread-safe by default (no synchronization needed), safe to share across boundaries (defensive copying unnecessary), and cacheable (hashCode stable across lifetime). Records support all Java features: implements interfaces, generic type parameters, custom methods (beyond auto-generated), static members, and nested classes. When to use records: immutable data (DTOs, events, config), validated value objects (enforce invariants), API contracts (request/response shapes), and domain primitives (type-safe wrappers). When to avoid: mutable state (records are inherently immutable), complex validation (compact constructors limited—use builder pattern), inheritance (records cannot extend classes, only implement interfaces), and framework requirements (some frameworks need no-arg constructors or setters).

## Example 72: Sealed Classes for Exhaustive Type Hierarchies

Sealed classes restrict which types can implement/extend them, enabling compiler-verified exhaustive pattern matching without default cases—catch missing cases at compile-time, not runtime.

```java
// SEALED INTERFACE - restricts permitted implementations
public sealed interface PaymentStatus
    permits Pending, Processing, Completed, Failed {
    // => Only listed types can implement this interface
    // => Compiler knows complete set of subtypes
}

// PERMITTED IMPLEMENTATIONS
public record Pending() implements PaymentStatus {}
    // => Empty record - no data needed for pending state

public record Processing(String transactionId) implements PaymentStatus {}
    // => transactionId tracks in-flight transaction

public record Completed(String transactionId, LocalDateTime completedAt)
    implements PaymentStatus {}
    // => Captures completion metadata

public record Failed(String transactionId, String errorMessage)
    implements PaymentStatus {}
    // => Stores failure reason

// EXHAUSTIVE PATTERN MATCHING
public String describeStatus(PaymentStatus status) {
    return switch (status) {
        // => Compiler verifies all cases covered
        case Pending() -> "Payment pending approval";
            // => Matches Pending instances (no fields to extract)
        case Processing(var txId) -> "Processing transaction " + txId;
            // => Deconstructs Processing, extracts transactionId into txId
        case Completed(var txId, var completedAt) ->
            "Completed " + txId + " at " + completedAt;
            // => Extracts both fields from Completed
        case Failed(var txId, var error) ->
            "Failed " + txId + ": " + error;
            // => Extracts transactionId and errorMessage
        // NO DEFAULT NEEDED - compiler knows all cases handled!
        // => Adding new permitted type causes compile error here
    };
}

// USAGE
PaymentStatus status1 = new Pending();
System.out.println(describeStatus(status1));
    // => Output: Payment pending approval

PaymentStatus status2 = new Processing("TX-12345");
System.out.println(describeStatus(status2));
    // => Output: Processing transaction TX-12345

PaymentStatus status3 = new Completed("TX-12345", LocalDateTime.now());
System.out.println(describeStatus(status3));
    // => Output: Completed TX-12345 at 2026-02-03T10:30:00

// TYPE SAFETY - adding new status requires updating all switches
// If you add: public record Cancelled(...) implements PaymentStatus {}
// Compiler ERROR: describeStatus() must handle Cancelled case!
```

**Key Takeaway**: Sealed types create closed type hierarchies where the compiler knows all possible subtypes. Pattern matching with sealed types requires no default case—the compiler verifies exhaustiveness. Adding new subtypes triggers compile errors in all pattern matches, forcing updates.

**Why It Matters**: Traditional polymorphism with open hierarchies (non-sealed interfaces/classes) allows unknown implementations at runtime—pattern matching requires default cases ("catch-all"), which silently swallow new subtypes without compile errors. Example: if PaymentStatus is open, adding Cancelled compiles without errors—existing switches use default case, potentially causing incorrect behavior (e.g., treating Cancelled as generic "other"). Sealed types make illegal states unrepresentable: if PaymentStatus permits only 4 types, no 5th type can exist—compiler-enforced domain model. This enables "parse, don't validate" at the type level: instead of `if (status.equals("completed"))` (string comparison, typo-prone, case-sensitive), use `case Completed()` (compile-time verified, refactor-safe). Real-world applications: state machines (order status: draft → submitted → approved → shipped → delivered), result types (success/failure with typed errors), protocol messages (request/response variants in RPC), and domain events (event sourcing: OrderCreated, OrderShipped, OrderCancelled). Exhaustiveness checking catches regression bugs: refactoring adds new state → all pattern matches must handle it → compiler identifies missing logic. Contrast with enums: enums are sealed types for constants (no associated data), sealed classes/records hold data per variant. When enums insufficient (each variant needs different fields), use sealed types. Performance: sealed types enable JIT optimizations—compiler converts switches to jump tables (O(1) lookup), devirtualizes polymorphic calls (sealed hierarchy known at compile-time), and eliminates null checks (exhaustive patterns). Pattern matching deconstruction extracts fields without casting: `case Completed(var txId, var completedAt)` vs traditional `if (status instanceof Completed c) { String txId = c.transactionId(); ... }`—less boilerplate, more declarative. Java's sealed types inspired by algebraic data types (ADTs) in functional languages: Haskell's `data`, Scala's `sealed trait`, Rust's `enum`. When to use sealed types: closed domain models (finite states), exhaustive error handling (typed errors), protocol definitions (message variants), and state machines (lifecycle states). When to avoid: extensible hierarchies (plugin architectures need open types), evolving APIs (adding permitted types breaks binary compatibility), and simple enums (no per-variant data needed). Sealed types available in Java 17+ (finalized feature).

## Example 73: Optional for Null Safety

Optional explicitly represents the presence or absence of a value, eliminating NullPointerException by forcing callers to handle both cases through type-safe API.

```java
// BAD - null encourages NullPointerException
public User findUserById(String userId) {
    // => Returns null if not found
    // => Caller might forget null check → NPE at runtime
    return userDatabase.get(userId);  // => null if userId not found
}

User user = findUserById("USER-123");
String email = user.getEmail();  // => NPE if user is null!

// GOOD - Optional makes absence explicit
public Optional<User> findUserById(String userId) {
    // => Type system forces caller to handle absence
    // => Optional<User> clearly states "may or may not contain User"
    return Optional.ofNullable(userDatabase.get(userId));
        // => ofNullable: wraps null as Optional.empty()
        // => ofNullable: wraps non-null as Optional.of(value)
}

// SAFE USAGE 1 - map and orElse
Optional<User> userOpt = findUserById("USER-123");
    // => userOpt is Optional.empty() or Optional.of(user)

String email = userOpt
    .map(User::getEmail)
        // => If present: extracts email → Optional.of(email)
        // => If empty: skips map → Optional.empty()
    .orElse("no-email@example.com");
        // => If present: returns email
        // => If empty: returns default value
System.out.println(email);
    // => Output: user@example.com (if found) or no-email@example.com (if not found)

// SAFE USAGE 2 - pattern matching (Java 21+)
String displayName = switch (userOpt) {
    case Optional.of(User u) -> u.getFullName();
        // => Matches present value, extracts User into u
    case Optional.empty() -> "Unknown User";
        // => Matches absent value
};
System.out.println(displayName);
    // => Output: John Doe (if found) or Unknown User (if not found)

// SAFE USAGE 3 - ifPresent for side effects
userOpt.ifPresent(u -> System.out.println("Found user: " + u.getFullName()));
    // => Executes lambda only if Optional contains value
    // => No output if empty

// SAFE USAGE 4 - orElseThrow for required values
try {
    User requiredUser = userOpt.orElseThrow(() ->
        new UserNotFoundException("USER-123"));
        // => Throws if Optional is empty
        // => Returns User if present
} catch (UserNotFoundException e) {
    System.out.println("Error: " + e.getMessage());
        // => Output: Error: User not found: USER-123
}
```

**Key Takeaway**: Optional replaces null with explicit type-level absence representation. Use `map()` to transform values, `orElse()`/`orElseGet()` for defaults, `orElseThrow()` for required values, and pattern matching for exhaustive handling. Never call `get()` without checking `isPresent()`.

**Why It Matters**: Tony Hoare called null his "billion-dollar mistake"—NullPointerException is Java's most common runtime error. Null is invisible in type signatures: `User findUser(String id)` doesn't indicate null is possible—callers must remember to check (often forgotten). Optional makes absence explicit: `Optional<User> findUser(String id)` forces caller to handle both cases—compiler won't allow `userOpt.getEmail()` (no such method), must unwrap via `map()/orElse()`. This shifts errors left: forgot null check → NPE at runtime (production), forgot Optional handling → compile error (development). Optional API encourages functional patterns: chaining with `map()`/`flatMap()`, avoiding null checks, and declarative error handling. Contrast: null requires imperative checks (`if (user != null) { ... } else { ... }`), Optional enables declarative pipelines (`userOpt.map(...).orElse(...)`). Real-world applications: database queries (findById may not exist), configuration values (optional settings with defaults), API responses (nullable fields), and validation results (success or absence). Performance: Optional creates wrapper object (allocation overhead)—avoid in hot loops or primitive types (use OptionalInt/OptionalLong/OptionalDouble instead). When to use Optional: return values (make absence explicit), optional fields in domain models (nullable properties), and API contracts (document nullability). When to avoid Optional: method parameters (use overloads or @Nullable annotation instead), instance fields (memory overhead—use null with @Nullable), primitive values (use specialized OptionalInt/Long/Double), and collections (prefer empty list over Optional<List>). Common mistake: calling `get()` without checking—defeats purpose of Optional. Use `orElse()`/`orElseThrow()` instead. Optional is monadic: `map()` for transformations, `flatMap()` for chaining operations that return Optional, and `filter()` for conditional presence. Example: `userOpt.map(User::getAddress).flatMap(Address::getZipCode).filter(zip -> zip.startsWith("90")).orElse("Unknown")`—declarative, null-safe, composable. Java's Optional inspired by functional languages: Haskell's Maybe, Scala's Option, Rust's Option<T>. Modern languages embrace optional types: Kotlin's nullable types (String?), Swift's optionals (String?), TypeScript's union types (string | null). When to use Optional: Java 8+ codebases, null-safe APIs, functional-style code. When to avoid: performance-critical code (allocation overhead), legacy compatibility (many libraries expect null), and primitive types (use specialized variants).

## Example 74: JSpecify Annotations for Null Safety

JSpecify provides standardized null safety annotations (@NullMarked, @Nullable) that work with static analysis tools like NullAway to catch null pointer bugs at compile-time.

```java
// PACKAGE-LEVEL NULL SAFETY
// File: package-info.java
@NullMarked  // => All types in package are non-null by default
package com.example.payment;

import org.jspecify.annotations.NullMarked;

// CLASS WITH NULL SAFETY ANNOTATIONS
public class PaymentProcessor {
    // NON-NULL FIELDS (default from @NullMarked)
    private final String processorId;
        // => Cannot be null - guaranteed by @NullMarked
    private final BigDecimal feeRate;
        // => Non-null - must be initialized in constructor

    // NULLABLE FIELD (explicit annotation)
    private final @Nullable String notes;
        // => Explicitly marked nullable - may be null
        // => Static analysis tools track nullability

    // CONSTRUCTOR
    public PaymentProcessor(String processorId, BigDecimal feeRate,
                           @Nullable String notes) {
        // => processorId: non-null (NullAway verifies at compile-time)
        // => feeRate: non-null (NullAway verifies)
        // => notes: nullable (explicitly allowed to be null)

        this.processorId = processorId;
            // => NullAway ERROR if processorId is null
        this.feeRate = feeRate;
            // => NullAway ERROR if feeRate is null
        this.notes = notes;
            // => OK - notes is @Nullable
    }

    // NON-NULL RETURN (default)
    public String getProcessorId() {
        return processorId;
            // => Guaranteed non-null return
            // => NullAway ERROR if returning null here
    }

    // NULLABLE RETURN (explicit)
    public @Nullable String getNotes() {
        return notes;
            // => May return null - caller must handle
    }

    // DEREFERENCING NULLABLE REQUIRES CHECK
    public int getNotesLength() {
        // WRONG - NullAway compile ERROR
        // return notes.length();
        // => ERROR: dereferenced expression 'notes' is @Nullable

        // CORRECT - null check before dereference
        if (notes == null) {
            return 0;
                // => Safe: explicit null check
        }
        return notes.length();
            // => Safe: notes proven non-null by if-check
    }

    // PASSING NULLABLE TO NON-NULL PARAMETER
    public void logProcessor(String message) {
        // => message is non-null (from @NullMarked)
        System.out.println(processorId + ": " + message);
    }

    public void printNotes() {
        // WRONG - NullAway compile ERROR
        // logProcessor(notes);
        // => ERROR: passing @Nullable expression 'notes' to @NonNull parameter

        // CORRECT - check before passing
        if (notes != null) {
            logProcessor(notes);
                // => Safe: notes proven non-null
        } else {
            logProcessor("No notes available");
        }
    }
}

// USAGE
PaymentProcessor processor = new PaymentProcessor(
    "PROC-001",                  // => Non-null processorId
    new BigDecimal("0.029"),     // => Non-null feeRate
    null                         // => Nullable notes (OK)
);

String id = processor.getProcessorId();
    // => id guaranteed non-null by type system

@Nullable String notes = processor.getNotes();
    // => notes may be null - caller must handle
if (notes != null) {
    System.out.println("Notes: " + notes);
        // => Safe dereference after null check
}
```

**Key Takeaway**: JSpecify's @NullMarked makes all types in a package non-null by default. Use @Nullable to explicitly mark parameters/fields/returns that may be null. Static analysis tools (NullAway) enforce null checks at compile-time: dereferencing @Nullable without checks, passing @Nullable to @NonNull parameters, and returning null from @NonNull methods all trigger compile errors.

**Why It Matters**: Null safety annotations shift null pointer detection from runtime (NPE in production) to compile-time (build errors in development). Traditional Java lacks null safety: `String getName()` doesn't indicate if null is possible—developers must guess or read documentation (often outdated). JSpecify standardizes nullability contracts: `@Nullable String` clearly states "may be null", `String` (with @NullMarked) clearly states "never null"—machine-verifiable, always up-to-date. Static analysis tools (NullAway) perform flow-sensitive null checking: tracks null state through control flow, verifies null checks before dereference, and ensures non-null fields initialized. Example: `if (notes != null) { notes.length(); }`—NullAway knows notes is non-null inside if-block. Industry adoption: Spring Boot 4+ uses JSpecify for entire framework, Uber uses NullAway on all Android builds since 2018, and Google recommends JSpecify for new Java code. Performance: zero runtime overhead—annotations are compile-time only, no runtime checks, and no performance impact. Integration: NullAway runs as Error Prone plugin during compilation, IntelliJ IDEA recognizes JSpecify annotations for inspections, and Eclipse JDT supports JSpecify via language server. Real-world impact: Uber reported 98% reduction in NullPointerExceptions after NullAway deployment, Spring Boot migration to JSpecify caught hundreds of latent bugs, and NullAway finds null bugs in 10% of new code at Google. When to use: new Java projects (establish null safety from start), refactoring legacy code (gradual @NullMarked adoption), and API boundaries (document nullability contracts). When to avoid: legacy codebases without buy-in (requires team discipline), performance-critical hot paths (prefer primitives), and frameworks requiring null (some DI frameworks use null injection). Common patterns: prefer Optional for return values (more explicit than @Nullable), use @Nullable for parameters (clearer than overloading), and package-level @NullMarked (default non-null reduces annotations). Comparison to other solutions: JSR-305 (deprecated, fragmented ecosystem), JetBrains annotations (IDE-specific), Checker Framework (more powerful but heavier), and Kotlin's nullable types (language-level, not Java). JSpecify is the future: standardized by industry consortium, adopted by Spring/Google/Uber, and works across tools (IDEs, static analyzers, build systems).

## Example 75: Sealed Classes with Exhaustive Pattern Matching

Combine sealed types with pattern matching to create type-safe state machines where the compiler verifies all states are handled—missing case = compile error, not runtime bug.

```java
// SEALED RESULT TYPE - closed set of outcomes
public sealed interface ValidationResult
    permits Valid, InvalidEmail, InvalidAge, InvalidCountry {
    // => Compiler knows exactly 4 permitted types
}

public record Valid(String email, int age, String country)
    implements ValidationResult {}
    // => Successful validation with extracted data

public record InvalidEmail(String email, String reason)
    implements ValidationResult {}
    // => Email validation failed

public record InvalidAge(int age, String reason)
    implements ValidationResult {}
    // => Age validation failed

public record InvalidCountry(String country, String reason)
    implements ValidationResult {}
    // => Country validation failed

// VALIDATION FUNCTION
public ValidationResult validateUser(String email, String ageStr, String country) {
    // EMAIL VALIDATION
    if (email == null || !email.matches("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$")) {
        return new InvalidEmail(email, "Invalid email format");
            // => Returns typed error with context
    }

    // AGE VALIDATION
    int age;
    try {
        age = Integer.parseInt(ageStr);
            // => Parse string to int
    } catch (NumberFormatException e) {
        return new InvalidAge(-1, "Age must be a number");
            // => Returns typed error for non-numeric age
    }
    if (age < 18 || age > 120) {
        return new InvalidAge(age, "Age must be 18-120");
            // => Returns typed error for out-of-range age
    }

    // COUNTRY VALIDATION
    if (country == null || country.length() != 2) {
        return new InvalidCountry(country, "Country code must be 2 letters");
            // => Returns typed error for invalid country code
    }

    // ALL VALID
    return new Valid(email, age, country);
        // => Returns success with validated data
}

// EXHAUSTIVE PATTERN MATCHING
public String formatResult(ValidationResult result) {
    return switch (result) {
        // => Compiler verifies ALL cases handled
        case Valid(var email, var age, var country) ->
            String.format("Valid user: %s, %d, %s", email, age, country);
            // => Deconstructs Valid record, extracts all fields

        case InvalidEmail(var email, var reason) ->
            String.format("Email error '%s': %s", email, reason);
            // => Deconstructs InvalidEmail, extracts email and reason

        case InvalidAge(var age, var reason) ->
            String.format("Age error %d: %s", age, reason);
            // => Deconstructs InvalidAge, extracts age and reason

        case InvalidCountry(var country, var reason) ->
            String.format("Country error '%s': %s", country, reason);
            // => Deconstructs InvalidCountry, extracts country and reason

        // NO DEFAULT - compiler knows all cases covered!
        // => Adding new ValidationResult subtype causes compile ERROR here
    };
}

// USAGE
ValidationResult result1 = validateUser("alice@example.com", "25", "US");
System.out.println(formatResult(result1));
    // => Output: Valid user: alice@example.com, 25, US

ValidationResult result2 = validateUser("invalid-email", "25", "US");
System.out.println(formatResult(result2));
    // => Output: Email error 'invalid-email': Invalid email format

ValidationResult result3 = validateUser("bob@example.com", "15", "US");
System.out.println(formatResult(result3));
    // => Output: Age error 15: Age must be 18-120

ValidationResult result4 = validateUser("carol@example.com", "30", "USA");
System.out.println(formatResult(result4));
    // => Output: Country error 'USA': Country code must be 2 letters

// TYPE-SAFE ERROR HANDLING
ValidationResult result = validateUser("user@example.com", "25", "US");
if (result instanceof Valid(var email, var age, var country)) {
    // => Pattern matching extracts fields if Valid
    System.out.println("Success: " + email);
        // => Only executes for Valid case
} else {
    System.out.println("Validation failed: " + formatResult(result));
        // => Handles all error cases
}
```

**Key Takeaway**: Sealed types + pattern matching = compiler-verified exhaustiveness. Each validation error is a distinct type carrying relevant context. Pattern matching with deconstruction extracts fields directly: `case Valid(var email, var age, var country)`. Adding new error type breaks all pattern matches—compiler forces updates.

**Why It Matters**: Traditional error handling uses exceptions (invisible in type signatures, non-local control flow) or error codes (lose context, require documentation). Sealed types make errors explicit: `ValidationResult` clearly states "returns Valid OR InvalidEmail OR InvalidAge OR InvalidCountry"—no surprises, no hidden failures. Exhaustive checking prevents bugs: adding new validation rule (e.g., InvalidPhone) causes compile errors in all switches—can't forget to handle new case. Contrast with exceptions: adding new exception type compiles without errors, existing catch-blocks may swallow it with generic `catch (Exception e)`. Pattern matching deconstruction reduces boilerplate: traditional approach requires `if (result instanceof InvalidEmail) { InvalidEmail err = (InvalidEmail) result; String email = err.email(); ... }`—verbose, cast-heavy. Modern pattern matching: `case InvalidEmail(var email, var reason)` extracts fields in one line. Real-world applications: form validation (multiple fields, different error types per field), API request validation (header/body/query param errors), state machines (workflow: draft → submitted → approved → rejected), and protocol handling (message parsing: success/syntax error/semantic error). Type-safe errors enable better UX: each error type carries specific context—InvalidEmail has problematic email string + reason, InvalidAge has problematic age value + reason. Client code can render field-specific errors: "Email 'user@' is invalid: Missing domain" vs generic "Validation failed". Performance: sealed types + pattern matching compile to efficient jump tables (O(1) dispatch), no runtime type checking overhead (compiler generates optimized bytecode), and zero cost abstraction (same performance as if-else chains, better maintainability). Comparison to Either/Try: sealed types are more expressive (N different error types vs single error channel), exhaustiveness-checked (compiler verifies all cases), and Java-native (no library dependency). Use Either for binary success/failure, sealed types for multiple error variants. Sealed types available in Java 17+ (finalized), pattern matching enhanced in Java 21 (record patterns, exhaustiveness). Modern Java encourages sealed types for domain modeling: state machines, result types, protocol messages, and error hierarchies.

## Example 76: Phantom Types for Compile-Time Validation States

Phantom types use generic type parameters to encode state (validated/unvalidated) at compile-time, preventing operations on unvalidated data—type system enforces validation workflow.

```java
// PHANTOM TYPE MARKERS - never instantiated
interface Validated {}
interface Unvalidated {}

// GENERIC DTO WITH VALIDATION STATE
public class PaymentRequest<V> {
    // => Type parameter V encodes validation state
    // => V is phantom: no instances of V exist

    private final String payerId;
    private final BigDecimal amount;
    private final String currency;

    // PRIVATE CONSTRUCTOR - internal only
    private PaymentRequest(String payerId, BigDecimal amount, String currency) {
        this.payerId = payerId;
        this.amount = amount;
        this.currency = currency;
    }

    // FACTORY - creates unvalidated request
    public static PaymentRequest<Unvalidated> create(
            String payerId, BigDecimal amount, String currency) {
        return new PaymentRequest<>(payerId, amount, currency);
            // => Returns PaymentRequest<Unvalidated>
            // => Type states "not yet validated"
    }

    // VALIDATION - transitions type to Validated
    public PaymentRequest<Validated> validate() {
        // => Changes type from Unvalidated to Validated

        if (payerId == null || payerId.isBlank()) {
            throw new ValidationException("Payer ID required");
                // => Validation failure prevents Validated instance
        }
        if (amount == null || amount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new ValidationException("Amount must be positive");
                // => Rejects non-positive amounts
        }
        if (currency == null || currency.length() != 3) {
            throw new ValidationException("Invalid currency code");
                // => Rejects malformed currency codes
        }

        // ALL VALID - create new instance with Validated type
        return new PaymentRequest<>(payerId, amount, currency);
            // => Type is now PaymentRequest<Validated>
            // => Compiler tracks validation state
    }

    // GETTERS
    public String getPayerId() { return payerId; }
    public BigDecimal getAmount() { return amount; }
    public String getCurrency() { return currency; }
}

// SERVICE - only accepts validated requests
public class PaymentService {
    // TYPE CONSTRAINT - must be Validated
    public Payment processPayment(PaymentRequest<Validated> request) {
        // => Compile ERROR if passed PaymentRequest<Unvalidated>
        // => Type system guarantees request is validated!

        return new Payment(
            request.getPayerId(),
            request.getAmount(),
            Currency.getInstance(request.getCurrency())
        );
            // => Safe to use request data - validation guaranteed
    }
}

// USAGE
PaymentService service = new PaymentService();

// STEP 1 - Create unvalidated request
PaymentRequest<Unvalidated> rawRequest = PaymentRequest.create(
    "PAYER-12345",
    new BigDecimal("1000.00"),
    "USD"
);
    // => Type: PaymentRequest<Unvalidated>

// COMPILE ERROR - cannot pass unvalidated to service
// service.processPayment(rawRequest);
// => ERROR: Required PaymentRequest<Validated>, got PaymentRequest<Unvalidated>

// STEP 2 - Validate to change type
PaymentRequest<Validated> validatedRequest = rawRequest.validate();
    // => Type: PaymentRequest<Validated>
    // => Type changed from Unvalidated to Validated

// STEP 3 - Now can pass to service
Payment payment = service.processPayment(validatedRequest);
    // => Compiles successfully - correct type
System.out.println("Payment processed: " + payment);

// VALIDATION FAILURE
try {
    PaymentRequest<Unvalidated> invalidRequest = PaymentRequest.create(
        "",  // => Invalid: blank payer ID
        new BigDecimal("1000.00"),
        "USD"
    );
    invalidRequest.validate();  // => Throws ValidationException
} catch (ValidationException e) {
    System.out.println("Validation failed: " + e.getMessage());
        // => Output: Validation failed: Payer ID required
}
```

**Key Takeaway**: Phantom types use generic parameters to encode state in the type system. `PaymentRequest<Unvalidated>` and `PaymentRequest<Validated>` are different types—compiler prevents passing Unvalidated to methods expecting Validated. Validation transitions type from Unvalidated to Validated.

**Why It Matters**: Runtime validation is error-prone: developers forget to call `validate()`, validation status is invisible in type signatures (`processPayment(PaymentRequest request)` doesn't indicate if validation is required), and validation errors surface at runtime (production). Phantom types shift validation enforcement to compile-time: `processPayment(PaymentRequest<Validated>)` clearly requires validated request, compiler ERROR if passing unvalidated, and bugs caught during development. This creates type-safe state machines: unvalidated → validate() → validated—state transitions encoded in types. Prevents "forgot to validate" bugs: `PaymentRequest<Unvalidated>` cannot be passed to `processPayment()` without compilation error—impossible to use unvalidated data. Real-world applications: builder patterns (incomplete → complete builder states), workflow states (draft → submitted → approved documents), resource lifecycle (opened → used → closed file handles), and security levels (untrusted → sanitized → safe HTML). Pattern origin: Haskell phantom types, used extensively in type-safe API design (e.g., database queries with compile-time schema validation). Performance: zero runtime overhead—phantom types erased to raw types at runtime (type parameter V doesn't exist in bytecode), validation logic runs regardless of pattern, and compile-time type checking only. Phantom types vs runtime flags: runtime approach uses `boolean validated` field (unchecked, easy to forget, couples validation to data structure), phantom approach uses type parameter (compiler-checked, impossible to forget, separation of concerns). Limitations: verbose (separate types for each state), requires generic type parameters (some frameworks don't support), and cannot mix states in collections (List<PaymentRequest<Validated>> cannot hold Unvalidated). When to use phantom types: critical validation (security, financial), state machine enforcement (lifecycle validation), API contracts (require validated inputs), and compile-time safety (prevent runtime errors). When to avoid: simple validation (runtime checks sufficient), dynamic state (state determined at runtime), and framework limitations (dependency injection may require raw types). Phantom types exemplify "make illegal states unrepresentable"—if compiler allows PaymentRequest<Unvalidated> in processPayment(), it's an illegal state. Type system prevents illegal states from compiling.

## Example 77: Smart Constructors for Validated Value Objects

Smart constructors use private constructors with static factory methods to enforce validation—if an instance exists, it's guaranteed valid. Type system prevents invalid instances.

```java
// VALUE OBJECT WITH SMART CONSTRUCTOR
public class EmailAddress {
    // EMAIL REGEX PATTERN
    private static final Pattern EMAIL_PATTERN = Pattern.compile(
        "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
    );
        // => Validates: user@domain.tld format

    private final String value;

    // PRIVATE CONSTRUCTOR - cannot be called externally
    private EmailAddress(String value) {
        // => Only accessible from static factory methods
        // => External code cannot bypass validation
        this.value = value;
    }

    // SMART CONSTRUCTOR - validates before creating instance
    public static EmailAddress of(String email) {
        // => Public factory method - only way to create EmailAddress

        if (email == null || email.isBlank()) {
            throw new IllegalArgumentException("Email cannot be null or blank");
                // => Validation 1: presence check
        }

        if (!EMAIL_PATTERN.matcher(email).matches()) {
            throw new IllegalArgumentException("Invalid email format: " + email);
                // => Validation 2: format check
                // => Throws if regex doesn't match
        }

        // ALL VALID - create instance
        return new EmailAddress(email.toLowerCase().trim());
            // => Normalizes email (lowercase, trimmed)
            // => Returns guaranteed-valid instance
    }

    // ALTERNATIVE - returns Optional instead of throwing
    public static Optional<EmailAddress> tryParse(String email) {
        // => Returns Optional.empty() if validation fails
        // => Returns Optional.of(EmailAddress) if validation succeeds

        try {
            return Optional.of(of(email));
                // => Delegates to throwing version
                // => Wraps in Optional
        } catch (IllegalArgumentException e) {
            return Optional.empty();
                // => Converts exception to Optional.empty()
        }
    }

    public String getValue() {
        return value;
            // => Guaranteed valid email
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof EmailAddress other &&
               this.value.equals(other.value);
            // => Value-based equality
    }

    @Override
    public int hashCode() {
        return value.hashCode();
            // => Consistent with equals
    }

    @Override
    public String toString() {
        return value;
            // => Returns email string
    }
}

// USAGE IN DOMAIN MODEL
public record User(
    String userId,
    String fullName,
    EmailAddress email,  // => Type guarantees validity!
    @Nullable String phoneNumber
) {
    // => email field is EmailAddress, not String
    // => Cannot assign unvalidated String
}

// USAGE
// VALID EMAIL
EmailAddress validEmail = EmailAddress.of("user@example.com");
    // => validEmail.getValue() is "user@example.com" (lowercase, trimmed)
System.out.println("Email: " + validEmail);
    // => Output: Email: user@example.com

User user = new User("USER-001", "Alice Smith", validEmail, null);
    // => Type-safe: email guaranteed valid

// INVALID EMAIL - throws exception
try {
    EmailAddress invalid = EmailAddress.of("not-an-email");
        // => Throws IllegalArgumentException
} catch (IllegalArgumentException e) {
    System.out.println("Error: " + e.getMessage());
        // => Output: Error: Invalid email format: not-an-email
}

// COMPILE ERROR - cannot bypass validation
// EmailAddress email = new EmailAddress("invalid");
// => ERROR: EmailAddress() has private access

// OPTIONAL VERSION - non-throwing
Optional<EmailAddress> emailOpt = EmailAddress.tryParse("maybe@example.com");
if (emailOpt.isPresent()) {
    EmailAddress email = emailOpt.get();
    System.out.println("Parsed: " + email);
        // => Output: Parsed: maybe@example.com
} else {
    System.out.println("Invalid email");
}

// TYPE-SAFE METHOD SIGNATURES
public void sendWelcomeEmail(EmailAddress recipient) {
    // => Parameter type guarantees valid email
    // => No need to re-validate inside method
    System.out.println("Sending welcome email to " + recipient.getValue());
        // => Safe to use recipient.getValue() - guaranteed valid
}
```

**Key Takeaway**: Smart constructors combine private constructors with static factory methods to enforce validation. Private constructor prevents direct instantiation—external code cannot bypass validation. Static factory method (of/tryParse) performs validation before calling private constructor. If EmailAddress instance exists, it's guaranteed valid.

**Why It Matters**: Traditional validation scatters checks throughout codebase: every method accepting `String email` must validate—easy to forget, duplicated logic, runtime errors if missed. Smart constructors centralize validation: validate once at construction, type system guarantees validity thereafter, and methods accepting EmailAddress don't re-validate (already guaranteed). This implements "parse, don't validate": validate at boundary (user input, API request, database read), parse into type-safe value object (EmailAddress), and domain logic uses validated types (EmailAddress, not String). Benefits: compile-time safety (cannot create invalid EmailAddress), refactor-safe (changing validation logic updates one place), and self-documenting (EmailAddress type communicates validation). Real-world applications: domain primitives (UserId, OrderId, ProductSKU), value objects (Money, Percentage, Temperature), constrained types (PositiveInteger, NonEmptyString), and business identifiers (TaxId, AccountNumber, PhoneNumber). Performance: smart constructors add validation overhead at construction, but amortize cost—validate once, use many times without re-checking. Validation cost is O(1) for most checks (regex matching, range validation). Immutability synergy: EmailAddress is immutable (final field, no setters)—validated state cannot change after construction. Thread-safe by default. Comparison to JSR-303 Bean Validation: smart constructors validate at construction (fail-fast), Bean Validation validates mutable beans (fail-late after multiple setters), and smart constructors are type-safe (invalid instances cannot exist), Bean Validation is runtime-checked (invalid beans can exist temporarily). Use smart constructors for immutable domain models, Bean Validation for mutable DTOs/form objects. Common pattern variations: throwing version (of()) for contexts where invalid input is programmer error, Optional version (tryParse()) for contexts where invalid input is expected user error, and Either version (parse()) for contexts needing validation error details. When to use smart constructors: domain-driven design (value objects, entities), type-safe APIs (validated parameters), and immutable data (records, value objects). When to avoid: mutable objects (validation state can become stale), complex validation (multi-field dependencies better suited for builders), and framework requirements (JPA entities need public no-arg constructors). Smart constructors embody "make illegal states unrepresentable"—if EmailAddress exists, it's valid. No such thing as invalid EmailAddress.

## Example 78: Type-Safe Builder Pattern with Compile-Time Completeness

Use nested builder interfaces to enforce required fields at compile-time—cannot call build() until all required fields set. Type system guides usage.

```java
// COMPLEX DOMAIN OBJECT
public class Account {
    private final String accountId;
    private final String ownerName;
    private final BigDecimal balance;
    private final String currency;
    private final LocalDate openedDate;

    // PRIVATE CONSTRUCTOR
    private Account(Builder builder) {
        // => Only accessible from builder
        this.accountId = builder.accountId;
        this.ownerName = builder.ownerName;
        this.balance = builder.balance;
        this.currency = builder.currency;
        this.openedDate = builder.openedDate;
    }

    // BUILDER ENTRY POINT
    public static AccountIdStep builder() {
        return new Builder();
            // => Returns first step interface
            // => Forces setting accountId first
    }

    // STEP INTERFACES - define required field order
    public interface AccountIdStep {
        OwnerNameStep accountId(String accountId);
            // => Next step: OwnerNameStep
    }

    public interface OwnerNameStep {
        BalanceStep ownerName(String ownerName);
            // => Next step: BalanceStep
    }

    public interface BalanceStep {
        CurrencyStep balance(BigDecimal balance);
            // => Next step: CurrencyStep
    }

    public interface CurrencyStep {
        OpenedDateStep currency(String currency);
            // => Next step: OpenedDateStep
    }

    public interface OpenedDateStep {
        BuildStep openedDate(LocalDate openedDate);
            // => Final step: BuildStep
    }

    public interface BuildStep {
        Account build();
            // => Only available after all fields set
    }

    // BUILDER IMPLEMENTATION - implements all step interfaces
    private static class Builder implements
            AccountIdStep, OwnerNameStep, BalanceStep,
            CurrencyStep, OpenedDateStep, BuildStep {
        // => Single class implements all interfaces
        // => Type changes as fields are set

        private String accountId;
        private String ownerName;
        private BigDecimal balance;
        private String currency;
        private LocalDate openedDate;

        @Override
        public OwnerNameStep accountId(String accountId) {
            this.accountId = accountId;
                // => Sets accountId field
            return this;
                // => Returns this, but type is now OwnerNameStep
                // => Can only call ownerName() next
        }

        @Override
        public BalanceStep ownerName(String ownerName) {
            this.ownerName = ownerName;
                // => Sets ownerName field
            return this;
                // => Type is now BalanceStep
        }

        @Override
        public CurrencyStep balance(BigDecimal balance) {
            this.balance = balance;
                // => Sets balance field
            return this;
                // => Type is now CurrencyStep
        }

        @Override
        public OpenedDateStep currency(String currency) {
            this.currency = currency;
                // => Sets currency field
            return this;
                // => Type is now OpenedDateStep
        }

        @Override
        public BuildStep openedDate(LocalDate openedDate) {
            this.openedDate = openedDate;
                // => Sets openedDate field (last required field)
            return this;
                // => Type is now BuildStep - build() available!
        }

        @Override
        public Account build() {
            // => All fields guaranteed non-null by type system!
            // => Each step interface ensures field is set
            return new Account(this);
                // => Creates Account with all validated fields
        }
    }

    // GETTERS
    public String getAccountId() { return accountId; }
    public String getOwnerName() { return ownerName; }
    public BigDecimal getBalance() { return balance; }
    public String getCurrency() { return currency; }
    public LocalDate getOpenedDate() { return openedDate; }
}

// USAGE - compiler enforces all required fields
Account account = Account.builder()
    .accountId("ACC-12345")           // => Step 1: AccountIdStep → OwnerNameStep
    .ownerName("Alice Smith")         // => Step 2: OwnerNameStep → BalanceStep
    .balance(new BigDecimal("10000")) // => Step 3: BalanceStep → CurrencyStep
    .currency("USD")                  // => Step 4: CurrencyStep → OpenedDateStep
    .openedDate(LocalDate.of(2025, 1, 1)) // => Step 5: OpenedDateStep → BuildStep
    .build();                         // => Step 6: BuildStep - now can build!

System.out.println("Account: " + account.getAccountId()
    + ", Owner: " + account.getOwnerName()
    + ", Balance: " + account.getBalance()
    + " " + account.getCurrency());
    // => Output: Account: ACC-12345, Owner: Alice Smith, Balance: 10000 USD

// COMPILE ERROR - cannot skip required fields
// Account incomplete = Account.builder()
//     .accountId("ACC-12345")
//     .balance(new BigDecimal("10000"))
//     .build();
// => ERROR: No build() method available in BalanceStep!
// => ownerName() must be called before currency()

// COMPILE ERROR - cannot call build() prematurely
// Account incomplete = Account.builder()
//     .accountId("ACC-12345")
//     .build();
// => ERROR: No build() method in OwnerNameStep!
```

**Key Takeaway**: Type-safe builders use step interfaces to enforce required field order at compile-time. Each setter returns the next step interface—build() only available after all required fields set. Compiler prevents skipping fields or calling build() prematurely.

**Why It Matters**: Traditional builders allow incomplete objects: `Account.builder().accountId("ACC-12345").build()` compiles but creates invalid Account (missing owner, balance, etc.)—fails at runtime with NPE or validation error. Type-safe builders shift completeness checking to compile-time: cannot call build() until all required fields set—compiler guides developer through required steps, prevents incomplete objects, and self-documents required fields through type signatures. Real-world applications: complex domain objects (many required fields), immutable configuration (builders for config objects), API request builders (enforce required parameters), and fluent APIs (guide users through valid usage patterns). Performance: type-safe builders have zero runtime overhead vs traditional builders—step interfaces erased at runtime (no reflection, no runtime checks), and same bytecode generated as traditional builder. Readability benefit: step interfaces self-document field order and requirements—looking at builder() signature shows all required steps. Contrast with traditional builder: needs documentation to specify required fields, runtime validation in build() (late error detection), and easy to forget required fields (compiles but fails at runtime). Limitations: verbose (many interfaces), fixed field order (cannot reorder steps), and no optional fields in step chain (optional fields added to BuildStep). Pattern variations: optional fields via BuildStep methods (after required fields), groups of fields via combined steps (SetAddressStep accepts all address fields), and conditional steps (branching based on field values). When to use type-safe builders: complex objects with many required fields, immutable domain models (builders for records/value objects), public APIs (guide users, prevent misuse), and configuration objects (enforce required settings). When to avoid: simple objects (records with compact constructors simpler), optional-heavy models (most fields optional), and dynamic construction (field requirements determined at runtime). Type-safe builders implement "make illegal states unrepresentable"—incomplete Account cannot be represented in type system. IDE autocomplete guides through steps—developer sees only valid next methods. Pattern inspired by Scala's builder pattern, also called "telescoping builder" or "step builder". Modern Java trend: prefer records with builders for complex validated objects—combines immutability, validation, and compile-time completeness.

## Example 79: Generic Type Constraints for Type Safety

Use bounded type parameters to restrict generic types at compile-time, preventing incompatible types and enabling type-safe operations on constrained types.

```java
// UNBOUNDED GENERIC - accepts any type
class UnsafeContainer<T> {
    // => T can be any type: String, Integer, Object, etc.
    private T value;

    public void set(T value) {
        this.value = value;
    }

    public T get() {
        return value;
    }

    // PROBLEM - cannot call Number-specific methods
    // public double doubleValue() {
    //     return value.doubleValue();  // => COMPILE ERROR: T might not be Number
    // }
}

// BOUNDED GENERIC - restricts T to Number subtypes
class NumberContainer<T extends Number> {
    // => T must be Number or subclass: Integer, Double, BigDecimal, etc.
    // => Cannot be String, Object, or other non-Number types

    private T value;

    public void set(T value) {
        this.value = value;
            // => Type-checked: must be Number subtype
    }

    public T get() {
        return value;
    }

    // SAFE - can call Number methods
    public double doubleValue() {
        return value.doubleValue();
            // => Guaranteed to work: T extends Number → has doubleValue()
    }

    public int intValue() {
        return value.intValue();
            // => Guaranteed to work: Number interface defines intValue()
    }

    // TYPE-SAFE COMPARISON
    public boolean isGreaterThan(T other) {
        // => Both value and other are Number subtypes
        return this.doubleValue() > other.doubleValue();
            // => Safe: both have doubleValue() method
    }
}

// MULTIPLE BOUNDS - must satisfy all constraints
class ComparableContainer<T extends Number & Comparable<T>> {
    // => T must extend Number AND implement Comparable<T>
    // => Examples: Integer, Double, BigDecimal (all extend Number and Comparable)
    // => Counter-example: AtomicInteger (extends Number but not Comparable)

    private T value;

    public void set(T value) {
        this.value = value;
    }

    public T get() {
        return value;
    }

    // SAFE - can use Number methods
    public double doubleValue() {
        return value.doubleValue();
            // => From Number bound
    }

    // SAFE - can use Comparable methods
    public boolean isGreaterThan(T other) {
        return value.compareTo(other) > 0;
            // => From Comparable<T> bound
            // => Type-safe comparison: same type T
    }

    public T max(T other) {
        return value.compareTo(other) >= 0 ? value : other;
            // => Returns greater of two values
            // => Type-safe: both are T
    }
}

// USAGE
NumberContainer<Integer> intContainer = new NumberContainer<>();
intContainer.set(42);
    // => Sets value to 42 (Integer)
System.out.println("Double value: " + intContainer.doubleValue());
    // => Output: Double value: 42.0

NumberContainer<BigDecimal> decimalContainer = new NumberContainer<>();
decimalContainer.set(new BigDecimal("123.45"));
    // => Sets value to 123.45 (BigDecimal)
System.out.println("Int value: " + decimalContainer.intValue());
    // => Output: Int value: 123

// COMPILE ERROR - String doesn't extend Number
// NumberContainer<String> stringContainer = new NumberContainer<>();
// => ERROR: Type parameter 'String' is not within its bound

// MULTIPLE BOUNDS USAGE
ComparableContainer<Integer> comparableInt = new ComparableContainer<>();
comparableInt.set(100);
System.out.println("Greater than 50? " + comparableInt.isGreaterThan(50));
    // => Output: Greater than 50? true

Integer maxValue = comparableInt.max(200);
System.out.println("Max: " + maxValue);
    // => Output: Max: 200
```

**Key Takeaway**: Bounded type parameters (T extends SomeType) restrict generic types to specific class hierarchies. Single bound: `T extends Number` allows Number methods. Multiple bounds: `T extends Number & Comparable<T>` requires both Number and Comparable. Compiler verifies type constraints—incompatible types cause compile errors.

**Why It Matters**: Unbounded generics (T without constraints) accept any type—cannot call type-specific methods without casting (unsafe, defeats generics purpose). Bounded generics enable type-safe operations: `T extends Number` guarantees doubleValue() exists, compiler verifies at compile-time, and no runtime casting needed. Multiple bounds enable rich APIs: `T extends Number & Comparable<T>` supports both arithmetic and ordering—single type parameter, multiple capabilities. Performance: zero runtime overhead—type erasure removes bounds at runtime, same bytecode as manual casts. Use bounded generics for: numeric algorithms (require Number), collection constraints (sorted collections require Comparable), and type-safe APIs (restrict valid types). Avoid over-constraining—only add bounds when needed. Bounded generics embody "make illegal states unrepresentable"—NumberContainer<String> is illegal, prevented by compiler.

## Error Handling Patterns

## Example 80: Exception Hierarchy - Checked vs Unchecked

Java distinguishes between checked exceptions (compile-time verified) and unchecked exceptions (runtime errors). Understanding this hierarchy guides when to use each exception type.

```mermaid
graph TD
    Throwable[Throwable<br/>Root of all exceptions]
    Throwable --> Error[Error<br/>System-level failures]
    Throwable --> Exception[Exception<br/>Recoverable conditions]

    Error --> OOM[OutOfMemoryError<br/>JVM out of memory]

    Exception --> Checked[Checked Exceptions<br/>Must handle or declare]
    Exception --> Runtime[RuntimeException<br/>Unchecked, programming errors]

    Checked --> IO[IOException<br/>File/network operations]
    Checked --> SQL[SQLException<br/>Database operations]

    Runtime --> NPE[NullPointerException<br/>Null dereference]
    Runtime --> Illegal[IllegalArgumentException<br/>Invalid method argument]

    style Throwable fill:#0173B2,color:#fff
    style Error fill:#DE8F05,color:#000
    style Exception fill:#029E73,color:#fff
    style Checked fill:#CC78BC,color:#000
    style Runtime fill:#CA9161,color:#000
```

**Checked exception example (IOException)**:

```java
import java.io.*;

// CHECKED EXCEPTION - must handle or declare
public class FileProcessor {
    // => Method signature declares IOException (checked)
    // => Compiler REQUIRES callers to handle this exception
    public String readFile(String path) throws IOException {
        // => throws IOException tells compiler "this method may fail"
        // => Callers must catch IOException or propagate it

        BufferedReader reader = new BufferedReader(new FileReader(path));
            // => FileReader constructor throws IOException (file not found)
            // => BufferedReader constructor throws IOException (initialization)

        String content = reader.readLine();
            // => readLine() throws IOException (read error)

        reader.close();
            // => close() throws IOException (close error)

        return content;
            // => Returns first line of file
    }

    // CALLER - must handle checked exception
    public void processFile() {
        try {
            String content = readFile("data.txt");
                // => Compiler enforces exception handling
                // => Without try-catch: compile error "unhandled exception IOException"
            System.out.println("Content: " + content);
                // => Output: Content: [first line of file]
        } catch (IOException e) {
            // => Catches IOException from readFile()
            System.err.println("File error: " + e.getMessage());
                // => Output: File error: data.txt (No such file or directory)
        }
    }
}
```

**Unchecked exception example (IllegalArgumentException)**:

```java
public class Calculator {
    // NO throws declaration - RuntimeException is unchecked
    public int divide(int numerator, int denominator) {
        // => Method does NOT declare IllegalArgumentException
        // => Compiler does NOT require callers to handle it

        if (denominator == 0) {
            // => Validation check before operation
            throw new IllegalArgumentException("Denominator cannot be zero");
                // => Throws unchecked exception (RuntimeException subclass)
                // => No compile-time enforcement
        }

        return numerator / denominator;
            // => Safe division: denominator guaranteed non-zero
    }

    // CALLER - handling is optional
    public void calculate() {
        // OPTION 1: Handle exception
        try {
            int result = divide(10, 0);
                // => Throws IllegalArgumentException
            System.out.println("Result: " + result);
        } catch (IllegalArgumentException e) {
            // => Catches programming error
            System.err.println("Invalid input: " + e.getMessage());
                // => Output: Invalid input: Denominator cannot be zero
        }

        // OPTION 2: Let exception propagate
        int result2 = divide(10, 2);
            // => Valid input: no exception thrown
            // => result2 is 5
        System.out.println("Result: " + result2);
            // => Output: Result: 5
    }
}
```

**Key Takeaway**: Checked exceptions (IOException, SQLException) require compile-time handling via try-catch or throws declaration—use for recoverable failures. Unchecked exceptions (RuntimeException subclasses like IllegalArgumentException, NullPointerException) don't require handling—use for programming errors that callers shouldn't recover from. Error class represents system failures (OutOfMemoryError)—applications shouldn't catch these.

**Why It Matters**: Exception hierarchy enforces error handling discipline. Checked exceptions make error conditions explicit in method signatures: `readFile() throws IOException` documents that file operations may fail—callers must acknowledge failure possibility, compiler verifies handling, and no silent failures. Unchecked exceptions represent programming errors: `divide(10, 0)` is caller mistake, not recoverable condition—no need to pollute every method signature with `throws IllegalArgumentException`, focus on fixing bug (pass non-zero denominator), and fail fast during development. Error class signals JVM failures: OutOfMemoryError cannot be recovered, catching Error is anti-pattern (masking serious problem), and let JVM handle shutdown gracefully. Design decision: checked for expected failures (network timeout, file not found, database unavailable), unchecked for programming errors (null argument, invalid state, contract violation). Java's checked exception controversy: verbose (forces handling even for unrecoverable errors), but explicit (caller knows method may fail). Modern tendency: prefer unchecked for most cases, use checked only for truly recoverable failures where caller can take alternative action.

## Example 81: Try-Catch-Finally Resource Management

Try-catch-finally provides structured exception handling with guaranteed cleanup. Finally block always executes regardless of success, exception, or return statements.

**Basic try-catch-finally pattern**:

```java
import java.io.*;

public class FileProcessor {
    public void processFile(String path) {
        BufferedReader reader = null;
            // => Initialize outside try block
            // => Accessible in finally block for cleanup

        try {
            // TRY BLOCK - attempt risky operation
            reader = new BufferedReader(new FileReader(path));
                // => May throw FileNotFoundException (file doesn't exist)

            String line = reader.readLine();
                // => May throw IOException (read error)

            System.out.println("First line: " + line);
                // => Output: First line: [content]

        } catch (FileNotFoundException e) {
            // CATCH BLOCK 1 - handle file not found
            // => Catches specific exception type first
            System.err.println("File not found: " + path);
                // => Output: File not found: data.txt

        } catch (IOException e) {
            // CATCH BLOCK 2 - handle other IO errors
            // => Catches broader exception type after specific ones
            // => Order matters: specific before general
            System.err.println("Error reading file: " + e.getMessage());
                // => Output: Error reading file: Connection reset

        } finally {
            // FINALLY BLOCK - always executes
            // => Runs whether try succeeds, exception thrown, or return executed
            // => Guaranteed cleanup point

            if (reader != null) {
                // => Null check: reader may not be initialized if FileNotFoundException
                try {
                    reader.close();
                        // => close() may throw IOException
                        // => Requires nested try-catch (verbose!)
                } catch (IOException e) {
                    // => Catches close() exception separately
                    System.err.println("Error closing file: " + e.getMessage());
                }
            }
        }
    }
}
```

**Finally block execution guarantees**:

```java
public class FinallyExecution {
    // CASE 1: Normal completion
    public String case1Normal() {
        try {
            System.out.println("Try block");
                // => Output: Try block
            return "Success";
                // => Return value prepared
        } finally {
            System.out.println("Finally block");
                // => Output: Finally block
                // => Executes BEFORE return
        }
        // => Final output order: "Try block", "Finally block"
        // => Returns: "Success"
    }

    // CASE 2: Exception thrown
    public String case2Exception() {
        try {
            System.out.println("Try block");
                // => Output: Try block
            throw new RuntimeException("Error");
                // => Exception thrown, control jumps to catch
        } catch (RuntimeException e) {
            System.out.println("Catch block: " + e.getMessage());
                // => Output: Catch block: Error
            return "Handled";
                // => Return value prepared
        } finally {
            System.out.println("Finally block");
                // => Output: Finally block
                // => Executes BEFORE return from catch
        }
        // => Final output order: "Try block", "Catch block: Error", "Finally block"
        // => Returns: "Handled"
    }

    // CASE 3: Return in try, finally still executes
    public int case3ReturnInTry() {
        int value = 10;
        try {
            System.out.println("Try block");
                // => Output: Try block
            return value;
                // => Return statement encountered
                // => value is 10 (captured)
        } finally {
            value = 20;
                // => Modifies value AFTER return captured
                // => Does NOT change return value
            System.out.println("Finally block, value = " + value);
                // => Output: Finally block, value = 20
        }
        // => Final output order: "Try block", "Finally block, value = 20"
        // => Returns: 10 (NOT 20! Return value captured before finally)
    }

    // CASE 4: Finally can override return value (anti-pattern!)
    public String case4FinallyOverride() {
        try {
            return "Try return";
                // => Return prepared: "Try return"
        } finally {
            return "Finally return";
                // => OVERRIDES try return value
                // => Anti-pattern: confusing behavior!
        }
        // => Returns: "Finally return" (finally wins!)
    }
}
```

**Key Takeaway**: Try-catch-finally provides structured exception handling: try block contains risky code, catch blocks handle specific exception types (order specific to general), and finally block guarantees cleanup execution. Finally executes in all cases: normal completion (before return), exception thrown (after catch, before return), and return in try block (finally runs before actual return). Multiple catch blocks enable granular error handling: catch FileNotFoundException before IOException (FileNotFoundException extends IOException), and compiler prevents unreachable catch blocks (general before specific). Finally block return value override is possible but strongly discouraged—confusing semantics.

**Why It Matters**: Finally block solves resource cleanup problem: without finally, cleanup code duplication required (after try success AND in each catch block), and missed cleanup if new exception type added. With finally: single cleanup location, guaranteed execution, and no cleanup code duplication. Critical for resource management: file handles must be closed (prevent resource leaks), database connections must be returned to pool (prevent connection exhaustion), and network sockets must be closed (prevent file descriptor exhaustion). Return value capture timing: try block `return value` captures value at return statement, finally modifications after capture don't affect return value, but finally `return` overrides try/catch return (anti-pattern). Exception handling evolution: try-catch-finally (Java 1.0, manual cleanup, verbose), try-with-resources (Java 7, automatic cleanup for AutoCloseable), and structured concurrency (Java 21+, scoped value propagation). Modern practice: prefer try-with-resources over manual finally for resource cleanup, reserve finally for non-resource cleanup (logging, metrics, transaction commit), and avoid return in finally block (confusing semantics). Finally block doesn't catch exceptions: if finally throws exception, original exception from try/catch is suppressed (use try-with-resources for proper suppression handling).

## Example 82: Try-With-Resources Automatic Cleanup

Try-with-resources automatically closes resources that implement AutoCloseable interface. Resources are closed in reverse order of declaration, even if exceptions occur.

**Manual cleanup vs automatic cleanup**:

```java
import java.io.*;

public class ResourceManagement {
    // MANUAL CLEANUP - verbose, error-prone
    public String manualCleanup(String path) throws IOException {
        BufferedReader reader = null;
            // => Must initialize outside try

        try {
            reader = new BufferedReader(new FileReader(path));
                // => Open resource
            return reader.readLine();
                // => Read data
        } finally {
            if (reader != null) {
                // => Null check required
                try {
                    reader.close();
                        // => Nested try-catch for close()
                } catch (IOException e) {
                    // => Handle close() exception separately
                    e.printStackTrace();
                }
            }
        }
        // => Problems: verbose (nested try-catch), error-prone (forgot null check = NPE)
    }

    // AUTOMATIC CLEANUP - concise, safe
    public String automaticCleanup(String path) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
            // => Resource declared in try(...) parentheses
            // => Must implement AutoCloseable interface
            // => Automatically closed when try block exits

            return reader.readLine();
                // => Read data
        } // => reader.close() called automatically here
          // => Even if exception thrown or return executed
          // => No explicit close() needed

        // => Benefits: concise (no manual close), safe (always closed), proper exception handling
    }
}
```

**Multiple resources - reverse order closing**:

```java
import java.io.*;

public class MultipleResources {
    public void copyFile(String source, String dest) throws IOException {
        try (
            // Resource 1: opened first
            BufferedReader reader = new BufferedReader(new FileReader(source));
                // => reader created first

            // Resource 2: opened second
            BufferedWriter writer = new BufferedWriter(new FileWriter(dest))
                // => writer created second
        ) {
            String line;
            while ((line = reader.readLine()) != null) {
                // => Read each line from source
                writer.write(line);
                    // => Write to destination
                writer.newLine();
                    // => Add newline
            }

        } // => Automatic closing happens here
          // => Order: writer.close() called FIRST (reverse order)
          // => Then: reader.close() called SECOND
          // => Reverse order ensures dependent resources closed first
    }
}
```

**Custom AutoCloseable resource**:

```java
// CUSTOM RESOURCE - implements AutoCloseable
class DatabaseConnection implements AutoCloseable {
    private boolean closed = false;

    public DatabaseConnection(String url) {
        System.out.println("Opening connection to: " + url);
            // => Output: Opening connection to: jdbc:mysql://localhost
            // => Resource initialization
    }

    public void executeQuery(String sql) {
        if (closed) {
            // => Check if resource already closed
            throw new IllegalStateException("Connection closed");
        }
        System.out.println("Executing: " + sql);
            // => Output: Executing: SELECT * FROM users
    }

    @Override
    public void close() {
        // => Required by AutoCloseable interface
        // => Called automatically by try-with-resources

        if (!closed) {
            System.out.println("Closing database connection");
                // => Output: Closing database connection
            closed = true;
                // => Mark as closed
        }
    }
}

// USAGE - automatic cleanup
public class DatabaseExample {
    public void queryDatabase() {
        try (DatabaseConnection conn = new DatabaseConnection("jdbc:mysql://localhost")) {
            // => Opens connection
            // => Output: Opening connection to: jdbc:mysql://localhost

            conn.executeQuery("SELECT * FROM users");
                // => Output: Executing: SELECT * FROM users

        } // => conn.close() called automatically
          // => Output: Closing database connection
          // => Guaranteed cleanup even if exception thrown
    }
}
```

**Exception suppression with try-with-resources**:

```java
import java.io.*;

class ProblematicResource implements AutoCloseable {
    private final String name;

    public ProblematicResource(String name) {
        this.name = name;
    }

    public void useResource() throws IOException {
        throw new IOException("Error using " + name);
            // => Primary exception from resource use
    }

    @Override
    public void close() throws IOException {
        throw new IOException("Error closing " + name);
            // => Secondary exception from close()
    }
}

public class SuppressionExample {
    public void demonstrateSuppression() {
        try (ProblematicResource resource = new ProblematicResource("db")) {
            // => Resource created successfully

            resource.useResource();
                // => Throws IOException: "Error using db"
                // => This is PRIMARY exception

        } catch (IOException e) {
            // => Catches primary exception from useResource()
            System.out.println("Primary exception: " + e.getMessage());
                // => Output: Primary exception: Error using db

            // SUPPRESSED EXCEPTIONS - from close()
            Throwable[] suppressed = e.getSuppressed();
                // => Array of exceptions suppressed during cleanup
                // => suppressed.length is 1

            for (Throwable s : suppressed) {
                System.out.println("Suppressed exception: " + s.getMessage());
                    // => Output: Suppressed exception: Error closing db
            }
        }

        // => Primary exception preserved: "Error using db"
        // => Close exception attached as suppressed: "Error closing db"
        // => Both exceptions available for debugging
    }
}
```

**Key Takeaway**: Try-with-resources automatically closes resources implementing AutoCloseable interface. Resources declared in try(...) parentheses are closed automatically when try block exits (normal completion, exception, or return). Multiple resources close in reverse order of declaration: last opened, first closed. Exception suppression: if both try block and close() throw exceptions, try exception is primary, close exception becomes suppressed exception accessible via getSuppressed().

**Why It Matters**: Manual resource cleanup is error-prone: forgot to close (resource leak), close() throws exception (complicates error handling), and null check required (verbose). Try-with-resources eliminates these issues: guaranteed cleanup (even with exception or return), proper exception handling (suppressed exceptions), and no null checks (resource initialized in try statement). Real-world impact: resource leaks cause production outages (file descriptor exhaustion crashes server), memory leaks from unclosed connections (heap exhaustion), and database connection pool starvation (blocked transactions). AutoCloseable contract: close() method is idempotent (safe to call multiple times), close() releases resources (file handles, network sockets, database connections), and close() should not throw exceptions if possible (simplifies cleanup). Multiple resource order matters: writer depends on reader (close writer first to flush buffers), child resource depends on parent (close child first), and reverse order ensures proper cleanup cascade. Exception suppression preserves debugging context: primary exception is what went wrong in business logic, suppressed exceptions show cleanup failures, and both available in stack trace. Modern practice: always use try-with-resources for AutoCloseable objects, implement AutoCloseable for custom resources needing cleanup, and make close() idempotent (track state, guard against double-close).

## Example 83: Multi-Catch Exception Handling

Multi-catch blocks allow handling multiple exception types in single catch block, reducing code duplication. Exceptions are unrelated types (not in same inheritance hierarchy).

**Before multi-catch - duplicated handling**:

```java
import java.io.*;
import java.text.*;

public class BeforeMultiCatch {
    public void parseAndWrite(String number, String file) {
        try {
            // Operation 1: parse number (may throw NumberFormatException)
            int value = Integer.parseInt(number);
                // => ParseException if number is "abc"

            // Operation 2: write to file (may throw IOException)
            PrintWriter writer = new PrintWriter(file);
                // => IOException if file path invalid
            writer.println(value);
            writer.close();

        } catch (NumberFormatException e) {
            // DUPLICATE ERROR HANDLING
            System.err.println("Error: " + e.getMessage());
                // => Output: Error: For input string: "abc"
            logError(e);
                // => Log to file
            notifyAdmin(e);
                // => Send alert

        } catch (IOException e) {
            // DUPLICATE ERROR HANDLING (same as above!)
            System.err.println("Error: " + e.getMessage());
                // => Output: Error: file.txt (Permission denied)
            logError(e);
                // => Log to file
            notifyAdmin(e);
                // => Send alert
        }

        // => Problem: identical handling code duplicated
    }

    private void logError(Exception e) { /* ... */ }
    private void notifyAdmin(Exception e) { /* ... */ }
}
```

**With multi-catch - unified handling**:

```java
import java.io.*;

public class WithMultiCatch {
    public void parseAndWrite(String number, String file) {
        try {
            // Operation 1: parse number (NumberFormatException)
            int value = Integer.parseInt(number);
                // => May throw NumberFormatException

            // Operation 2: write to file (IOException)
            PrintWriter writer = new PrintWriter(file);
                // => May throw IOException
            writer.println(value);
            writer.close();

        } catch (NumberFormatException | IOException e) {
            // MULTI-CATCH: handle both exception types identically
            // => Pipe symbol | separates exception types
            // => e is implicitly final (cannot be reassigned)
            // => Common supertype is Exception (inferred)

            System.err.println("Error: " + e.getMessage());
                // => Handles NumberFormatException: "For input string: 'abc'"
                // => OR IOException: "file.txt (Permission denied)"

            logError(e);
                // => Single logging call for both types
            notifyAdmin(e);
                // => Single notification for both types
        }

        // => Benefit: no code duplication, single error handling path
    }

    private void logError(Exception e) { /* ... */ }
    private void notifyAdmin(Exception e) { /* ... */ }
}
```

**Multi-catch with specific handling before general handling**:

```java
import java.io.*;
import java.sql.*;

public class MixedCatchBlocks {
    public void processData(String file, String query) {
        try {
            // Operation 1: file I/O
            BufferedReader reader = new BufferedReader(new FileReader(file));
                // => May throw IOException
            String data = reader.readLine();
            reader.close();

            // Operation 2: database query
            Connection conn = DriverManager.getConnection("jdbc:mysql://localhost");
                // => May throw SQLException
            Statement stmt = conn.createStatement();
            stmt.executeQuery(query);

            // Operation 3: arithmetic
            int result = 100 / 0;
                // => May throw ArithmeticException

        } catch (FileNotFoundException e) {
            // SPECIFIC HANDLING - file not found requires different action
            System.err.println("File not found: " + e.getMessage());
                // => Output: File not found: data.txt (No such file or directory)
            createDefaultFile();
                // => Special recovery: create default file

        } catch (IOException | SQLException e) {
            // MULTI-CATCH - group related exceptions with same handling
            // => IOException: file read errors (not FileNotFoundException)
            // => SQLException: database errors
            // => Both are infrastructure failures

            System.err.println("Infrastructure error: " + e.getMessage());
                // => Output: Infrastructure error: Connection refused
            retryOperation(e);
                // => Retry makes sense for transient failures

        } catch (ArithmeticException e) {
            // SPECIFIC HANDLING - arithmetic error is programming bug
            System.err.println("Calculation error: " + e.getMessage());
                // => Output: Calculation error: / by zero
            // => Should fix code, not retry
        }
    }

    private void createDefaultFile() { /* ... */ }
    private void retryOperation(Exception e) { /* ... */ }
}
```

**Multi-catch restrictions and rules**:

```java
import java.io.*;

public class MultiCatchRules {
    public void demonstrateRules() {
        try {
            riskyOperation();

        } catch (IOException | IllegalArgumentException e) {
            // RULE 1: Exception parameter is implicitly final
            // e = new RuntimeException();  // ← COMPILE ERROR: cannot assign

            // RULE 2: Can call methods common to all caught types
            e.printStackTrace();
                // => Both IOException and IllegalArgumentException have printStackTrace()
            String message = e.getMessage();
                // => Both have getMessage()

            // RULE 3: Common supertype inferred (Exception in this case)
            // => IOException extends Exception
            // => IllegalArgumentException extends RuntimeException extends Exception
            // => Common type: Exception

            if (e instanceof IOException) {
                // => Can narrow type with instanceof
                IOException ioEx = (IOException) e;
                // => Now can call IOException-specific methods
            }
        }

        // INVALID - cannot multi-catch related types
        // catch (Exception | IOException e) {  // ← COMPILE ERROR
        // => IOException is subclass of Exception
        // => Already caught by Exception, making IOException redundant

        // INVALID - duplicate exception types
        // catch (IOException | IOException e) {  // ← COMPILE ERROR
        // => Cannot list same type multiple times
    }

    private void riskyOperation() throws IOException { /* ... */ }
}
```

**Key Takeaway**: Multi-catch (Java 7+) handles multiple unrelated exception types in single catch block using pipe syntax: `catch (IOException | SQLException e)`. Reduces code duplication when exceptions require identical handling. Exception parameter is implicitly final (cannot reassign). Can only call methods common to all caught exception types. Cannot multi-catch related types (Exception | IOException invalid - IOException already subclass of Exception).

**Why It Matters**: Code duplication is error-prone: identical catch blocks require synchronized updates (change one, must change all), and easy to miss updating one branch (inconsistent error handling). Multi-catch eliminates duplication: single error handling logic for multiple exception types, and easier maintenance (update once). When to use multi-catch: exceptions unrelated in hierarchy (IOException and SQLException are independent), and exceptions handled identically (same logging, retry, recovery). When NOT to use multi-catch: exceptions in same hierarchy (catch parent type instead), or exceptions need different handling (use separate catch blocks). Multi-catch infers common supertype: `IOException | SQLException` → common type is Exception, can only call Exception methods (getMessage(), printStackTrace()), and cannot call SQLException.getSQLState() without instanceof check. Final parameter prevents accidental reassignment: `e = new RuntimeException()` would lose original exception, and compiler prevents this bug. Catch block ordering still matters: specific before general (FileNotFoundException before IOException), multi-catch can appear before or after specific catches, and most specific matching catch block executes. Performance: zero overhead at runtime—same bytecode as separate catch blocks, compiler syntax sugar, and no runtime type checking beyond normal exception dispatch.

## Example 84: Exception Chaining and Root Cause Preservation

Exception chaining preserves root cause when wrapping exceptions in higher-level abstractions. Enables complete stack trace across abstraction layers.

**Problem: Lost root cause**:

```java
import java.io.*;

// BAD - loses original exception information
public class LostRootCause {
    public String loadConfiguration(String filename) throws ConfigurationException {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
                // => May throw FileNotFoundException
            String config = reader.readLine();
            reader.close();
            return config;

        } catch (IOException e) {
            // WRONG - creates new exception without cause
            throw new ConfigurationException("Failed to load configuration");
                // => Original IOException lost!
                // => No stack trace from file operation
                // => Debugging nightmare: where did it fail?
        }
    }
}

class ConfigurationException extends Exception {
    public ConfigurationException(String message) {
        super(message);
    }
}
```

**Solution: Exception chaining with cause**:

```java
import java.io.*;

// GOOD - preserves root cause through exception chain
public class PreservedRootCause {
    public String loadConfiguration(String filename) throws ConfigurationException {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
                // => May throw FileNotFoundException (root cause)
            String config = reader.readLine();
                // => May throw IOException (root cause)
            reader.close();
            return config;

        } catch (IOException e) {
            // CORRECT - wraps IOException as cause
            throw new ConfigurationException("Failed to load config from: " + filename, e);
                // => First parameter: high-level error message
                // => Second parameter: root cause (IOException)
                // => Complete stack trace preserved
        }
    }
}

class ConfigurationException extends Exception {
    // CONSTRUCTOR - accepts cause parameter
    public ConfigurationException(String message, Throwable cause) {
        super(message, cause);
            // => Calls Exception(String, Throwable) constructor
            // => Establishes exception chain: ConfigurationException → IOException
    }
}

// USAGE
public class ChainedExceptionDemo {
    public void demonstrateChain() {
        PreservedRootCause config = new PreservedRootCause();
        try {
            config.loadConfiguration("missing.txt");
                // => File doesn't exist

        } catch (ConfigurationException e) {
            // HIGH-LEVEL EXCEPTION
            System.err.println("Error: " + e.getMessage());
                // => Output: Error: Failed to load config from: missing.txt

            // ROOT CAUSE ACCESS
            Throwable cause = e.getCause();
                // => Returns the original IOException
                // => cause.getClass() is FileNotFoundException

            System.err.println("Root cause: " + cause.getClass().getSimpleName());
                // => Output: Root cause: FileNotFoundException

            System.err.println("Root message: " + cause.getMessage());
                // => Output: Root message: missing.txt (No such file or directory)

            // FULL STACK TRACE
            e.printStackTrace();
                // => Output shows complete chain:
                // => ConfigurationException: Failed to load config from: missing.txt
                // =>   at PreservedRootCause.loadConfiguration(...)
                // => Caused by: java.io.FileNotFoundException: missing.txt
                // =>   at java.io.FileInputStream.<init>(...)
        }
    }
}
```

**Multi-layer exception chaining**:

```java
// LAYER 1: Data access layer
class DatabaseException extends Exception {
    public DatabaseException(String message, Throwable cause) {
        super(message, cause);
    }
}

class UserRepository {
    public String findUser(String id) throws DatabaseException {
        try {
            // Simulate database query (throws SQLException)
            throw new java.sql.SQLException("Connection timeout");
                // => LAYER 0: Root cause (infrastructure)

        } catch (java.sql.SQLException e) {
            throw new DatabaseException("Failed to query user: " + id, e);
                // => LAYER 1: Wraps SQLException
                // => DatabaseException → SQLException
        }
    }
}

// LAYER 2: Service layer
class ServiceException extends Exception {
    public ServiceException(String message, Throwable cause) {
        super(message, cause);
    }
}

class UserService {
    private UserRepository repository = new UserRepository();

    public String getUser(String id) throws ServiceException {
        try {
            return repository.findUser(id);
                // => May throw DatabaseException

        } catch (DatabaseException e) {
            throw new ServiceException("User service error for ID: " + id, e);
                // => LAYER 2: Wraps DatabaseException
                // => ServiceException → DatabaseException → SQLException
        }
    }
}

// LAYER 3: Controller layer
class Controller {
    private UserService service = new UserService();

    public void handleRequest(String userId) {
        try {
            String user = service.getUser(userId);
                // => May throw ServiceException
            System.out.println("User: " + user);

        } catch (ServiceException e) {
            // NAVIGATE EXCEPTION CHAIN
            System.err.println("Top-level error: " + e.getMessage());
                // => Output: Top-level error: User service error for ID: 123

            Throwable cause1 = e.getCause();
                // => cause1 is DatabaseException
            System.err.println("Layer 1 cause: " + cause1.getMessage());
                // => Output: Layer 1 cause: Failed to query user: 123

            Throwable cause2 = cause1.getCause();
                // => cause2 is SQLException (root cause)
            System.err.println("Root cause: " + cause2.getMessage());
                // => Output: Root cause: Connection timeout

            // FULL CHAIN PRINTOUT
            e.printStackTrace();
                // => ServiceException: User service error for ID: 123
                // =>   at UserService.getUser(...)
                // => Caused by: DatabaseException: Failed to query user: 123
                // =>   at UserRepository.findUser(...)
                // => Caused by: java.sql.SQLException: Connection timeout
                // =>   at UserRepository.findUser(...)
        }
    }
}
```

**Key Takeaway**: Exception chaining preserves root cause when wrapping exceptions: use `throw new HighLevelException(message, originalException)` to establish chain. Call `getCause()` to access wrapped exception. Stack trace includes all layers from high-level exception down to root cause. Enables debugging across abstraction boundaries—see infrastructure failure (SQLException) from high-level service exception.

**Why It Matters**: Lost root causes destroy debugging productivity: high-level "Configuration failed" says WHAT failed but not WHY (file missing? Permission denied? Network timeout?), and developers waste hours reproducing issues. Exception chaining solves this: root cause preserved through all layers (controller → service → repository → JDBC driver), and single stack trace shows complete failure path. Real-world example: web request fails with "Service unavailable"—with chaining: ServiceException → DatabaseException → SQLException: "Connection pool exhausted", without chaining: ServiceException with no context (check database? Check network? Check connection pool? No clue!). Layer abstraction benefits: controller doesn't know about SQLException (proper encapsulation), service translates infrastructure exceptions to domain exceptions, but root cause still accessible via getCause() (best of both worlds). Exception chain navigation: getCause() returns immediate cause (one layer down), iterate cause chain for root cause: `while (ex.getCause() != null) ex = ex.getCause()`, and modern stack traces print entire chain automatically. initCause() alternative to constructor: if exception class doesn't accept cause in constructor, can call initCause() after construction (only once! Second call throws IllegalStateException). Suppressed exceptions vs chained exceptions: chained exceptions for wrapping (translation across layers), and suppressed exceptions for cleanup failures (try-with-resources). Performance: exception creation is expensive (captures stack trace), but chaining adds negligible overhead (just storing reference), and same exception performance characteristics as non-chained. Testing exception chains: verify high-level exception type (assertEquals(ServiceException.class, e.getClass())), verify root cause type (assertInstanceOf(SQLException.class, getRootCause(e))), and verify error messages at each layer.

## Example 85: Optional for Null Safety

Optional<T> provides type-safe alternative to null for representing absence of value. Prevents NullPointerException by making absence explicit in method signatures.

```mermaid
graph LR
    Method["Method Returns<br/>Optional&lt;String&gt;"] --> Present["Value Present<br/>Optional.of#40;'value'#41;"]
    Method --> Empty["Value Absent<br/>Optional.empty#40;#41;"]

    Present --> Get["get#40;#41;<br/>Returns 'value'"]
    Empty --> GetError["get#40;#41;<br/>Throws NoSuchElementException"]

    Present --> OrElse["orElse#40;'default'#41;<br/>Returns 'value'"]
    Empty --> OrElseDefault["orElse#40;'default'#41;<br/>Returns 'default'"]

    style Method fill:#0173B2,color:#fff
    style Present fill:#029E73,color:#fff
    style Empty fill:#DE8F05,color:#000
    style Get fill:#029E73,color:#fff
    style GetError fill:#CC78BC,color:#000
    style OrElse fill:#029E73,color:#fff
    style OrElseDefault fill:#029E73,color:#fff
```

**Problem: Null returns are implicit**:

```java
// BAD - null is invisible in method signature
public class UserRepository {
    public String findUserEmail(String userId) {
        // => Method signature doesn't indicate email might be absent
        // => Callers don't know to check for null

        if (userId.equals("123")) {
            return "user@example.com";
                // => Found: return email
        }
        return null;
            // => Not found: return null (implicit absence)
            // => NullPointerException waiting to happen!
    }
}

// DANGEROUS USAGE
String email = repository.findUserEmail("456");
    // => email is null (user not found)
    // => No compiler warning!

int length = email.length();
    // => NullPointerException! Runtime crash!
```

**Solution: Optional makes absence explicit**:

```java
import java.util.Optional;

// GOOD - Optional makes absence explicit in signature
public class UserRepository {
    public Optional<String> findUserEmail(String userId) {
        // => Return type is Optional<String>
        // => Signals "email might be absent"
        // => Callers forced to handle absence

        if (userId.equals("123")) {
            return Optional.of("user@example.com");
                // => PRESENT: wrap value in Optional
                // => Optional.of() throws NPE if argument is null (fail-fast)
        }
        return Optional.empty();
            // => ABSENT: return empty Optional
            // => Type-safe representation of "no value"
    }
}

// SAFE USAGE - multiple ways to handle absence
public class UserService {
    private UserRepository repository = new UserRepository();

    public void demonstrateOptional() {
        // PATTERN 1: isPresent() + get() (verbose but explicit)
        Optional<String> emailOpt = repository.findUserEmail("123");
            // => emailOpt is Optional<String> (not null!)

        if (emailOpt.isPresent()) {
            // => Check if value is present
            // => true for "123", false for "456"

            String email = emailOpt.get();
                // => SAFE: only call get() after isPresent() check
                // => email is "user@example.com"

            System.out.println("Email: " + email);
                // => Output: Email: user@example.com
        } else {
            System.out.println("Email not found");
                // => Output: Email not found
        }

        // PATTERN 2: orElse() - provide default value
        String email2 = repository.findUserEmail("456")
            .orElse("no-email@example.com");
                // => If present: return value
                // => If absent: return default "no-email@example.com"
                // => email2 is always non-null String
        System.out.println("Email: " + email2);
            // => Output: Email: no-email@example.com

        // PATTERN 3: orElseGet() - lazy default (computed on demand)
        String email3 = repository.findUserEmail("456")
            .orElseGet(() -> generateDefaultEmail());
                // => If present: return value (generateDefaultEmail() NOT called)
                // => If absent: call generateDefaultEmail() and return result
                // => Lazy evaluation: only compute default if needed

        // PATTERN 4: orElseThrow() - convert absence to exception
        try {
            String email4 = repository.findUserEmail("456")
                .orElseThrow(() -> new UserNotFoundException("User 456 not found"));
                    // => If present: return value
                    // => If absent: throw exception
        } catch (UserNotFoundException e) {
            System.err.println("Error: " + e.getMessage());
                // => Output: Error: User 456 not found
        }

        // PATTERN 5: ifPresent() - consume value if present
        repository.findUserEmail("123")
            .ifPresent(email -> System.out.println("Found: " + email));
                // => If present: execute lambda with value
                // => If absent: do nothing
                // => Output: Found: user@example.com

        // PATTERN 6: map() - transform value if present
        String domain = repository.findUserEmail("123")
            .map(email -> email.substring(email.indexOf('@') + 1))
                // => If present: extract domain from "user@example.com"
                // => Returns Optional<String> containing "example.com"
                // => If absent: returns Optional.empty()
            .orElse("unknown");
                // => Provide default for absent case
        System.out.println("Domain: " + domain);
            // => Output: Domain: example.com
    }

    private String generateDefaultEmail() {
        return "generated-" + System.currentTimeMillis() + "@example.com";
    }
}

class UserNotFoundException extends Exception {
    public UserNotFoundException(String message) {
        super(message);
    }
}
```

**Optional chaining with flatMap**:

```java
import java.util.Optional;

class User {
    private final String id;
    private final Address address;  // => May be null

    public User(String id, Address address) {
        this.id = id;
        this.address = address;
    }

    public Optional<Address> getAddress() {
        return Optional.ofNullable(address);
            // => ofNullable: returns empty() if address is null
            // => of: throws NPE if address is null
    }
}

class Address {
    private final String city;  // => May be null

    public Address(String city) {
        this.city = city;
    }

    public Optional<String> getCity() {
        return Optional.ofNullable(city);
    }
}

public class OptionalChaining {
    public String getUserCity(User user) {
        // NESTED OPTIONALS - need flatMap
        return Optional.ofNullable(user)
                // => Wrap user in Optional (handles null user)
            .flatMap(User::getAddress)
                // => If user present: call getAddress() → returns Optional<Address>
                // => flatMap unwraps: Optional<Optional<Address>> → Optional<Address>
                // => If user absent: returns Optional.empty()
            .flatMap(Address::getCity)
                // => If address present: call getCity() → returns Optional<String>
                // => flatMap unwraps: Optional<Optional<String>> → Optional<String>
                // => If address absent: returns Optional.empty()
            .orElse("Unknown");
                // => If city present: return city
                // => If any step absent: return "Unknown"

        // => SAFE: handles null user, null address, null city
        // => WITHOUT OPTIONAL: 3 null checks required!
    }

    public void demonstrateChaining() {
        User user1 = new User("1", new Address("Jakarta"));
        System.out.println(getUserCity(user1));
            // => Output: Jakarta

        User user2 = new User("2", new Address(null));
        System.out.println(getUserCity(user2));
            // => Output: Unknown (city is null)

        User user3 = new User("3", null);
        System.out.println(getUserCity(user3));
            // => Output: Unknown (address is null)

        System.out.println(getUserCity(null));
            // => Output: Unknown (user is null)
    }
}
```

**Key Takeaway**: Optional<T> makes absence explicit in method signatures, preventing NullPointerException through type-safe handling. Create Optional with: of(value) for non-null values (throws NPE if null), empty() for absence, ofNullable(value) for potentially null values. Extract value with: get() after isPresent() check (throws NoSuchElementException if empty), orElse(default) for fallback value, orElseGet(supplier) for lazy default, orElseThrow() to convert absence to exception. Transform Optional with: map() for simple transformation (Optional<T> → Optional<U>), flatMap() for nested Optionals (prevents Optional<Optional<T>>), and filter() to conditionally keep value.

**Why It Matters**: Null references are "billion-dollar mistake" (Tony Hoare): NullPointerException is most common Java exception, null checks scattered throughout codebase (defensive programming), and method signatures don't indicate nullable returns (implicit contract). Optional solves this: return type Optional<String> explicitly signals "value might be absent", compiler doesn't prevent null (still language feature), but convention makes absence visible. Optional anti-patterns: DON'T use for fields (just make field nullable and document), DON'T call get() without isPresent() (defeats purpose—use orElse instead), DON'T use ofNullable everywhere (use of() to enforce non-null, fail-fast), and DON'T return null from method returning Optional (return Optional.empty()). Optional benefits: explicit absence in API (self-documenting), chainable operations (map, flatMap, filter), and functional style encourages handling absence. Performance overhead: Optional is object allocation (heap overhead), but negligible for non-hot paths, and avoid Optional in tight loops (primitive streams, direct null checks). When to use Optional: return types for methods that may not find value (repository queries, config lookups), and method parameters rarely (nullable parameters discouraged, use method overloading). Java Optional vs Scala/Kotlin: Scala has Option type (Some/None sealed hierarchy), Kotlin has nullable types (String? vs String with compiler enforcement), and Java Optional is library-based (no language-level enforcement). map vs flatMap: map for transformations returning plain values (Optional<String> → Optional<Integer>), and flatMap for transformations returning Optional (prevents Optional<Optional<T>>).

## Example 86: Custom Result Type for Functional Error Handling

Result<T, E> type represents computation that may succeed with value T or fail with error E. Enables functional error handling without exceptions.

**Custom Result implementation**:

```java
// RESULT TYPE - sealed for exhaustiveness
public sealed interface Result<T, E> permits Success, Failure {
    // => Sealed: only Success and Failure can implement Result
    // => Compiler enforces handling both cases

    // FACTORY METHODS
    static <T, E> Result<T, E> success(T value) {
        return new Success<>(value);
            // => Wrap successful value
    }

    static <T, E> Result<T, E> failure(E error) {
        return new Failure<>(error);
            // => Wrap error value
    }

    // QUERY METHODS
    boolean isSuccess();
    boolean isFailure();

    // TRANSFORMATION METHODS
    <U> Result<U, E> map(Function<T, U> mapper);
        // => Transform success value

    <U> Result<U, E> flatMap(Function<T, Result<U, E>> mapper);
        // => Chain operations that return Result

    T orElse(T defaultValue);
        // => Extract value or provide default

    T orElseThrow(Function<E, ? extends RuntimeException> exceptionMapper);
        // => Convert to exception if failure
}

// SUCCESS CASE - contains value
record Success<T, E>(T value) implements Result<T, E> {
    // => Record: immutable, automatic equals/hashCode/toString
    // => value is the successful result

    @Override
    public boolean isSuccess() { return true; }

    @Override
    public boolean isFailure() { return false; }

    @Override
    public <U> Result<U, E> map(Function<T, U> mapper) {
        return Result.success(mapper.apply(value));
            // => Apply transformation to value
            // => Wrap result in new Success
    }

    @Override
    public <U> Result<U, E> flatMap(Function<T, Result<U, E>> mapper) {
        return mapper.apply(value);
            // => Apply transformation that returns Result
            // => Return Result directly (no double-wrapping)
    }

    @Override
    public T orElse(T defaultValue) {
        return value;
            // => Success: return actual value (ignore default)
    }

    @Override
    public T orElseThrow(Function<E, ? extends RuntimeException> exceptionMapper) {
        return value;
            // => Success: return value (no exception)
    }
}

// FAILURE CASE - contains error
record Failure<T, E>(E error) implements Result<T, E> {
    // => error is the failure reason

    @Override
    public boolean isSuccess() { return false; }

    @Override
    public boolean isFailure() { return true; }

    @Override
    @SuppressWarnings("unchecked")
    public <U> Result<U, E> map(Function<T, U> mapper) {
        return (Result<U, E>) this;
            // => Failure: skip transformation (no value to transform)
            // => Cast is safe: Failure<anything, E> is compatible
    }

    @Override
    @SuppressWarnings("unchecked")
    public <U> Result<U, E> flatMap(Function<T, Result<U, E>> mapper) {
        return (Result<U, E>) this;
            // => Failure: skip transformation
    }

    @Override
    public T orElse(T defaultValue) {
        return defaultValue;
            // => Failure: return default (no actual value)
    }

    @Override
    public T orElseThrow(Function<E, ? extends RuntimeException> exceptionMapper) {
        throw exceptionMapper.apply(error);
            // => Failure: convert error to exception and throw
    }
}
```

**Using Result for error handling**:

```java
// ERROR TYPE - domain-specific errors
enum UserError {
    USER_NOT_FOUND("User not found"),
    INVALID_EMAIL("Invalid email format"),
    DATABASE_ERROR("Database connection failed");

    private final String message;

    UserError(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}

// SERVICE USING RESULT
class UserService {
    public Result<String, UserError> findUserEmail(String userId) {
        // => Returns Result: either Success<String> or Failure<UserError>
        // => No exceptions thrown!

        if (userId == null || userId.isEmpty()) {
            return Result.failure(UserError.INVALID_EMAIL);
                // => Validation failure
        }

        if (userId.equals("123")) {
            return Result.success("user@example.com");
                // => Success case: email found
        }

        return Result.failure(UserError.USER_NOT_FOUND);
            // => Failure case: user doesn't exist
    }

    public Result<String, UserError> getUserDomain(String userId) {
        // => Chain operations using flatMap
        return findUserEmail(userId)
                // => Returns Result<String, UserError>
            .flatMap(email -> extractDomain(email));
                // => Only called if findUserEmail succeeds
                // => extractDomain returns Result<String, UserError>
                // => flatMap unwraps: no nested Result
    }

    private Result<String, UserError> extractDomain(String email) {
        int atIndex = email.indexOf('@');
        if (atIndex < 0) {
            return Result.failure(UserError.INVALID_EMAIL);
                // => Invalid email format
        }
        String domain = email.substring(atIndex + 1);
        return Result.success(domain);
            // => Extracted domain
    }
}

// USAGE - pattern matching with switch expression (Java 21+)
public class ResultDemo {
    public void demonstrateResult() {
        UserService service = new UserService();

        // EXAMPLE 1: Pattern matching (exhaustive)
        Result<String, UserError> result1 = service.findUserEmail("123");

        String message1 = switch (result1) {
            case Success<String, UserError> s -> "Email: " + s.value();
                // => Success case: extract value
                // => Output: Email: user@example.com

            case Failure<String, UserError> f -> "Error: " + f.error().getMessage();
                // => Failure case: extract error
                // => Not executed for userId "123"
        };
        System.out.println(message1);
            // => Output: Email: user@example.com

        // EXAMPLE 2: orElse for default value
        String email2 = service.findUserEmail("456")
            .orElse("no-email@example.com");
                // => Success: return email
                // => Failure: return default "no-email@example.com"
        System.out.println("Email: " + email2);
            // => Output: Email: no-email@example.com

        // EXAMPLE 3: orElseThrow to convert to exception
        try {
            String email3 = service.findUserEmail("456")
                .orElseThrow(error -> new RuntimeException(error.getMessage()));
                    // => Failure: convert UserError to RuntimeException
        } catch (RuntimeException e) {
            System.err.println("Exception: " + e.getMessage());
                // => Output: Exception: User not found
        }

        // EXAMPLE 4: Chaining with flatMap
        Result<String, UserError> domainResult = service.getUserDomain("123");

        switch (domainResult) {
            case Success<String, UserError> s ->
                System.out.println("Domain: " + s.value());
                    // => Output: Domain: example.com

            case Failure<String, UserError> f ->
                System.err.println("Error: " + f.error().getMessage());
                    // => Not executed (success case)
        }

        // EXAMPLE 5: Multiple operations with map
        Result<Integer, UserError> lengthResult = service.findUserEmail("123")
            .map(String::length);
                // => Success("user@example.com") → Success(16)
                // => Failure preserved if findUserEmail failed

        switch (lengthResult) {
            case Success<Integer, UserError> s ->
                System.out.println("Email length: " + s.value());
                    // => Output: Email length: 16

            case Failure<Integer, UserError> f ->
                System.err.println("Error: " + f.error().getMessage());
        }
    }
}
```

**Key Takeaway**: Result<T, E> type explicitly represents success (value of type T) or failure (error of type E) without exceptions. Sealed interface with Success and Failure records enables pattern matching for exhaustive handling. Use map() to transform success value, flatMap() to chain Result-returning operations (prevents nested Result), and orElse()/orElseThrow() to extract value with fallback. Functional error handling: errors are values (not control flow), composable (chain with flatMap), and type-safe (compiler enforces handling both cases).

**Why It Matters**: Exception-based error handling has limitations: exceptions are invisible in method signatures (IOException hidden unless declared), control flow via exceptions is non-local (jumps stack frames), and exceptions encourage catch-all blocks (lost error context). Result type solves this: errors explicit in return type (Result<User, UserError> documents both success and failure), errors are values (can be passed, stored, transformed), and type system enforces handling (sealed types require pattern match covering Success/Failure). Railway-oriented programming: imagine two tracks—success track (green) and failure track (red), operations stay on success track if they succeed, and switch to failure track on first error (no error propagation code needed). flatMap enables composition: operation1().flatMap(r1 → operation2(r1)).flatMap(r2 → operation3(r2)), each operation only executes if previous succeeded, and first failure short-circuits entire chain. Comparison to exceptions: Result is explicit (see Result<T, E> in signature), exceptions are implicit (need to check throws clause or docs), Result is composable (map/flatMap), exceptions require try-catch nesting, Result is pure (no side effects), and exceptions are side effects (control flow). When to use Result: validation pipelines (accumulate errors), functional codebases (avoid exceptions), and explicit error handling (make failures visible). When to use exceptions: integration with existing exception-based APIs, truly exceptional conditions (out of memory, hardware failure), and resource management (try-with-resources for AutoCloseable). Result libraries: Vavr (io.vavr.control.Either), Result4j (functional error handling), and custom implementations (as shown). Performance: Result allocation per operation (heap overhead), but predictable cost (no stack unwinding), and suitable for most use cases (avoid in hot loops).

## Example 87: Retry Pattern with Exponential Backoff

Retry pattern automatically retries failed operations with increasing delays between attempts. Exponential backoff prevents overwhelming failing services.

```mermaid
graph LR
    Attempt1["Attempt 1<br/>Immediate"] --> Fail1{Success?}
    Fail1 -->|No| Wait1["Wait 1s<br/>Backoff"]
    Fail1 -->|Yes| Success[Return Result]

    Wait1 --> Attempt2["Attempt 2<br/>After 1s"]
    Attempt2 --> Fail2{Success?}
    Fail2 -->|No| Wait2["Wait 2s<br/>Exponential"]
    Fail2 -->|Yes| Success

    Wait2 --> Attempt3["Attempt 3<br/>After 2s"]
    Attempt3 --> Fail3{Success?}
    Fail3 -->|No| GiveUp["Max Retries<br/>Throw Exception"]
    Fail3 -->|Yes| Success

    style Attempt1 fill:#0173B2,color:#fff
    style Attempt2 fill:#0173B2,color:#fff
    style Attempt3 fill:#0173B2,color:#fff
    style Success fill:#029E73,color:#fff
    style GiveUp fill:#DE8F05,color:#000
    style Wait1 fill:#CC78BC,color:#000
    style Wait2 fill:#CC78BC,color:#000
```

**Simple retry implementation**:

```java
import java.util.function.Supplier;

public class RetryUtil {
    // RETRY WITH EXPONENTIAL BACKOFF
    public static <T> T retry(
        Supplier<T> operation,
            // => Operation to retry (no parameters, returns T)
        int maxAttempts,
            // => Maximum number of attempts
        long initialDelayMs
            // => Initial delay in milliseconds (doubles each retry)
    ) throws Exception {
        // => Throws exception if all retries fail

        int attempt = 1;
            // => Current attempt number
        long delay = initialDelayMs;
            // => Current delay (increases exponentially)

        while (true) {
            try {
                System.out.println("Attempt " + attempt + " of " + maxAttempts);
                    // => Log attempt number

                T result = operation.get();
                    // => Execute operation
                    // => If succeeds: return immediately

                System.out.println("Success on attempt " + attempt);
                    // => Log success

                return result;
                    // => Return result (exit retry loop)

            } catch (Exception e) {
                // => Operation failed

                if (attempt >= maxAttempts) {
                    // FINAL ATTEMPT FAILED - give up
                    System.err.println("All " + maxAttempts + " attempts failed");
                    throw new Exception("Max retries exceeded", e);
                        // => Throw exception with original cause
                }

                // MORE RETRIES AVAILABLE - wait and retry
                System.err.println("Attempt " + attempt + " failed: " + e.getMessage());
                System.out.println("Retrying in " + delay + "ms...");

                try {
                    Thread.sleep(delay);
                        // => Wait before next attempt
                        // => Exponential backoff: 1s, 2s, 4s, 8s...
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                        // => Restore interrupt status
                    throw new Exception("Retry interrupted", ie);
                }

                attempt++;
                    // => Increment attempt counter
                delay *= 2;
                    // => Double delay (exponential backoff)
                    // => 1000ms → 2000ms → 4000ms → 8000ms
            }
        }
    }
}

// USAGE EXAMPLE
public class RetryDemo {
    private static int failureCount = 0;
        // => Track failures for demonstration

    public static String unreliableOperation() throws Exception {
        failureCount++;
            // => Increment failure counter

        if (failureCount < 3) {
            // SIMULATE TRANSIENT FAILURE
            throw new Exception("Temporary network error");
                // => Fails on attempts 1 and 2
        }

        // SUCCESS ON THIRD ATTEMPT
        return "Operation successful";
            // => Returns on attempt 3
    }

    public static void main(String[] args) {
        try {
            String result = RetryUtil.retry(
                RetryDemo::unreliableOperation,
                    // => Method reference to retry
                5,
                    // => Max 5 attempts
                1000
                    // => Initial delay: 1 second
            );

            System.out.println("Result: " + result);
                // => Output: Result: Operation successful

        } catch (Exception e) {
            System.err.println("Operation failed: " + e.getMessage());
        }

        // => Console output:
        // => Attempt 1 of 5
        // => Attempt 1 failed: Temporary network error
        // => Retrying in 1000ms...
        // => Attempt 2 of 5
        // => Attempt 2 failed: Temporary network error
        // => Retrying in 2000ms...
        // => Attempt 3 of 5
        // => Success on attempt 3
        // => Result: Operation successful
    }
}
```

**Retry with predicate (selective retry)**:

```java
import java.util.function.Predicate;
import java.util.function.Supplier;

public class SelectiveRetry {
    // RETRY ONLY FOR SPECIFIC EXCEPTIONS
    public static <T> T retryIf(
        Supplier<T> operation,
        int maxAttempts,
        long initialDelayMs,
        Predicate<Exception> shouldRetry
            // => Predicate: returns true if should retry this exception
    ) throws Exception {

        int attempt = 1;
        long delay = initialDelayMs;

        while (true) {
            try {
                return operation.get();
                    // => Success: return immediately

            } catch (Exception e) {
                // CHECK IF SHOULD RETRY THIS EXCEPTION
                if (!shouldRetry.test(e)) {
                    // => Exception not retryable (e.g., validation error)
                    System.err.println("Non-retryable exception: " + e.getClass().getSimpleName());
                    throw e;
                        // => Fail immediately (don't retry)
                }

                if (attempt >= maxAttempts) {
                    // => Max retries reached
                    throw new Exception("Max retries exceeded", e);
                }

                System.err.println("Retryable failure on attempt " + attempt);

                try {
                    Thread.sleep(delay);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new Exception("Retry interrupted", ie);
                }

                attempt++;
                delay *= 2;
            }
        }
    }
}

// USAGE - retry only for transient errors
public class SelectiveRetryDemo {
    public static void main(String[] args) {
        try {
            String result = SelectiveRetry.retryIf(
                () -> {
                    // Simulate transient network error
                    throw new java.io.IOException("Connection timeout");
                },
                3,
                    // => Max 3 attempts
                1000,
                    // => Initial delay 1s
                e -> e instanceof java.io.IOException
                    // => Retry ONLY IOException (network errors)
                    // => Don't retry IllegalArgumentException (validation errors)
            );

        } catch (Exception e) {
            System.err.println("Final failure: " + e.getMessage());
                // => Output: Final failure: Max retries exceeded
        }

        // TRY WITH NON-RETRYABLE EXCEPTION
        try {
            SelectiveRetry.retryIf(
                () -> {
                    throw new IllegalArgumentException("Invalid input");
                        // => Validation error (not transient)
                },
                3,
                1000,
                e -> e instanceof java.io.IOException
                    // => Predicate returns false for IllegalArgumentException
            );

        } catch (Exception e) {
            System.err.println("Immediate failure: " + e.getMessage());
                // => Output: Immediate failure: Invalid input
                // => No retries attempted (not retryable exception)
        }
    }
}
```

**Key Takeaway**: Retry pattern automatically retries failed operations up to max attempts with delays between retries. Exponential backoff doubles delay each retry (1s → 2s → 4s → 8s), preventing overwhelming failing service. Selective retry uses predicate to retry only transient failures (network timeout, temporary unavailability), not permanent failures (validation errors, resource not found). Implementation wraps operation in try-catch loop, increments attempt counter, and sleeps between retries with exponential delay.

**Why It Matters**: Distributed systems fail transiently: network packets dropped (TCP retransmits), services temporarily overloaded (load spike), and database connections timeout (connection pool exhausted). Retry pattern handles transient failures automatically: first attempt fails (network blip), second attempt succeeds (network recovered), and user sees success (transparent recovery). Exponential backoff prevents thundering herd: 1000 clients fail simultaneously, all retry immediately (overwhelming failing service further), exponential backoff spreads retries over time (service recovers gracefully). Retry risks: retrying non-idempotent operations causes duplicates (double charge, duplicate order), retrying permanent failures wastes resources (user not found won't fix itself), and infinite retries delay failure detection (circuit breaker needed). When to retry: network timeouts (transient), service unavailable 503 (temporary overload), database deadlock (retry after random delay), and rate limiting 429 (respect Retry-After header). When NOT to retry: validation errors 400 (client bug, won't fix itself), authentication failures 401 (wrong credentials), authorization failures 403 (insufficient permissions), and resource not found 404 (won't appear on retry). Retry configuration: max attempts (3-5 typical), initial delay (100ms-1s), max delay (cap exponential growth), and jitter (randomize delay to prevent synchronized retries). Advanced patterns: decorrelate jitter (random delay within range), circuit breaker (stop retrying failing service), and bulkhead (isolate retry threads). Production considerations: metrics (track retry attempts, success rate), logging (record transient failures for ops team), and timeout (total retry time limit). Libraries: Resilience4j (retry, circuit breaker, rate limiter), Failsafe (retry policies), and Spring Retry (annotation-based retry).

## Example 88: Circuit Breaker Pattern

Circuit breaker prevents cascading failures by stopping requests to failing service. Transitions between CLOSED (normal), OPEN (failing), and HALF_OPEN (testing recovery) states.

```mermaid
graph LR
    Closed[CLOSED State<br/>Requests flow normally] --> FailThreshold{Failure<br/>Threshold<br/>Exceeded?}
    FailThreshold -->|Yes| Open[OPEN State<br/>Requests fail fast]
    FailThreshold -->|No| Closed

    Open --> Timeout{Timeout<br/>Elapsed?}
    Timeout -->|No| Open
    Timeout -->|Yes| HalfOpen[HALF_OPEN State<br/>Test with limited requests]

    HalfOpen --> TestSuccess{Test<br/>Successful?}
    TestSuccess -->|Yes| Closed
    TestSuccess -->|No| Open

    style Closed fill:#029E73,color:#fff
    style Open fill:#DE8F05,color:#000
    style HalfOpen fill:#CC78BC,color:#000
```

**Circuit breaker implementation**:

```java
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

public class CircuitBreaker {
    // STATES
    private enum State {
        CLOSED,      // => Normal operation: requests flow through
        OPEN,        // => Failing: requests fail immediately
        HALF_OPEN    // => Testing: allow limited requests to test recovery
    }

    private State state = State.CLOSED;
        // => Current state (starts CLOSED)

    private final int failureThreshold;
        // => Number of failures to trip breaker (CLOSED → OPEN)

    private final long timeoutMs;
        // => Time to wait before testing recovery (OPEN → HALF_OPEN)

    private final AtomicInteger failureCount = new AtomicInteger(0);
        // => Current consecutive failure count

    private final AtomicLong lastFailureTime = new AtomicLong(0);
        // => Timestamp of last failure (for timeout calculation)

    public CircuitBreaker(int failureThreshold, long timeoutMs) {
        this.failureThreshold = failureThreshold;
            // => e.g., 5 failures trips breaker
        this.timeoutMs = timeoutMs;
            // => e.g., 60000ms (1 minute) before retry
    }

    // EXECUTE OPERATION THROUGH CIRCUIT BREAKER
    public <T> T execute(Supplier<T> operation) throws Exception {
        // CHECK STATE
        synchronized (this) {
            if (state == State.OPEN) {
                // OPEN STATE - check if timeout elapsed
                if (System.currentTimeMillis() - lastFailureTime.get() > timeoutMs) {
                    // => Timeout elapsed: try recovery
                    state = State.HALF_OPEN;
                        // => Transition: OPEN → HALF_OPEN
                    System.out.println("Circuit breaker: OPEN → HALF_OPEN (testing recovery)");
                } else {
                    // => Still within timeout period
                    throw new Exception("Circuit breaker is OPEN (service unavailable)");
                        // => Fail fast: don't attempt request
                        // => Prevents overwhelming failing service
                }
            }
        }

        // ATTEMPT OPERATION
        try {
            T result = operation.get();
                // => Execute operation
                // => May throw exception

            // SUCCESS - reset breaker
            synchronized (this) {
                if (state == State.HALF_OPEN) {
                    state = State.CLOSED;
                        // => Recovery successful: HALF_OPEN → CLOSED
                    System.out.println("Circuit breaker: HALF_OPEN → CLOSED (service recovered)");
                }
                failureCount.set(0);
                    // => Reset failure counter
            }

            return result;
                // => Return successful result

        } catch (Exception e) {
            // FAILURE - increment counter and check threshold
            synchronized (this) {
                int failures = failureCount.incrementAndGet();
                    // => Increment failure count

                lastFailureTime.set(System.currentTimeMillis());
                    // => Record failure timestamp

                if (state == State.HALF_OPEN) {
                    // HALF_OPEN FAILURE - recovery failed
                    state = State.OPEN;
                        // => Transition: HALF_OPEN → OPEN
                    System.out.println("Circuit breaker: HALF_OPEN → OPEN (recovery failed)");

                } else if (failures >= failureThreshold) {
                    // THRESHOLD EXCEEDED - trip breaker
                    state = State.OPEN;
                        // => Transition: CLOSED → OPEN
                    System.out.println("Circuit breaker: CLOSED → OPEN (threshold " +
                                     failureThreshold + " exceeded)");
                }
            }

            throw e;
                // => Re-throw exception
        }
    }

    public State getState() {
        return state;
    }
}

// USAGE EXAMPLE
public class CircuitBreakerDemo {
    private static int requestCount = 0;
        // => Track requests for demonstration

    public static String unreliableService() throws Exception {
        requestCount++;

        if (requestCount <= 7) {
            // SIMULATE FAILING SERVICE (first 7 requests fail)
            throw new Exception("Service unavailable");
        }

        // SERVICE RECOVERS (8th request onwards)
        return "Service response: success";
    }

    public static void main(String[] args) throws InterruptedException {
        CircuitBreaker breaker = new CircuitBreaker(
            5,
                // => Failure threshold: 5 consecutive failures trips breaker
            3000
                // => Timeout: 3 seconds before testing recovery
        );

        // REQUESTS 1-5: CLOSED state, failures increment counter
        for (int i = 1; i <= 5; i++) {
            try {
                breaker.execute(CircuitBreakerDemo::unreliableService);
            } catch (Exception e) {
                System.out.println("Request " + i + " failed: " + e.getMessage());
                    // => Output: Request 1 failed: Service unavailable
                    // => ... (requests 2-4)
                    // => Output: Request 5 failed: Service unavailable
            }
        }
        // => State: CLOSED → OPEN (threshold exceeded)

        System.out.println("Breaker state: " + breaker.getState());
            // => Output: Breaker state: OPEN

        // REQUESTS 6-7: OPEN state, fail immediately
        for (int i = 6; i <= 7; i++) {
            try {
                breaker.execute(CircuitBreakerDemo::unreliableService);
            } catch (Exception e) {
                System.out.println("Request " + i + " failed fast: " + e.getMessage());
                    // => Output: Request 6 failed fast: Circuit breaker is OPEN
                    // => Output: Request 7 failed fast: Circuit breaker is OPEN
                    // => Note: unreliableService() NOT called (fail fast)
            }
        }

        // WAIT FOR TIMEOUT
        System.out.println("Waiting 3 seconds for circuit breaker timeout...");
        Thread.sleep(3000);
            // => Wait for timeout to elapse

        // REQUEST 8: HALF_OPEN state, test recovery
        try {
            String result = breaker.execute(CircuitBreakerDemo::unreliableService);
            System.out.println("Request 8 succeeded: " + result);
                // => Output: Request 8 succeeded: Service response: success
                // => State: HALF_OPEN → CLOSED (recovery successful)
        } catch (Exception e) {
            System.out.println("Request 8 failed: " + e.getMessage());
        }

        System.out.println("Breaker state: " + breaker.getState());
            // => Output: Breaker state: CLOSED

        // REQUEST 9: CLOSED state, normal operation
        try {
            String result = breaker.execute(CircuitBreakerDemo::unreliableService);
            System.out.println("Request 9 succeeded: " + result);
                // => Output: Request 9 succeeded: Service response: success
        } catch (Exception e) {
            System.out.println("Request 9 failed: " + e.getMessage());
        }
    }
}
```

**Key Takeaway**: Circuit breaker pattern prevents cascading failures by stopping requests to failing service. Three states: CLOSED (normal operation, requests flow), OPEN (service failing, requests fail fast without attempting), and HALF_OPEN (testing recovery with limited requests). State transitions: CLOSED → OPEN when failure threshold exceeded, OPEN → HALF_OPEN after timeout elapses, HALF_OPEN → CLOSED if test request succeeds (service recovered), and HALF_OPEN → OPEN if test request fails (service still down). Fail fast in OPEN state prevents overwhelming failing service and provides immediate feedback to caller.

**Why It Matters**: Cascading failures destroy distributed systems: Service A calls Service B (synchronous dependency), Service B goes down (hardware failure, deployment bug), Service A threads blocked waiting for B (thread pool exhausted), Service A becomes unavailable (can't serve any requests), Services calling A fail (cascade spreads), and entire system collapses (cascading failure). Circuit breaker stops cascade: first 5 requests to B fail normally (threshold not reached), 6th request fails and exceeds threshold (breaker opens), requests 7-100 fail immediately (fail fast, no waiting), Service A threads available (not blocked on B), Service A continues serving requests not needing B (partial degradation), and cascade contained (doesn't spread). OPEN state benefits: no resource exhaustion (threads not blocked), fast failure (immediate error, no timeout wait), and service B relief (no traffic while recovering). Timeout and recovery: breaker opens and stops traffic to B (B has time to recover), after timeout (e.g., 60 seconds), breaker transitions to HALF_OPEN (test if B recovered), send one test request (canary), if succeeds: B recovered, transition to CLOSED, and if fails: B still down, back to OPEN. Half-open state prevents: thundering herd on recovery (all clients retry simultaneously), and premature recovery (false positive from single success). Configuration tuning: failure threshold (5-10 failures typical), timeout duration (10-60 seconds for recovery), and half-open test requests (1 or small percentage). Advanced features: failure percentage threshold (instead of count), slow call threshold (treat slow responses as failures), and sliding window (track failures over time window). Real-world example: Netflix Hystrix (original circuit breaker library), Resilience4j (modern alternative), and Spring Cloud Circuit Breaker (abstraction over libraries). Monitoring: circuit breaker state changes (alert on OPEN state), failure rates (track service health), and recovery times (measure timeout effectiveness). Circuit breaker vs retry: retry handles transient failures (network blip), circuit breaker handles sustained failures (service down), and combining both: retry with circuit breaker wrapper (retry within CLOSED state, fail fast in OPEN state). Testing circuit breakers: simulate failures (inject faults), verify state transitions (CLOSED → OPEN → HALF_OPEN → CLOSED), and validate fail-fast behavior (OPEN state doesn't call service).
