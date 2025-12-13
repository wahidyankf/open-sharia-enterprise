---
title: "Typing: Sealed Class"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/src/typing/sealed_class.dart).

</aside>

```dart
sealed class TShirtSize {}

class Small extends TShirtSize {}

class Medium extends TShirtSize {}

class ExtraLarge extends TShirtSize {
  final int size;

  ExtraLarge(this.size);
}

void printSizeInfo(TShirtSize theSize) {
  // When not exhaustive, the compiler will refuse to compile
  switch (theSize) {
    case Small _:
      print("Small");
    case Medium _:
      print("Medium");
    case ExtraLarge val:
      print("E" + "x" * val.size + "tra Large");
  }
}

sealed class Role {
  void printInfo() {
    // When not exhaustive, the compiler will refuse to compile
    switch (this) {
      case Teacher teacher:
        print(
            "Name: ${teacher.name}, License: ${teacher.teachingLicense}, Major: ${teacher.teachingMajor}");
        break;
      case Student student:
        print(
            "Student: ${student.name}, ID: ${student.studentId}, School: ${student.school}");
        break;
    }
  }
}

class Teacher extends Role {
  final String name;
  final String teachingLicense;
  final String teachingMajor;

  Teacher(this.name, this.teachingLicense, this.teachingMajor);
}

class Student extends Role {
  final String name;
  final String studentId;
  final String school;

  Student(this.name, this.studentId, this.school);
}

void main() {
  printSizeInfo(Small()); // Output: Small
  printSizeInfo(Medium()); // Output: Medium
  printSizeInfo(ExtraLarge(1)); // Output: Extra Large
  printSizeInfo(ExtraLarge(3)); // Output: Exxxtra Large

  Teacher("John Doe", "123456", "Math")
      .printInfo(); // Output: Name: John Doe, License: 123456, Major: Math
  Student("Jane Doe", "123456", "University of Life")
      .printInfo(); // Output: Student: Jane Doe, ID: 123456, School: University of Life
}
```

The provided Dart code is straightforward and does the following:

1. A `sealed` class `TShirtSize` is declared, restricting inheritance to a specific set of classes defined in the same file.
2. Three classes `Small`, `Medium`, and `ExtraLarge` are defined that extend the `TShirtSize` sealed class. These represent specific t-shirt sizes.
3. The `ExtraLarge` class has an integer `size` field, assigned via the constructor.
4. The `printSizeInfo` function accepts an instance of `TShirtSize` and uses an exhaustive switch case to print different outputs depending on the actual class of the instance. Dart's exhaustiveness check on sealed classes ensures that all subclasses are covered.
5. A `sealed` class `Role` is declared with a `printInfo` method, which prints different information based on whether the instance is a `Teacher` or a `Student`.
6. The `Teacher` and `Student` classes extend the `Role` sealed class. Each class has unique fields like `name`, `teachingLicense`, `teachingMajor` for `Teacher` and `name`, `studentId`, `school` for `Student`.
7. In the `main` function, instances of `Small`, `Medium`, and `ExtraLarge` are created and passed to the `printSizeInfo` function, printing "Small", "Medium", "Extra Large" and "Exxxtra Large" respectively.
8. The `main` function also creates instances of `Teacher` and `Student`, and calls their `printInfo` method, printing "Name: John Doe, License: 123456, Major: Math" and "Student: Jane Doe, ID: 123456, School: University of Life" respectively.

The key takeaways from this code are the use of sealed classes for type safety and pattern matching, exhaustive switch cases for complete handling of all subclasses, and efficient printing of information based on the class of the instance.
