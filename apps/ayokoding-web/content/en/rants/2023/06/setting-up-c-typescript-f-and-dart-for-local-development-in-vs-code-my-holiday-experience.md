---
title: "Setting Up C TypeScript F and Dart for Local Development in vs Code My Holiday Experience"
weight: 10020
date: 2023-06-29T07:20:00+07:00
draft: false
---

**Similar article:** [Menyiapkan C, TypeScript, F#, dan Dart untuk Pengembangan Lokal dalam VS Code: Pengalaman Liburan Saya](/id/celoteh/2023/06/menyiapkan-c-typescript-f-dan-dart-untuk-pengembangan-lokal-dalam-vs-code-pengalaman-liburan-saya)

بِسْــــــــــــــــــمِ اللهِ الرَّحْمَنِ الرَّحِيْمِ

In the name of Allah, The Most Gracious and The Most Merciful

## Introduction

During the Eid al Adha holiday, I took the opportunity to dive into setting up various programming languages in Visual Studio Code (VS Code). As a software engineer, it's crucial to stay updated with the latest tools and technologies, and this holiday provided the perfect opportunity to explore new programming languages and enhance my development skills.

This article will share my experience setting up C, TypeScript, F#, and Dart in VS Code. These languages were chosen based on their popularity, versatility, and potential for future projects. By setting up the tooling for these languages, I aimed to create a seamless local development environment allowing me to efficiently write, debug, and test code in each language. And at the end of this article, I will share what I learned from setting up these exciting languages.

## The Setup

### General VS Code Setup

Before diving into the specific language setups, I want to mention my general VS Code setup. To streamline my workflow, I found a few extensions to be extremely helpful:

- **Run Command**: This extension allows me to bind commands to keyboard shortcuts, making running specific tasks or scripts easier. You can find it [here](https://marketplace.visualstudio.com/items?itemName=edonet.vscode-command-runner).
- **Multi-Command**: With this extension, I can bind custom commands to a keyboard shortcut, enabling me to execute multiple commands with a single keystroke. You can find it [here](https://marketplace.visualstudio.com/items?itemName=ryuta46.multi-command).

By setting up these extensions, I could assign shortcuts for running and debugging files and projects, enhancing my productivity.

### C

I set up C in VS Code to relearn algorithms and data structures. C is known for its minimal abstraction and manual memory management, making it a great language to dive deep into the fundamentals.

Setting up C in VS Code involved a few additional steps. I needed to create a `launch.json` file and a `tasks.json` file to configure the build and debugging process using the Makefile. These files allowed me to run and debug my C programs directly within VS Code.

I used the [C/C++ extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools) for VS Code to enhance my C development experience.

I created a repository on GitHub where I documented the setup process and shared the Makefile template, `launch.json`, and `tasks.json` files. You can find the setup I used [here](https://github.com/organiclever/ayokoding/tree/main/contents/c-cookbook/c-cookbook-primary).

### F#

Next, I wanted to explore a statically typed functional programming language, and F# caught my attention. Initially, I considered using OCaml, but the project setup proved challenging.

The setup for F# in VS Code was relatively smooth. I encountered a minor hiccup when manually installing Fantomas for code reformatting. Overall, the experience was seamless, although the .NET extension did consume a significant amount of CPU and memory resources. To optimize performance, I disabled the .NET extension in projects that didn't require it. I used the [Ionide extension](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) for VS Code to enhance my F# development experience.

F# also has a relatively smooth text-editor integration with its REPL, although it may not be as extensive as the integration provided by Clojure.

I created a repository on GitHub where I documented the setup process and shared the necessary files. You can find the setup I used [here](https://github.com/organiclever/ayokoding/tree/main/contents/fsharp-cookbook/fsharp-cookbook-primary).

### TypeScript

Given the popularity of JavaScript, I wanted to explore TypeScript, a statically typed superset of JavaScript. I chose to use Deno, a secure runtime for JavaScript and TypeScript, as it provides a ready-to-use TypeScript setup out of the box.

Setting up TypeScript with Deno in VS Code was a breeze. Deno's integration with VS Code made writing and running TypeScript code easy. I used the [Deno extension](https://marketplace.visualstudio.com/items?itemName=denoland.vscode-deno) for VS Code to enhance my TypeScript development experience.

I created a repository on GitHub where I documented the setup process and shared some sample TypeScript code. You can find the setup I used [here](https://github.com/organiclever/ayokoding/tree/main/contents/typescript-cookbook/typescript-cookbook-primary).

### Dart

Lastly, I decided to delve into Dart, the programming language for building Flutter applications. I was particularly excited about introducing sealed classes in Dart, which addressed a long-standing issue.

Setting up Dart in VS Code was a breeze. The Dart SDK and Flutter framework provided a smooth setup process, allowing me to get started with Flutter development quickly. I used the [Dart extension](https://marketplace.visualstudio.com/items?itemName=Dart-Code.dart-code) for VS Code to enhance my Dart development experience.

I created a repository on GitHub where I documented the setup process and shared a simple Flutter app. You can find the setup I used [here](https://github.com/organiclever/ayokoding/tree/main/contents/dart-cookbook/dart-cookbook-primary).

## What Did I Learn?

### General

When we start learning programming languages, it's essential to understand their families or paradigms. By understanding the paradigm behind the language, it becomes easier to learn and work with the language. For instance, while tinkering with various programming languages, I discovered that Dart, although having new syntax, was easy to learn and work with. I installed it within a short period of time and ran my first scripts in no time. Additionally, I was able to explore the testing facilities of Dart within 3 hours of use, which helped me to improve my programming skills. Therefore, when learning programming languages, it is essential to emphasize the language's family or paradigm as it helps to understand the language's concepts, principles, and syntax.

When it comes to coding, every programmer has their preferences. Some like to work with languages that offer more structural editing features, such as LISP and its Paredit feature. Others may prefer languages that offer different features that they find more useful. However, navigating and editing the code can be challenging when working with languages that don't offer the structural editing features that a programmer is used to. This can lead to frustration and decreased productivity. Despite this setback, it's important to remember that all programming languages have unique benefits and drawbacks. By learning different languages and their features, programmers can expand their skill sets and become more versatile.

### C

In his statement, Joe Armstrong (one of the co-designers of the Erlang programming language) emphasized the importance of a Makefile in setting up a C project. While some may argue that other tools can be used for this purpose, I agree with Armstrong that Makefiles are a simple and effective solution. Utilizing Makefiles not only streamlines the process of compiling and linking C code but also helps automate the build process, making it more efficient.

Furthermore, having a solid foundation in the Linux Command Line and related technologies can significantly assist in learning to use Makefiles. With the help of tools like ChatGPT, one can easily acquire knowledge and enhance their skills in this area. It is important to note that while Makefiles may seem daunting at first, with practice and persistence, they can become second nature to any developer looking to streamline their C project setup process.

### F#

Unfortunately, setting up OCaml can be cumbersome and less streamlined than F#. On the other hand, F# offers a much more intuitive and user-friendly setup process, making it an ideal choice for beginners interested in exploring the world of statically typed functional programming. Additionally, F# has a supportive community with plenty of resources and documentation available online, which can aid in the learning process and help troubleshoot any issues that may arise. Overall, while both OCaml and F# have their unique advantages and disadvantages, for those looking for a smooth and easy entry into the world of functional programming, F# is the better choice.

Integrating the text editor with the F# REPL is relatively smooth and seamless, allowing for easy and efficient coding. However, while the integration may not be as extensive as that provided by Clojure, it still provides a significant amount of functionality and ease of use for the user. Additionally, the F# REPL offers a range of features that make it a powerful tool in its own right, including the ability to execute code snippets and work with various data types. As such, the F# REPL is an excellent choice for developers looking for a powerful and flexible programming tool to help them create high-quality code efficiently and effectively.

### Dart

Many developers have come to appreciate Dart and its unique features. It has gained popularity because of its smooth setup experience, making it easy to start. In addition, the language comes with a pre-installed linter, which helps developers avoid common mistakes and write better code. With its new functional programming-ish style, Dart has opened up new possibilities for developers to write more efficient code. The fluent interaction style also reminds some developers of Scala, making it a familiar and comfortable choice. Moreover, Clojure Dart expands the possibilities of using Dart, making it a more versatile language for different projects. All these reasons have contributed to the growing popularity of Dart, and it is becoming increasingly difficult for developers to ignore this language any longer.

## Closure

In conclusion, my Eid al Adha holiday was well-spent exploring and setting up C, TypeScript, F#, and Dart in VS Code. Each language had its unique setup process, but overall, I found the experience rewarding and educational. I am considering using Deno in future projects that require TypeScript, as it exempts me from the hassle of setting up the environment and ensuring the browser, server, and TypeScript setup work well together. Additionally, Dart's seamless implementation of sealed classes and the creation and testing of the Option and Result types have elevated the value and temptations of using Dart (and Flutter) in my future personal projects.

If you want to explore the Option and Result types in Dart, I have created a repository where I have implemented and tested them. You can find it [here](https://github.com/organiclever/ayokoding/tree/main/contents/dart-cookbook/dart-cookbook-primary/src/typing_utils). Feel free to check it out and see how they can enhance your Dart projects.

Please note that my setup experience was on a MacBook M1 Pro 13-inch, so your experience may vary depending on your system. This article inspires others to explore new programming languages and encourages language developers to prioritize user-friendly setups.
