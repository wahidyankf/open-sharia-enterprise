---
title: "Deps and CLI"
date: 2025-03-16T07:20:00+07:00
draft: false
---

Clojure is a robust language within the functional programming landscape, and it runs on the Java Virtual Machine (JVM). In addition to its innovative language features, Clojure boasts advanced tooling that significantly improves developer workflow. This article guides you through two essential tools within the Clojure ecosystem: `deps.edn` and the Clojure CLI (Command Line Interface).

## The Cornerstone of Dependency Management: `deps.edn`

The `deps.edn` file is the heart of Clojure's dependency management system, crafted in the EDN (Extensible Data Notation) format. Developers can use this file to declare the external libraries and dependencies their project requires in a declarative manner. By creating a map with the `:deps` key, developers can list the dependencies and their respective versions. Furthermore, aliases can be defined for different build configurations, such as development, testing, or production environments.

The true potential of `deps.edn` emerges when integrated with the Clojure CLI, which interprets and utilizes `deps.edn` efficiently. This integration allows the CLI to resolve and fetch the specified dependencies from popular repositories such as Maven or Clojars.

## The Power of the Clojure CLI in Development

The Clojure CLI is more than just a command-line tool—it's a powerful asset for creating, running, and managing Clojure projects. It simplifies the development process and offers a streamlined workflow through the provision of various functions:

1. **Interactive Development**: The CLI can launch a REPL (Read-Eval-Print Loop) session, enabling the exploration and evaluation of Clojure expressions. This fosters a swift development cycle and encourages iterative experimentation.
2. **Script Execution**: The CLI allows for the direct execution of Clojure scripts from the command line. This facilitates the running of standalone Clojure programs without needing a whole project structure.
3. **Integration with `deps.edn`**: The Clojure CLI works harmoniously with `deps.edn`. During the execution of a Clojure project, it automatically reads the `deps.edn` file, resolves the dependencies and makes them accessible for your project.

## Leiningen vs. `deps.edn` and Clojure CLI

Before `deps.edn` and the Clojure CLI, Leiningen was the preferred build tool for Clojure projects. Although Leiningen provides similar functionality for managing dependencies and building projects, there are key differences to be aware of:

1. **Configuration**: Leiningen uses a `project.clj` file for configuration, whereas `deps.edn` provides a lighter and more flexible alternative.
2. **Build Process**: Leiningen employs a traditional build process, including setting up a classpath and compiling code. Conversely, the Clojure CLI is lighter and more interactive, providing a REPL and enabling dynamic code evaluation.
3. **Ease of Use**: The Clojure CLI is designed for simplicity and ease of use with a minimalistic command-line interface. At the same time, Leiningen provides a broader range of features and complex configuration options.

The decision between these tools largely depends on your project's specific requirements and your development team's preferences, as each tool has its strengths.

## Leiningen's `project.clj` and Its Translation to `deps.edn`

If you have an existing Clojure project using Leiningen with a `project.clj` file for dependency management, consider transitioning to the `deps.edn` format to leverage the benefits of the Clojure CLI. This transition requires translating your `project.clj` file into a `deps.edn` file, a process involving several critical steps:

1. **Create a `deps.edn` file**: Begin with a blank slate by creating a new `deps.edn` file in your project's root directory.
2. **Define the `:deps` and `:paths` keys**: These keys will list your project's dependencies and versions, and designate the source code directories, respectively.
3. **Migrate the main class and JVM options**: Use the `:aliases` key to include your main class, and the `:jvm-opts` key to specify JVM options.
4. **Migrate plugin dependencies and project-level configuration**: If your `project.clj` file includes plugin dependencies, you can define aliases for different build configurations in `deps.edn`. Include project-level configurations directly in `deps.edn`.
5. **Migrate project-specific configurations**: Any project-specific configurations, such as `:uberjar-name`, should also be included in `deps.edn`.

Here's an example of a `project.clj` file and its translation to a `deps.edn` file:

### **project.clj**

```clojure
(defproject my-project "1.0.0"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [ring/ring-core "1.10.2"]
                 [ring/ring-jetty-adapter "1.10.2"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler my-project.handler/app}
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.4.3"]]}})

```

### **deps.edn**

```clojure
{:deps
 {org.clojure/clojure {:mvn/version "1.10.3"}
  org.clojure/tools.nrepl {:mvn/version "0.2.13"}
  ring/ring-core {:mvn/version "1.10.2"}
  ring/ring-jetty-adapter {:mvn/version "1.10.2"}}
 :aliases
 {:dev {:extra-deps
        {javax.servlet/servlet-api {:mvn/version "2.5"}
         ring/ring-mock {:mvn/version "0.4.3"}}}}
 :main-opts ["-m" "my-project.handler/app"]}
```

By following these steps, you will create a `deps.edn` file that accurately mirrors your project's dependencies, build configurations, and specifications.

## Conclusion

`deps.edn` and the Clojure CLI are indispensable components in the Clojure ecosystem, significantly simplifying dependency management and enhancing the development experience. With `deps.edn`, developers can clearly and succinctly specify project dependencies, providing a transparent overview of the project's requirements.

The Clojure CLI, working in tandem with `deps.edn`, seamlessly resolves and manages dependencies. It offers a lightweight, interactive approach to building and running Clojure projects, simplifying the development workflow.

`deps.edn` and the Clojure CLI allow developers to focus on crafting clean and efficient Clojure code rather than getting bogged down with managing dependencies and build configurations. These tools make managing dependencies and building Clojure projects more straightforward and efficient.

## Further Reading

For those interested in learning more about these tools, the [Official Clojure Deps and CLI Documentation](https://clojure.org/guides/deps_and_cli) is a comprehensive guide that provides deeper insights into their usage.
