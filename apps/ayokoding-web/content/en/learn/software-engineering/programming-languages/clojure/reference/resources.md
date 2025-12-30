---
title: "Resources"
date: 2025-12-30T14:13:06+07:00
draft: false
weight: 1000050
description: Curated list of official documentation, community resources, tools, and learning materials for Clojure development
---

**Curated collection** of essential Clojure resources for learners and professional functional programmers. From official documentation to community tools.

## Official Documentation

### Language Reference

**[Clojure Official Website](https://clojure.org/)**

Official Clojure language documentation and guides.

- Complete language reference
- Getting started guides
- API documentation
- Community resources

**[Clojure API Reference](https://clojure.github.io/clojure/)**

Complete API documentation for Clojure core.

- All core functions
- Namespace organization
- Usage examples
- Source code links

**[ClojureDocs](https://clojuredocs.org/)**

Community-powered documentation with practical examples.

- Real-world usage examples
- User comments and tips
- Cross-references
- See-also recommendations
- Community contributions

**[Clojure Cheatsheet](https://clojure.org/api/cheatsheet)**

Interactive reference organized by category.

- Quick lookup by function type
- Searchable and filterable
- Links to full documentation
- Categorized by use case

### Official Guides

**[Getting Started](https://clojure.org/guides/getting_started)**

Official installation and setup guide.

- JVM installation
- Clojure CLI tools
- REPL basics
- First program

**[Learn Clojure](https://clojure.org/guides/learn/syntax)**

Comprehensive learning path from Clojure.org.

- Syntax fundamentals
- Functions and data structures
- Sequential processing
- State management

**[Clojure from the Ground Up](https://aphyr.com/tags/Clojure-from-the-ground-up)**

Kyle Kingsbury's detailed introduction series.

- Beginner-friendly explanations
- Progressive complexity
- Practical examples
- Conceptual clarity

## Development Tools

### Build Tools

**[Leiningen](https://leiningen.org/)**

Most popular Clojure build tool and project automation.

- Project scaffolding
- Dependency management
- REPL integration
- Plugin ecosystem
- Simple configuration (project.clj)

**[deps.edn and Clojure CLI](https://clojure.org/guides/deps_and_cli)**

Official dependency management and CLI tools.

- Minimal configuration
- Dependency resolution
- Tool execution
- Modern approach
- Git dependencies support

**[Boot](https://boot-clj.com/)**

Scriptable build tool with composable tasks.

- Programmatic build scripts
- Task composition
- Flexible pipelines
- Interactive development

**[Shadow-cljs](https://shadow-cljs.github.io/docs/UsersGuide.html)**

ClojureScript build tool focused on modern JavaScript.

- Hot code reloading
- NPM integration
- Multiple output targets
- Development server
- Production optimization

### IDEs and Editors

**[Cursive (IntelliJ IDEA)](https://cursive-ide.com/)**

Feature-rich IDE plugin for Clojure development.

- Intelligent code completion
- Structural editing
- Integrated REPL
- Debugging support
- Commercial (free for non-commercial)

**[Calva (Visual Studio Code)](https://calva.io/)**

Clojure extension for VS Code with interactive REPL.

- REPL integration
- Paredit support
- Inline evaluation
- Debugger
- Free and open-source

**[CIDER (Emacs)](https://cider.mx/)**

Powerful Clojure development environment for Emacs.

- Interactive programming
- Advanced debugging
- Code navigation
- Documentation lookup
- Test runner integration

**[Conjure (Neovim)](https://github.com/Olical/conjure)**

Interactive programming environment for Neovim.

- REPL integration
- Evaluation in buffer
- Log window
- Multiple REPL support
- Lightweight

**[Nightcode](https://sekao.net/nightcode/)**

Simple IDE designed for beginners.

- Built-in tutorials
- REPL integration
- Simple interface
- Cross-platform

### REPL Tools

**[Rebel Readline](https://github.com/bhauman/rebel-readline)**

Enhanced REPL with readline features.

- Syntax highlighting
- Multi-line editing
- Auto-completion
- Command history
- Inline documentation

**[Figwheel Main](https://figwheel.org/)**

ClojureScript build tool with hot reloading.

- Live code reloading
- CSS reloading
- Build notifications
- REPL integration
- Great for web development

## Libraries and Frameworks

### Web Development

**[Ring](https://github.com/ring-clojure/ring)**

Foundation for HTTP handling in Clojure.

- HTTP abstraction
- Middleware system
- Request/response maps
- Adapter for various servers
- Core of web ecosystem

**[Compojure](https://github.com/weavejester/compojure)**

Routing library for Ring.

- Concise routing DSL
- Route matching
- Parameter extraction
- Middleware support
- Widely used

**[Reitit](https://github.com/metosin/reitit)**

Modern, data-driven routing library.

- Fast routing
- OpenAPI integration
- Bidirectional routing
- Middleware per route
- Schema validation

**[Pedestal](http://pedestal.io/)**

Web framework for services and APIs.

- Asynchronous processing
- Interceptor chain
- Server-sent events
- WebSocket support
- Production-ready

**[Luminus](https://luminusweb.com/)**

Micro-framework for web applications.

- Project templates
- Curated library stack
- Documentation
- Best practices
- Batteries included

### Data Validation

**[Spec](https://clojure.org/guides/spec)**

Built-in specification and validation library.

- Data shape specification
- Function contracts
- Generative testing
- Error reporting
- Core library (no dependency)

**[Malli](https://github.com/metosin/malli)**

Data-driven schema library.

- Schema as data
- Fast validation
- Transformation
- JSON schema generation
- Rich error messages

**[Schema](https://github.com/plumatic/schema)**

Declarative data description and validation.

- Concise syntax
- Runtime validation
- Documentation
- Coercion support
- Mature ecosystem

### Database Access

**[next.jdbc](https://github.com/seancorfield/next-jdbc)**

Modern JDBC wrapper for Clojure.

- Simple API
- Performance optimized
- Connection pooling
- Transaction support
- Official recommendation

**[HugSQL](https://www.hugsql.org/)**

SQL-first database library.

- SQL files as functions
- Named parameters
- Multiple queries per file
- Result transformation
- ClojureScript support

**[Honey SQL](https://github.com/seancorfield/honeysql)**

SQL DSL for Clojure.

- Composable queries
- Clojure data structures
- Vendor-specific support
- Extensible
- Type-safe

**[Datomic](https://www.datomic.com/)**

Database designed for Clojure.

- Immutable facts
- Time-aware queries
- Datalog query language
- ACID transactions
- Built by Cognitect

**[Datascript](https://github.com/tonsky/datascript)**

In-memory Datalog database.

- Datomic-like API
- Client-side database
- ClojureScript support
- Reactive queries
- No server needed

### Testing

**[clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)**

Built-in testing framework.

- Simple assertions
- Test fixtures
- Test namespaces
- No dependencies
- Standard approach

**[Midje](https://github.com/marick/Midje)**

BDD-style testing framework.

- Readable syntax
- Mocking support
- Fact checking
- Tabular tests
- Prerequisites

**[test.check](https://github.com/clojure/test.check)**

Property-based testing library.

- Generative testing
- Shrinking failures
- Custom generators
- Spec integration
- QuickCheck port

**[Expectations](https://github.com/clojure-expectations/expectations)**

Minimalist testing framework.

- Concise syntax
- Data-driven
- Quick to learn
- Good error messages
- Lightweight

### Data Processing

**[core.async](https://github.com/clojure/core.async)**

Asynchronous programming with channels.

- CSP-style concurrency
- Channels for communication
- Go blocks
- Timeout support
- ClojureScript compatible

**[Manifold](https://github.com/clj-commons/manifold)**

Asynchronous programming toolkit.

- Deferreds (promises)
- Streams
- Time operations
- Graph structures
- Performance focused

**[Claypoole](https://github.com/TheClimateCorporation/claypoole)**

Threadpool tools for parallel processing.

- Parallel map/reduce
- Custom threadpools
- Priority scheduling
- Fork-join support
- Easy parallelism

## ClojureScript Resources

### Official Documentation

**[ClojureScript Website](https://clojurescript.org/)**

Official ClojureScript documentation.

- Quick start guide
- Differences from Clojure
- JavaScript interop
- Build tools
- Browser REPL

**[ClojureScript API](https://cljs.github.io/api/)**

Complete ClojureScript API reference.

- Core functions
- JavaScript interop
- Syntax differences
- Browser APIs
- Node.js support

### Frameworks and Tools

**[Reagent](https://reagent-project.github.io/)**

Minimalist React wrapper for ClojureScript.

- Hiccup syntax
- Reactive atoms
- Clean API
- React component interop
- Lightweight

**[Re-frame](https://day8.github.io/re-frame/)**

Framework for building React apps.

- Unidirectional data flow
- Event handling
- Subscription model
- Effect management
- Well documented

**[Fulcro](https://fulcro.fulcrologic.com/)**

Full-stack framework for web development.

- Isomorphic Clojure/Script
- Data-driven UI
- Normalized database
- Server integration
- Complex UIs

## Learning Resources

### Books

**[Clojure for the Brave and True](https://www.braveclojure.com/)**

Free online book by Daniel Higginbotham.

- Beginner-friendly
- Humorous approach
- Comprehensive coverage
- Practical examples
- Available online and print

**[Programming Clojure](https://pragprog.com/titles/shcloj3/programming-clojure-third-edition/)** by Alex Miller, Stuart Halloway, Aaron Bedra

Comprehensive introduction to Clojure.

- Core concepts
- Practical techniques
- Updated for modern Clojure
- From Pragmatic Programmers

**[Clojure Applied](https://pragprog.com/titles/vmclojeco/clojure-applied/)** by Ben Vandgrift and Alex Miller

Practical guide to building applications.

- Real-world patterns
- Architecture decisions
- Production considerations
- Best practices

**[The Joy of Clojure](https://www.manning.com/books/the-joy-of-clojure-second-edition)** by Michael Fogus and Chris Houser

Deep dive into Clojure philosophy and design.

- Advanced concepts
- Functional thinking
- Language internals
- Design patterns

**[Living Clojure](https://www.oreilly.com/library/view/living-clojure/9781491909270/)** by Carin Meier

Introduction with training plan.

- Step-by-step learning
- Exercises
- Community resources
- Beginner focus

**[Clojure in Action](https://www.manning.com/books/clojure-in-action-second-edition)** by Amit Rathore and Francis Avila

Practical guide to Clojure development.

- Web applications
- Concurrency
- Testing
- Production deployment

### Online Courses

**[Clojure Koans](http://clojurekoans.com/)**

Learning Clojure through test-driven practice.

- Interactive exercises
- Incremental learning
- Test-based
- Fill in the blanks
- Free

**[4Clojure](http://www.4clojure.com/)**

Problem-solving platform for Clojure.

- Coding challenges
- Difficulty levels
- Community solutions
- Learn by doing
- Free

**[Exercism - Clojure Track](https://exercism.org/tracks/clojure)**

Coding exercises with mentorship.

- 100+ exercises
- Mentor feedback
- Community solutions
- Test-driven learning
- Free

**[PurelyFunctional.tv](https://purelyfunctional.tv/)**

Video courses by Eric Normand.

- Beginner to advanced
- Practical focus
- Web development
- Email newsletter
- Subscription-based

**[Lambda Island](https://lambdaisland.com/)**

High-quality screencasts and courses.

- Modern tools and libraries
- Professional production
- ClojureScript focus
- Subscription-based
- Episode format

### Community

**[Clojurians Slack](https://clojurians.slack.com/)**

Active Slack community (get invite at [clojurians.net](http://clojurians.net/)).

- Thousands of members
- Beginner-friendly channels
- Library-specific channels
- Job postings
- Helpful community

**[ClojureVerse](https://clojureverse.org/)**

Community forum for discussions.

- Long-form discussions
- Announcements
- Help requests
- Library showcases
- Friendly atmosphere

**[r/Clojure (Reddit)](https://www.reddit.com/r/Clojure/)**

Clojure subreddit.

- News and articles
- Project showcases
- Discussions
- Weekly threads
- Community voting

**[Clojure Google Group](https://groups.google.com/g/clojure)**

Official mailing list.

- Technical discussions
- Announcements
- Design decisions
- Historical archive
- Long-form communication

**[Clojure Subreddit Wiki](https://www.reddit.com/r/Clojure/wiki/index/)**

Curated resources from r/Clojure.

- Learning paths
- Library recommendations
- Tool comparisons
- Community FAQs

### Blogs and Newsletters

**[Planet Clojure](http://planet.clojure.in/)**

Aggregator of Clojure blogs.

- Multiple authors
- Recent posts
- Diverse topics
- RSS feed available

**[The REPL](https://www.therepl.net/)**

Weekly Clojure newsletter by Daniel Compton.

- Curated articles
- Library updates
- Community news
- Job postings

**[Clojure Weekly](http://reborg.net/)**

Weekly Clojure newsletter.

- Technical articles
- Library releases
- Conference talks
- Community highlights

**[Lambda Island Blog](https://lambdaisland.com/blog)**

Technical blog on Clojure and ClojureScript.

- Deep dives
- Tool tutorials
- Best practices
- Video content

**[Functional Works Blog](https://functional.works-hub.com/learn/)**

Functional programming articles including Clojure.

- Career advice
- Technical tutorials
- Industry trends
- Job listings

### Video Content

**[ClojureTV (YouTube)](https://www.youtube.com/user/ClojureTV)**

Official Clojure YouTube channel.

- Conference talks
- Presentations
- Rich Hickey talks
- Community contributions

**[London Clojurians](https://www.youtube.com/c/LondonClojurians)**

Talks from London Clojure meetup.

- Monthly talks
- Diverse topics
- Community speakers
- Regular uploads

**[Clojure/conj](https://www.youtube.com/c/ClojureConj)**

Talks from Clojure/conj conference.

- Expert speakers
- Advanced topics
- Community showcase
- Annual conference

**[Eric Normand's YouTube](https://www.youtube.com/c/EricNormand)**

Educational Clojure content.

- Beginner tutorials
- Functional programming concepts
- Problem-solving
- Weekly episodes

## Package Repositories

**[Clojars](https://clojars.org/)**

Primary repository for Clojure libraries.

- Open-source libraries
- Easy publishing
- Maven-compatible
- Search and documentation
- Community-driven

**[Maven Central](https://central.sonatype.com/)**

Java libraries usable from Clojure.

- Java ecosystem access
- Stable releases
- Official libraries
- Dependency resolution

## Development Tools

**[Kibit](https://github.com/jonase/kibit)**

Static code analyzer suggesting idiomatic Clojure.

- Pattern matching
- Improvement suggestions
- Linter integration
- Educational

**[Eastwood](https://github.com/jonase/eastwood)**

Clojure lint tool.

- Warning detection
- Code quality
- Configurable rules
- CI integration

**[Cljfmt](https://github.com/weavejester/cljfmt)**

Code formatting tool.

- Consistent style
- Configurable rules
- Editor integration
- CI/CD compatible

**[clj-kondo](https://github.com/clj-kondo/clj-kondo)**

Fast linter for Clojure code.

- Static analysis
- Fast performance
- Editor integration
- Actionable warnings

## Online REPL and Playgrounds

**[Nextjournal](https://nextjournal.com/)**

Collaborative computational notebooks.

- Live code execution
- Data visualization
- Shareable notebooks
- Version control

**[Repl.it - Clojure](https://replit.com/languages/clojure)**

Online Clojure REPL and IDE.

- Browser-based coding
- Real-time collaboration
- Package management
- Free tier available

**[Try Clojure](http://tryclojure.org/)**

Simple browser-based REPL for beginners.

- No installation needed
- Tutorial included
- Quick experiments
- Beginner-friendly

## Conferences and Meetups

**[Clojure/conj](https://clojure-conj.org/)**

Annual North American Clojure conference.

- Expert talks
- Workshops
- Community gathering
- Historical recordings

**[EuroClojure](https://euroclojure.org/)**

Annual European Clojure conference.

- International speakers
- Technical content
- Community networking
- Video recordings

**[Clojure/north](https://clojurenorth.com/)**

Canadian Clojure conference.

- Regional focus
- Diverse speakers
- Practical topics
- Community building

**[Meetup.com - Clojure Groups](https://www.meetup.com/topics/clojure/)**

Local Clojure user groups worldwide.

- Regular meetings
- Lightning talks
- Networking
- Study groups

## Podcasts

**[Cognicast](http://blog.cognitect.com/cognicast/)**

Podcast from Cognitect (Clojure creators).

- Rich Hickey interviews
- Core team insights
- Language design
- Community guests

**[defn](https://soundcloud.com/defn-771544745)**

Conversational Clojure podcast.

- Community interviews
- Library showcases
- Learning experiences
- Casual format

**[The REPL Podcast](https://www.therepl.net/podcast/)**

Interviews with Clojure developers.

- Developer stories
- Project deep-dives
- Career advice
- Production experiences

## Official Social Media

**[X (formerly Twitter): @clojure](https://twitter.com/clojure)**

Official Clojure X account.

- News and updates
- Community highlights
- Event announcements
- Library releases

**[Clojure LinkedIn](https://www.linkedin.com/groups/2891509/)**

Clojure professionals group.

- Job postings
- Industry discussions
- Professional networking
- Event announcements

## Contributing to Clojure

**[Clojure Contributor Guide](https://clojure.org/community/contributing)**

How to contribute to Clojure development.

- Contribution workflow
- Contributor agreement
- JIRA workflow
- Patch submission

**[Clojure JIRA](https://clojure.atlassian.net/)**

Issue tracking for Clojure core.

- Bug reports
- Enhancement proposals
- Patch screening
- Voting system

**[GitHub - Clojure](https://github.com/clojure/)**

GitHub organization for Clojure projects.

- Source code
- Contrib libraries
- Read-only mirrors
- Issue tracking (some projects)

## Platform-Specific

**[ClojureCLR](https://github.com/clojure/clojure-clr)**

Clojure on .NET Common Language Runtime.

- .NET interop
- Windows support
- CLR integration
- Community-maintained

**[Babashka](https://babashka.org/)**

Fast-starting Clojure scripting environment.

- Native binary
- Instant startup
- Shell scripting
- CI/CD tasks

**[GraalVM Native Image](https://www.graalvm.org/)**

Compile Clojure to native executables.

- Fast startup
- Low memory
- Ahead-of-time compilation
- Command-line tools

## Learn More

**Comprehensive Learning Path**:

- [Initial Setup](/en/learn/software-engineering/programming-languages/clojure/tutorials/initial-setup) - Get started with Clojure
- [Quick Start](/en/learn/software-engineering/programming-languages/clojure/tutorials/quick-start) - Overview of key features
- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/clojure/tutorials/beginner) - Comprehensive fundamentals
- [How-To Guides](/en/learn/software-engineering/programming-languages/clojure/how-to) - Problem-solving guides
- [Cookbook](/en/learn/software-engineering/programming-languages/clojure/how-to/cookbook) - Practical recipes

**Reference Materials**:

- [Cheat Sheet](/en/learn/software-engineering/programming-languages/clojure/reference/cheat-sheet) - Quick syntax reference
- [Glossary](/en/learn/software-engineering/programming-languages/clojure/reference/glossary) - Clojure terminology
