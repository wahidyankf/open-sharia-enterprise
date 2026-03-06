---
title: Components & Code Architecture
description: C4 Level 3 component diagrams and Level 4 code architecture
category: reference
tags:
  - architecture
  - c4-model
  - components
created: 2025-11-29
updated: 2026-03-06
---

# Components & Code Architecture

C4 Level 3 component diagrams and Level 4 code architecture for the Open Sharia Enterprise platform.

## C4 Level 3: Component Diagrams

Shows the internal components within each container. Components are groupings of related functionality behind a well-defined interface.

### oseplatform-web Components (Hugo Static Site)

```mermaid
graph TB
    subgraph "Content"
        MD_CONTENT[Markdown Content<br/>Platform documentation]
        FRONTMATTER_OSE[Frontmatter<br/>Page metadata]
        ASSETS[Static Assets<br/>Images, CSS, JS]
    end

    subgraph "Theme - PaperMod"
        LAYOUTS_OSE[Layouts<br/>HTML templates]
        PARTIALS_OSE[Partials<br/>Reusable components]
        THEME_CONFIG[Theme Config<br/>config.yaml]
    end

    subgraph "Build Output"
        HTML_OSE[HTML Files<br/>Generated pages]
        STATIC_OSE[Static Files<br/>Processed assets]
    end

    HUGO_OSE[Hugo Build Engine<br/>v0.156.0 Extended]

    MD_CONTENT --> HUGO_OSE
    FRONTMATTER_OSE --> HUGO_OSE
    LAYOUTS_OSE --> HUGO_OSE
    PARTIALS_OSE --> HUGO_OSE
    THEME_CONFIG --> HUGO_OSE
    ASSETS --> HUGO_OSE
    HUGO_OSE --> HTML_OSE
    HUGO_OSE --> STATIC_OSE

    style MD_CONTENT fill:#0077b6,stroke:#03045e,color:#ffffff
    style LAYOUTS_OSE fill:#2a9d8f,stroke:#264653,color:#ffffff
    style HUGO_OSE fill:#e76f51,stroke:#9d0208,color:#ffffff
    style HTML_OSE fill:#457b9d,stroke:#1d3557,color:#ffffff
```

**Component Responsibilities:**

- **Markdown Content**: Platform marketing and documentation content
- **Layouts**: PaperMod theme templates for page structure
- **Theme Config**: Site configuration, navigation menus, theme settings

### ayokoding-cli Components (Go CLI Tool)

```mermaid
graph TB
    subgraph "CLI Interface"
        CMD_ROOT[Root Command<br/>Cobra CLI root]
        CMD_TITLES[Update Titles Command<br/>Title extraction & update]
        CMD_NAV[Regenerate Nav Command<br/>Navigation generation]
        CMD_FLAGS[Flags Parser<br/>Command-line arguments]
    end

    subgraph "Title Processing"
        TITLE_EXTRACTOR[Title Extractor<br/>Parse filename to title]
        FRONTMATTER_UPDATER[Frontmatter Updater<br/>Update YAML frontmatter]
        TITLE_FORMATTER[Title Formatter<br/>Format title text]
    end

    subgraph "Navigation Processing"
        NAV_SCANNER[Directory Scanner<br/>Traverse content tree]
        NAV_BUILDER[Navigation Builder<br/>Build nav structure]
        NAV_WRITER[Navigation Writer<br/>Write _index.md files]
        WEIGHT_CALC[Weight Calculator<br/>Level-based ordering]
    end

    subgraph "File Operations"
        FILE_READER[File Reader<br/>Read markdown files]
        FILE_WRITER[File Writer<br/>Write markdown files]
        YAML_PARSER[YAML Parser<br/>Parse/serialize frontmatter]
        MD_PARSER[Markdown Parser<br/>Parse markdown structure]
    end

    subgraph "Configuration"
        CONFIG_LOADER[Config Loader<br/>Load configuration]
        PATH_RESOLVER[Path Resolver<br/>Resolve file paths]
        LOGGER[Logger<br/>Structured logging]
    end

    CONTENT_DIR[ayokoding-web/content/<br/>Markdown files]

    CMD_ROOT --> CMD_TITLES
    CMD_ROOT --> CMD_NAV
    CMD_ROOT --> CMD_FLAGS
    CMD_TITLES --> TITLE_EXTRACTOR
    CMD_TITLES --> FRONTMATTER_UPDATER
    TITLE_EXTRACTOR --> TITLE_FORMATTER
    FRONTMATTER_UPDATER --> YAML_PARSER
    FRONTMATTER_UPDATER --> FILE_WRITER
    CMD_NAV --> NAV_SCANNER
    NAV_SCANNER --> NAV_BUILDER
    NAV_BUILDER --> WEIGHT_CALC
    NAV_BUILDER --> NAV_WRITER
    NAV_WRITER --> FILE_WRITER
    FILE_READER --> CONTENT_DIR
    FILE_WRITER --> CONTENT_DIR
    FILE_READER --> MD_PARSER
    FILE_READER --> YAML_PARSER
    CONFIG_LOADER --> PATH_RESOLVER
    PATH_RESOLVER --> FILE_READER

    style CMD_ROOT fill:#0077b6,stroke:#03045e,color:#ffffff
    style TITLE_EXTRACTOR fill:#2a9d8f,stroke:#264653,color:#ffffff
    style NAV_BUILDER fill:#2a9d8f,stroke:#264653,color:#ffffff
    style FILE_READER fill:#e76f51,stroke:#9d0208,color:#ffffff
    style FILE_WRITER fill:#e76f51,stroke:#9d0208,color:#ffffff
    style YAML_PARSER fill:#457b9d,stroke:#1d3557,color:#ffffff
    style CONTENT_DIR fill:#9d0208,stroke:#6a040f,color:#ffffff
```

**Component Responsibilities:**

- **Root Command**: CLI entry point, command routing, help text
- **Title Extractor**: Extract title from filename pattern (e.g., `01__intro.md` -> "Intro")
- **Frontmatter Updater**: Update YAML frontmatter in markdown files
- **Navigation Scanner**: Recursively scan content directory structure
- **Navigation Builder**: Build hierarchical navigation structure
- **Weight Calculator**: Calculate level-based ordering (level 1 = 100, level 2 = 200, etc.)
- **YAML Parser**: Parse and serialize YAML frontmatter

### rhino-cli Components (Go CLI Tool)

```mermaid
graph TB
    subgraph "CLI Interface"
        RHINO_ROOT[Root Command<br/>Repository automation]
        RHINO_FLAGS[Flags Parser<br/>Command-line arguments]
    end

    subgraph "Automation Modules"
        AUTO_MODULE[Automation Module<br/>Extensible automation]
    end

    subgraph "Infrastructure"
        RHINO_CONFIG[Config Loader<br/>Configuration]
        RHINO_LOGGER[Logger<br/>Logging]
    end

    RHINO_ROOT --> AUTO_MODULE
    RHINO_ROOT --> RHINO_FLAGS
    AUTO_MODULE --> RHINO_CONFIG
    AUTO_MODULE --> RHINO_LOGGER

    style RHINO_ROOT fill:#0077b6,stroke:#03045e,color:#ffffff
    style AUTO_MODULE fill:#2a9d8f,stroke:#264653,color:#ffffff
```

**Component Responsibilities:**

- **Root Command**: CLI entry point for repository automation tasks
- **Automation Module**: Extensible module system for automation workflows
- **Config Loader**: Load butler-specific configuration

### ayokoding-web Components (Hugo Static Site)

```mermaid
graph TB
    subgraph "Content"
        MD_CONTENT_AYO[Markdown Content<br/>Educational tutorials]
        FRONTMATTER_AYO[Frontmatter<br/>Auto-updated titles]
        NAV_FILES[Navigation Files<br/>Auto-generated _index.md]
        I18N_CONTENT[i18n Content<br/>Indonesian + English]
        ASSETS_AYO[Static Assets<br/>Images, diagrams]
    end

    subgraph "Theme - Hextra"
        LAYOUTS_AYO[Layouts<br/>Documentation templates]
        PARTIALS_AYO[Partials<br/>Navigation, sidebar]
        THEME_CONFIG_AYO[Theme Config<br/>Bilingual config]
    end

    subgraph "Build Output"
        HTML_AYO[HTML Files<br/>Generated pages]
        STATIC_AYO[Static Files<br/>Processed assets]
        SEARCH_INDEX[Search Index<br/>Client-side search]
    end

    HUGO_AYO[Hugo Build Engine<br/>v0.156.0 Extended]
    AYOCLI_PROC[ayokoding-cli<br/>Pre-build processing]

    AYOCLI_PROC --> FRONTMATTER_AYO
    AYOCLI_PROC --> NAV_FILES
    MD_CONTENT_AYO --> HUGO_AYO
    FRONTMATTER_AYO --> HUGO_AYO
    NAV_FILES --> HUGO_AYO
    I18N_CONTENT --> HUGO_AYO
    LAYOUTS_AYO --> HUGO_AYO
    PARTIALS_AYO --> HUGO_AYO
    THEME_CONFIG_AYO --> HUGO_AYO
    ASSETS_AYO --> HUGO_AYO
    HUGO_AYO --> HTML_AYO
    HUGO_AYO --> STATIC_AYO
    HUGO_AYO --> SEARCH_INDEX

    style MD_CONTENT_AYO fill:#0077b6,stroke:#03045e,color:#ffffff
    style AYOCLI_PROC fill:#6a4c93,stroke:#22223b,color:#ffffff
    style NAV_FILES fill:#2a9d8f,stroke:#264653,color:#ffffff
    style HUGO_AYO fill:#e76f51,stroke:#9d0208,color:#ffffff
    style SEARCH_INDEX fill:#457b9d,stroke:#1d3557,color:#ffffff
```

**Component Responsibilities:**

- **ayokoding-cli**: Pre-build processing (title updates, navigation generation)
- **Markdown Content**: Programming, AI, and security educational content
- **Navigation Files**: Auto-generated navigation structure with level-based weights
- **i18n Content**: Bilingual support (Indonesian primary, English secondary)
- **Search Index**: Client-side search for documentation

## C4 Level 4: Code Architecture

Shows implementation details for critical components. Focus on Go CLI tool package structures and key implementation patterns.

### ayokoding-cli Package Structure (Go)

```mermaid
classDiagram
    class main {
        +main() void
    }

    class RootCmd {
        +Execute() error
        -initConfig() void
    }

    class UpdateTitlesCmd {
        +Run() error
        -scanContentDir() []string
        -updateFile(path) error
    }

    class RegenerateNavCmd {
        +Run() error
        -buildNavigationTree() NavTree
        -writeIndexFiles(tree) error
    }

    class TitleExtractor {
        +ExtractFromFilename(path) string
        -parseFilename(name) string
        -formatTitle(raw) string
    }

    class FrontmatterUpdater {
        +UpdateTitle(path, title) error
        -readFile(path) ([]byte, error)
        -parseFrontmatter(content) map[string]interface{}
        -serializeFrontmatter(data) []byte
        -writeFile(path, content) error
    }

    class NavigationScanner {
        +ScanDirectory(root) NavTree
        -walkDir(path) error
        -isMarkdownFile(path) bool
        -extractMetadata(path) Metadata
    }

    class NavigationBuilder {
        +BuildTree(files) NavTree
        -calculateWeights(tree) NavTree
        -sortByWeight(nodes) []NavNode
    }

    class WeightCalculator {
        +CalculateWeight(level) int
        +GetLevelFromPath(path) int
    }

    class NavWriter {
        +WriteIndexFiles(tree) error
        -generateIndexContent(node) string
        -writeFile(path, content) error
    }

    class FileReader {
        +ReadMarkdown(path) (string, error)
        +ParseYAML(content) (map[string]interface{}, error)
    }

    class FileWriter {
        +WriteMarkdown(path, content) error
        +SerializeYAML(data) ([]byte, error)
    }

    class Config {
        -string ContentDir
        -string BaseURL
        -bool Verbose
        +Load() error
        +Validate() error
    }

    class Logger {
        +Info(msg) void
        +Error(msg) void
        +Debug(msg) void
    }

    main --> RootCmd
    RootCmd --> UpdateTitlesCmd
    RootCmd --> RegenerateNavCmd
    RootCmd --> Config
    UpdateTitlesCmd --> TitleExtractor
    UpdateTitlesCmd --> FrontmatterUpdater
    UpdateTitlesCmd --> FileReader
    UpdateTitlesCmd --> FileWriter
    RegenerateNavCmd --> NavigationScanner
    RegenerateNavCmd --> NavigationBuilder
    RegenerateNavCmd --> NavWriter
    NavigationBuilder --> WeightCalculator
    NavWriter --> FileWriter
    FrontmatterUpdater --> FileReader
    FrontmatterUpdater --> FileWriter
    UpdateTitlesCmd --> Logger
    RegenerateNavCmd --> Logger
```

**Go Package Design Patterns:**

- **Command Pattern**: Cobra-based CLI with subcommands
- **Single Responsibility**: Each struct handles one specific task
- **Dependency Injection**: Explicit dependencies passed to constructors
- **Error Handling**: Explicit error returns, no exceptions
- **Interface Abstraction**: FileReader/FileWriter interfaces for testability
- **Configuration Management**: Centralized config loading and validation
- **Structured Logging**: Consistent logging throughout the application

### Key Sequence Diagrams

**Content Processing Flow (ayokoding-cli + ayokoding-web):**

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant Git as Git Hook<br/>(pre-commit)
    participant CLI as ayokoding-cli
    participant TitleCmd as Update Titles<br/>Command
    participant NavCmd as Regenerate Nav<br/>Command
    participant FileSystem as Content<br/>Directory
    participant Hugo as Hugo Build

    Dev->>Git: git commit
    Git->>Git: Check if ayokoding-web affected
    alt ayokoding-web affected
        Git->>CLI: nx build ayokoding-cli
        CLI-->>Git: CLI binary built
        Git->>TitleCmd: Execute update-titles
        TitleCmd->>FileSystem: Scan content/ directory
        FileSystem-->>TitleCmd: List of markdown files
        loop For each file
            TitleCmd->>TitleCmd: Extract title from filename
            TitleCmd->>FileSystem: Read file + frontmatter
            FileSystem-->>TitleCmd: File content
            TitleCmd->>TitleCmd: Update title in frontmatter
            TitleCmd->>FileSystem: Write updated file
        end
        TitleCmd-->>Git: Titles updated
        Git->>NavCmd: Execute regenerate-nav
        NavCmd->>FileSystem: Scan content/ tree
        FileSystem-->>NavCmd: Directory structure
        NavCmd->>NavCmd: Build navigation tree
        NavCmd->>NavCmd: Calculate weights by level
        loop For each directory
            NavCmd->>NavCmd: Generate _index.md
            NavCmd->>FileSystem: Write _index.md
        end
        NavCmd-->>Git: Navigation regenerated
        Git->>Git: Stage updated content files
        Git->>Hugo: Continue with commit
    else not affected
        Git->>Hugo: Continue with commit
    end
```
