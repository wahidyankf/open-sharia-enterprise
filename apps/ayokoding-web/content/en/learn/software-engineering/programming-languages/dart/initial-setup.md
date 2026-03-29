---
title: "Initial Setup"
weight: 100001
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Step-by-step guide to installing Dart SDK and setting up development environment"
tags: ["dart", "installation", "setup"]
---

This guide walks you through installing the Dart SDK and setting up your development environment for Dart programming.

## Installing Dart SDK

The Dart SDK includes the Dart VM, core libraries, and command-line tools needed for Dart development.

### Windows Installation

**Using Chocolatey (Recommended)**:

Open PowerShell as Administrator and run:

```powershell
choco install dart-sdk
```

**Using Direct Download**:

1. Visit [dart.dev/get-dart](https://dart.dev/get-dart)
2. Download the Windows installer
3. Run the installer and follow the prompts
4. Add Dart to your system PATH (installer does this automatically)

**Verify installation**:

```powershell
dart --version
# => Output: Dart SDK version: 3.x.x (stable)
```

### macOS Installation

**Using Homebrew (Recommended)**:

```bash
brew tap dart-lang/dart
brew install dart
```

**Verify installation**:

```bash
dart --version
# => Output: Dart SDK version: 3.x.x (stable)
```

### Linux Installation

**Using apt (Debian/Ubuntu)**:

```bash
sudo apt update
sudo apt install apt-transport-https
wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo gpg --dearmor -o /usr/share/keyrings/dart.gpg
echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' | sudo tee /etc/apt/sources.list.d/dart_stable.list

sudo apt update
sudo apt install dart
```

**Verify installation**:

```bash
dart --version
# => Output: Dart SDK version: 3.x.x (stable)
```

## Optional: Installing Flutter SDK

If you plan to build mobile, web, or desktop applications with Flutter, install the Flutter SDK (which includes Dart).

### Why Install Flutter?

- **Hot reload** - Instant code updates during development
- **Rich UI framework** - Build beautiful cross-platform interfaces
- **Includes Dart** - Flutter SDK includes Dart SDK automatically
- **Unified tooling** - Single tool for mobile, web, and desktop

### Flutter Installation Steps

**Windows**:

1. Download Flutter SDK from [flutter.dev](https://flutter.dev/docs/get-started/install/windows)
2. Extract the zip file to desired location (e.g., `C:\src\flutter`)
3. Add `C:\src\flutter\bin` to system PATH
4. Run `flutter doctor` to check dependencies

**macOS**:

```bash
cd ~/development
unzip ~/Downloads/flutter_macos_*.zip
export PATH="$PATH:`pwd`/flutter/bin"
flutter doctor
```

Add to `.zshrc` or `.bash_profile`:

```bash
export PATH="$PATH:$HOME/development/flutter/bin"
```

**Linux**:

```bash
cd ~/development
tar xf ~/Downloads/flutter_linux_*.tar.xz
export PATH="$PATH:`pwd`/flutter/bin"
flutter doctor
```

Add to `.bashrc`:

```bash
export PATH="$PATH:$HOME/development/flutter/bin"
```

**Verify Flutter installation**:

```bash
flutter doctor
# => Checks all Flutter dependencies
# => Reports any missing requirements

flutter --version
# => Output: Flutter 3.x.x • Dart 3.x.x
```

## IDE Setup

Choose your preferred IDE and install Dart/Flutter plugins.

### Visual Studio Code (Recommended)

**Why VS Code?**:

- Lightweight and fast
- Excellent Dart/Flutter extensions
- Integrated terminal
- Free and open-source

**Installation steps**:

1. Install [Visual Studio Code](https://code.visualstudio.com/)
2. Open VS Code
3. Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
4. Search and install "Dart"
5. (Optional) Search and install "Flutter" if using Flutter

**Verify setup**:

Create a test file `test.dart`:

```dart
void main() {
  print('Hello from VS Code!'); // => Output: Hello from VS Code!
}
```

Run with F5 or right-click → "Run Without Debugging"

### Android Studio / IntelliJ IDEA

**Why Android Studio/IntelliJ?**:

- Powerful refactoring tools
- Advanced debugging
- Built-in Flutter tools
- Professional IDE features

**Installation steps**:

1. Install [Android Studio](https://developer.android.com/studio) or [IntelliJ IDEA](https://www.jetbrains.com/idea/)
2. Open Settings/Preferences
3. Go to Plugins
4. Search and install "Dart"
5. (Optional) Search and install "Flutter" if using Flutter
6. Restart IDE

**Verify setup**:

1. Create new Dart project: File → New → Project → Dart
2. Select Dart SDK location
3. Create a simple application
4. Run with Shift+F10

### Command-Line Only

You can use any text editor and run Dart from the command line:

**Popular editors**:

- Vim with dart-vim-plugin
- Emacs with dart-mode
- Sublime Text with Dart plugin
- Notepad++ with Dart syntax highlighting

**Running Dart files**:

```bash
dart run filename.dart
# => Executes Dart file
```

## Your First Dart Project

Create your first Dart project to verify everything works.

### Using Command Line

```bash
# Create project directory
mkdir zakat_calculator
cd zakat_calculator

# Initialize Dart project
dart create . --template console

# => Creates project structure:
# => - bin/         (executable scripts)
# => - lib/         (library code)
# => - test/        (test files)
# => - pubspec.yaml (project configuration)
```

**Project structure created**:

```
zakat_calculator/
├── bin/
│   └── zakat_calculator.dart  # Main entry point
├── lib/
│   └── zakat_calculator.dart  # Library code
├── test/
│   └── zakat_calculator_test.dart  # Unit tests
├── pubspec.yaml               # Dependencies
├── analysis_options.yaml      # Linting rules
└── README.md                  # Project documentation
```

### Hello World Program

Edit `bin/zakat_calculator.dart`:

```dart
void main() {
  // => Entry point of the program
  print('As-salamu alaykum! Welcome to Dart!'); // => Output greeting
                                                // => Prints to console
}
```

**Run the program**:

```bash
dart run bin/zakat_calculator.dart
# => Output: As-salamu alaykum! Welcome to Dart!
```

## Package Management with pubspec.yaml

Dart uses `pubspec.yaml` for dependency management, similar to `package.json` in Node.js or `requirements.txt` in Python.

### Understanding pubspec.yaml

```yaml
name: zakat_calculator # => Project name (lowercase with underscores)
description: A Zakat calculator application. # => Brief description
version: 1.0.0 # => Semantic versioning

environment:
  sdk:
    ">=3.0.0 <4.0.0" # => Dart SDK version constraints
    # => Requires Dart 3.x

dependencies: # => Runtime dependencies
  intl:
    ^0.18.0 # => Internationalization package
    # => Caret (^) allows compatible updates

dev_dependencies: # => Development-only dependencies
  lints: ^3.0.0 # => Dart linting rules
  test: ^1.24.0 # => Testing framework
```

### Installing Packages

**Add a package**:

Edit `pubspec.yaml` to add dependencies, then run:

```bash
dart pub get
# => Downloads dependencies
# => Creates pubspec.lock (lockfile)
# => Updates .dart_tool/ directory
```

**Common packages**:

- **intl** - Internationalization and formatting
- **http** - HTTP client for REST APIs
- **path** - File path manipulation
- **test** - Unit testing framework

### Using Packages in Code

```dart
import 'package:intl/intl.dart'; // => Import intl package

void main() {
  var formatter = NumberFormat.currency(
    // => Create currency formatter
    locale: 'id_ID',              // => Indonesian locale
    symbol: 'Rp',                 // => Currency symbol: Rupiah
    decimalDigits: 0,             // => No decimal places
  );

  double zakat = 1250000;         // => Zakat amount: 1,250,000 IDR
  print('Zakat: ${formatter.format(zakat)}'); // => Output: Zakat: Rp1.250.000
                                              // => Formatted with locale
}
```

## Running Dart Programs

### Direct Execution

Run a Dart file directly:

```bash
dart run bin/program.dart
# => Executes program
# => Uses JIT compilation (fast startup for development)
```

### Compiled Executable

Compile to native executable for production:

```bash
dart compile exe bin/program.dart -o zakat_calculator
# => Compiles to native machine code
# => Output: zakat_calculator executable
# => Uses AOT compilation (fast execution)

./zakat_calculator
# => Run compiled executable
# => No Dart VM required
```

**Benefits of compilation**:

- Faster startup time
- No Dart SDK required on target machine
- Smaller memory footprint
- Production-ready deployment

### Running Tests

```bash
dart test
# => Runs all tests in test/ directory
# => Reports pass/fail status
```

## Verifying Your Installation

Run this checklist to ensure everything is properly installed:

### Dart SDK Check

```bash
dart --version
# => Should output: Dart SDK version: 3.x.x
```

### Package Manager Check

```bash
dart pub --version
# => Should output: Dart SDK version: 3.x.x
```

### Create and Run Test Project

```bash
mkdir test_project
cd test_project
dart create . --template console
dart run
# => Should output: Hello world: 42!
```

### IDE Plugin Check

Open your IDE and verify:

- [ ] Dart syntax highlighting works
- [ ] Code completion suggests Dart keywords
- [ ] Can run Dart files from IDE
- [ ] Error highlighting shows syntax errors

### Flutter Check (If Installed)

```bash
flutter doctor
# => Checks Flutter installation
# => Reports any issues

flutter --version
# => Shows Flutter and Dart versions
```

## Troubleshooting Common Issues

### Dart Command Not Found

**Problem**: `dart: command not found`

**Solution**:

- **Windows**: Ensure Dart is added to system PATH
- **macOS/Linux**: Add Dart to PATH in `.bashrc`, `.zshrc`, or `.bash_profile`:

```bash
export PATH="$PATH:/usr/lib/dart/bin"
```

Reload shell configuration:

```bash
source ~/.bashrc  # or ~/.zshrc
```

### Permission Denied on Linux/macOS

**Problem**: Permission denied when running `dart pub get`

**Solution**: Fix ownership of Dart cache:

```bash
sudo chown -R $USER ~/.pub-cache
```

### IDE Not Recognizing Dart

**Problem**: IDE doesn't recognize Dart syntax or SDK

**Solution**:

1. Verify Dart plugin is installed and enabled
2. Configure Dart SDK path in IDE settings
3. Restart IDE after installing plugins
4. Ensure project has `pubspec.yaml` file

### Flutter Doctor Reports Issues

**Problem**: `flutter doctor` shows X marks

**Solution**: Follow the specific recommendations from `flutter doctor` output. Common fixes:

- Install missing Android SDK components
- Accept Android licenses: `flutter doctor --android-licenses`
- Install Xcode command-line tools (macOS): `xcode-select --install`

## Next Steps

Now that you have Dart installed and configured, proceed to:

1. **Quick Start** - Build a complete Zakat Calculator application
2. **By Example** - Learn through annotated code examples
3. **By Concept** - Deep dive into Dart concepts

Continue to [Quick Start](/en/learn/software-engineering/programming-languages/dart/quick-start) to build your first real Dart application.
