---
title: Initial Setup
date: 2026-02-02T00:00:00+07:00
draft: false
weight: 100001
description: Install .NET SDK, configure F# development environment, and verify your F# Interactive setup - achieving 0-5% language coverage
tags: ["f-sharp", "fsharp", "dotnet", "setup", "installation", "tutorial", "ionide"]
---

**Get F# running on your machine.** This tutorial guides you through installing the .NET SDK, setting up your F# development environment with Ionide, and verifying everything works with F# Interactive. By the end, you'll have a working F# development environment ready for functional programming.

## What You'll Achieve

- Install .NET 8 SDK (latest LTS version)
- Configure VS Code with Ionide extension
- Verify installation with F# Interactive (REPL)
- Understand F# project structure and tooling
- Set up your first F# project

**Coverage**: 0-5% (environment setup foundation)

## Prerequisites

- **Operating System**: Windows 10/11, macOS 10.15+, or Linux (Ubuntu 20.04+, Fedora 36+, Debian 11+)
- **Disk Space**: 500 MB for .NET SDK, 200 MB for VS Code
- **Internet Connection**: Required for downloading SDK and extensions

**No prior programming knowledge required** - this tutorial starts from zero.

## Step 1: Install .NET SDK

F# uses the same .NET SDK as C#. If you already have .NET SDK 8 installed, skip to Step 2.

### Windows

**Option A: Installer (Recommended)**

1. Visit [https://dotnet.microsoft.com/download](https://dotnet.microsoft.com/download)
2. Click "Download .NET 8.0 SDK" (LTS version)
3. Run the downloaded installer (.exe file)
4. Follow installation wizard (default options work well)
5. Restart terminal/PowerShell after installation

**Option B: Winget (Command Line)**

```powershell
winget install Microsoft.DotNet.SDK.8
```

**Verification**:

```powershell
dotnet --version
# => Should show 8.0.xxx
```

### macOS

**Option A: Installer**

1. Visit [https://dotnet.microsoft.com/download](https://dotnet.microsoft.com/download)
2. Download .NET 8.0 SDK for macOS
3. Open the .pkg file and follow installation wizard
4. Restart terminal after installation

**Option B: Homebrew (Recommended)**

```bash
brew install --cask dotnet-sdk
```

**Verification**:

```bash
dotnet --version
# => Should show 8.0.xxx
```

### Linux (Ubuntu/Debian)

```bash
# Add Microsoft package repository
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

# Install .NET SDK
sudo apt-get update
sudo apt-get install -y dotnet-sdk-8.0

# Verify installation
dotnet --version
# => Should show 8.0.xxx
```

### Linux (Fedora)

```bash
# Add Microsoft package repository
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
sudo wget -O /etc/yum.repos.d/microsoft-prod.repo https://packages.microsoft.com/config/fedora/37/prod.repo

# Install .NET SDK
sudo dnf install dotnet-sdk-8.0

# Verify installation
dotnet --version
# => Should show 8.0.xxx
```

## Step 2: Install VS Code with Ionide

**Why VS Code + Ionide?**

- **Ionide** is the premier F# extension for VS Code
- Excellent F# Interactive (REPL) integration
- IntelliSense with type information tooltips
- Inline error checking and suggestions
- Free and open source
- Cross-platform (Windows, macOS, Linux)

**Installation**:

1. Download VS Code from [https://code.visualstudio.com/](https://code.visualstudio.com/)
2. Install Ionide-fsharp extension:
   - Open VS Code
   - Click Extensions icon (or Ctrl+Shift+X / Cmd+Shift+X)
   - Search for "Ionide-fsharp"
   - Click Install

**Recommended Extensions**:

- **Ionide-fsharp** (Ionide) - Core F# support with REPL integration
- **Ionide-Paket** (Ionide) - Package management
- **.NET Install Tool** (Microsoft) - Manage SDK versions

## Step 3: Verify Installation with F# Interactive

F# Interactive (FSI) is a REPL (Read-Eval-Print-Loop) that evaluates F# code interactively.

### Start F# Interactive from Terminal

```bash
dotnet fsi
# => Microsoft (R) F# Interactive version 12.8.xxx
# => For help type #help;;
# >
```

### Try Basic F# Commands

```fsharp
> 2 + 3;;
val it: int = 5

> let name = "Alice";;
val name: string = "Alice"

> printfn "Hello, %s!" name;;
Hello, Alice!
val it: unit = ()

> #quit;;
# (exits FSI)
```

**Success!** If FSI responds with results, your F# environment is working correctly.

### F# Interactive in VS Code

Ionide integrates FSI directly into VS Code:

1. Open VS Code
2. Create new file: `test.fsx` (F# script file)
3. Type: `printfn "Hello from F#!"`
4. Highlight the line
5. Press Alt+Enter (or Cmd+Enter on macOS)
6. FSI panel opens at bottom with output: `Hello from F#!`

## Step 4: Create Your First F# Project

### Create Console Application

```bash
# Create new directory
mkdir test-fsharp
cd test-fsharp

# Create new F# console application
dotnet new console -lang F#

# You should see:
# => The template "Console App" was created successfully.
# => Processing post-creation actions...
# => Running 'dotnet restore'...
# => Restore succeeded.
```

### Inspect Generated Files

```bash
ls
# => Program.fs          # Your main code file
# => test-fsharp.fsproj  # Project file (build configuration)
# => obj/                # Build artifacts (ignore)
```

### Examine Program.fs

```bash
cat Program.fs
```

You should see:

```fsharp
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
```

**Note**: F# uses `printfn` (print formatted with newline) instead of C#'s `Console.WriteLine`.

### Run Your First Program

```bash
dotnet run
# => Hello from F#
```

**Success!** If you see "Hello from F#", your F# environment is working correctly.

## Step 5: Understanding F# Project Structure

### Key Commands

```bash
# Version information
dotnet --version        # SDK version
dotnet --info          # Detailed environment info

# F# Interactive
dotnet fsi             # Start REPL
dotnet fsi script.fsx  # Run F# script file

# Project commands
dotnet new             # List available templates
dotnet new console -lang F#    # Create F# console app
dotnet new classlib -lang F#   # Create F# class library
dotnet new web -lang F#        # Create F# web app

# Build and run
dotnet build           # Compile project
dotnet run             # Build and run
dotnet clean           # Remove build outputs

# Package management
dotnet add package <name>    # Add NuGet package
dotnet restore              # Restore dependencies
```

### Project File (.fsproj)

The `.fsproj` file contains build configuration:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="Program.fs" />
  </ItemGroup>
</Project>
```

**Key differences from C#**:

- **File order matters**: `<Compile Include="...">` items must be in dependency order
- **No implicit usings**: F# has simpler namespace auto-opening
- **Explicit file list**: All .fs files must be listed in order

## Step 6: F# Interactive Features

### Script Files vs Project Files

- **Script files** (.fsx): Standalone scripts run with `dotnet fsi`
- **Project files** (.fs): Compiled files in .fsproj projects

### Load Dependencies in FSI

```fsharp
#r "nuget: FSharp.Data"  // Load NuGet package
#load "Helper.fs"        // Load local file

open FSharp.Data         // Use loaded package
```

### FSI Directives

```fsharp
#help;;           // Show help
#time "on";;      // Show execution time
#time "off";;     // Hide execution time
#quit;;           // Exit FSI
```

## Step 7: Configure VS Code for F# Development

### Recommended Settings

Create `.vscode/settings.json` in your project:

```json
{
  "FSharp.enableTreeView": true,
  "FSharp.showExplorerOnStartup": false,
  "editor.formatOnSave": true,
  "FSharp.fsac.silencedLogs": ["Lsp.LanguageServerFeatures"]
}
```

### Ionide Features

- **F5** - Start debugging
- **Ctrl+F5** - Run without debugging
- **Alt+Enter** - Send line to FSI
- **Ctrl+Shift+P** → "FSI: Start" - Start F# Interactive panel
- **Ctrl+.** - Quick fixes and suggestions
- **F12** - Go to definition
- **Shift+F12** - Find all references

### Signature Help

Hover over any function to see:

- Type signature
- Documentation comments
- Parameter information

```fsharp
List.map  // Hover shows: ('T -> 'U) -> 'T list -> 'U list
```

## Troubleshooting

### "dotnet: command not found"

**Solution**: SDK not in PATH. Restart terminal or add manually:

- **Windows**: Already in PATH after installer
- **macOS/Linux**: Add to `~/.bashrc` or `~/.zshrc`:

```bash
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$PATH:$DOTNET_ROOT
```

### "Cannot find F# compiler"

**Solution**: Verify F# tools installed with SDK:

```bash
dotnet new console -lang F#
# If this works, F# is installed correctly
```

### Ionide not working in VS Code

**Solution**:

1. Ensure Ionide-fsharp extension installed
2. Open folder containing `.fsproj` file (not individual .fs file)
3. Wait for "Workspace loaded" notification in bottom status bar
4. If still broken: Reload VS Code (Ctrl+Shift+P → "Reload Window")

### "The current .NET SDK does not support targeting .NET 8.0"

**Solution**: Update to .NET 8 SDK:

```bash
dotnet --version
# If < 8.0, download and install from https://dotnet.microsoft.com/download
```

## F# vs C# Project Differences

| Aspect                   | F#                                  | C#                       |
| ------------------------ | ----------------------------------- | ------------------------ |
| **File extension**       | .fs (project), .fsx (script)        | .cs                      |
| **File order**           | Matters (dependency order)          | Doesn't matter           |
| **REPL**                 | F# Interactive (FSI)                | C# Interactive (limited) |
| **Script support**       | First-class (.fsx files)            | Basic (.csx files)       |
| **Implicit usings**      | No (simpler auto-open)              | Yes (can be disabled)    |
| **Top-level statements** | Not needed (no ceremony by default) | Yes (C# 9+)              |

## What's Next?

With your environment set up, you're ready to start learning F#:

1. **[Quick Start](/en/learn/software-engineering/programming-languages/f-sharp/quick-start)** - Write your first meaningful F# program with functional programming touchpoints
2. **[By Example: Beginner](/en/learn/software-engineering/programming-languages/f-sharp/by-example/beginner)** - Learn through 30 annotated code examples

## Key Takeaways

- **F# runs on .NET SDK** - Same platform as C#, full interoperability
- **Ionide is the F# VS Code extension** - Excellent REPL integration and tooling
- **F# Interactive (FSI) is powerful** - REPL for exploration and scripting
- **File order matters in F#** - Dependencies must be listed before dependents
- **Script files (.fsx) are first-class** - Rapid prototyping without project setup

## Why It Matters

F# Interactive revolutionizes the development workflow - you can explore APIs, test algorithms, and validate business logic interactively before committing to compiled code. This REPL-driven development reduces feedback cycles and increases productivity. The Ionide extension brings this power directly into VS Code, making F# development seamless across all platforms. Script files enable rapid prototyping and data exploration without the overhead of creating full projects, making F# ideal for experimentation and learning.
