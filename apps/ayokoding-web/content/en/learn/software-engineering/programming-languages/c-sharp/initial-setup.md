---
title: Initial Setup
date: 2026-02-02T00:00:00+07:00
draft: false
weight: 100001
description: Install .NET SDK, configure your development environment, and verify your C# setup - achieving 0-5% language coverage
tags: ["c-sharp", "csharp", "dotnet", "setup", "installation", "tutorial"]
---

**Get C# running on your machine.** This tutorial guides you through installing the .NET SDK, setting up your development environment, and verifying everything works. By the end, you'll have a working C# development environment ready for learning.

## What You'll Achieve

- Install .NET 8 SDK (latest LTS version)
- Configure your development environment (VS Code or Visual Studio)
- Verify installation with a test program
- Understand .NET SDK structure and tooling
- Set up your first C# project

**Time to complete**: 15-30 minutes (varies by internet speed and platform)

**Coverage**: 0-5% (environment setup foundation)

## Prerequisites

- **Operating System**: Windows 10/11, macOS 10.15+, or Linux (Ubuntu 20.04+, Fedora 36+, Debian 11+)
- **Disk Space**: 500 MB for .NET SDK, 2 GB for Visual Studio (optional), 200 MB for VS Code
- **Internet Connection**: Required for downloading SDK and extensions

**No prior programming knowledge required** - this tutorial starts from zero.

## Step 1: Install .NET SDK

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

## Step 2: Choose Your Editor

### Option A: Visual Studio Code (Recommended for Learning)

**Why VS Code?**

- Lightweight and fast
- Cross-platform (Windows, macOS, Linux)
- Excellent C# extension with IntelliSense
- Free and open source
- Great for learning before committing to full IDE

**Installation**:

1. Download from [https://code.visualstudio.com/](https://code.visualstudio.com/)
2. Install C# Dev Kit extension:
   - Open VS Code
   - Click Extensions icon (or Ctrl+Shift+X / Cmd+Shift+X)
   - Search for "C# Dev Kit"
   - Click Install (installs C# extension + C# Dev Kit)

**Verification**:

1. Open VS Code
2. Create new file: `test.cs`
3. Type: `Console.WriteLine("Hello");`
4. IntelliSense should suggest `WriteLine` (autocomplete working)

### Option B: Visual Studio (Full IDE)

**Why Visual Studio?**

- Full-featured IDE with advanced debugging
- Best for enterprise development
- Built-in project templates and designers
- Windows and macOS only (no Linux)

**Installation**:

1. Download Visual Studio 2022 Community (free) from [https://visualstudio.microsoft.com/](https://visualstudio.microsoft.com/)
2. During installation, select ".NET desktop development" workload
3. Optional: Also select "ASP.NET and web development" for web apps

### Option C: JetBrains Rider

**Why Rider?**

- Professional IDE with excellent refactoring
- Cross-platform
- Powerful code analysis
- Commercial (paid, 30-day trial available)

**Installation**: Download from [https://www.jetbrains.com/rider/](https://www.jetbrains.com/rider/)

**For this tutorial series, we'll use VS Code** in examples for maximum accessibility.

## Step 3: Verify Installation

Let's confirm everything works by creating and running a simple program:

### Create Test Project

```bash
# Create new directory
mkdir test-csharp
cd test-csharp

# Create new console application
dotnet new console

# You should see:
# => The template "Console App" was created successfully.
# => Processing post-creation actions...
# => Running 'dotnet restore'...
# => Restore succeeded.
```

### Inspect Generated Files

```bash
ls
# => Program.cs    # Your main code file
# => test-csharp.csproj  # Project file (build configuration)
# => obj/          # Build artifacts (ignore)
```

### Examine Program.cs

```bash
cat Program.cs
```

You should see:

```csharp
// See https://aka.ms/new-console-template for more information
Console.WriteLine("Hello, World!");
```

### Run Your First Program

```bash
dotnet run
# => Hello, World!
```

**Success!** If you see "Hello, World!", your C# environment is working correctly.

## Step 4: Understanding .NET SDK Structure

### Key Commands

```bash
# Version information
dotnet --version        # SDK version
dotnet --info          # Detailed environment info

# Project commands
dotnet new             # List available templates
dotnet new console     # Create console app
dotnet new webapi      # Create Web API
dotnet new classlib    # Create class library

# Build and run
dotnet build           # Compile project
dotnet run             # Build and run
dotnet clean           # Remove build outputs

# Package management
dotnet add package <name>    # Add NuGet package
dotnet restore              # Restore dependencies
```

### Project File (.csproj)

The `.csproj` file contains build configuration:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>
</Project>
```

**Key settings**:

- `OutputType`: Exe (executable) vs Library
- `TargetFramework`: net8.0 (.NET 8)
- `ImplicitUsings`: Auto-import common namespaces
- `Nullable`: Enable nullable reference types

## Step 5: Configure VS Code for C# Development

If using VS Code, set up these productivity features:

### Recommended Settings

Create `.vscode/settings.json` in your project:

```json
{
  "omnisharp.enableRoslynAnalyzers": true,
  "omnisharp.enableEditorConfigSupport": true,
  "editor.formatOnSave": true,
  "csharp.semanticHighlighting.enabled": true
}
```

### Recommended Extensions

- **C# Dev Kit** (Microsoft) - Core C# support
- **C#** (Microsoft) - Language support
- **.NET Install Tool** (Microsoft) - Manage SDK versions
- **NuGet Package Manager** (jmrog) - Browse and install packages

### Keyboard Shortcuts

- `F5` - Start debugging
- `Ctrl+F5` - Run without debugging
- `F12` - Go to definition
- `Shift+F12` - Find all references
- `Ctrl+.` - Quick actions and refactorings

## Troubleshooting

### "dotnet: command not found"

**Solution**: SDK not in PATH. Restart terminal or add manually:

- **Windows**: Already in PATH after installer
- **macOS/Linux**: Add to `~/.bashrc` or `~/.zshrc`:

```bash
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$PATH:$DOTNET_ROOT
```

### "It was not possible to find any compatible framework version"

**Solution**: Wrong .NET version or missing runtime.

```bash
# Check installed SDKs
dotnet --list-sdks

# Check installed runtimes
dotnet --list-runtimes

# If missing, reinstall .NET 8 SDK
```

### IntelliSense not working in VS Code

**Solution**:

1. Ensure C# Dev Kit extension installed
2. Open folder containing `.csproj` file (not individual .cs file)
3. Wait for "Restore complete" notification in bottom status bar
4. If still broken: Reload VS Code (Ctrl+Shift+P → "Reload Window")

### "The current .NET SDK does not support targeting .NET 8.0"

**Solution**: Update to .NET 8 SDK:

```bash
dotnet --version
# If < 8.0, download and install from https://dotnet.microsoft.com/download
```

## What's Next?

With your environment set up, you're ready to start learning C#:

1. **[Quick Start](/en/learn/software-engineering/programming-languages/c-sharp/quick-start)** - Write your first meaningful C# program with basic syntax touchpoints
2. **[By Example: Beginner](/en/learn/software-engineering/programming-languages/c-sharp/by-example/beginner)** - Learn through 30 annotated code examples

## Key Takeaways

- **.NET SDK includes compiler, runtime, and tools** - Everything needed for C# development
- **dotnet CLI is your primary tool** - Build, run, test, and manage projects from command line
- **Cross-platform from day one** - Same code runs on Windows, macOS, and Linux
- **VS Code provides excellent C# support** - Free, lightweight, and powerful enough for professional work

## Why It Matters

.NET 8 represents Microsoft's unified platform vision - one SDK for desktop, web, mobile, cloud, and IoT development. Installing the SDK gives you access to ASP.NET Core for web development, Entity Framework Core for databases, MAUI for cross-platform mobile apps, and thousands of NuGet packages for every domain. The cross-platform nature means you can develop on macOS, deploy to Linux servers, and target Windows desktops with a single codebase - a capability that was impossible with legacy .NET Framework.
