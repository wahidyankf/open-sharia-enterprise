---
title: "Initial Setup"
date: 2025-12-09T00:00:00+07:00
draft: false
weight: 1000001
description: "Get Go installed and running your first program"
tags:
  - golang
  - installation
  - setup
  - hello-world
---

**Get Go up and running quickly.** This guide covers installation and your first "Hello, World!" program. No programming experience needed.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Go installed and verified
- ✅ Your first Go program running
- ✅ Confidence that Go works on your system

## 🔄 Installation Verification Flow

Here's the simple path from download to running code:

```mermaid
graph TD
    A[Download Go from go.dev/dl] --> B[Install on Your OS]
    B --> C[Verify: go version]
    C --> D{Version Shows?}
    D -->|Yes| E[Create hello.go]
    D -->|No| F[Check PATH/Restart Terminal]
    F --> C
    E --> G[Run: go run hello.go]
    G --> H{Prints Hello World?}
    H -->|Yes| I[Success! Go Works]
    H -->|No| J[Check File Location]
    J --> G

    style I fill:#029E73,stroke:#029E73,stroke-width:2px
    style D fill:#DE8F05,stroke:#DE8F05,stroke-width:2px
    style H fill:#DE8F05,stroke:#DE8F05,stroke-width:2px
```

This diagram shows every verification checkpoint - you'll know immediately if something's wrong and where to look.

## 📋 Prerequisites

- Basic familiarity with your computer's terminal/command line
- No programming experience needed
- Internet connection to download Go

## 💾 Step 1: Download and Install Go

### Windows

1. Visit [go.dev/dl](https://go.dev/dl)
2. Click the Windows installer (`.msi` file)
3. Run the installer and follow the prompts
4. Default installation path (`C:\Program Files\Go`) is fine
5. Installer automatically adds Go to PATH

**Windows Installation Tips**:

- The installer adds Go to `C:\Program Files\Go\bin`
- PATH is updated automatically (no manual configuration needed)
- If using chocolatey: `choco install golang` (alternative method)
- Administrator privileges required for installation
- Windows Defender might scan the installer (normal, let it finish)

### macOS

1. Visit [go.dev/dl](https://go.dev/dl)
2. Choose the appropriate version:
   - **Apple Silicon** (M1/M2/M3): Download `go*.darwin-arm64.pkg`
   - **Intel**: Download `go*.darwin-amd64.pkg`
3. Run the installer and follow the prompts
4. Default installation path (`/usr/local/go`) is fine

**macOS Installation Tips**:

- Apple Silicon users: verify you download the ARM64 version (not AMD64)
- Intel users: download the AMD64 version
- Installer automatically adds `/usr/local/go/bin` to PATH
- Xcode Command Line Tools might be required for some packages
- If using homebrew: `brew install go` (alternative method)
- macOS may show "unidentified developer" warning (allow in Security & Privacy)

### Linux

Using package manager (Ubuntu/Debian):

```bash
sudo apt update

sudo apt install golang-go

go version
```

**Note**: Package manager versions might be older. For the latest version, use manual installation below.

Or, manual installation:

1. Visit [go.dev/dl](https://go.dev/dl)
2. Download the Linux tarball (`.tar.gz`)
3. Extract to `/usr/local`:

```bash
tar -C /usr/local -xzf go*.tar.gz
```

4. Add Go to your PATH by adding this line to `~/.bashrc` or `~/.zshrc`:

```bash
export PATH=$PATH:/usr/local/go/bin
```

Then reload your shell:

```bash
source ~/.bashrc  # or source ~/.zshrc
```

**Linux Installation Tips**:

- Manual installation gives you the latest version
- Package manager installation is easier but may be outdated
- For Arch Linux: `sudo pacman -S go`
- For Fedora: `sudo dnf install golang`
- Check your shell (echo $SHELL) to know which profile to edit
- Some distributions place Go in `/usr/lib/go` instead of `/usr/local/go`

## ✅ Step 2: Verify Installation

Open a new terminal/command prompt and run:

```bash
go version
```

**Expected output**:

```
go version go1.25.5 linux/amd64
```

(Version number will vary - Go 1.24.x or 1.25.x depending on your installation)

**If you get an error**:

- "command not found": Go isn't in your PATH. Try restarting your terminal.
- On macOS/Linux: Edit your shell profile and reload it.

**Additional verification**:

```bash
go env

```

## 🚀 Step 3: Create Your First Program

Create a new file called `hello.go`:

```go
package main

import "fmt"

func main() {
	fmt.Println("Hello, World!")
}
```

**What this means** (don't memorize, just read):

- `package main` - This is the main program (not a library)
- `import "fmt"` - Use the `fmt` package for printing
- `func main()` - The entry point (where the program starts)
- `fmt.Println(...)` - Print text to the screen

## ▶️ Step 4: Run Your Program

In the same directory as `hello.go`:

```bash
go run hello.go
```

**Expected output**:

```
Hello, World!
```

Congratulations! You've run your first Go program!

## 📦 Step 5: Build an Executable

Want to create a standalone program you can run without `go run`?

```bash
go build hello.go
```

This creates an executable:

- **Windows**: `hello.exe`
- **macOS/Linux**: `hello`

Run the executable:

```bash
hello.exe

./hello
```

Both will output:

```
Hello, World!
```

## 🧩 Understanding Go Modules (Optional)

Go uses modules to manage dependencies. For simple programs like hello.go, you don't need a module. But as you advance, you'll use:

```bash
go mod init example.com/hello

```

You'll learn more about modules in the [Quick Start tutorial](/en/learn/software-engineering/programming-language/golang/tutorials/quick-start) and [Manage Go modules effectively](/en/learn/software-engineering/programming-language/golang/how-to/manage-go-modules).

## ✔️ Verification Checklist

Before moving forward, verify:

- [ ] `go version` shows Go 1.24.x or 1.25.x (current stable versions)
- [ ] `go env` shows GOROOT and GOPATH correctly
- [ ] `go run hello.go` prints "Hello, World!"
- [ ] `go build hello.go` creates an executable
- [ ] The executable runs and prints "Hello, World!"

## 🎉 You're Done!

You've successfully installed Go and run your first program. You're ready for the next step.

## 📚 What's Next?

Now that Go is working, you have two paths:

**Quick learner**: [Golang Quick Start](/en/learn/software-engineering/programming-language/golang/tutorials/quick-start)

- Learn core syntax and basic patterns
- Understand enough to explore Go independently

**Comprehensive learner**: [Complete Beginner's Guide to Go](/en/learn/software-engineering/programming-language/golang/tutorials/beginner)

- Comprehensive coverage of Go fundamentals
- Hands-on exercises and practice
- Ready to build real applications

**Problem solver**: [Golang Cookbook](/en/learn/software-engineering/programming-language/golang/how-to/cookbook)

- Practical recipes for common patterns
- Real-world problem solving

## 🆘 Troubleshooting

**Problem**: "go: command not found" after installation

**Solution**:

1. Verify installation completed successfully
2. Restart your terminal/command prompt
3. On macOS: Go installs to `/usr/local/go`. If error persists, add to your shell profile:
   ```bash
   export PATH=$PATH:/usr/local/go/bin
   ```
4. On Linux with manual installation: Verify the tarball was extracted to `/usr/local`

**Problem**: Multiple Go versions installed

**Solution**: Verify which Go is in use:

```bash
which go  # macOS/Linux
where go  # Windows
```

Remove old versions and keep only one.

**Problem**: "hello.go: file not found" when running

**Solution**: Ensure you're in the same directory as `hello.go`. Check with:

```bash
ls  # macOS/Linux, shows files in current directory
dir  # Windows, shows files in current directory
```

**Problem**: "Permission denied" when running executable on macOS/Linux

**Solution**: Make it executable:

```bash
chmod +x hello
./hello
```

**Problem**: Wrong architecture downloaded (M1/M2 Mac with AMD64 installer)

**Solution**: Check your Mac's chip:

```bash
uname -m
```

Download the correct installer for your architecture.

**Problem**: "go: cannot find main module" error

**Solution**: You don't need a module for simple programs. Just run `go run hello.go` without initializing a module. Modules are covered in [Quick Start tutorial](/en/learn/software-engineering/programming-language/golang/tutorials/quick-start).

**Problem**: Antivirus blocks Go installation (Windows)

**Solution**: Temporarily disable antivirus during installation, or add an exception for the Go installer. Go is safe - downloaded from official go.dev site.

**Problem**: `go env` shows incorrect GOPATH

**Solution**: GOPATH defaults to `$HOME/go` (or `%USERPROFILE%\go` on Windows). To change it:

```bash
export GOPATH=$HOME/mygopath

```

Then restart your terminal.

## 🔧 Common Installation Issues

**Issue**: Package manager Go version too old

**Fix**: Use manual installation to get the latest version. Remove package manager version first:

```bash
sudo apt remove golang-go

```

**Issue**: Go compiles but programs crash immediately

**Fix**: Verify you downloaded the correct architecture:

- Apple Silicon (M1/M2/M3): ARM64 version
- Intel Mac: AMD64 version
- Linux: Check with `uname -m` and download matching version

**Issue**: Cannot write to GOPATH directory

**Fix**: GOPATH should be in your home directory (where you have write permissions). Don't use `/usr/local/go` as GOPATH - that's GOROOT (where Go itself is installed).

```bash
export GOPATH=$HOME/go

export GOPATH=/usr/local/go
```

---

**Still stuck?** Visit [go.dev/doc](https://go.dev/doc) or the [Go community forums](https://go.dev/help).
