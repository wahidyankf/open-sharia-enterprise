---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get Elixir installed and running on your system - Erlang/OTP installation, Elixir setup, IEx access, and your first program"
tags: ["elixir", "installation", "setup", "erlang", "beam", "beginner"]
---

**Want to start programming in Elixir?** This initial setup guide gets Elixir installed and working on your system in minutes. By the end, you'll have Elixir running and will execute your first program interactively.

This tutorial provides 0-5% coverage - just enough to get Elixir working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/programming-languages/elixir/quick-start) (5-30% coverage).

## Prerequisites

Before installing Elixir, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- A text editor (VS Code, IntelliJ IDEA, Emacs, Vim, or any editor)
- Basic command-line navigation skills
- Erlang/OTP (we'll install this first)

No prior Erlang or functional programming experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** Erlang/OTP (the BEAM virtual machine)
2. **Install** Elixir on your operating system
3. **Access** IEx (Interactive Elixir shell)
4. **Execute** Elixir code interactively
5. **Create** and run your first Elixir project with Mix

## Erlang/OTP Installation

Elixir runs on the Erlang VM (BEAM), so we need Erlang first.

### Windows Erlang/OTP Installation

**Step 1: Download Erlang/OTP Installer**

1. Visit [https://www.erlang.org/downloads](https://www.erlang.org/downloads)
2. Download Windows installer for OTP 26 or later (e.g., `otp_win64_26.2.exe`)

**Alternative: Use Chocolatey**

If you have Chocolatey installed:

```powershell
choco install erlang
```

**Step 2: Run Installer**

1. Double-click downloaded `.exe` file
2. Follow installation wizard:
   - Accept license agreement
   - Keep default installation path (`C:\Program Files\Erlang OTP`)
   - Click Install
3. Click Finish

**Step 3: Verify Installation**

Open new Command Prompt or PowerShell:

```cmd
erl -version
```

Expected output:

```
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 14.2
```

**Troubleshooting Windows**:

- If `erl` not found, add `C:\Program Files\Erlang OTP\bin` to PATH
- Restart Command Prompt after installation

### macOS Erlang/OTP Installation

**Step 1: Install via Homebrew**

```bash
brew install erlang
```

Homebrew downloads and installs Erlang/OTP automatically.

**Step 2: Verify Installation**

```bash
erl -version
```

Expected output:

```
Erlang (SMP,ASYNC_THREADS,JIT) (BEAM) emulator version 14.2
```

**Alternative: asdf Version Manager (Recommended for developers)**

asdf allows managing multiple Erlang and Elixir versions:

```bash
brew install asdf

echo -e "\n. $(brew --prefix asdf)/libexec/asdf.sh" >> ~/.zshrc
source ~/.zshrc

asdf plugin add erlang

asdf install erlang 26.2

asdf global erlang 26.2
```

**Troubleshooting macOS**:

- If Homebrew installation fails, run `brew update` first
- For Apple Silicon Macs, Homebrew installs ARM-optimized builds automatically

### Linux Erlang/OTP Installation

**Ubuntu/Debian**:

```bash
sudo apt update
sudo apt install build-essential autoconf m4 libncurses5-dev \
  libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev libgl1-mesa-dev \
  libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop \
  libxml2-utils libncurses-dev openjdk-11-jdk

wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt update

sudo apt install esl-erlang
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install erlang
```

**Arch Linux**:

```bash
sudo pacman -S erlang
```

**Alternative: asdf Version Manager (Recommended)**

```bash
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0

echo '. "$HOME/.asdf/asdf.sh"' >> ~/.bashrc
source ~/.bashrc

asdf plugin add erlang

sudo apt install build-essential autoconf m4 libncurses5-dev \
  libwxgtk3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev \
  libssh-dev unixodbc-dev xsltproc fop libxml2-utils

asdf install erlang 26.2
asdf global erlang 26.2
```

**Verify Installation**:

```bash
erl -version
```

Expected output:

```
Erlang (SMP,ASYNC_THREADS,JIT) (BEAM) emulator version 14.2
```

**Troubleshooting Linux**:

- If compilation fails, ensure all build dependencies installed
- Check disk space (Erlang compilation requires ~2GB temporary space)

## Elixir Installation

With Erlang installed, now install Elixir.

### Windows Elixir Installation

**Step 1: Download Elixir Installer**

Visit [https://elixir-lang.org/install.html#windows](https://elixir-lang.org/install.html#windows)

Use the Web Installer (recommended) or download precompiled zip.

**Alternative: Chocolatey**

```powershell
choco install elixir
```

**Step 2: Verify Installation**

Open new Command Prompt:

```cmd
elixir --version
```

Expected output:

```
Erlang/OTP 26 [erts-14.2] [64-bit] [smp:8:8] [async-threads:1] [jit]

Elixir 1.16.0 (compiled with Erlang/OTP 26)
```

**Troubleshooting Windows**:

- Ensure both Erlang and Elixir bin directories are in PATH
- Restart Command Prompt after installation

### macOS Elixir Installation

**Step 1: Install via Homebrew**

```bash
brew install elixir
```

**Alternative: asdf (if you installed Erlang via asdf)**

```bash
asdf plugin add elixir

asdf install elixir 1.16.0-otp-26

asdf global elixir 1.16.0-otp-26
```

**Step 2: Verify Installation**

```bash
elixir --version
```

Expected output:

```
Erlang/OTP 26 [erts-14.2] [64-bit] [smp:10:10] [async-threads:1] [jit]

Elixir 1.16.0 (compiled with Erlang/OTP 26)
```

**Troubleshooting macOS**:

- If `elixir` not found, ensure Homebrew bin directory in PATH
- Run `brew doctor` to diagnose Homebrew issues

### Linux Elixir Installation

**Ubuntu/Debian** (using Erlang Solutions repository):

```bash
sudo apt update
sudo apt install elixir
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install elixir
```

**Arch Linux**:

```bash
sudo pacman -S elixir
```

**Alternative: asdf (Recommended)**

```bash
asdf plugin add elixir

asdf install elixir 1.16.0-otp-26

asdf global elixir 1.16.0-otp-26
```

**Verify Installation**:

```bash
elixir --version
```

Expected output:

```
Erlang/OTP 26 [erts-14.2] [64-bit] [smp:8:8] [async-threads:1] [jit]

Elixir 1.16.0 (compiled with Erlang/OTP 26)
```

**Troubleshooting Linux**:

- Ensure Elixir compiled for correct OTP version
- If version mismatch, use asdf to manage compatible versions

## Your First IEx Session

IEx (Interactive Elixir) is Elixir's REPL for interactive programming.

### Launch IEx

Open terminal and run:

```bash
iex
```

You'll see the Elixir prompt:

```
Erlang/OTP 26 [erts-14.2] [64-bit] [smp:8:8] [async-threads:1] [jit]

Interactive Elixir (1.16.0) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)>
```

The `iex(1)>` prompt means IEx is ready for input (number increments with each command).

### Execute Your First Elixir Code

**Print "Hello, World!"**:

```elixir
iex(1)> IO.puts("Hello, World!")
```

Output:

```
Hello, World!
:ok
```

Explanation:

- `IO.puts` - Function to print with newline
- `"Hello, World!"` - String argument
- `:ok` - Return value (atom indicating success)

**Basic Arithmetic**:

```elixir
iex(2)> 2 + 3
5

iex(3)> 10 * 5
50

iex(4)> 20 / 4
5.0
```

Elixir uses standard infix operators for arithmetic.

**String Concatenation**:

```elixir
iex(5)> "Hello, " <> "Elixir!"
"Hello, Elixir!"
```

**Define a Variable**:

```elixir
iex(6)> x = 42
42

iex(7)> x
42

iex(8)> message = "Hello, Elixir!"
"Hello, Elixir!"

iex(9)> message
"Hello, Elixir!"
```

**Pattern Matching**:

```elixir
iex(10)> {a, b, c} = {1, 2, 3}
{1, 2, 3}

iex(11)> a
1

iex(12)> b
2
```

**Define a Function**:

```elixir
iex(13)> greet = fn name -> "Hello, #{name}!" end
#Function<44.65746770/1 in :erl_eval.expr/5>

iex(14)> greet.("Alice")
"Hello, Alice!"
```

**Call Standard Library Functions**:

```elixir
iex(15)> String.upcase("elixir")
"ELIXIR"

iex(16)> String.length("Hello")
5

iex(17)> Enum.map([1, 2, 3], fn x -> x * 2 end)
[2, 4, 6]
```

### IEx Helper Functions

**Help**:

```elixir
iex(18)> h()
```

Shows IEx help overview.

**Function Documentation**:

```elixir
iex(19)> h String.upcase
```

Shows documentation for `String.upcase`.

**Module Information**:

```elixir
iex(20)> i "hello"
```

Shows information about the value (type, protocols, etc.).

**Exit IEx**:

Press `Ctrl+C` twice, or:

```elixir
iex(21)> System.halt()
```

## Create Your First Elixir Project

Mix is Elixir's build tool and project manager.

### Verify Mix Installation

Mix installs with Elixir:

```bash
mix --version
```

Expected output:

```
Mix 1.16.0 (compiled with Erlang/OTP 26)
```

### Create a New Project

```bash
mix new hello_elixir
```

Output:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/hello_elixir.ex
* creating test
* creating test/test_helper.exs
* creating test/hello_elixir_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more:

    cd hello_elixir
    mix test

Run "mix help" for more commands.
```

This creates `hello_elixir/` directory with project structure:

```
hello_elixir/
├── mix.exs              # Project configuration
├── README.md            # Project documentation
├── .formatter.exs       # Code formatter configuration
├── .gitignore           # Git ignore rules
├── lib/
│   └── hello_elixir.ex  # Main source file
└── test/
    ├── test_helper.exs
    └── hello_elixir_test.exs
```

### Explore Project Structure

Navigate into project:

```bash
cd hello_elixir
```

**View mix.exs**:

```bash
cat mix.exs
```

Contents:

```elixir
defmodule HelloElixir.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello_elixir,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end
end
```

**View lib/hello_elixir.ex**:

```bash
cat lib/hello_elixir.ex
```

Contents:

```elixir
defmodule HelloElixir do
  @moduledoc """
  Documentation for `HelloElixir`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> HelloElixir.hello()
      :world

  """
  def hello do
    :world
  end
end
```

### Run Tests

Elixir projects include tests by default:

```bash
mix test
```

Output:

```
Compiling 1 file (.ex)
Generated hello_elixir app
..

Finished in 0.03 seconds (0.00s async, 0.03s sync)
1 doctest, 1 test, 0 failures

Randomized with seed 123456
```

Tests pass - your project works!

### Modify the Module

Edit `lib/hello_elixir.ex`:

```elixir
defmodule HelloElixir do
  @moduledoc """
  Greetings module for learning Elixir.
  """

  @doc """
  Greet someone by name.

  ## Examples

      iex> HelloElixir.greet("Alice")
      "Hello, Alice! Welcome to Elixir."

  """
  def greet(name) do
    "Hello, #{name}! Welcome to Elixir."
  end

  @doc """
  Add two numbers.

  ## Examples

      iex> HelloElixir.add(2, 3)
      5

  """
  def add(a, b) do
    a + b
  end
end
```

### Start Project IEx

Launch IEx with project loaded:

```bash
iex -S mix
```

Output:

```
Erlang/OTP 26 [erts-14.2] [64-bit] [smp:8:8] [async-threads:1] [jit]

Compiling 1 file (.ex)
Generated hello_elixir app
Interactive Elixir (1.16.0) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)>
```

Now test your functions:

```elixir
iex(1)> HelloElixir.greet("Bob")
"Hello, Bob! Welcome to Elixir."

iex(2)> HelloElixir.add(10, 32)
42
```

**Why `iex -S mix`?** It compiles and loads your project, enabling interactive testing.

### Compile the Project

Compile without starting IEx:

```bash
mix compile
```

Output:

```
Compiling 1 file (.ex)
Generated hello_elixir app
```

Compiled files go to `_build/` directory.

### Run Specific Function

Execute a function from command line:

```bash
mix run -e 'IO.puts(HelloElixir.greet("World"))'
```

Output:

```
Hello, World! Welcome to Elixir.
```

### Format Code

Elixir includes automatic code formatter:

```bash
mix format
```

This formats all `.ex` and `.exs` files according to Elixir style guide.

## Understanding Elixir's Interactive Development

Elixir emphasizes fast feedback through interactive development.

### IEx Workflow

Typical Elixir development:

1. Start IEx with `iex -S mix`
2. Write or modify code in source files
3. Recompile in IEx: `recompile()`
4. Test functions interactively
5. Iterate based on results

This tight feedback loop enables rapid experimentation.

### Useful IEx Commands

**Recompile Project**:

```elixir
iex(1)> recompile()
```

Recompiles changed files without restarting IEx.

**Auto-complete**:

Type module or function name and press Tab:

```elixir
iex(2)> String.[TAB]
```

Shows available functions in String module.

**Previous Expression**:

```elixir
iex(3)> 2 + 3
5

iex(4)> v(3)
5
```

`v(3)` retrieves result from line 3.

**Clear Screen**:

```elixir
iex(5)> clear()
```

Or press `Ctrl+L`.

## Development Environment Setup

Several editors provide excellent Elixir support.

### VS Code with ElixirLS

**Step 1: Install VS Code**

Download from [https://code.visualstudio.com/](https://code.visualstudio.com/)

**Step 2: Install ElixirLS Extension**

1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X or Cmd+Shift+X)
3. Search for "ElixirLS"
4. Click Install

**Step 3: Open Elixir Project**

1. File → Open Folder → Select `hello_elixir` directory
2. ElixirLS automatically compiles project
3. Get autocomplete, go-to-definition, and inline documentation

ElixirLS provides debugging, mix task integration, and formatter integration.

### IntelliJ IDEA with Elixir Plugin

**Step 1: Install IntelliJ IDEA**

Download Community Edition from [https://www.jetbrains.com/idea/download/](https://www.jetbrains.com/idea/download/)

**Step 2: Install Elixir Plugin**

1. Open IntelliJ IDEA
2. File → Settings → Plugins
3. Search for "Elixir"
4. Install and restart IDE

**Step 3: Import Project**

1. File → Open → Select `hello_elixir` directory
2. Configure Elixir SDK (point to Elixir installation)
3. Project compiles automatically

Provides refactoring, debugging, and mix integration.

### Emacs with Alchemist (Advanced)

Emacs users often use Alchemist mode:

1. Install Emacs
2. Install Alchemist: Add to `.emacs` or `init.el`:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
```

Then `M-x package-install RET alchemist RET`

1. Open Elixir file and start IEx: `M-x alchemist-iex-project-run`

## Verify Your Setup Works

Let's confirm everything is functioning correctly.

### Test 1: Erlang Installed

```bash
erl -version
```

Should show Erlang version 23 or later.

### Test 2: Elixir Installed

```bash
elixir --version
```

Should show Elixir 1.14 or later with matching OTP version.

### Test 3: Mix Works

```bash
mix --version
```

Should show Mix version matching Elixir version.

### Test 4: IEx Access

```bash
iex
```

Should open IEx. Try `IO.puts("Test")`, should print "Test". Exit with Ctrl+C twice.

### Test 5: Project Creation and Testing

```bash
mix new test_project
cd test_project
mix test
```

Tests should pass.

**All tests passed?** Your Elixir setup is complete!

## Summary

**What you've accomplished**:

- Installed Erlang/OTP (BEAM virtual machine)
- Installed Elixir on your operating system
- Accessed and used IEx (Interactive Elixir shell)
- Executed Elixir code interactively
- Created and ran your first Mix project
- Understood interactive development workflow
- Ran tests with Mix test framework

**Key commands learned**:

- `erl -version` - Check Erlang installation
- `elixir --version` - Check Elixir version
- `mix --version` - Check Mix version
- `iex` - Start interactive shell
- `mix new <name>` - Create new project
- `mix compile` - Compile project
- `mix test` - Run tests
- `mix format` - Format code
- `iex -S mix` - Start IEx with project loaded

**Skills gained**:

- Erlang/OTP and Elixir environment setup
- IEx interactive shell usage
- Mix project management basics
- Basic Elixir syntax understanding
- Project structure navigation

## Next Steps

**Ready to learn Elixir syntax and concepts?**

- [Quick Start](/en/learn/software-engineering/programming-languages/elixir/quick-start) (5-30% coverage) - Touch all core Elixir concepts in a fast-paced tour

**Want comprehensive fundamentals?**

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/programming-languages/elixir/by-example) - Learn through heavily annotated examples

**Want to understand Elixir's design philosophy?**

- [Overview](/en/learn/software-engineering/programming-languages/elixir/overview) - Why Elixir exists and when to use it

## Troubleshooting Common Issues

### "erl: command not found"

**Problem**: Erlang not installed or not in PATH.

**Solution**:

- Install Erlang following platform-specific instructions above
- Verify PATH includes Erlang bin directory
- Restart terminal after installation

### "elixir: command not found"

**Problem**: Elixir not in PATH or not installed.

**Solution**:

- Install Elixir following platform-specific instructions
- Check PATH includes Elixir installation directory
- Restart terminal after installation

### OTP version mismatch

**Problem**: Elixir compiled with different OTP version than installed Erlang.

**Solution**:

- Use asdf to manage both Erlang and Elixir with compatible versions
- Ensure Elixir 1.16 uses OTP 24-27
- Reinstall Elixir to match installed OTP version

### "Mix could not compile"

**Problem**: Project compilation fails.

**Solution**:

- Delete `_build/` directory: `rm -rf _build`
- Delete `deps/` directory: `rm -rf deps`
- Recompile: `mix deps.get && mix compile`
- Check syntax errors in source files

### IEx crashes on startup

**Problem**: IEx exits immediately or crashes.

**Solution**:

- Check Erlang installation: `erl` should start Erlang shell
- Verify Elixir version compatibility with OTP
- Check terminal supports UTF-8 encoding
- Try starting with verbose output: `iex --verbose`

### Windows: "The program can't start because erl.exe is missing"

**Problem**: Erlang not properly installed on Windows.

**Solution**:

- Reinstall Erlang from official website
- Ensure installation completed without errors
- Add `C:\Program Files\Erlang OTP\bin` to system PATH
- Restart computer after installation

## Further Resources

**Official Documentation**:

- [Elixir Official Site](https://elixir-lang.org/) - Official Elixir website
- [Elixir Documentation](https://hexdocs.pm/elixir/) - Complete API documentation
- [Mix Documentation](https://hexdocs.pm/mix/) - Mix build tool guide
- [IEx Documentation](https://hexdocs.pm/iex/) - Interactive shell reference

**Interactive Learning**:

- [Elixir School](https://elixirschool.com/) - Free Elixir tutorial
- [Exercism Elixir Track](https://exercism.org/tracks/elixir) - Practice problems with mentorship
- [Koans](https://github.com/elixirkoans/elixir-koans) - Learn through failing tests

**Books**:

- [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/) - Dave Thomas, comprehensive introduction
- [Elixir in Action](https://www.manning.com/books/elixir-in-action-second-edition) - Saša Jurić, practical approach
- [The Little Elixir & OTP Guidebook](https://www.manning.com/books/the-little-elixir-and-otp-guidebook) - Benjamin Tan, OTP focus

**Community**:

- [Elixir Forum](https://elixirforum.com/) - Official discussion forum
- [Elixir Slack](https://elixir-slackin.herokuapp.com/) - Community chat
- [/r/elixir](https://www.reddit.com/r/elixir/) - Reddit community
- [ElixirConf](https://elixirconf.com/) - Annual conference
