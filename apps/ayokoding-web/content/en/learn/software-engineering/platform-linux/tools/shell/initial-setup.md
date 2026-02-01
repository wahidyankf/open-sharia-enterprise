---
title: "Initial Setup"
date: 2025-01-29T00:00:00+07:00
draft: false
weight: 10000000
description: "Get your Linux shell environment set up and working - accessing terminals, shell verification, and your first commands"
tags: ["shell", "bash", "linux", "terminal", "setup", "beginner"]
---

**Want to start using the Linux command-line?** This initial setup guide gets you into a working shell environment in minutes. By the end, you'll have accessed a terminal and executed your first commands.

This tutorial provides 0-5% coverage - just enough to get your shell environment working. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/platform-linux/tools/shell/quick-start) (5-30% coverage).

## Prerequisites

Before starting with the shell, you need:

- A Linux system (native installation, WSL2, virtual machine, or live USB)
- OR a macOS system (Unix-based with similar shell)
- OR Windows with WSL2 (Windows Subsystem for Linux)
- Basic computer literacy (opening applications, using keyboard)
- Willingness to type commands instead of clicking icons

No prior programming or command-line experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Access** a terminal emulator on your operating system
2. **Verify** which shell you're using (Bash, Zsh, Fish)
3. **Execute** basic commands to navigate and inspect your system
4. **Understand** the command prompt and command structure
5. **Configure** basic shell settings for better usability

## Platform-Specific Terminal Access

Choose your operating system and follow the steps to access a terminal.

### Linux Native Installation

**Step 1: Find Your Terminal Emulator**

Most Linux distributions include a terminal emulator by default. Look for:

- **GNOME Desktop** (Ubuntu, Fedora): Press `Ctrl+Alt+T` or search for "Terminal" in Activities
- **KDE Plasma** (Kubuntu, openSUSE): Press `Ctrl+Alt+T` or find "Konsole" in application menu
- **XFCE** (Xubuntu): Search for "Terminal Emulator" or "Xfce Terminal"
- **Cinnamon** (Linux Mint): Search for "Terminal" in menu
- **i3/Other Tiling WMs**: Usually `Mod+Enter` (check your WM config)

**Step 2: Launch Terminal**

Click the terminal icon or use the keyboard shortcut. A window appears with text similar to:

```
username@hostname:~$
```

This is your **command prompt** - the shell is ready for commands.

**Step 3: Verify You Have Access**

Type this command and press Enter:

```bash
echo "Hello, Shell!"
```

Expected output:

```
Hello, Shell!
```

If you see this output, your terminal is working correctly.

**Common Terminal Emulators on Linux**:

- **GNOME Terminal** - Default on Ubuntu, simple and functional
- **Konsole** - KDE's feature-rich terminal with tabs and profiles
- **Alacritty** - GPU-accelerated, minimal, cross-platform
- **Kitty** - GPU-accelerated with image support
- **Terminator** - Multiple terminal panes in one window
- **st (simple terminal)** - Suckless minimal terminal

### macOS Terminal Access

**Step 1: Open Terminal Application**

macOS includes Terminal.app by default. Access it via:

**Method 1: Spotlight Search**

1. Press `Cmd+Space` to open Spotlight
2. Type "Terminal"
3. Press Enter to launch

**Method 2: Applications Folder**

1. Open Finder
2. Navigate to Applications → Utilities
3. Double-click "Terminal"

**Step 2: Verify Terminal Works**

After opening Terminal, you'll see a prompt like:

```
username@MacBook-Pro ~ %
```

Test with:

```bash
echo "Hello, Shell!"
```

Expected output:

```
Hello, Shell!
```

**Alternative: iTerm2 (Recommended)**

iTerm2 is a popular Terminal replacement with advanced features:

1. Download from [https://iterm2.com/](https://iterm2.com/)
2. Install the downloaded .zip file
3. Launch iTerm2 from Applications

iTerm2 provides split panes, search, better color support, and extensive customization.

### Windows with WSL2 (Windows Subsystem for Linux)

**Step 1: Check if WSL2 is Installed**

Open PowerShell as Administrator and run:

```powershell
wsl --list --verbose
```

If you see a Linux distribution listed with version 2, WSL2 is installed. If not, continue to Step 2.

**Step 2: Install WSL2 (if needed)**

In PowerShell as Administrator:

```powershell
wsl --install
```

This installs:

- WSL2 feature
- Ubuntu Linux distribution (default)
- Linux kernel

**Restart your computer** after installation completes.

**Step 3: Launch WSL Terminal**

**Method 1: Windows Terminal (Recommended)**

1. Install Windows Terminal from Microsoft Store (if not already installed)
2. Launch Windows Terminal
3. Click the dropdown arrow next to the + tab button
4. Select your Linux distribution (e.g., "Ubuntu")

**Method 2: Direct Launch**

1. Press Windows key
2. Type "Ubuntu" (or your installed distribution name)
3. Press Enter

**Step 4: First Launch Setup**

On first launch, you'll be prompted to:

1. Create a username (lowercase, no spaces)
2. Set a password (you won't see characters as you type - this is normal)
3. Confirm password

Example:

```
Enter new UNIX username: alice
New password: [type password - nothing appears]
Retype new password: [type same password]
```

After setup, you'll see:

```
alice@DESKTOP-ABC123:~$
```

**Step 5: Verify WSL Works**

```bash
echo "Hello, WSL!"
```

Expected output:

```
Hello, WSL!
```

**Troubleshooting WSL**:

- If `wsl --install` fails, ensure you're running PowerShell as Administrator
- If WSL1 is installed instead of WSL2, upgrade: `wsl --set-version Ubuntu 2`
- If Ubuntu doesn't start, try `wsl --update` in PowerShell

## Shell Verification

After accessing your terminal, verify which shell you're using.

### Check Your Current Shell

```bash
echo $SHELL
```

Common outputs:

- `/bin/bash` - Bash (Bourne Again Shell)
- `/bin/zsh` - Zsh (Z Shell)
- `/usr/bin/fish` - Fish (Friendly Interactive Shell)
- `/bin/sh` - Bourne Shell (minimal, POSIX-compliant)

### Verify Shell Version

**For Bash**:

```bash
bash --version
```

Output example:

```
GNU bash, version 5.1.16(1)-release (x86_64-pc-linux-gnu)
```

**For Zsh**:

```bash
zsh --version
```

Output example:

```
zsh 5.8.1 (x86_64-ubuntu-linux-gnu)
```

**For Fish**:

```bash
fish --version
```

Output example:

```
fish, version 3.6.1
```

### Understanding Default Shells

**Linux distributions**:

- **Ubuntu/Debian**: Bash by default
- **Fedora**: Bash by default
- **Arch Linux**: Bash by default
- **macOS** (Catalina and later): Zsh by default
- **macOS** (Mojave and earlier): Bash by default

Most tutorials and scripts target Bash for maximum compatibility. This guide focuses on Bash but mentions Zsh/Fish differences where relevant.

## First Commands and Command Structure

Let's execute some basic commands to understand command-line structure.

### Command Prompt Anatomy

Your prompt contains information:

```
username@hostname:current-directory$
```

Example breakdown:

```
alice@ubuntu-desktop:~$
```

- `alice` - Your username
- `ubuntu-desktop` - Computer hostname
- `~` - Current directory (~ means home directory)
- `$` - Regular user prompt (# means root/admin user)

### Basic Command Structure

Commands follow this pattern:

```
command [options] [arguments]
```

- **command** - The program to run
- **options** - Flags that modify behavior (usually start with - or --)
- **arguments** - Inputs to the command (files, directories, text)

### Your First Commands

**1. Print Working Directory (where am I?)**

```bash
pwd
```

Output example:

```
/home/alice
```

This shows your current location in the file system.

**2. List Directory Contents (what's here?)**

```bash
ls
```

Output example:

```
Desktop  Documents  Downloads  Music  Pictures  Videos
```

Shows files and directories in current location.

**3. List with Details**

```bash
ls -l
```

Output example:

```
drwxr-xr-x 2 alice alice 4096 Jan 29 10:00 Desktop
drwxr-xr-x 2 alice alice 4096 Jan 29 10:00 Documents
drwxr-xr-x 2 alice alice 4096 Jan 29 10:00 Downloads
```

Shows permissions, ownership, size, and modification date.

**4. List Including Hidden Files**

```bash
ls -la
```

Output includes files starting with `.` (hidden files like `.bashrc`).

**5. Check Command History**

```bash
history
```

Shows recently executed commands.

**6. Clear Screen**

```bash
clear
```

Or press `Ctrl+L` - clears terminal display (history remains).

### Command Execution Examples

**Display Text**:

```bash
echo "This is my first command!"
```

Output:

```
This is my first command!
```

**Show Current Date and Time**:

```bash
date
```

Output example:

```
Wed Jan 29 14:30:22 WIB 2025
```

**Show Logged-in Users**:

```bash
whoami
```

Output:

```
alice
```

**Display System Information**:

```bash
uname -a
```

Output example:

```
Linux ubuntu-desktop 5.15.0-91-generic #101-Ubuntu SMP x86_64 GNU/Linux
```

## Understanding Navigation

File system navigation is fundamental to shell usage.

### Directory Structure

Linux and macOS use hierarchical directory structure:

```
/                   # Root directory (top of file system)
├── home/          # User home directories
│   └── alice/     # Your home directory
│       ├── Documents/
│       ├── Downloads/
│       └── Desktop/
├── etc/           # System configuration files
├── usr/           # User programs and libraries
├── var/           # Variable data (logs, temp files)
└── tmp/           # Temporary files
```

### Special Directory Symbols

- `/` - Root directory (top of entire file system)
- `~` - Your home directory (shortcut for `/home/username`)
- `.` - Current directory
- `..` - Parent directory (one level up)
- `-` - Previous directory (where you were before)

### Change Directory Command

**Go to home directory**:

```bash
cd ~
```

Or just:

```bash
cd
```

**Go to specific directory**:

```bash
cd Documents
```

**Go up one level**:

```bash
cd ..
```

**Go to root directory**:

```bash
cd /
```

**Go back to previous directory**:

```bash
cd -
```

### Navigation Practice Sequence

Try this sequence to practice navigation:

```bash
pwd

cd ~

ls

cd Documents

pwd

cd ..

pwd
```

## Basic Shell Configuration

Configure your shell for better usability.

### View Shell Configuration Files

Bash uses these configuration files:

- `~/.bashrc` - Configuration for interactive shells
- `~/.bash_profile` - Configuration for login shells
- `~/.profile` - Generic shell configuration

Zsh uses:

- `~/.zshrc` - Main Zsh configuration

Fish uses:

- `~/.config/fish/config.fish` - Fish configuration

### Check if Configuration Files Exist

```bash
ls -la ~ | grep bash
```

Output shows .bashrc and other bash-related files.

### View Current Configuration

```bash
cat ~/.bashrc
```

This displays your Bash configuration. Don't worry if you don't understand it yet - we'll cover this in later tutorials.

### Simple Customization: Add Color to ls

Edit your `.bashrc` (for Bash) or `.zshrc` (for Zsh):

```bash
nano ~/.bashrc
```

Add this line at the end:

```bash
alias ls='ls --color=auto'
```

Save and exit:

- Press `Ctrl+O` to save
- Press `Enter` to confirm filename
- Press `Ctrl+X` to exit

Reload configuration:

```bash
source ~/.bashrc
```

Now `ls` shows colored output (directories in blue, executables in green, etc.).

### Understanding Command Aliases

Aliases create shortcuts for longer commands.

**View current aliases**:

```bash
alias
```

**Create temporary alias** (lasts until you close terminal):

```bash
alias ll='ls -la'
```

Now typing `ll` executes `ls -la`.

**Make alias permanent**: Add to `~/.bashrc` or `~/.zshrc`.

## Useful Shell Shortcuts

Keyboard shortcuts improve shell efficiency.

### Navigation Shortcuts

- `Ctrl+A` - Move cursor to beginning of line
- `Ctrl+E` - Move cursor to end of line
- `Ctrl+U` - Delete from cursor to beginning of line
- `Ctrl+K` - Delete from cursor to end of line
- `Ctrl+W` - Delete word before cursor
- `Ctrl+L` - Clear screen (same as `clear` command)

### History Shortcuts

- `Up Arrow` - Previous command in history
- `Down Arrow` - Next command in history
- `Ctrl+R` - Search command history (type to search, Enter to execute)
- `!!` - Repeat last command
- `!$` - Last argument of previous command

### Command Execution Shortcuts

- `Tab` - Auto-complete commands and file names
- `Ctrl+C` - Cancel current command (interrupt)
- `Ctrl+D` - Exit shell (or send EOF)
- `Ctrl+Z` - Suspend current command (move to background)

### Practice These Shortcuts

Try this exercise:

1. Type a long command: `echo "This is a very long message for practice"`
2. Press `Ctrl+A` - cursor jumps to start
3. Press `Ctrl+E` - cursor jumps to end
4. Press `Ctrl+U` - entire line deleted
5. Press `Up Arrow` - previous command returns
6. Press `Enter` - execute command again

## Verify Your Setup Works

Let's confirm everything is functioning correctly.

### Test 1: Shell Access

Open a new terminal window. You should see a command prompt.

### Test 2: Basic Commands

```bash
pwd && ls && whoami
```

Should print: current directory, directory contents, and your username.

### Test 3: Navigation

```bash
cd ~ && pwd && cd Documents && pwd && cd ..
```

Should navigate to home, then Documents, then back.

### Test 4: History

```bash
history | tail -5
```

Shows your last 5 commands.

### Test 5: Tab Completion

Type `ec` and press Tab. It should complete to `echo` (or show options if multiple commands start with "ec").

**All tests passed?** Your shell setup is complete!

## Summary

**What you've accomplished**:

- Accessed a terminal emulator on your operating system
- Verified which shell you're using (Bash, Zsh, or Fish)
- Executed basic commands to navigate and inspect your system
- Understood command prompt structure and command syntax
- Learned essential keyboard shortcuts for efficiency
- Configured basic shell settings with aliases

**Key commands learned**:

- `pwd` - Print working directory (current location)
- `ls` - List directory contents
- `cd` - Change directory
- `echo` - Print text to output
- `whoami` - Display current username
- `date` - Show current date and time
- `clear` - Clear terminal screen
- `history` - Show command history
- `alias` - Create command shortcuts

**Skills gained**:

- Platform-specific terminal access
- Shell identification and version checking
- Basic command execution and syntax understanding
- File system navigation concepts
- Configuration file awareness

## Next Steps

**Ready to learn essential shell commands?**

- [Quick Start](/en/learn/software-engineering/platform-linux/tools/shell/quick-start) (5-30% coverage) - Touch all core shell concepts in a fast-paced tour

**Want comprehensive command mastery?**

- [Beginner Tutorial](/en/learn/software-engineering/platform-linux/tools/shell/by-concept/beginner) (0-60% coverage) - Deep dive into shell fundamentals with extensive practice

**Prefer code-first learning?**

- [By-Example Tutorial](/en/learn/software-engineering/platform-linux/tools/shell/by-example) - Learn through heavily annotated shell examples

**Want to understand shell philosophy?**

- [Overview](/en/learn/software-engineering/platform-linux/tools/shell/overview) - Why command-line interfaces matter and when to use them

## Troubleshooting Common Issues

### "Command not found" errors

**Problem**: Shell doesn't recognize commands.

**Solution**:

- Verify correct spelling: `ls` not `1s` (lowercase L, not number 1)
- Check if command is installed: `which ls` shows path if installed
- Some commands need installation: `sudo apt install <package>` (Ubuntu/Debian)

### Terminal doesn't open (Linux)

**Problem**: Terminal emulator won't launch.

**Solution**:

- Try alternative keyboard shortcut: `Ctrl+Alt+F2` opens TTY terminal
- Install terminal emulator: `sudo apt install gnome-terminal` (if using GNOME)
- Check if desktop environment is running properly

### WSL2 Ubuntu fails to start (Windows)

**Problem**: WSL distribution won't launch.

**Solution**:

- Update WSL: `wsl --update` in PowerShell (as Administrator)
- Restart WSL service: `wsl --shutdown` then relaunch Ubuntu
- Check WSL version: `wsl --list --verbose` (should show version 2)
- Reinstall distribution: `wsl --unregister Ubuntu` then `wsl --install -d Ubuntu`

### Permission denied errors

**Problem**: Can't execute commands or access files.

**Solution**:

- Don't use `sudo` unless necessary (avoid `sudo` for normal file operations)
- Check file permissions: `ls -l filename`
- For system operations requiring root, use `sudo`: `sudo apt update`
- Never run `sudo rm` or `sudo chmod` without understanding consequences

### Colors don't appear in terminal

**Problem**: `ls` output is monochrome.

**Solution**:

- Add alias to `~/.bashrc`: `alias ls='ls --color=auto'`
- Reload config: `source ~/.bashrc`
- Check terminal supports colors: Most modern terminals do
- Try different terminal emulator if problem persists

### Shortcuts don't work

**Problem**: `Ctrl+A`, `Ctrl+E`, etc. don't work as expected.

**Solution**:

- Verify you're in Bash/Zsh (Fish uses different shortcuts)
- Check terminal emulator hasn't reassigned shortcuts
- Some shortcuts conflict with desktop environment keybindings
- Try in different terminal emulator (Alacritty, Kitty, iTerm2)

## Further Resources

**Official Documentation**:

- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/) - Complete Bash reference
- [Zsh Documentation](https://zsh.sourceforge.io/Doc/) - Zsh user guide
- [Fish Documentation](https://fishshell.com/docs/current/) - Fish shell documentation
- [Linux Command Line Basics](https://ubuntu.com/tutorials/command-line-for-beginners) - Ubuntu tutorial

**Interactive Learning**:

- [Learn Shell](https://www.learnshell.org/) - Interactive shell tutorial
- [OverTheWire Bandit](https://overthewire.org/wargames/bandit/) - Command-line wargame
- [Terminus](https://web.mit.edu/mprat/Public/web/Terminus/Web/main.html) - Command-line game

**Books and Guides**:

- [The Linux Command Line](http://linuxcommand.org/tlcl.php) by William Shotts - Free comprehensive book
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) - The Linux Documentation Project

**Community**:

- [/r/linux4noobs](https://www.reddit.com/r/linux4noobs/) - Reddit community for Linux beginners
- [Unix & Linux Stack Exchange](https://unix.stackexchange.com/) - Q&A for Unix and Linux users
- [Ubuntu Forums](https://ubuntuforums.org/) - Community help for Ubuntu users
