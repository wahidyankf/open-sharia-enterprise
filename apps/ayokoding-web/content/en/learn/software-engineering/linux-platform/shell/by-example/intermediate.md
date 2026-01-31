---
title: "Intermediate"
date: 2025-12-30T07:56:02+07:00
draft: false
weight: 10000002
description: "Examples 31-55: Advanced text processing, scripting patterns, and automation (40-75% coverage)"
tags: ["linux", "shell", "bash", "tutorial", "by-example", "intermediate", "scripting"]
---

## Intermediate Level (40-75% Coverage)

This level covers intermediate shell concepts through 25 self-contained examples. Each example demonstrates production patterns used in automation, text processing, and system administration.

---

### Example 31: Text Processing (sed)

The `sed` (stream editor) command performs text transformations on files or streams, supporting substitution, deletion, insertion, and line-based operations.

```bash
# Basic substitution (first occurrence per line)
echo "hello world" | sed 's/world/universe/'
                                # => Creates new stream from echo output
                                # => sed processes line-by-line
                                # => 's/world/universe/' is substitution command
                                # => s: substitute, /world/: pattern to find
                                # => /universe/: replacement text
                                # => Matches only FIRST occurrence per line
                                # => Output: hello universe

# Global substitution (all occurrences)
echo "foo bar foo" | sed 's/foo/baz/g'
                                # => Input: "foo bar foo"
                                # => g flag: global replacement
                                # => Without g: only first "foo" replaced
                                # => With g: ALL occurrences replaced
                                # => Output: baz bar baz

# Substitute in file (preview)
sed 's/old/new/g' file.txt      # => Reads file.txt line-by-line
                                # => Applies substitution to each line
                                # => Prints transformed output to stdout
                                # => Original file.txt UNCHANGED (safe preview)

# In-place editing
sed -i 's/old/new/g' file.txt   # => -i: in-place mode
                                # => Modifies file.txt DIRECTLY
                                # => NO backup created
                                # => DANGEROUS: no undo if wrong!
                                # => Use only when certain

# In-place with backup
sed -i.bak 's/old/new/g' file.txt
                                # => -i.bak: in-place with backup extension
                                # => Creates file.txt.bak (original content)
                                # => Then modifies file.txt
                                # => Safer: can restore from .bak
                                # => Production best practice

# Delete lines matching pattern
sed '/^#/d' file.txt            # => /^#/: regex pattern (lines starting with #)
                                # => d: delete command
                                # => ^: start of line anchor
                                # => Removes comment lines
                                # => Prints remaining lines to stdout

# Delete line number
sed '3d' file.txt               # => 3: line number (third line)
                                # => d: delete command
                                # => Removes only line 3
                                # => Other lines unchanged

# Delete range
sed '2,5d' file.txt             # => 2,5: line range (lines 2 through 5 inclusive)
                                # => d: delete command
                                # => Removes lines 2, 3, 4, 5
                                # => Lines 1, 6+ remain

# Print specific lines
sed -n '1,10p' file.txt         # => -n: suppress automatic printing
                                # => 1,10: line range (1 through 10)
                                # => p: print command (explicit print)
                                # => Without -n: prints all + specified lines (duplication)
                                # => With -n: prints ONLY specified lines

# Multiple operations
sed -e 's/foo/bar/g' -e 's/baz/qux/g' file.txt
                                # => -e: expression flag (multiple commands)
                                # => First -e: replace foo → bar globally
                                # => Second -e: replace baz → qux globally
                                # => Applied sequentially to each line
                                # => Both transformations in single pass

# Practical: remove trailing whitespace
sed 's/[[:space:]]*$//' file.txt
                                # => [[:space:]]: character class (spaces, tabs, etc.)
                                # => *: zero or more occurrences
                                # => $: end of line anchor
                                # => Matches trailing whitespace
                                # => Replaces with empty string (removes)
                                # => Cleans up messy files

# Practical: comment out lines
sed 's/^/# /' code.sh           # => s/^/# /: substitution command
                                # => ^: start of line (zero-width)
                                # => Replaces start with "# "
                                # => Effectively adds "# " to beginning
                                # => Converts code to comments

# Practical: extract email addresses
sed -n 's/.*\([a-zA-Z0-9.]*@[a-zA-Z0-9.]*\).*/\1/p' contacts.txt
                                # => -n: suppress default printing
                                # => .*: match any characters before email
                                # => \(...\): capture group (backreference)
                                # => [a-zA-Z0-9.]*: username part
                                # => @: literal @ symbol
                                # => [a-zA-Z0-9.]*: domain part
                                # => \1: reference to captured group
                                # => p: print only matching lines
                                # => Extracts email, discards rest of line
```

**Key Takeaway**: Use `sed` for quick text transformations with `s/pattern/replacement/g` for global substitution, `/pattern/d` for deletion, and `-i.bak` for safe in-place editing - it's perfect for automated text processing in scripts.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 32: Text Processing (awk)

The `awk` command is a powerful text processing language for field-based data manipulation, pattern matching, and reporting - ideal for CSV, logs, and tabular data.

```bash
# Print specific field
echo "one two three" | awk '{print $2}'
                                # => awk splits input by whitespace (default)
                                # => $1="one", $2="two", $3="three"
                                # => {print $2}: action block prints second field
                                # => Output: two

# Print multiple fields
ps aux | awk '{print $1, $11}'  # => ps aux: process listing with columns
                                # => awk receives each line as input
                                # => $1: first field (username)
                                # => $11: eleventh field (command name)
                                # => Comma adds space between fields
                                # => Prints username and command for each process

# Custom delimiter
awk -F':' '{print $1}' /etc/passwd
                                # => -F':': sets field separator to colon
                                # => /etc/passwd format: username:password:uid:gid:...
                                # => $1: first field (username)
                                # => Extracts usernames from each line

# Field count
echo "a b c d" | awk '{print NF}'
                                # => NF: built-in variable (Number of Fields)
                                # => Input has 4 space-separated fields
                                # => Output: 4

# Last field
echo "a b c d" | awk '{print $NF}'
                                # => $NF: references last field
                                # => NF=4, so $NF is $4
                                # => Useful when field count varies
                                # => Output: d

# Pattern matching
awk '/error/ {print $0}' logfile.txt
                                # => /error/: regex pattern (line contains "error")
                                # => {print $0}: action if pattern matches
                                # => $0: entire line (all fields)
                                # => Only prints lines containing "error"
                                # => Acts like grep but allows further processing

# Conditional actions
awk '$3 > 100 {print $1, $3}' data.txt
                                # => $3 > 100: numeric comparison condition
                                # => Executes action block only if third field > 100
                                # => {print $1, $3}: prints first and third fields
                                # => Skips lines not meeting condition

# BEGIN and END blocks
awk 'BEGIN {sum=0} {sum+=$1} END {print sum}' numbers.txt
                                # => BEGIN {sum=0}: runs ONCE before input
                                # => Initializes sum variable to 0
                                # => {sum+=$1}: runs for EACH line, adds first field
                                # => END {print sum}: runs ONCE after all input
                                # => Prints accumulated sum

# Calculate average
awk '{sum+=$1; count++} END {print sum/count}' numbers.txt
                                # => {sum+=$1; count++}: for each line
                                # => sum accumulates total, count tracks lines
                                # => END {print sum/count}: calculates average
                                # => Division performed after all input processed

# Multiple conditions
awk '$1 == "error" && $2 > 100 {print}' log.txt
                                # => $1 == "error": string equality check
                                # => &&: logical AND operator
                                # => $2 > 100: numeric comparison
                                # => Both conditions must be true
                                # => {print}: prints entire line ($0 implicit)

# Formatted output
awk '{printf "%-10s %5d\n", $1, $2}' data.txt
                                # => printf: formatted print (like C printf)
                                # => %-10s: left-align string, width 10
                                # => %5d: right-align integer, width 5
                                # => \n: explicit newline
                                # => $1, $2: values to format

# Practical: sum disk usage
du -b * | awk '{total+=$1} END {print total " bytes"}'
                                # => du -b: disk usage in bytes
                                # => {total+=$1}: accumulate first field (size)
                                # => END: runs after all files processed
                                # => String concatenation: total " bytes"
                                # => Prints total size

# Practical: process CSV
awk -F',' '{print $2, $3}' data.csv
                                # => -F',': set field separator to comma
                                # => CSV: comma-separated values
                                # => $2, $3: second and third columns
                                # => Extracts specific columns from CSV

# Practical: count occurrences
awk '{count[$1]++} END {for (word in count) print word, count[word]}' words.txt
                                # => count[$1]++: associative array (hash map)
                                # => $1: first field used as key
                                # => ++: increment count for that word
                                # => END: after all input processed
                                # => for (word in count): iterate over keys
                                # => print word, count[word]: word and its frequency
```

**Key Takeaway**: Use `awk` for field-based text processing with `$1, $2, ...` for columns, `-F` for custom delimiters, and `BEGIN/END` blocks for initialization/summary - it's more powerful than `cut` and ideal for log analysis and data extraction.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 33: Command Line Arguments and Parsing

Production scripts need robust argument parsing to handle options, flags, and validation. Use `getopts` for POSIX-compliant option parsing or manual parsing for flexibility.

```mermaid
%% Argument parsing flow
graph TD
    A[Script Called] --> B{Has Arguments?}
    B -->|No| C[Show Usage]
    B -->|Yes| D[Parse Options]
    D --> E{Valid Options?}
    E -->|No| F[Error + Exit]
    E -->|Yes| G[Validate Arguments]
    G --> H{Valid?}
    H -->|No| F
    H -->|Yes| I[Execute Logic]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#CC78BC,color:#000
    style D fill:#029E73,color:#fff
    style E fill:#DE8F05,color:#000
    style F fill:#CC78BC,color:#000
    style G fill:#029E73,color:#fff
    style H fill:#DE8F05,color:#000
    style I fill:#0173B2,color:#fff
```

```bash
#!/bin/bash

# Basic argument check
if [ $# -eq 0 ]; then           # => $#: number of arguments passed to script
                                # => -eq 0: numeric equality (zero arguments)
    echo "Usage: $0 <filename>" # => $0: script name
                                # => Prints usage message to stdout
    exit 1                      # => Exit code 1 (error)
                                # => Stops script execution
fi

# Using getopts for option parsing
while getopts "vho:f:" opt; do  # => getopts: POSIX-compliant option parser
                                # => "vho:f:": option string
                                # => v,h: flags (no argument)
                                # => o:,f:: options requiring argument (colon suffix)
                                # => opt: variable receiving matched option
    case $opt in                # => case statement matches option value
        v)
            VERBOSE=true        # => -v flag detected
                                # => Sets boolean variable
                                # => No argument required
            ;;
        h)
            echo "Usage: $0 [-v] [-o output] -f file"
                                # => -h flag: shows help
            exit 0              # => Exit code 0 (success)
                                # => Help is not an error
            ;;
        o)
            OUTPUT="$OPTARG"    # => -o option detected
                                # => $OPTARG: argument provided to option
                                # => Example: -o myfile → OUTPUT="myfile"
            ;;
        f)
            FILE="$OPTARG"      # => -f option detected
                                # => Required for script logic
                                # => Example: -f input.txt → FILE="input.txt"
            ;;
        \?)
            echo "Invalid option: -$OPTARG"
                                # => \?: unrecognized option
                                # => $OPTARG: the invalid option provided
            exit 1              # => Exit with error
                                # => Prevents script from running with bad options
            ;;
    esac
done

# Shift past parsed options
shift $((OPTIND-1))             # => OPTIND: index of next argument to process
                                # => $((OPTIND-1)): arithmetic expansion
                                # => shift N: removes first N arguments from $@
                                # => Remaining non-option arguments now in $1, $2, ...

# Validate required arguments
if [ -z "$FILE" ]; then         # => -z: string is empty (zero length)
                                # => Checks if FILE was set by -f option
    echo "Error: -f file is required"
                                # => Error message to stdout
    exit 1                      # => Exit with error code
fi

# Practical example: backup script with options
#!/bin/bash
set -e                          # => Exit immediately on any error
                                # => Prevents partial execution

usage() {                       # => Function definition
    cat << EOF                  # => Here-document (multi-line string)
Usage: $0 [OPTIONS] source destination
                                # => $0: script name placeholder
Options:
    -c          Compress backup # => Flag option (no argument)
    -v          Verbose output  # => Flag option
    -e <ext>    Exclude file extension
                                # => Option with argument
    -h          Show this help  # => Help flag
EOF
    exit 1                      # => Exit after showing usage
}

COMPRESS=false                  # => Initialize variables with defaults
VERBOSE=false                   # => Boolean flags default to false
EXCLUDE=""                      # => String option default to empty

while getopts "cve:h" opt; do   # => Parse options
                                # => c,v,h: flags (no argument)
                                # => e:: option with required argument
    case $opt in
        c) COMPRESS=true ;;     # => Set flag to true
        v) VERBOSE=true ;;      # => Set flag to true
        e) EXCLUDE="$OPTARG" ;; # => Store argument value
                                # => Example: -e .log → EXCLUDE=".log"
        h) usage ;;             # => Call usage function (exits)
        ?) usage ;;             # => Invalid option: show usage and exit
    esac
done

shift $((OPTIND-1))             # => Remove parsed options from $@
                                # => Remaining: positional arguments

if [ $# -ne 2 ]; then           # => $#: count of remaining arguments
                                # => -ne 2: not equal to 2
                                # => Expects exactly source and destination
    echo "Error: source and destination required"
    usage                       # => Show usage and exit
fi

SOURCE="$1"                     # => First positional argument
DEST="$2"                       # => Second positional argument

[ "$VERBOSE" = true ] && echo "Backing up $SOURCE to $DEST"
                                # => [ ... ]: test condition
                                # => &&: logical AND (execute if true)
                                # => Conditional one-liner

if [ "$COMPRESS" = true ]; then # => Check compression flag
    tar -czf "$DEST/backup.tar.gz" "$SOURCE"
                                # => tar: create compressed archive
                                # => -c: create, -z: gzip, -f: filename
else
    cp -r "$SOURCE" "$DEST"     # => Copy recursively (no compression)
                                # => -r: recursive (directories)
fi

[ "$VERBOSE" = true ] && echo "Backup complete"
                                # => Conditional verbose output
```

**Key Takeaway**: Use `getopts` for standard option parsing with flags and arguments, validate all inputs before processing, and provide clear usage messages - always check `$#` for argument count and exit with non-zero status on errors.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 34: Error Handling and Exit Codes

Robust scripts handle errors gracefully, validate inputs, log failures, and return meaningful exit codes. Use `set -e` for fail-fast behavior and trap for cleanup.

```bash
#!/bin/bash

# Strict mode (recommended for production)
set -e                          # => Exit immediately on any error
                                # => Prevents partial execution
                                # => Command returning non-zero exits script
set -u                          # => Exit on undefined variable reference
                                # => Catches typos like $VARIABL instead of $VARIABLE
                                # => Prevents silent bugs from empty variables
set -o pipefail                 # => Pipeline fails if ANY command fails
                                # => Default: only last command determines exit code
                                # => Example: cat missing.txt | grep foo
                                # => Without pipefail: succeeds if grep succeeds
                                # => With pipefail: fails because cat failed

# Error handling function
error_exit() {
    echo "Error: $1" >&2        # => $1: first function argument (error message)
                                # => >&2: redirect stdout to stderr (file descriptor 2)
                                # => Errors should go to stderr, not stdout
    exit 1                      # => Exit code 1 (error)
                                # => Stops script execution
}

# Validate file exists
[ -f "$config_file" ] || error_exit "Config file not found"
                                # => [ -f "$config_file" ]: test if file exists
                                # => ||: logical OR (if test fails, execute right side)
                                # => Short-circuit evaluation
                                # => Compact error handling pattern

# Trap for cleanup on exit
cleanup() {                     # => Function runs on script exit
    echo "Cleaning up..."
    rm -f /tmp/tempfile_$$      # => $$: current script's process ID
                                # => Creates unique temp file per script run
                                # => rm -f: force remove (no error if missing)
}
trap cleanup EXIT               # => trap: registers signal handler
                                # => cleanup: function to call
                                # => EXIT: pseudo-signal (script exit, any reason)
                                # => Ensures cleanup runs even on errors

# Trap specific signals
trap 'echo "Interrupted"; exit 130' INT
                                # => INT: SIGINT signal (Ctrl+C)
                                # => Single-quoted string: command to execute
                                # => exit 130: convention (128 + signal number 2)
                                # => Graceful handling of user interruption

# Check command success
if ! grep -q "pattern" file.txt; then
                                # => !: logical NOT
                                # => grep -q: quiet (no output, exit code only)
                                # => if !: execute block if command FAILS
    echo "Pattern not found"
    exit 1
fi

# Alternative: check exit code
grep -q "pattern" file.txt      # => Execute command
if [ $? -ne 0 ]; then           # => $?: exit code of last command
                                # => -ne 0: not equal to zero (failure)
                                # => 0 = success, non-zero = failure
    echo "Pattern not found"
    exit 1
fi

# Conditional execution with ||
command || error_exit "Command failed"
                                # => ||: executes right side if left fails
                                # => Compact error handling
                                # => If command succeeds (exit 0), error_exit skipped
                                # => If command fails, error_exit called

# Conditional execution with &&
command && echo "Success" || echo "Failed"
                                # => &&: executes if left succeeds
                                # => ||: executes if left fails
                                # => Chain: success message OR failure message
                                # => One-liner conditional output

# Practical: database backup with error handling
#!/bin/bash
set -euo pipefail               # => Strict mode (all three options)

BACKUP_DIR="/backup"            # => Configuration constants
DB_NAME="production"            # => Database name
LOG_FILE="/var/log/backup.log"  # => Log file path

log() {                         # => Logging function
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
                                # => $(date ...): command substitution (timestamp)
                                # => $1: log message argument
                                # => tee -a: write to stdout AND append to file
                                # => Both console and file logging
}

error_exit() {                  # => Error handling with logging
    log "ERROR: $1"             # => Log error message
    exit 1                      # => Exit with error code
}

cleanup() {                     # => Cleanup function
    [ -f "$TEMP_FILE" ] && rm -f "$TEMP_FILE"
                                # => [ -f "$TEMP_FILE" ]: test if temp file exists
                                # => &&: if exists, remove it
                                # => Conditional cleanup (no error if TEMP_FILE unset)
    log "Cleanup completed"     # => Log cleanup action
}

trap cleanup EXIT               # => Register cleanup on exit
trap 'error_exit "Script interrupted"' INT TERM
                                # => INT: Ctrl+C, TERM: kill command
                                # => Both call error_exit (logged interruption)

# Validate backup directory
[ -d "$BACKUP_DIR" ] || error_exit "Backup directory not found"
                                # => -d: test if directory exists
                                # => Fail fast if environment not ready

log "Starting backup of $DB_NAME"
                                # => Log operation start

# Create temporary file
TEMP_FILE=$(mktemp)             # => mktemp: creates secure temp file
                                # => Returns path like /tmp/tmp.XXXXXXXXXX
                                # => $(...): command substitution
                                # => Assigns path to TEMP_FILE variable

# Perform backup
if mysqldump "$DB_NAME" > "$TEMP_FILE"; then
                                # => mysqldump: MySQL backup utility
                                # => >: redirect output to temp file
                                # => if: check exit code
    BACKUP_FILE="$BACKUP_DIR/${DB_NAME}_$(date +%Y%m%d_%H%M%S).sql"
                                # => ${VAR}: variable expansion
                                # => $(date ...): timestamp for unique filename
                                # => Format: production_20251231_235959.sql
    mv "$TEMP_FILE" "$BACKUP_FILE"
                                # => Move temp file to final location
    log "Backup successful: $BACKUP_FILE"
                                # => Log success with filename
else
    error_exit "Database backup failed"
                                # => mysqldump failed: log and exit
fi

# Verify backup
if [ ! -s "$BACKUP_FILE" ]; then
                                # => !: logical NOT
                                # => -s: file exists and is NOT empty
                                # => !-s: file missing or empty
    error_exit "Backup file is empty"
                                # => Safety check: backup created but empty
fi

log "Backup completed successfully"
exit 0                          # => Exit code 0 (success)
                                # => Explicit success exit
```

**Key Takeaway**: Use `set -euo pipefail` for strict error handling, trap for cleanup operations, and always validate inputs before processing - return exit code 0 for success and non-zero for errors, and log errors to stderr with `>&2`.

**Why It Matters**: Error handling prevents partial failures that leave systems in inconsistent states. Production scripts must handle errors gracefully to enable recovery and maintain system integrity.

---

### Example 35: Process Management (ps, kill, jobs)

Process management involves listing processes, monitoring resource usage, and controlling process lifecycle with signals for graceful shutdown or forceful termination.

```bash
# List all processes
ps aux                          # => ps: process status command
                                # => a: show processes from ALL users
                                # => u: user-oriented format (user, CPU%, MEM%, etc.)
                                # => x: include processes without controlling terminal
                                # => Output: PID, USER, %CPU, %MEM, COMMAND, etc.

# List processes for current user
ps -u $USER                     # => -u: filter by username
                                # => $USER: environment variable (current user)
                                # => Shows only processes owned by you

# Filter processes
ps aux | grep "python"          # => ps aux: list all processes
                                # => |: pipe output to grep
                                # => grep "python": filter lines containing "python"
                                # => Note: grep process itself appears in results
                                # => Add: | grep -v grep to exclude grep itself

# Process tree
ps auxf                         # => f: forest view (ASCII tree)
                                # => Shows parent-child relationships
                                # => Indentation indicates process hierarchy
                                # => Useful for understanding process spawning

# Top - interactive process monitor
top                             # => Real-time process monitoring
                                # => Updates every 3 seconds by default
                                # => Shows CPU, memory usage, load average
                                # => Press q to quit
                                # => Press k to kill (prompts for PID)
                                # => Press M to sort by memory usage

# Find process by name
pgrep python                    # => pgrep: process grep
                                # => Searches process names for "python"
                                # => Output: PIDs only (one per line)
                                # => Example output: 1234\n5678

pgrep -l python                 # => -l: list format (long)
                                # => Output: PID and process name
                                # => Example: 1234 python3

# Kill process by PID
kill 1234                       # => Sends SIGTERM (signal 15) to PID 1234
                                # => SIGTERM: termination signal (graceful)
                                # => Process can catch signal and cleanup
                                # => Process can ignore signal
                                # => Gives process chance to save state

# Force kill
kill -9 1234                    # => -9: sends SIGKILL (signal 9)
                                # => SIGKILL: immediate termination
                                # => Cannot be caught, blocked, or ignored
                                # => Process killed immediately by kernel
                                # => Use only when SIGTERM fails
                                # => No cleanup possible

# Kill by name
pkill python                    # => pkill: kill processes by name
                                # => Sends SIGTERM to ALL processes named "python"
                                # => DANGEROUS: kills all matching processes
                                # => Use carefully in multi-user systems

# Kill with pattern
pkill -f "python script.py"     # => -f: match full command line
                                # => Searches entire command, not just process name
                                # => Example: matches "python script.py arg1 arg2"
                                # => More precise than pkill python

# Check if process is running
if pgrep -x nginx > /dev/null; then
                                # => pgrep -x: exact match (process name exactly "nginx")
                                # => > /dev/null: discard output (only need exit code)
                                # => if: checks exit code (0 = found, 1 = not found)
    echo "Nginx is running"     # => Executed if pgrep found process
else
    echo "Nginx is not running" # => Executed if pgrep found nothing
fi

# Job control
command &                       # => &: run command in background
                                # => Shell returns immediately
                                # => Process runs independently
jobs                            # => List background jobs for current shell
                                # => Shows job number, status, command
fg %1                           # => fg: foreground
                                # => %1: job number 1
                                # => Brings job to foreground (interactive)
bg %1                           # => bg: background
                                # => %1: job number 1
                                # => Resumes stopped job in background
kill %1                         # => kill: terminate job
                                # => %1: job number (not PID)

# Wait for process to finish
sleep 30 &                      # => Run sleep in background
                                # => sleep 30: wait 30 seconds
BGPID=$!                        # => $!: PID of last background job
                                # => Captures sleep's PID
wait $BGPID                     # => wait: blocks until PID exits
                                # => Script pauses here for 30 seconds
echo "Background process completed"
                                # => Executes after sleep finishes

# Practical: graceful service restart
#!/bin/bash

SERVICE_NAME="myapp"            # => Service name for logging
PID_FILE="/var/run/myapp.pid"   # => PID file location

# Get PID
if [ -f "$PID_FILE" ]; then     # => -f: test if file exists
                                # => PID file exists means service may be running
    PID=$(cat "$PID_FILE")      # => cat: read file contents
                                # => $(...): command substitution
                                # => PID now contains process ID

    # Check if process exists
    if kill -0 "$PID" 2>/dev/null; then
                                # => kill -0: signal 0 (null signal)
                                # => Doesn't kill, just checks if process exists
                                # => 2>/dev/null: discard error output
                                # => Exit code 0 if process exists
        echo "Stopping $SERVICE_NAME (PID: $PID)"
        kill "$PID"             # => Send SIGTERM (default signal)
                                # => Requests graceful shutdown

        # Wait up to 10 seconds
        for i in {1..10}; do    # => Brace expansion: {1..10} → 1 2 3 ... 10
                                # => Loop 10 times (10 second timeout)
            if ! kill -0 "$PID" 2>/dev/null; then
                                # => !: logical NOT
                                # => kill -0: check if process still exists
                                # => If process gone, condition true
                echo "Process stopped gracefully"
                break           # => Exit loop early (process stopped)
            fi
            sleep 1             # => Wait 1 second before next check
        done

        # Force kill if still running
        if kill -0 "$PID" 2>/dev/null; then
                                # => After 10 second timeout
                                # => Check if process STILL exists
            echo "Forcing stop..."
            kill -9 "$PID"      # => Send SIGKILL (force termination)
                                # => Cannot be ignored
        fi
    fi
fi

# Start new process
./myapp &                       # => Run myapp in background
                                # => &: background execution
echo $! > "$PID_FILE"           # => $!: PID of last background job
                                # => Write PID to file for next restart
echo "Service started"
```

**Key Takeaway**: Use `ps aux` to list processes, `pgrep/pkill` for name-based operations, and `kill` with SIGTERM (default) before SIGKILL (-9) - always verify process existence with `kill -0` before sending signals, and use PID files for reliable service management.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 36: File Permissions and Ownership (chmod, chown)

Linux file permissions control read, write, and execute access for owner, group, and others. Permissions are critical for security and proper system operation.

```mermaid
%% Permission structure
graph TD
    A[Permission String] --> B[Type]
    A --> C[Owner rwx]
    A --> D[Group rwx]
    A --> E[Others rwx]
    B --> F[- = file, d = directory]
    C --> G[r=4, w=2, x=1]
    D --> G
    E --> G
    G --> H[Sum for Octal]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#CC78BC,color:#000
    style G fill:#DE8F05,color:#000
    style H fill:#0173B2,color:#fff
```

```bash
# View permissions
ls -l file.txt                  # => ls: list command
                                # => -l: long format (detailed)
                                # => Output: -rw-r--r-- 1 user group 1234 Dec 30 file.txt
                                # => Permission breakdown:
                                # => -: file type (- = regular file, d = directory, l = symlink)
                                # => rw-: owner permissions (read, write, no execute)
                                # => r--: group permissions (read only)
                                # => r--: others permissions (read only)
                                # => 1: hard link count
                                # => user: owner name
                                # => group: group name
                                # => 1234: file size in bytes

# Permission values
# r (read) = 4: View file contents or list directory contents
# w (write) = 2: Modify file or create/delete files in directory
# x (execute) = 1: Run file as program or enter directory (cd)

# Octal notation (most common)
chmod 644 file.txt              # => chmod: change mode (permissions)
                                # => 644: octal notation
                                # => First digit (6): owner permissions = 4+2 = rw-
                                # => Second digit (4): group permissions = 4 = r--
                                # => Third digit (4): others permissions = 4 = r--
                                # => Result: -rw-r--r--

chmod 755 script.sh             # => 755: common for executables
                                # => First digit (7): owner = 4+2+1 = rwx
                                # => Second digit (5): group = 4+1 = r-x
                                # => Third digit (5): others = 4+1 = r-x
                                # => Result: -rwxr-xr-x
                                # => Owner: full access, others: read and execute

chmod 700 private.sh            # => 700: private executable
                                # => First digit (7): owner = 4+2+1 = rwx
                                # => Second digit (0): group = 0 = ---
                                # => Third digit (0): others = 0 = ---
                                # => Result: -rwx------
                                # => Only owner has any access

# Symbolic notation
chmod u+x script.sh             # => u+x: symbolic notation
                                # => u: user (owner)
                                # => +: add permission
                                # => x: execute permission
                                # => Adds execute for owner only
                                # => Other permissions unchanged

chmod go-w file.txt             # => go-w: symbolic notation
                                # => g: group, o: others
                                # => -: remove permission
                                # => w: write permission
                                # => Removes write for group AND others

chmod a+r file.txt              # => a+r: symbolic notation
                                # => a: all (user, group, others)
                                # => +: add permission
                                # => r: read permission
                                # => Adds read for everyone

chmod u=rwx,g=rx,o=r file.txt   # => Absolute symbolic notation
                                # => u=rwx: set owner to rwx (discard previous)
                                # => g=rx: set group to r-x
                                # => o=r: set others to r--
                                # => Equivalent to chmod 754

# Recursive permissions
chmod -R 755 /var/www/html      # => -R: recursive flag
                                # => Applies 755 to directory
                                # => AND all subdirectories
                                # => AND all files within
                                # => Processes entire directory tree

# Make script executable
chmod +x deploy.sh              # => +x without user prefix
                                # => Implies a+x (all users)
                                # => Adds execute permission for owner, group, others
                                # => Now can run: ./deploy.sh

# Change ownership
chown alice file.txt            # => chown: change owner
                                # => Changes owner to "alice"
                                # => Requires root/sudo privileges
                                # => Group unchanged

chown alice:developers file.txt # => owner:group syntax
                                # => Changes owner to "alice"
                                # => AND group to "developers"
                                # => Single command for both

# Change group only
chgrp developers file.txt       # => chgrp: change group
                                # => Changes group to "developers"
                                # => Owner unchanged
                                # => Alternative to chown :developers

# Recursive ownership
chown -R www-data:www-data /var/www
                                # => -R: recursive
                                # => Changes owner to www-data
                                # => AND group to www-data
                                # => Applies to /var/www and ALL contents

# Practical: web server permissions
# Files: 644 (rw-r--r--)
find /var/www/html -type f -exec chmod 644 {} \;
                                # => find: search for files
                                # => -type f: regular files only (not directories)
                                # => -exec: execute command on each match
                                # => chmod 644 {}: {} replaced with found file path
                                # => \;: terminates -exec command
                                # => Sets all files to 644

# Directories: 755 (rwxr-xr-x)
find /var/www/html -type d -exec chmod 755 {} \;
                                # => -type d: directories only
                                # => chmod 755: rwxr-xr-x
                                # => Directories need execute for cd access
                                # => Sets all directories to 755

# Uploads directory (writable by web server)
chmod 775 /var/www/html/uploads
                                # => 775: rwxrwxr-x
                                # => Owner: rwx, Group: rwx, Others: r-x
                                # => Group (www-data) can write
chown www-data:www-data /var/www/html/uploads
                                # => Web server process runs as www-data
                                # => Needs ownership to write files

# Practical: secure private key
chmod 600 ~/.ssh/id_rsa         # => 600: rw-------
                                # => Owner: rw- (read, write)
                                # => Group: --- (no access)
                                # => Others: --- (no access)
                                # => SSH requires this for security
                                # => Refuses to use key if permissions too open
```

**Key Takeaway**: Use octal notation (644, 755, 700) for absolute permission sets, symbolic notation (u+x, go-w) for relative changes - remember that 644 is standard for files, 755 for directories/executables, and 600 for private keys, and always verify permissions after changes.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 37: Archiving and Compression (tar, gzip, zip)

Archiving combines multiple files into one file, while compression reduces file size. The `tar` command creates archives, often combined with `gzip` or `bzip2` for compression.

```bash
# Create tar archive
tar -cf archive.tar files/      # => tar: tape archive utility
                                # => -c: create mode (new archive)
                                # => -f archive.tar: output filename
                                # => files/: directory to archive
                                # => Creates uncompressed archive

# Create compressed tar (gzip)
tar -czf archive.tar.gz files/  # => -c: create mode
                                # => -z: compress with gzip
                                # => -f archive.tar.gz: output filename
                                # => Combines archiving + compression
                                # => Extension: .tar.gz or .tgz
                                # => Faster compression, larger size

# Create compressed tar (bzip2)
tar -cjf archive.tar.bz2 files/ # => -c: create mode
                                # => -j: compress with bzip2
                                # => Better compression ratio than gzip
                                # => Slower than gzip
                                # => Extension: .tar.bz2 or .tbz2

# Create compressed tar (xz)
tar -cJf archive.tar.xz files/  # => -c: create mode
                                # => -J: compress with xz
                                # => Best compression ratio
                                # => Slowest compression
                                # => Extension: .tar.xz

# Extract tar archive
tar -xf archive.tar             # => -x: extract mode
                                # => -f archive.tar: input filename
                                # => Extracts to current directory
                                # => Preserves directory structure

# Extract compressed tar
tar -xzf archive.tar.gz         # => -x: extract mode
                                # => -z: decompress with gzip
                                # => Auto-detects compression in modern tar
tar -xjf archive.tar.bz2        # => -x: extract mode
                                # => -j: decompress with bzip2

# Extract to specific directory
tar -xzf archive.tar.gz -C /tmp # => -x: extract mode
                                # => -z: gzip decompression
                                # => -C /tmp: change to /tmp before extracting
                                # => Extracts contents to /tmp/

# List archive contents
tar -tzf archive.tar.gz         # => -t: list mode (table of contents)
                                # => -z: gzip decompression
                                # => Lists files without extracting
                                # => Preview archive contents

# Verbose output
tar -czfv backup.tar.gz data/   # => -c: create
                                # => -z: gzip
                                # => -f backup.tar.gz: filename
                                # => -v: verbose (show files as processed)
                                # => Prints each file while archiving

# Exclude files
tar -czf backup.tar.gz --exclude='*.log' --exclude='.git' project/
                                # => --exclude='*.log': skip log files
                                # => --exclude='.git': skip .git directory
                                # => Multiple --exclude flags allowed
                                # => Glob patterns supported

# Compress existing file (gzip)
gzip large_file.txt             # => gzip: compression utility
                                # => Creates large_file.txt.gz
                                # => Original file is REMOVED!
                                # => Destructive by default

# Preserve original
gzip -k large_file.txt          # => -k: keep original file
                                # => Creates large_file.txt.gz
                                # => Original large_file.txt remains
                                # => Non-destructive

# Decompress gzip
gunzip file.txt.gz              # => gunzip: decompression utility
                                # => Restores file.txt
                                # => Removes file.txt.gz
gzip -d file.txt.gz             # => -d: decompress mode
                                # => Same as gunzip

# Zip (compatible with Windows)
zip -r archive.zip files/       # => zip: ZIP archive utility
                                # => -r: recursive (include subdirectories)
                                # => Creates Windows-compatible archive
                                # => Preserves original files

# Unzip
unzip archive.zip               # => unzip: extract ZIP archives
                                # => Extracts to current directory
                                # => Shows file names while extracting

# Unzip to directory
unzip archive.zip -d /tmp       # => -d /tmp: destination directory
                                # => Extracts to /tmp/
                                # => Creates directory if needed

# Practical: backup with date
backup_name="backup_$(date +%Y%m%d_%H%M%S).tar.gz"
                                # => $(date ...): command substitution
                                # => +%Y%m%d_%H%M%S: timestamp format
                                # => Example: backup_20251231_235959.tar.gz
tar -czf "$backup_name" /data   # => Creates timestamped backup
                                # => Unique filename prevents overwrites
echo "Backup created: $backup_name"

# Practical: incremental backup
tar -czf backup_full.tar.gz --listed-incremental=snapshot.file /data
                                # => --listed-incremental: incremental backup mode
                                # => snapshot.file: tracks changes since last backup
                                # => First run: full backup, creates snapshot
                                # => Subsequent runs: only changed files

# Practical: split large archive
tar -czf - /data | split -b 100M - backup.tar.gz.part
                                # => tar -czf -: output to stdout (-)
                                # => |: pipe to split
                                # => split -b 100M: split into 100MB chunks
                                # => -: read from stdin
                                # => backup.tar.gz.part: output prefix
                                # => Creates: backup.tar.gz.partaa, partab, etc.
                                # => Restore: cat backup.tar.gz.part* | tar -xzf -
```

**Key Takeaway**: Use `tar -czf` to create gzip-compressed archives, `tar -xzf` to extract, and `-v` for verbose output - remember that `tar` preserves permissions and directory structure, making it ideal for backups, and gzip removes original files unless `-k` is used.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 38: Network Operations (curl, wget, ssh)

Network commands download files, interact with APIs, and access remote systems. The `curl` and `wget` commands handle HTTP requests, while `ssh` provides secure remote access.

```bash
# Download file with wget
wget https://example.com/file.txt
                                # => wget: web get utility
                                # => Downloads file to current directory
                                # => Saves with original filename (file.txt)
                                # => Shows progress bar
                                # => Auto-resumes on interruption

# Download with custom name
wget -O custom_name.txt https://example.com/file.txt
                                # => -O: output filename (capital O)
                                # => Saves as custom_name.txt
                                # => Overwrites if exists

# Download multiple files
wget -i urls.txt                # => -i: input file containing URLs
                                # => urls.txt: one URL per line
                                # => Downloads all files sequentially

# Basic HTTP request with curl
curl https://api.example.com/data
                                # => curl: transfer data with URLs
                                # => Makes HTTP GET request
                                # => Prints response to stdout
                                # => No file saved by default

# Save to file
curl -o output.json https://api.example.com/data
                                # => -o: output filename (lowercase o)
                                # => Saves response to output.json
                                # => Useful for API responses

# Follow redirects
curl -L https://github.com/file # => -L: follow Location headers (redirects)
                                # => Many URLs redirect (http → https)
                                # => Without -L: stops at redirect
                                # => With -L: follows to final destination

# Show headers
curl -I https://example.com     # => -I: HEAD request (headers only)
                                # => Shows: Content-Type, Content-Length, etc.
                                # => No response body
                                # => Useful for checking status codes

# Send POST request
curl -X POST -d "name=Alice&age=30" https://api.example.com/users
                                # => -X POST: HTTP method (POST)
                                # => -d: data payload (request body)
                                # => URL-encoded format: key1=value1&key2=value2
                                # => Content-Type: application/x-www-form-urlencoded (default)

# Send JSON data
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice","age":30}' \
  https://api.example.com/users
                                # => \: line continuation (multi-line command)
                                # => -H: custom header
                                # => Content-Type: application/json (JSON payload)
                                # => -d: JSON data (single-quoted to preserve format)

# Authentication
curl -u username:password https://api.example.com/protected
                                # => -u: HTTP Basic Authentication
                                # => username:password format
                                # => Encodes credentials in Authorization header
                                # => INSECURE over HTTP (use HTTPS!)

# Download with progress bar
curl -# -O https://example.com/large_file.zip
                                # => -#: progress bar (instead of stats)
                                # => -O: save with original filename (capital O)
                                # => Shows percentage complete

# SSH - remote command execution
ssh user@server 'ls /var/log'   # => ssh: secure shell
                                # => user@server: username and hostname
                                # => 'ls /var/log': command to execute remotely
                                # => Output returned to local terminal
                                # => Connection closes after command

# SSH with key
ssh -i ~/.ssh/id_rsa user@server
                                # => -i: identity file (private key path)
                                # => ~/.ssh/id_rsa: typical SSH key location
                                # => Key-based auth (no password prompt)
                                # => More secure than password auth

# Copy file to remote (scp)
scp file.txt user@server:/path/to/destination
                                # => scp: secure copy (over SSH)
                                # => file.txt: local source
                                # => user@server: remote destination
                                # => :/path/to/destination: absolute path on server
                                # => Transfers over encrypted SSH connection

# Copy from remote
scp user@server:/path/to/file.txt local_dir/
                                # => Source: remote (user@server:path)
                                # => Destination: local (local_dir/)
                                # => Downloads file to local machine

# Copy directory recursively
scp -r directory/ user@server:/path/
                                # => -r: recursive (includes subdirectories)
                                # => Copies entire directory tree
                                # => Preserves structure

# Rsync (better than scp for large transfers)
rsync -avz /local/path/ user@server:/remote/path/
                                # => rsync: remote sync (efficient transfer)
                                # => -a: archive mode (preserves permissions, times)
                                # => -v: verbose (show files)
                                # => -z: compress during transfer
                                # => Only transfers changed files (delta sync)
                                # => Resumes on interruption
                                # => Faster than scp for updates

# Practical: API health check
if curl -f -s -o /dev/null https://api.example.com/health; then
                                # => -f: fail on HTTP errors (4xx, 5xx)
                                # => -s: silent (no progress)
                                # => -o /dev/null: discard output
                                # => if: check exit code (0 = success)
    echo "API is up"            # => Exit code 0: HTTP 2xx/3xx
else
    echo "API is down"          # => Exit code non-zero: HTTP 4xx/5xx or network error
fi

# Practical: download and extract
curl -L https://releases.example.com/app.tar.gz | tar -xzf -
                                # => curl -L: download with redirects
                                # => Outputs to stdout (no -o flag)
                                # => |: pipe to tar
                                # => tar -xzf -: extract gzip from stdin
                                # => One-liner: download and extract

# Practical: remote backup
ssh user@server 'tar -czf - /data' > backup.tar.gz
                                # => ssh: execute remote command
                                # => tar -czf - /data: create archive to stdout
                                # => >: redirect stdout to local file
                                # => Streams compressed data over SSH
                                # => Creates local backup.tar.gz
```

**Key Takeaway**: Use `curl` for API interactions and flexible HTTP requests, `wget` for downloading files with resume support, and `ssh/scp/rsync` for secure remote access and file transfer - remember that `curl` writes to stdout by default while `wget` saves to files.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 39: Scheduling Tasks (cron, at)

Task scheduling automates recurring jobs with `cron` or one-time execution with `at`. Cron is essential for maintenance tasks, backups, and periodic monitoring.

```bash
# View current user's crontab
crontab -l                      # => crontab: cron table management
                                # => -l: list mode
                                # => Shows all scheduled jobs for current user
                                # => Empty if no jobs scheduled

# Edit crontab
crontab -e                      # => -e: edit mode
                                # => Opens editor ($EDITOR or vi)
                                # => Syntax-checks on save
                                # => Rejects invalid cron syntax

# Crontab format
# Minute Hour Day Month DayOfWeek Command
# 0-59   0-23 1-31 1-12  0-6      (0=Sunday)
                                # => Five time fields + command
                                # => Minute: 0-59
                                # => Hour: 0-23 (0 is midnight)
                                # => Day: 1-31
                                # => Month: 1-12
                                # => DayOfWeek: 0-6 (0=Sunday, 6=Saturday)
                                # => *: any value (wildcard)
                                # => */n: every n units
                                # => n,m,o: specific values (comma-separated)
                                # => n-m: range of values

# Examples of cron schedules:
# 0 2 * * *    /backup.sh       # => 0: minute 0
                                # => 2: hour 2 (2:00 AM)
                                # => *: any day of month
                                # => *: any month
                                # => *: any day of week
                                # => Daily at 2:00 AM
# 30 */6 * * * /check.sh        # => 30: minute 30
                                # => */6: every 6 hours (0, 6, 12, 18)
                                # => Every 6 hours at :30 (00:30, 06:30, 12:30, 18:30)
# 0 0 * * 0    /weekly.sh       # => 0 0: midnight (00:00)
                                # => *: any day of month
                                # => *: any month
                                # => 0: Sunday
                                # => Weekly on Sunday at midnight
# 0 9 1 * *    /monthly.sh      # => 0 9: 9:00 AM
                                # => 1: first day of month
                                # => *: any month
                                # => *: any day of week
                                # => Monthly on 1st at 9:00 AM
# */15 * * * * /monitor.sh      # => */15: every 15 minutes (0, 15, 30, 45)
                                # => *: every hour
                                # => Runs 96 times per day

# Practical crontab entries
crontab -e                      # => Open editor to add:

# Backup database daily at 2 AM
0 2 * * * /usr/local/bin/backup_db.sh >> /var/log/backup.log 2>&1
                                # => 0 2: 2:00 AM
                                # => >>: append stdout to log
                                # => 2>&1: redirect stderr to stdout
                                # => Both stdout and stderr logged

# Clean temp files every Sunday at 3 AM
0 3 * * 0 find /tmp -type f -mtime +7 -delete
                                # => 0 3 * * 0: Sunday 3:00 AM
                                # => find: search temp files
                                # => -mtime +7: modified more than 7 days ago
                                # => -delete: remove matching files

# Check disk space every hour
0 * * * * df -h | mail -s "Disk Report" admin@example.com
                                # => 0 *: top of every hour
                                # => df -h: disk free (human-readable)
                                # => |: pipe to mail
                                # => mail -s: send email with subject

# Special cron strings
@reboot    /startup.sh          # => @reboot: at system startup
                                # => Runs once per boot
                                # => Useful for service initialization
@daily     /daily_task.sh       # => @daily: equivalent to 0 0 * * *
                                # => Midnight every day
@hourly    /hourly_task.sh      # => @hourly: equivalent to 0 * * * *
                                # => Start of every hour
@weekly    /weekly_task.sh      # => @weekly: equivalent to 0 0 * * 0
                                # => Sunday midnight
@monthly   /monthly_task.sh     # => @monthly: equivalent to 0 0 1 * *
                                # => First of month midnight

# One-time task with at
echo "/backup.sh" | at 14:30    # => at: one-time task scheduler
                                # => 14:30: 2:30 PM
                                # => Runs /backup.sh once at specified time
                                # => Job removed after execution

# Schedule for specific date
echo "/backup.sh" | at 2:00 AM 2025-12-31
                                # => Specific date and time
                                # => Natural language time accepted
                                # => Runs once at 2025-12-31 02:00

# List scheduled at jobs
atq                             # => at queue
                                # => Lists pending one-time jobs
                                # => Shows job number, date/time, user

# Remove at job
atrm 1                          # => at remove
                                # => 1: job number (from atq)
                                # => Cancels scheduled job

# Practical: maintenance script with logging
#!/bin/bash
# Add to crontab: 0 3 * * * /path/to/maintenance.sh

LOGFILE="/var/log/maintenance.log"
                                # => Log file path

log() {                         # => Logging function
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOGFILE"
                                # => $(date ...): timestamp
                                # => >>: append to log file
                                # => Creates timestamped log entry
}

log "Starting maintenance"      # => Log start

# Clean old logs (older than 30 days)
find /var/log -name "*.log" -mtime +30 -delete
                                # => find: search log directory
                                # => -name "*.log": log files only
                                # => -mtime +30: older than 30 days
                                # => -delete: remove matching files
log "Cleaned old logs"

# Optimize database
mysqlcheck --optimize --all-databases
                                # => mysqlcheck: MySQL maintenance utility
                                # => --optimize: optimize tables
                                # => --all-databases: all databases
log "Optimized databases"

# Update package list
apt-get update -qq              # => apt-get update: refresh package list
                                # => -qq: quiet (minimal output)
log "Updated package list"

log "Maintenance completed"     # => Log completion
```

**Key Takeaway**: Use cron for recurring tasks with `crontab -e` to edit schedules, and `at` for one-time execution - always redirect cron output to log files for debugging, and remember that cron runs with limited environment variables so use absolute paths.

**Why It Matters**: This shell scripting concept is fundamental for production automation and system administration. Understanding this pattern enables you to write more robust and maintainable scripts for deployment, monitoring, and infrastructure management tasks.

---

### Example 40: Script Best Practices

Production scripts require robust error handling, logging, argument validation, and clear documentation. Following best practices ensures reliability and maintainability.

```bash
#!/bin/bash
#
# Script: backup_database.sh
# Description: Automated database backup with compression and rotation
# Author: DevOps Team
# Date: 2025-12-30
# Usage: ./backup_database.sh [-v] [-d database] [-o output_dir]
#
                                # => Shebang: specifies bash interpreter
                                # => Header comments: documentation
                                # => Best practice: always document purpose and usage

# Strict error handling
set -euo pipefail               # => set -e: exit on error
                                # => set -u: exit on undefined variable
                                # => set -o pipefail: pipeline fails if any command fails
                                # => Strict mode: fail fast

# Configuration
readonly SCRIPT_NAME=$(basename "$0")
                                # => readonly: constant (cannot be modified)
                                # => basename: extracts filename from path
                                # => $0: script path
                                # => Example: /path/to/script.sh → script.sh
readonly SCRIPT_DIR=$(dirname "$(readlink -f "$0")")
                                # => readlink -f: resolves symlinks to real path
                                # => dirname: extracts directory from path
                                # => Script's directory path
readonly LOG_FILE="/var/log/${SCRIPT_NAME%.sh}.log"
                                # => ${SCRIPT_NAME%.sh}: removes .sh extension
                                # => Parameter expansion: pattern removal
                                # => Example: backup_database.sh → backup_database.log
readonly BACKUP_DIR="${BACKUP_DIR:-/backup}"
                                # => ${VAR:-default}: use VAR or default if unset
                                # => Allows environment override
                                # => Default: /backup
readonly RETENTION_DAYS=7       # => Constant: backup retention period

# Global variables
VERBOSE=false                   # => Boolean flag: default off
DATABASE=""                     # => Required argument: set by getopts
OUTPUT_DIR="$BACKUP_DIR"        # => Initialize from constant

# Logging function
log() {                         # => Centralized logging function
    local level="$1"            # => local: function-scoped variable
                                # => $1: first argument (log level)
    shift                       # => shift: removes $1, shifts arguments left
                                # => $2 becomes $1, $3 becomes $2, etc.
    local message="$*"          # => $*: all remaining arguments as single string
                                # => Captures full log message
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
                                # => $(date ...): current timestamp
                                # => Format: YYYY-MM-DD HH:MM:SS

    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
                                # => Formatted log entry
                                # => tee -a: write to stdout AND append to file
                                # => Both console and file logging

    if [ "$level" = "ERROR" ]; then
                                # => String equality check
        echo "[$timestamp] [$level] $message" >&2
                                # => >&2: redirect to stderr
                                # => Errors go to stderr
    fi
}

# Error handling
error_exit() {                  # => Centralized error exit function
    log "ERROR" "$1"            # => Log error with ERROR level
                                # => $1: error message
    exit 1                      # => Exit code 1 (failure)
}

# Cleanup on exit
cleanup() {                     # => Cleanup function (runs on exit)
    local exit_code=$?          # => $?: exit code of last command
                                # => Captures script's exit status

    if [ $exit_code -ne 0 ]; then
                                # => -ne: not equal (numeric)
                                # => Non-zero: error occurred
        log "ERROR" "Script failed with exit code $exit_code"
    fi

    # Remove temp files
    [ -f "$TEMP_FILE" ] && rm -f "$TEMP_FILE"
                                # => [ -f "$TEMP_FILE" ]: check if temp file exists
                                # => &&: execute if true
                                # => Conditional cleanup

    log "INFO" "Cleanup completed"
}
trap cleanup EXIT               # => trap: register signal handler
                                # => cleanup: function to call
                                # => EXIT: on script exit (any reason)

# Handle interrupts
interrupted() {                 # => Interrupt handler function
    log "WARNING" "Script interrupted by user"
    exit 130                    # => 130: convention for SIGINT (128 + 2)
}
trap interrupted INT TERM       # => INT: Ctrl+C, TERM: kill command
                                # => Both call interrupted function

# Usage information
usage() {                       # => Help function
    cat << EOF                  # => Here-document: multi-line string
Usage: $SCRIPT_NAME [OPTIONS]   # => $SCRIPT_NAME: script filename

Automated database backup with compression and rotation.

Options:
    -v              Verbose output
    -d DATABASE     Database name (required)
    -o DIR          Output directory (default: $BACKUP_DIR)
    -h              Show this help message

Examples:
    $SCRIPT_NAME -d production
    $SCRIPT_NAME -v -d mydb -o /mnt/backup

EOF
    exit 0                      # => Exit with success (help is not error)
}

# Parse command line arguments
parse_args() {                  # => Argument parsing function
    while getopts "vd:o:h" opt; do
                                # => getopts: POSIX option parser
                                # => "vd:o:h": option string
                                # => v,h: flags (no argument)
                                # => d:,o:: require argument
        case $opt in            # => case: match option
            v)
                VERBOSE=true    # => -v flag: enable verbose
                ;;
            d)
                DATABASE="$OPTARG"
                                # => -d option: database name
                                # => $OPTARG: argument value
                ;;
            o)
                OUTPUT_DIR="$OPTARG"
                                # => -o option: output directory
                ;;
            h)
                usage           # => -h: show help and exit
                ;;
            \?)
                error_exit "Invalid option: -$OPTARG"
                                # => \?: invalid option
                ;;
            :)
                error_exit "Option -$OPTARG requires an argument"
                                # => :: missing required argument
                ;;
        esac
    done

    shift $((OPTIND-1))         # => Remove parsed options from arguments
                                # => $OPTIND: next argument index
}

# Validate arguments
validate_args() {               # => Argument validation function
    if [ -z "$DATABASE" ]; then # => -z: string is empty
                                # => DATABASE required
        error_exit "Database name is required (-d option)"
    fi

    if [ ! -d "$OUTPUT_DIR" ]; then
                                # => !: logical NOT
                                # => -d: directory exists
                                # => Check directory exists
        error_exit "Output directory does not exist: $OUTPUT_DIR"
    fi

    if [ ! -w "$OUTPUT_DIR" ]; then
                                # => -w: writable
                                # => Check write permissions
        error_exit "Output directory is not writable: $OUTPUT_DIR"
    fi
}

# Check prerequisites
check_prerequisites() {         # => Dependency check function
    local required_commands=("mysqldump" "gzip")
                                # => Array of required commands
                                # => ( ): array syntax

    for cmd in "${required_commands[@]}"; do
                                # => ${array[@]}: all array elements
                                # => Iterate over commands
        if ! command -v "$cmd" &> /dev/null; then
                                # => command -v: check if command exists
                                # => &> /dev/null: discard output
                                # => !: fail if command not found
            error_exit "Required command not found: $cmd"
        fi
    done
}

# Rotate old backups
rotate_backups() {              # => Backup rotation function
    log "INFO" "Rotating backups older than $RETENTION_DAYS days"

    find "$OUTPUT_DIR" -name "${DATABASE}_*.sql.gz" -mtime +$RETENTION_DAYS -delete
                                # => find: search files
                                # => -name: filename pattern
                                # => ${DATABASE}_*: database-specific backups
                                # => -mtime +N: modified more than N days ago
                                # => -delete: remove matching files

    local deleted_count=$(find "$OUTPUT_DIR" -name "${DATABASE}_*.sql.gz" -mtime +$RETENTION_DAYS | wc -l)
                                # => wc -l: count lines
                                # => Count deleted backups

    [ "$VERBOSE" = true ] && log "INFO" "Deleted $deleted_count old backup(s)"
                                # => Conditional verbose logging
}

# Main backup function
backup_database() {             # => Core backup logic
    local backup_file="${OUTPUT_DIR}/${DATABASE}_$(date +%Y%m%d_%H%M%S).sql.gz"
                                # => Construct timestamped filename
                                # => Example: production_20251231_235959.sql.gz

    log "INFO" "Starting backup of database: $DATABASE"

    # Create backup with compression
    if mysqldump --single-transaction --quick "$DATABASE" | gzip > "$backup_file"; then
                                # => mysqldump: MySQL backup utility
                                # => --single-transaction: consistent snapshot
                                # => --quick: stream rows (low memory)
                                # => |: pipe to gzip
                                # => gzip: compress output
                                # => >: redirect to file
        local size=$(du -h "$backup_file" | cut -f1)
                                # => du -h: disk usage, human-readable
                                # => cut -f1: extract first field (size)
        log "INFO" "Backup successful: $backup_file (size: $size)"
    else
        error_exit "Backup failed for database: $DATABASE"
    fi

    # Verify backup integrity
    if [ ! -s "$backup_file" ]; then
                                # => -s: file exists and NOT empty
                                # => !-s: file missing or empty
        error_exit "Backup file is empty: $backup_file"
    fi

    # Test gzip integrity
    if ! gzip -t "$backup_file"; then
                                # => gzip -t: test integrity
                                # => Verifies file is valid gzip
        error_exit "Backup file is corrupted: $backup_file"
    fi
}

# Main execution
main() {                        # => Main entry point function
    log "INFO" "Starting $SCRIPT_NAME"

    parse_args "$@"             # => "$@": all script arguments
    validate_args               # => Validate after parsing
    check_prerequisites         # => Check dependencies

    backup_database             # => Execute backup
    rotate_backups              # => Clean old backups

    log "INFO" "$SCRIPT_NAME completed successfully"
}

# Run main function
main "$@"                       # => Execute main with all script arguments
```

**Key Takeaway**: Use `set -euo pipefail` for strict error handling, create dedicated logging and error functions, validate all inputs, provide usage documentation, and implement cleanup with trap - always use readonly for constants, check prerequisites before execution, and return meaningful exit codes.

**Why It Matters**: Error handling prevents partial failures that leave systems in inconsistent states. Production scripts must handle errors gracefully to enable recovery and maintain system integrity.

---

### Example 41: Find Command Mastery

The `find` command searches for files based on name, type, size, time, permissions, and more. It's essential for locating files and performing bulk operations in scripts.

```bash
# Find by name
find /home -name "*.log"         # => find: search utility
                                 # => /home: starting directory (search path)
                                 # => -name: match filename pattern
                                 # => "*.log": glob pattern (case-sensitive)
                                 # => Searches recursively through all subdirectories
                                 # => Finds all files ending with .log

# Case-insensitive name
find /home -iname "readme*"      # => -iname: case-insensitive name match
                                 # => "readme*": matches any case variation
                                 # => Matches: README, readme, Readme, rEaDmE.txt
                                 # => Useful for cross-platform file searches

# Find by type
find /var -type f                # => -type f: filter by file type
                                 # => f: regular files
                                 # => Excludes directories, symlinks, devices
                                 # => Searches entire /var directory tree
                                 # => Returns only regular files

find /var -type d -name "log*"   # => -type d: directories only
                                 # => -name "log*": directories starting with "log"
                                 # => Combines type and name filters
                                 # => Both conditions must match (AND logic)

# Find by size
find . -size +100M               # => .: current directory
                                 # => -size: filter by file size
                                 # => +100M: greater than 100 megabytes
                                 # => +: greater than, -: less than, no prefix: exact
                                 # => M: megabytes suffix

find . -size -1k                 # => -1k: less than 1 kilobyte
                                 # => k: kilobytes, M: megabytes, G: gigabytes
                                 # => c: bytes (default if no suffix)
                                 # => Finds small files

# Find by time
find . -mtime -7                 # => -mtime: modification time filter
                                 # => -7: modified within last 7 days
                                 # => -N: less than N days ago (recent)
                                 # => +N: more than N days ago (old)
                                 # => Time measured in 24-hour periods

find . -mmin -60                 # => -mmin: modification time in minutes
                                 # => -60: modified within last 60 minutes
                                 # => More precise than -mtime
                                 # => Useful for recent changes

find . -atime +30                # => -atime: access time filter
                                 # => +30: accessed more than 30 days ago
                                 # => Identifies unused files
                                 # => Useful for cleanup scripts

# Find by permissions
find . -perm 755                 # => -perm 755: exact permission match
                                 # => Must match exactly rwxr-xr-x
                                 # => No partial matches
find . -perm -u=x                # => -u=x: symbolic permission notation
                                 # => -: all specified bits must be set
                                 # => u=x: user has execute permission
                                 # => Other permissions don't matter
find . -perm /u=x                # => /: any of specified bits set
                                 # => Matches if user OR group OR other has execute
                                 # => More permissive than -

# Find and execute
find . -name "*.tmp" -exec rm {} \;
                                 # => -exec: execute command on each match
                                 # => rm {}: command to run
                                 # => {}: placeholder replaced with found file path
                                 # => \;: terminates -exec command
                                 # => Runs rm once PER FILE (can be slow)

# Find with -exec (more efficient)
find . -name "*.log" -exec grep "error" {} +
                                 # => {}: placeholder for found files
                                 # => +: passes MULTIPLE files to single command
                                 # => Groups files together (fewer command invocations)
                                 # => More efficient than \; for bulk operations

# Combine conditions
find . -name "*.py" -size +10k   # => Multiple predicates (implicit AND)
                                 # => -name "*.py": must be Python file
                                 # => -size +10k: AND larger than 10KB
                                 # => Both conditions must be true

find . -name "*.py" -o -name "*.sh"
                                 # => -o: logical OR operator
                                 # => Matches Python files OR shell scripts
                                 # => Either condition satisfies match

# Negation
find . -not -name "*.bak"        # => -not: logical negation
                                 # => Excludes files matching pattern
                                 # => Finds everything EXCEPT .bak files
find . ! -name "*.bak"           # => !: alternative negation syntax
                                 # => Identical to -not
                                 # => Shorter notation

# Limit depth
find . -maxdepth 2 -name "*.conf"
                                 # => -maxdepth 2: limit search depth
                                 # => 1: current directory only
                                 # => 2: current + one subdirectory level
                                 # => Prevents deep recursion
                                 # => Only search 2 levels deep

# Practical: clean old files
find /tmp -type f -mtime +30 -delete
                                 # => /tmp: temporary file directory
                                 # => -type f: regular files only
                                 # => -mtime +30: older than 30 days
                                 # => -delete: remove matching files
                                 # => Automatic cleanup maintenance

# Practical: find large files
find / -type f -size +500M 2>/dev/null | head -20
                                 # => /: search entire filesystem
                                 # => -size +500M: larger than 500MB
                                 # => 2>/dev/null: suppress permission errors
                                 # => |: pipe to head
                                 # => head -20: show first 20 results
                                 # => Disk space troubleshooting

# Practical: fix permissions
find /var/www -type f -exec chmod 644 {} +
                                 # => -type f: files only
                                 # => chmod 644: rw-r--r--
                                 # => Files readable by all, writable by owner
find /var/www -type d -exec chmod 755 {} +
                                 # => -type d: directories only
                                 # => chmod 755: rwxr-xr-x
                                 # => Directories executable (cd-able)
                                 # => Web server permission standard
```

**Key Takeaway**: Use `find` with `-name` for pattern matching, `-type` for file types, `-size` and `-mtime` for filtering, and `-exec` for actions - combine conditions with `-a` (and), `-o` (or), and `!` (not) for complex queries.

**Why It Matters**: The `find` command is essential for locating files, cleaning up storage, fixing permissions, and performing bulk operations across directory trees in production environments.

---

### Example 42: Regular Expressions in Shell

Regular expressions enable powerful pattern matching in shell scripts using `grep`, `sed`, and `[[ =~ ]]`. They're essential for text validation, extraction, and transformation.

```mermaid
%% Regex components
graph TD
    A[Regular Expression] --> B[Literals]
    A --> C[Metacharacters]
    A --> D[Quantifiers]
    A --> E[Anchors]

    B --> F[abc - exact match]
    C --> G[. - any char]
    C --> H[[] - char class]
    D --> I[* + ? - repetition]
    E --> J[^ $ - position]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#000
    style E fill:#CA9161,color:#000
```

```bash
# Basic grep with regex
grep "error" logfile.txt         # => grep: search for patterns
                                 # => "error": literal string pattern
                                 # => Prints lines containing "error"
grep -E "error|warning" log.txt  # => -E: extended regex (ERE mode)
                                 # => |: alternation (OR operator)
                                 # => Matches lines with "error" OR "warning"

# Anchors
grep "^Start" file.txt           # => ^: start of line anchor
                                 # => Matches "Start" ONLY at line beginning
                                 # => "Start of line" matches, " Start" doesn't
grep "end$" file.txt             # => $: end of line anchor
                                 # => Matches "end" ONLY at line end
                                 # => "The end" matches, "end." doesn't
grep "^$" file.txt               # => ^$: empty line pattern
                                 # => ^: start, immediately followed by $: end
                                 # => Matches lines with no content

# Character classes
grep "[0-9]" file.txt            # => [0-9]: character class (any digit)
                                 # => Range notation: 0 through 9
                                 # => Matches lines containing any single digit
grep "[a-zA-Z]" file.txt         # => [a-zA-Z]: multiple ranges
                                 # => a-z: lowercase, A-Z: uppercase
                                 # => Matches any alphabetic character
grep "[^0-9]" file.txt           # => [^...]: negated character class
                                 # => ^: negation (inside brackets)
                                 # => Matches any NON-digit character

# Quantifiers
grep -E "ab*c" file.txt          # => *: zero or more quantifier
                                 # => b*: zero or more b's
                                 # => Matches: ac, abc, abbc, abbbc
grep -E "ab+c" file.txt          # => +: one or more quantifier
                                 # => b+: one or more b's
                                 # => Matches: abc, abbc (NOT ac)
grep -E "ab?c" file.txt          # => ?: zero or one quantifier
                                 # => b?: optional b
                                 # => Matches: ac, abc (NOT abbc)
grep -E "a{3}" file.txt          # => {n}: exactly n repetitions
                                 # => a{3}: exactly 3 a's
                                 # => Matches: aaa
grep -E "a{2,4}" file.txt        # => {m,n}: m to n repetitions
                                 # => a{2,4}: 2, 3, or 4 a's
                                 # => Matches: aa, aaa, aaaa

# Special characters
grep "\." file.txt              # => \.: escaped dot (literal period)
                                 # => Without \: . matches any character
                                 # => With \: . matches only period
grep -E "\$[0-9]+" file.txt     # => \$: escaped dollar (literal $)
                                 # => [0-9]+: one or more digits
                                 # => Matches: $10, $999

# Word boundaries
grep -w "error" file.txt         # => -w: whole word match
                                 # => Word boundaries enforced
                                 # => Matches: "error" (standalone)
                                 # => Doesn't match: "errors", "error_code"

# Case insensitive
grep -i "error" file.txt         # => -i: ignore case
                                 # => Matches: error, ERROR, Error, ErRoR

# Bash regex matching
email="user@example.com"         # => Test email string
if [[ "$email" =~ ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$ ]]; then
                                 # => [[ ... =~ ... ]]: bash regex match
                                 # => ^: start anchor
                                 # => [a-zA-Z0-9._%+-]+: username part
                                 # => @: literal @
                                 # => [a-zA-Z0-9.-]+: domain name
                                 # => \.: literal dot
                                 # => [a-zA-Z]{2,}: TLD (2+ letters)
                                 # => $: end anchor
    echo "Valid email"           # => Executed if pattern matches
fi

# Extract matched portion
if [[ "$text" =~ ([0-9]+) ]]; then
                                 # => (...): capture group
                                 # => [0-9]+: one or more digits
                                 # => Parentheses capture matched text
    echo "Number found: ${BASH_REMATCH[1]}"
                                 # => BASH_REMATCH: array of captures
                                 # => [0]: full match, [1]: first group
fi

# Sed with regex
sed 's/[0-9]\+/NUMBER/g' file.txt
                                 # => s/pattern/replacement/g
                                 # => [0-9]: digit class
                                 # => \+: one or more (escaped in sed BRE)
                                 # => Replace digit sequences with "NUMBER"

# Practical: validate IP address
ip="192.168.1.1"                 # => Test IP string
if [[ "$ip" =~ ^([0-9]{1,3}\.){3}[0-9]{1,3}$ ]]; then
                                 # => ([0-9]{1,3}\.){3}: three groups of digits.dot
                                 # => [0-9]{1,3}: 1-3 digits (octet)
                                 # => \.{3}: literal dot, repeated 3 times
                                 # => [0-9]{1,3}$: final octet
                                 # => Format check only (doesn't validate ranges)
    echo "Valid IP format"
fi

# Practical: extract version numbers
grep -oE "[0-9]+\.[0-9]+\.[0-9]+" file.txt
                                 # => -o: print only matched portion
                                 # => -E: extended regex
                                 # => [0-9]+: one or more digits
                                 # => \.: literal dot
                                 # => Pattern: X.Y.Z (semantic versioning)
                                 # => Example matches: 1.2.3, 10.20.30

# Practical: remove HTML tags
sed 's/<[^>]*>//g' page.html     # => <: literal opening bracket
                                 # => [^>]*: any characters except >
                                 # => >: literal closing bracket
                                 # => Matches entire tag: <tag attr="value">
                                 # => Replaces with empty string (removes)
```

**Key Takeaway**: Use anchors (`^$`) for position, character classes (`[]`) for sets, quantifiers (`*+?{}`) for repetition, and `-E` for extended regex - remember that `[[ =~ ]]` enables regex in bash with `BASH_REMATCH` for captures.

**Why It Matters**: Regular expressions enable powerful pattern matching for log analysis, input validation, text extraction, and data transformation that would be tedious or impossible with simple string matching.

---

### Example 43: Array Manipulation

Bash arrays store multiple values in a single variable. They're essential for handling lists of files, arguments, and dynamic data in scripts.

```bash
# Declare indexed array
fruits=("apple" "banana" "cherry")
                                 # => (...): array literal syntax
                                 # => Space-separated elements
                                 # => fruits[0]="apple", fruits[1]="banana", fruits[2]="cherry"
                                 # => Zero-based indexing

# Alternative declaration
declare -a numbers               # => declare -a: explicit array declaration
                                 # => -a: indexed array (default)
numbers[0]=10                    # => Assign to index 0
numbers[1]=20                    # => Assign to index 1
numbers[2]=30                    # => Assign to index 2
                                 # => Can assign to any index (sparse arrays allowed)

# Access elements
echo "${fruits[0]}"              # => ${array[index]}: element access syntax
                                 # => Curly braces required for array indexing
                                 # => Output: apple
echo "${fruits[1]}"              # => Access second element
                                 # => Output: banana

# Access all elements
echo "${fruits[@]}"              # => @: all elements as separate words
                                 # => Expands to: "apple" "banana" "cherry"
                                 # => Preserves word splitting
                                 # => Output: apple banana cherry
echo "${fruits[*]}"              # => *: all elements as single string
                                 # => Joins with first char of IFS (default: space)
                                 # => Expands to: "apple banana cherry"
                                 # => Output: apple banana cherry

# Array length
echo "${#fruits[@]}"             # => #: length operator
                                 # => ${#array[@]}: count of elements
                                 # => Output: 3

# Element length
echo "${#fruits[0]}"             # => ${#array[index]}: string length of element
                                 # => Length of "apple"
                                 # => Output: 5

# Append elements
fruits+=("date")                 # => +=(): append syntax
                                 # => Adds element to end of array
                                 # => fruits[3]="date"
fruits+=("elderberry" "fig")     # => Append multiple elements
                                 # => fruits[4]="elderberry", fruits[5]="fig"

# Slice array
echo "${fruits[@]:1:2}"          # => ${array[@]:offset:length}: slice syntax
                                 # => :1: start at index 1
                                 # => :2: take 2 elements
                                 # => Output: banana cherry

# All indices
echo "${!fruits[@]}"             # => !: indirection operator
                                 # => ${!array[@]}: list of indices
                                 # => Useful for sparse arrays with gaps
                                 # => Output: 0 1 2 3 4 5

# Iterate over array
for fruit in "${fruits[@]}"; do  # => "${array[@]}": quoted expansion (preserves spaces)
                                 # => Each element becomes separate argument
                                 # => Loop variable receives each element
    echo "Fruit: $fruit"
done

# Iterate with index
for i in "${!fruits[@]}"; do     # => "${!array[@]}": iterate over indices
                                 # => i receives index (0, 1, 2, ...)
    echo "Index $i: ${fruits[$i]}"
                                 # => Access element by index
done

# Remove element
unset 'fruits[1]'                # => unset: delete variable/element
                                 # => 'fruits[1]': quoted to prevent glob expansion
                                 # => Removes element but LEAVES GAP in indices
                                 # => fruits[0], fruits[2] exist, fruits[1] doesn't

# Copy array
copy=("${fruits[@]}")            # => ("${array[@]}"): expansion into new array
                                 # => Creates independent copy
                                 # => Modifications to copy don't affect original

# Join elements
IFS=','                          # => IFS: Internal Field Separator
                                 # => Set to comma
echo "${fruits[*]}"              # => *: joins with IFS
                                 # => Elements separated by comma
                                 # => Output: apple,cherry,date,elderberry,fig

# Split string to array
IFS=',' read -ra parts <<< "a,b,c"
                                 # => IFS=',': delimiter for splitting
                                 # => read -ra: read into array
                                 # => -r: raw (no backslash escaping)
                                 # => -a parts: store in array 'parts'
                                 # => <<<: here-string (feed string to stdin)
echo "${parts[1]}"               # => Access second element
                                 # => Output: b

# Practical: process file list
files=($(find . -name "*.sh"))   # => $(...): command substitution
                                 # => find output becomes array elements
                                 # => Each filename is separate element
echo "Found ${#files[@]} shell scripts"
                                 # => ${#files[@]}: count files

for file in "${files[@]}"; do    # => Iterate over found files
    echo "Processing: $file"     # => Process each file
    # Process each file
done

# Practical: command line args as array
args=("$@")                      # => "$@": all script arguments
                                 # => Store in array for later processing
for arg in "${args[@]}"; do      # => Iterate over arguments
    echo "Argument: $arg"
done

# Practical: filter array
filtered=()                      # => Initialize empty array
for item in "${fruits[@]}"; do   # => Iterate over source array
    if [[ "$item" == a* ]]; then # => Pattern match: starts with 'a'
                                 # => ==: pattern matching (not regex)
        filtered+=("$item")      # => Append matching items to filtered array
    fi
done
echo "Filtered: ${filtered[@]}"  # => Print filtered results
                                 # => Items starting with 'a': apple
```

**Key Takeaway**: Use `${array[@]}` to expand all elements, `${#array[@]}` for length, `${!array[@]}` for indices, and `+=()` to append - always quote `"${array[@]}"` to preserve elements with spaces.

**Why It Matters**: Arrays enable processing of file lists, command arguments, and dynamic data without creating temporary files or relying on error-prone string splitting.

---

### Example 44: Associative Arrays

Associative arrays (hash maps) store key-value pairs, enabling lookups by name rather than index. They're useful for configuration, counters, and data mapping.

```bash
# Declare associative array (bash 4+)
declare -A config                # => declare: variable declaration command
                                 # => -A: associative array type (hash map)
                                 # => Requires bash 4.0 or later
                                 # => Creates empty associative array

# Assign values
config["host"]="localhost"       # => ["key"]="value": assignment syntax
                                 # => String keys (not numeric indices)
config["port"]=8080              # => Numeric value (stored as string)
config["debug"]="true"           # => Boolean as string

# Alternative initialization
declare -A colors=(              # => Inline initialization
    ["red"]="#FF0000"            # => Key: "red", Value: "#FF0000"
    ["green"]="#00FF00"          # => Multiple key-value pairs
    ["blue"]="#0000FF"           # => Comma/newline separated
)

# Access values
echo "${config[host]}"           # => ${array[key]}: access by key
                                 # => Retrieves value for "host"
                                 # => Output: localhost
echo "${config[port]}"           # => Access port value
                                 # => Output: 8080

# Check if key exists
if [[ -v config["host"] ]]; then # => -v: test if variable/key is set
                                 # => config["host"]: specific key
                                 # => Returns true if key exists
    echo "Host is set"           # => Executed if key present
fi

# Default value if key missing
host="${config[host]:-127.0.0.1}"
                                 # => ${var:-default}: parameter expansion
                                 # => Returns config[host] if set
                                 # => Returns 127.0.0.1 if unset or empty
                                 # => Doesn't modify array

# Get all keys
echo "${!config[@]}"             # => !: indirection operator
                                 # => ${!array[@]}: all keys
                                 # => Order is unspecified (hash map)
                                 # => Output: host port debug

# Get all values
echo "${config[@]}"              # => ${array[@]}: all values
                                 # => No key information
                                 # => Output: localhost 8080 true

# Count entries
echo "${#config[@]}"             # => #: length operator
                                 # => ${#array[@]}: count of key-value pairs
                                 # => Output: 3

# Iterate over keys
for key in "${!config[@]}"; do   # => Loop over all keys
                                 # => ${!config[@]}: key list
    echo "$key = ${config[$key]}"
                                 # => Print key and corresponding value
done

# Add/update entries
config["timeout"]=30             # => New key: adds entry
                                 # => config["timeout"] didn't exist before
config["port"]=9000              # => Existing key: updates value
                                 # => Overwrites previous value (8080 → 9000)

# Remove entry
unset 'config["debug"]'          # => unset: remove variable/entry
                                 # => 'config["debug"]': quoted to prevent expansion
                                 # => Removes key-value pair completely

# Clear all
unset config                     # => Remove entire array
declare -A config                # => Re-declare as empty associative array
                                 # => Fresh start

# Practical: count occurrences
declare -A count                 # => Word frequency counter
while read -r word; do           # => Read words from file
                                 # => -r: raw (preserve backslashes)
    ((count["$word"]++))         # => ((...)):  arithmetic context
                                 # => Increment counter for word
                                 # => Auto-initializes to 0 if missing
done < words.txt                 # => < words.txt: redirect file to stdin

for word in "${!count[@]}"; do   # => Iterate over unique words
    echo "$word: ${count[$word]}"
                                 # => Print word and frequency
done

# Practical: configuration file parsing
declare -A settings              # => Config storage
while IFS='=' read -r key value; do
                                 # => IFS='=': split on equals sign
                                 # => read -r: read key and value
    # Skip comments and empty lines
    [[ "$key" =~ ^#.*$ || -z "$key" ]] && continue
                                 # => ^#.*$: lines starting with #
                                 # => -z "$key": empty key
                                 # => continue: skip iteration
    settings["$key"]="$value"    # => Store configuration
done < config.ini                # => Read from config file

echo "Database: ${settings[database]}"
                                 # => Access parsed config

# Practical: error code mapping
declare -A errors=(              # => Error code → message mapping
    [1]="File not found"         # => Exit code 1
    [2]="Permission denied"      # => Exit code 2
    [3]="Network error"          # => Exit code 3
    [4]="Timeout"                # => Exit code 4
)

exit_code=2                      # => Example error code
echo "Error: ${errors[$exit_code]:-Unknown error}"
                                 # => ${errors[$exit_code]:-Unknown error}
                                 # => Lookup error message
                                 # => Default to "Unknown error" if code not mapped
```

**Key Takeaway**: Use `declare -A` for associative arrays, `${array[key]}` to access values, `${!array[@]}` for all keys, and `-v` to check existence - associative arrays require bash 4.0+.

**Why It Matters**: Associative arrays enable O(1) lookups by key, making them ideal for configuration management, counting, caching, and mapping values without complex if-else chains.

---

### Example 45: String Manipulation

Bash provides powerful string manipulation through parameter expansion, enabling substring extraction, replacement, and transformation without external commands.

```bash
string="Hello, World!"           # => Test string for demonstrations

# String length
echo "${#string}"                # => ${#var}: string length operator
                                 # => #: prefix for length
                                 # => Counts characters (13 chars including comma, space, exclamation)
                                 # => Output: 13

# Substring extraction
echo "${string:0:5}"             # => ${var:offset:length}: substring syntax
                                 # => :0: start at index 0 (first character)
                                 # => :5: extract 5 characters
                                 # => Zero-based indexing
                                 # => Output: Hello

echo "${string:7}"               # => ${var:offset}: extract from offset to end
                                 # => :7: start at index 7
                                 # => Omitted length: to end of string
                                 # => Output: World!

echo "${string: -6}"             # => ${var: -n}: negative offset (from end)
                                 # => : -6: space before minus required
                                 # => Last 6 characters
                                 # => Output: World!

# Pattern removal (from start)
file="backup.tar.gz"             # => Filename with multiple extensions
echo "${file#*.}"                # => ${var#pattern}: remove shortest match from start
                                 # => #: single hash (non-greedy)
                                 # => *.: wildcard + dot pattern
                                 # => Removes "backup."
                                 # => Output: tar.gz

echo "${file##*.}"               # => ${var##pattern}: remove longest match from start
                                 # => ##: double hash (greedy)
                                 # => *.: matches all up to last dot
                                 # => Removes "backup.tar."
                                 # => Output: gz

# Pattern removal (from end)
echo "${file%.*}"                # => ${var%pattern}: remove shortest match from end
                                 # => %: single percent (non-greedy from end)
                                 # => .*: dot + wildcard
                                 # => Removes ".gz"
                                 # => Output: backup.tar

echo "${file%%.*}"               # => ${var%%pattern}: remove longest match from end
                                 # => %%: double percent (greedy from end)
                                 # => .*: matches from first dot to end
                                 # => Removes ".tar.gz"
                                 # => Output: backup

# Replacement
text="foo bar foo baz"           # => Text with repeated pattern
echo "${text/foo/FOO}"           # => ${var/pattern/replacement}: replace first
                                 # => /: single slash (replace once)
                                 # => Replaces first "foo" only
                                 # => Output: FOO bar foo baz

echo "${text//foo/FOO}"          # => ${var//pattern/replacement}: replace all
                                 # => //: double slash (global replacement)
                                 # => Replaces all occurrences of "foo"
                                 # => Output: FOO bar FOO baz

# Replace at start/end
echo "${text/#foo/START}"        # => ${var/#pattern/replacement}: replace at start
                                 # => /#: slash + hash (anchor to start)
                                 # => Only replaces if pattern at beginning
                                 # => Output: START bar foo baz

echo "${text/%baz/END}"          # => ${var/%pattern/replacement}: replace at end
                                 # => /%: slash + percent (anchor to end)
                                 # => Only replaces if pattern at end
                                 # => Output: foo bar foo END

# Case conversion (bash 4+)
name="John Doe"                  # => Mixed case string
echo "${name^^}"                 # => ${var^^}: uppercase all characters
                                 # => ^^: double caret (all uppercase)
                                 # => Requires bash 4.0+
                                 # => Output: JOHN DOE

echo "${name,,}"                 # => ${var,,}: lowercase all characters
                                 # => ,,: double comma (all lowercase)
                                 # => Output: john doe

echo "${name^}"                  # => ${var^}: uppercase first character
                                 # => ^: single caret (first char only)
                                 # => Rest of string unchanged
                                 # => Output: John doe

# Default values
echo "${undefined:-default}"     # => ${var:-default}: use default if unset/empty
                                 # => :-: colon dash operator
                                 # => Returns "default" without setting variable
                                 # => Variable remains unset
                                 # => Output: default

echo "${undefined:=default}"     # => ${var:=default}: set and use default
                                 # => :=: colon equals operator
                                 # => Sets undefined="default"
                                 # => Then returns value
                                 # => Output: default

echo "${undefined:?Error message}"
                                 # => ${var:?message}: error if unset/empty
                                 # => :?: colon question operator
                                 # => Prints error message to stderr
                                 # => Exits with code 1

# Practical: extract filename
path="/home/user/documents/file.txt"
                                 # => Full file path
filename="${path##*/}"           # => Remove longest match up to last /
                                 # => Extracts filename
                                 # => Output: file.txt
dirname="${path%/*}"             # => Remove shortest match from last /
                                 # => Extracts directory path
                                 # => Output: /home/user/documents
extension="${filename##*.}"      # => Get extension (after last dot)
                                 # => Output: txt
basename="${filename%.*}"        # => Get basename (before last dot)
                                 # => Output: file

# Practical: sanitize input
user_input="Hello   World"       # => Input with multiple spaces
clean="${user_input// /_}"       # => ${var//pattern/replacement}
                                 # => //: replace all occurrences
                                 # => " ": single space pattern
                                 # => _: underscore replacement
                                 # => Output: Hello___World

# Practical: URL parsing
url="https://example.com:8080/path"
                                 # => Full URL string
protocol="${url%%://*}"          # => Remove from :// onwards (greedy)
                                 # => Output: https
rest="${url#*://}"               # => Remove up to :// (protocol)
                                 # => Output: example.com:8080/path
host="${rest%%/*}"               # => Remove from first / onwards
                                 # => Output: example.com:8080
path_part="${rest#*/}"           # => Remove up to first /
                                 # => Output: path
```

**Key Takeaway**: Use `${#var}` for length, `${var:start:len}` for substrings, `#/##` to remove from start, `%/%%` to remove from end, and `/` for replacement - these are faster than calling `sed` or `awk`.

**Why It Matters**: String manipulation with parameter expansion is significantly faster than spawning external processes, making scripts more efficient and portable across systems.

---

### Example 46: Arithmetic Operations

Bash supports arithmetic operations using `$(( ))`, `let`, and `(( ))` for calculations without external tools like `bc` or `expr`.

```bash
# Arithmetic expansion
result=$((5 + 3))                # => $(( )): arithmetic expansion syntax
                                 # => Evaluates expression inside
                                 # => 5 + 3 = 8
                                 # => Assigns to result variable
echo "$result"                   # => Output: 8

# All operations
a=10                             # => Assign integer to variable
b=3                              # => Variables used in arithmetic context
echo $((a + b))                  # => +: addition operator
                                 # => 10 + 3 = 13
                                 # => Output: 13
echo $((a - b))                  # => -: subtraction operator
                                 # => 10 - 3 = 7
                                 # => Output: 7
echo $((a * b))                  # => *: multiplication operator
                                 # => 10 * 3 = 30
                                 # => Output: 30
echo $((a / b))                  # => /: integer division (truncates)
                                 # => 10 / 3 = 3 (not 3.333...)
                                 # => Output: 3
echo $((a % b))                  # => %: modulo (remainder)
                                 # => 10 % 3 = 1
                                 # => Output: 1
echo $((a ** b))                 # => **: exponentiation
                                 # => 10^3 = 1000
                                 # => Output: 1000

# Increment/decrement
((a++))                          # => (( )): arithmetic command (no output)
                                 # => a++: post-increment (use current, then add 1)
                                 # => a was 10, now 11
echo "$a"                        # => Output: 11

((++a))                          # => ++a: pre-increment (add 1, then use)
                                 # => a was 11, now 12
echo "$a"                        # => Output: 12

((a--))                          # => a--: post-decrement
                                 # => a was 12, now 11
((--a))                          # => --a: pre-decrement
                                 # => a was 11, now 10

# Compound assignment
((a += 5))                       # => +=: add and assign
                                 # => Equivalent to: a = a + 5
                                 # => a = 10 + 5 = 15
((a -= 3))                       # => -=: subtract and assign
                                 # => a = 15 - 3 = 12
((a *= 2))                       # => *=: multiply and assign
                                 # => a = 12 * 2 = 24
((a /= 4))                       # => /=: divide and assign
                                 # => a = 24 / 4 = 6

# Comparison in arithmetic context
if ((a > 5)); then               # => (( )): arithmetic comparison
                                 # => >: greater than
                                 # => Returns exit code: 0 if true, 1 if false
                                 # => if uses exit code
    echo "a is greater than 5"   # => Executed if condition true
fi

# Ternary operator
max=$(( a > b ? a : b ))         # => condition ? value_if_true : value_if_false
                                 # => a > b: comparison
                                 # => ?: ternary operator
                                 # => Returns larger value
echo "Max: $max"                 # => Output: larger of a or b

# Bitwise operations
echo $((5 & 3))                  # => &: bitwise AND
                                 # => 5 = 0101, 3 = 0011
                                 # => 0101 & 0011 = 0001 = 1
                                 # => Output: 1
echo $((5 | 3))                  # => |: bitwise OR
                                 # => 0101 | 0011 = 0111 = 7
                                 # => Output: 7
echo $((5 ^ 3))                  # => ^: bitwise XOR
                                 # => 0101 ^ 0011 = 0110 = 6
                                 # => Output: 6
echo $((~5))                     # => ~: bitwise NOT (complement)
                                 # => ~0101 = ...11111010 = -6 (two's complement)
                                 # => Output: -6
echo $((5 << 2))                 # => <<: left shift
                                 # => 0101 << 2 = 010100 = 20
                                 # => Multiply by 2^2
                                 # => Output: 20
echo $((20 >> 2))                # => >>: right shift
                                 # => 10100 >> 2 = 101 = 5
                                 # => Divide by 2^2
                                 # => Output: 5

# Hexadecimal and octal
echo $((0xFF))                   # => 0x prefix: hexadecimal
                                 # => FF (hex) = 255 (decimal)
                                 # => Output: 255
echo $((0777))                   # => 0 prefix: octal
                                 # => 777 (octal) = 511 (decimal)
                                 # => Output: 511
echo $((2#1010))                 # => base#number: arbitrary base
                                 # => 2#1010: binary 1010 = 10 (decimal)
                                 # => Output: 10

# Floating point (requires bc)
result=$(echo "scale=2; 10 / 3" | bc)
                                 # => bc: arbitrary precision calculator
                                 # => scale=2: 2 decimal places
                                 # => echo: pipes expression to bc
                                 # => Bash arithmetic is integer-only
echo "$result"                   # => Output: 3.33

# Practical: percentage calculation
total=250                        # => Total value
part=73                          # => Partial value
percent=$((part * 100 / total))  # => (73 * 100) / 250
                                 # => 7300 / 250 = 29
                                 # => Multiply first to avoid truncation
echo "Percentage: $percent%"     # => Output: 29%

# Practical: countdown
for ((i=10; i>=0; i--)); do      # => C-style for loop in (( ))
                                 # => i=10: initialization
                                 # => i>=0: condition
                                 # => i--: decrement
    echo "$i..."                 # => Print countdown
    sleep 1                      # => Wait 1 second
done

# Practical: calculate disk usage
used=$(df / | awk 'NR==2 {print $3}')
                                 # => df /: disk free for root
                                 # => awk NR==2: second line (data row)
                                 # => $3: used blocks column
total=$(df / | awk 'NR==2 {print $2}')
                                 # => $2: total blocks column
percent=$((used * 100 / total))  # => Calculate percentage
echo "Disk usage: $percent%"

# Practical: random number in range
min=1                            # => Minimum value (inclusive)
max=100                          # => Maximum value (inclusive)
random=$((RANDOM % (max - min + 1) + min))
                                 # => RANDOM: bash variable (0-32767)
                                 # => %: modulo for range
                                 # => (max - min + 1): range size (100)
                                 # => + min: shift to min value
                                 # => Result: 1-100
echo "Random: $random"           # => Output: random number between 1-100
```

**Key Takeaway**: Use `$(( ))` for calculations, `(( ))` for conditions/increment, and `bc` for floating-point math - remember bash arithmetic is integer-only without `bc`.

**Why It Matters**: Arithmetic operations enable loop counters, calculations, threshold checks, and resource monitoring without spawning external processes, improving script performance.

---

### Example 47: Date and Time Handling

The `date` command formats, parses, and calculates dates and times. It's essential for logging, scheduling, and time-based file naming in scripts.

```bash
# Current date and time
date                             # => date: display current date/time
                                 # => Output: Thu Dec 30 14:30:00 UTC 2025
                                 # => Default format includes day, month, date, time, timezone, year

# Custom format
date +%Y-%m-%d                   # => +FORMAT: custom output format
                                 # => %Y: 4-digit year, %m: month, %d: day
                                 # => Output: 2025-12-30
date +%H:%M:%S                   # => %H: hour (00-23), %M: minute, %S: second
                                 # => Output: 14:30:00
date "+%Y-%m-%d %H:%M:%S"        # => Combine date and time formats
                                 # => Quote format string if it contains spaces
                                 # => Output: 2025-12-30 14:30:00

# Common format codes
# %Y - 4-digit year (2025)       # => Full year with century
# %m - Month 01-12               # => Zero-padded month number
# %d - Day 01-31                 # => Zero-padded day of month
# %H - Hour 00-23                # => 24-hour format
# %M - Minute 00-59              # => Zero-padded minutes
# %S - Second 00-59              # => Zero-padded seconds
# %s - Unix timestamp (seconds since epoch)  # => Seconds since 1970-01-01 00:00:00 UTC
# %F - Full date (%Y-%m-%d)      # => ISO 8601 date format shortcut
# %T - Time (%H:%M:%S)           # => ISO 8601 time format shortcut
# %a - Abbreviated weekday (Mon) # => 3-letter weekday name
# %A - Full weekday (Monday)     # => Complete weekday name
# %b - Abbreviated month (Dec)   # => 3-letter month name
# %B - Full month (December)     # => Complete month name

# ISO 8601 format
date -Iseconds                   # => -I: ISO 8601 format option
                                 # => seconds: include seconds and timezone
                                 # => Output: 2025-12-30T14:30:00+00:00

# Unix timestamp
date +%s                         # => %s: seconds since Unix epoch
                                 # => Output: 1767106200
                                 # => Useful for date arithmetic and comparisons

# Convert timestamp to date
date -d @1767106200              # => -d: interpret date string
                                 # => @: prefix indicates Unix timestamp
                                 # => Converts timestamp to human-readable
                                 # => Output: Thu Dec 30 14:30:00 UTC 2025

# Date arithmetic
date -d "tomorrow"               # => -d: parse relative date string
                                 # => Output: tomorrow's date at current time
date -d "yesterday"              # => Relative date: previous day
                                 # => Output: yesterday's date at current time
date -d "+7 days"                # => +N: add N units to current date
                                 # => Output: 7 days from now
date -d "-1 week"                # => -N: subtract N units from current date
                                 # => Output: 1 week ago
date -d "next Monday"            # => Natural language date parsing
                                 # => Output: date of next Monday
date -d "last Friday"            # => Natural language: previous occurrence
                                 # => Output: date of last Friday
date -d "+2 hours"               # => Time arithmetic: add hours
                                 # => Output: 2 hours from now
date -d "2025-12-25 +5 days"     # => Combine specific date with offset
                                 # => Start from 2025-12-25, add 5 days
                                 # => Output: 2025-12-30

# Parse date string
date -d "2025-12-30" +%s         # => Parse ISO date to Unix timestamp
                                 # => Useful for date comparisons
                                 # => Output: 1767360000

# Practical: timestamped filename
backup_file="backup_$(date +%Y%m%d_%H%M%S).tar.gz"
                                 # => $(date ...): command substitution
                                 # => Format: YYYYMMDD_HHMMSS
                                 # => Creates sortable, unique filenames
echo "$backup_file"              # => Output: backup_20251230_143000.tar.gz

# Practical: log with timestamp
log_message() {
    echo "[$(date +%Y-%m-%d\ %H:%M:%S)] $1"
                                 # => \: escape space in format string
                                 # => $1: first function argument
                                 # => Prefixes message with timestamp
}
log_message "Script started"     # => Call function with message
                                 # => Output: [2025-12-30 14:30:00] Script started

# Practical: age of file in days
file_time=$(stat -c %Y "$file")  # => stat -c %Y: file modification time in Unix timestamp
                                 # => $file: file to check
now=$(date +%s)                  # => Current time as Unix timestamp
age_days=$(( (now - file_time) / 86400 ))
                                 # => (( )): arithmetic evaluation
                                 # => Subtract timestamps, divide by 86400 (seconds per day)
echo "File is $age_days days old"  # => Output: File is 5 days old

# Practical: elapsed time
start_time=$(date +%s)           # => Record start time as timestamp
# ... do work ...                # => Placeholder for operations to time
end_time=$(date +%s)             # => Record end time as timestamp
elapsed=$((end_time - start_time))  # => Calculate difference in seconds
echo "Elapsed: ${elapsed}s"      # => Output: Elapsed: 42s

# Practical: check if weekend
day=$(date +%u)                  # => %u: day of week (1=Monday, 7=Sunday)
                                 # => ISO 8601 weekday number
if ((day >= 6)); then            # => (( )): arithmetic comparison
                                 # => 6=Saturday, 7=Sunday
    echo "It's the weekend"
fi

# Practical: rotate logs by date
current_month=$(date +%Y-%m)     # => Extract year-month for log grouping
                                 # => Output: 2025-12
mv app.log "app_${current_month}.log"
                                 # => Rename log file with month suffix
                                 # => Output: app_2025-12.log
```

**Key Takeaway**: Use `date +FORMAT` for formatting, `date -d` for parsing and arithmetic, and `%s` for Unix timestamps - combine with filename suffixes for timestamped backups and logs.

**Why It Matters**: Proper date handling enables accurate logging, timestamped backups, scheduled task coordination, and time-based file retention policies in production scripts.

---

### Example 48: Here Documents

Here documents (heredocs) embed multi-line text directly in scripts, useful for configuration files, SQL queries, multi-line output, and documentation.

```bash
# Basic heredoc
cat << EOF                       # => << EOF: start heredoc, EOF is delimiter
                                 # => cat: reads and outputs heredoc content
This is a multi-line             # => Line 1: plain text preserved
text block that preserves        # => Line 2: formatting maintained
formatting and line breaks.      # => Line 3: line breaks preserved
EOF                              # => EOF: ends heredoc (must be alone on line)

# With variable expansion
name="Alice"                     # => Variable assignment
cat << EOF                       # => Unquoted delimiter enables expansion
Hello, $name!                    # => $name: variable expanded to "Alice"
Today is $(date +%A).            # => $(date +%A): command substitution executes
                                 # => Output: Today is Thursday.
Your home is $HOME.              # => $HOME: environment variable expanded
EOF

# Literal (no expansion)
cat << 'EOF'                     # => 'EOF': quoted delimiter prevents expansion
Variables like $HOME are NOT expanded.  # => $HOME displayed literally
Commands like $(date) are NOT executed.  # => $(date) displayed literally
This is literal text.            # => All content treated as literal strings
EOF

# Indent suppression
cat <<- EOF                      # => <<- : hyphen removes leading tabs
 This text is indented with tabs.  # => Leading tab removed from output
 The - removes leading tabs.     # => Leading tab removed from output
 Spaces are NOT removed.         # => Spaces preserved (only tabs removed)
EOF

# Write to file
cat << EOF > config.txt          # => > config.txt: redirect heredoc to file
server=localhost                 # => Configuration line 1
port=8080                        # => Configuration line 2
debug=true                       # => Configuration line 3
EOF                              # => Creates/overwrites config.txt

# Append to file
cat << EOF >> logfile.txt        # => >>: append heredoc to existing file
[$(date)] Script executed        # => Timestamp with message
EOF                              # => Adds to end of logfile.txt

# Here string (single line)
cat <<< "This is a here string"  # => <<<: here-string operator
                                 # => Single-line heredoc shortcut
                                 # => Output: This is a here string

# Variable from heredoc
config=$(cat << EOF              # => $(cat << EOF): capture heredoc in variable
{                                # => JSON object start
  "name": "myapp",               # => JSON field: name
  "version": "1.0.0"             # => JSON field: version
}                                # => JSON object end
EOF                              # => Heredoc delimiter
)                                # => Close command substitution
echo "$config"                   # => Output: entire JSON as string

# Practical: SQL query
mysql -u user -p database << EOF  # => mysql: execute SQL from heredoc
SELECT id, name, email           # => SQL SELECT statement
FROM users                       # => SQL FROM clause
WHERE active = 1                 # => SQL filter condition
ORDER BY name;                   # => SQL sort order
EOF                              # => Multiline SQL without escaping

# Practical: generate script
cat << 'EOF' > deploy.sh         # => 'EOF': literal mode (no variable expansion)
#!/bin/bash                      # => Shebang line preserved literally
set -e                           # => Exit on error (literal text)
echo "Deploying..."              # => Echo command (literal text)
git pull origin main             # => Git command (literal text)
npm install                      # => NPM command (literal text)
npm run build                    # => Build command (literal text)
echo "Deployed successfully"     # => Success message (literal text)
EOF                              # => Creates executable script file
chmod +x deploy.sh               # => Make generated script executable

# Practical: SSH with commands
ssh user@server << 'EOF'         # => SSH with heredoc as remote commands
cd /var/www/html                 # => Remote: change directory
git pull                         # => Remote: pull latest code
sudo systemctl restart nginx     # => Remote: restart web server
EOF                              # => All commands execute on remote server

# Practical: email content
sendmail recipient@example.com << EOF  # => sendmail: send email from heredoc
Subject: Daily Report            # => Email header: subject line
From: script@example.com         # => Email header: sender address
                                 # => Blank line separates headers from body
Daily server report for $(date +%F):  # => Body with date expansion
- Disk usage: $(df -h / | awk 'NR==2 {print $5}')
                                 # => Command substitution: disk usage
- Memory: $(free -m | awk 'NR==2 {print $3"/"$2" MB"}')
                                 # => Command substitution: memory stats
- Uptime: $(uptime -p)           # => Command substitution: system uptime
EOF

# Practical: help text
usage() {                        # => Function definition for help output
    cat << EOF                   # => Heredoc with variable expansion
Usage: $(basename "$0") [OPTIONS] <command>
                                 # => $(basename "$0"): script name
                                 # => [OPTIONS]: optional arguments
Options:                         # => Section header
    -h, --help     Show this help message
                                 # => Option documentation
    -v, --verbose  Enable verbose output
                                 # => Option documentation
    -d, --debug    Enable debug mode
                                 # => Option documentation

Commands:                        # => Section header
    start          Start the service
                                 # => Command documentation
    stop           Stop the service
                                 # => Command documentation
    status         Show service status
                                 # => Command documentation

Examples:                        # => Section header
    $(basename "$0") start       # => Usage example with script name
    $(basename "$0") -v status   # => Usage example with option
EOF
}                                # => End function definition
```

**Key Takeaway**: Use `<< EOF` for heredocs with variable expansion, `<< 'EOF'` for literal text, and `<<- EOF` to strip leading tabs - heredocs are cleaner than multiple `echo` statements for multi-line output.

**Why It Matters**: Here documents enable embedding configuration templates, SQL queries, and multi-line content directly in scripts, making them self-contained and easier to maintain than external template files.

---

### Example 49: Process Substitution

Process substitution treats command output as a file, enabling commands that require files to work with dynamic data. It uses `<(command)` for input and `>(command)` for output.

```bash
# Basic process substitution
diff <(ls dir1) <(ls dir2)       # => diff: compare two inputs
                                 # => <(ls dir1): process substitution creates /dev/fd/N
                                 # => Treats command output as file for diff
                                 # => Shows differences between two directory listings

# Sort and compare
diff <(sort file1.txt) <(sort file2.txt)
                                 # => <(sort file1.txt): sort first file
                                 # => <(sort file2.txt): sort second file
                                 # => Compare sorted outputs without temp files

# Join two sorted outputs
join <(sort users.txt) <(sort orders.txt)
                                 # => join: merge lines with common fields
                                 # => Both inputs must be sorted for join
                                 # => Process substitution sorts on-the-fly

# Read into while loop (preserves variables)
while read -r line; do           # => while loop in current shell
    ((count++))                  # => Increment counter
    echo "Line: $line"           # => Process each line
done < <(grep "pattern" file.txt)  # => < <(...): redirect from process substitution
                                 # => Avoids subshell, count persists
echo "Total: $count"             # => count is accessible here
                                 # => Unlike: grep | while (pipe creates subshell)

# Multiple input sources
paste <(cut -f1 file.txt) <(cut -f3 file.txt)
                                 # => paste: merge lines from multiple files
                                 # => <(cut -f1): extract column 1
                                 # => <(cut -f3): extract column 3
                                 # => Combines selected columns without temp files

# Output substitution
command > >(tee logfile.txt)     # => >(...): output process substitution
                                 # => Redirects stdout to command input
                                 # => tee: writes to file and continues pipeline
                                 # => Both displays and logs output

# Log and display
tar -czf - /data 2>&1 > >(gzip > backup.tar.gz) | tee -a backup.log
                                 # => tar -czf -: compress to stdout
                                 # => 2>&1: merge stderr with stdout
                                 # => >(gzip > backup.tar.gz): compress output
                                 # => | tee -a: also append to log file

# Compare remote and local
diff <(ssh server 'cat /etc/config') /etc/config
                                 # => <(ssh server 'cat ...'): fetch remote file
                                 # => Treats remote cat output as file
                                 # => /etc/config: local file for comparison
                                 # => No temp files needed for comparison

# Practical: check for differences
if diff -q <(sort expected.txt) <(sort actual.txt) > /dev/null; then
                                 # => diff -q: quiet mode (exit code only)
                                 # => Compare sorted versions
                                 # => > /dev/null: discard output
    echo "Files match"           # => Exit code 0: no differences
else
    echo "Files differ"          # => Exit code 1: files differ
fi

# Practical: merge sorted logs
sort -m <(sort log1.txt) <(sort log2.txt) <(sort log3.txt)
                                 # => sort -m: merge already-sorted files
                                 # => Three process substitutions
                                 # => Each file sorted independently
                                 # => Efficiently merges three sorted streams

# Practical: parallel processing
paste <(                         # => First process substitution
    grep "error" log.txt | wc -l  # => Count error lines
) <(                             # => Second process substitution
    grep "warning" log.txt | wc -l  # => Count warning lines
) <(                             # => Third process substitution
    grep "info" log.txt | wc -l  # => Count info lines
)                                # => paste combines counts into one line

# Practical: process both stdout and stderr
command 2> >(tee stderr.log >&2) > >(tee stdout.log)
                                 # => 2>: redirect stderr
                                 # => >(tee stderr.log >&2): log stderr, keep on stderr
                                 # => >: redirect stdout
                                 # => >(tee stdout.log): log stdout
                                 # => Both streams logged and displayed separately

# Note: Process substitution creates /dev/fd/N
echo <(echo test)                # => <(echo test): creates /dev/fd/N path
                                 # => echo: displays the path (not contents)
                                 # => Output: /dev/fd/63
                                 # => Temporary file descriptor, auto-managed
```

**Key Takeaway**: Use `<(command)` to provide command output as input file, `>(command)` to redirect output through a command, and `< <(command)` for while loops to preserve variable scope.

**Why It Matters**: Process substitution enables comparing dynamic outputs, processing command output in loops without subshell variable scope issues, and building complex data pipelines without temporary files.

---

### Example 50: Subshells and Command Grouping

Subshells run commands in isolated environments, while command grouping executes commands together. Understanding these enables safer scripts and complex pipelines.

```bash
# Subshell with ( )
(cd /tmp && ls)                  # => ( ): subshell creates isolated environment
                                 # => cd /tmp: changes directory in subshell only
                                 # => && ls: lists files if cd succeeds
pwd                              # => pwd: runs in parent shell
                                 # => Still in original directory (subshell isolated)

# Environment isolation
VAR="parent"                     # => Set variable in parent shell
(                                # => Start subshell
    VAR="child"                  # => Modify variable in subshell
    echo "Inside: $VAR"          # => Output: child (subshell value)
)                                # => End subshell, changes discarded
echo "Outside: $VAR"             # => Output: parent (original value preserved)

# Command grouping with { }
{ echo "one"; echo "two"; } > output.txt
                                 # => { }: group commands (not subshell)
                                 # => Both echoes redirected to same file
                                 # => Note: space after {, semicolon before }
                                 # => Runs in current shell context

# Key difference: grouping vs subshell
{ cd /tmp; ls; }                 # => { }: runs in current shell
                                 # => cd /tmp: changes PARENT directory (persists!)
(cd /tmp; ls)                    # => ( ): runs in subshell
                                 # => cd /tmp: changes only subshell directory

# Redirect grouped commands
{                                # => Start command group
    echo "Header"                # => First command output
    date                         # => Second command output
    echo "Footer"                # => Third command output
} > report.txt                   # => Single redirection for all commands
                                 # => All output written to report.txt

# Subshell for variable isolation
total=0                          # => Initialize in parent shell
while read -r num; do            # => Loop in current shell
    ((total += num))             # => Modify total in current shell
done < numbers.txt               # => < : redirect from file (no subshell)
echo "Total: $total"             # => Works: total modified in current shell

# Problematic: pipe creates subshell
total=0                          # => Initialize in parent shell
cat numbers.txt | while read -r num; do
                                 # => | : pipe creates subshell for while loop
    ((total += num))             # => Modifies total in SUBSHELL only
done                             # => Subshell exits, changes lost
echo "Total: $total"             # => Output: 0 (subshell changes don't persist!)

# Solution: process substitution
total=0                          # => Initialize in parent shell
while read -r num; do            # => Loop in current shell (not subshell)
    ((total += num))             # => Modifies total in current shell
done < <(cat numbers.txt)        # => < <(...): redirect from process substitution
                                 # => Avoids subshell, preserves variable scope
echo "Total: $total"             # => Works: total correctly modified

# Parallel execution
(command1) & (command2) & (command3) &
                                 # => ( ): each command in separate subshell
                                 # => &: run each in background
wait                             # => Wait for all background jobs to complete

# Background subshell
(                                # => Start subshell
    sleep 10                     # => Sleep for 10 seconds
    echo "Done in background"    # => Print after sleep
) &                              # => &: run subshell in background
echo "Continuing immediately"    # => Executes immediately (doesn't wait)

# Practical: safe directory operations
process_directory() {            # => Function definition
    (                            # => Subshell isolates directory change
        cd "$1" || exit 1        # => Change to arg directory, exit subshell on error
        for file in *.txt; do    # => Loop over txt files in new directory
            process "$file"      # => Process each file
        done
    )                            # => Subshell ends, directory change discarded
                                 # => Caller's directory unchanged
}

# Practical: temporary environment
(                                # => Subshell for environment isolation
    export PATH="/custom/bin:$PATH"  # => Prepend custom path (subshell only)
    export DEBUG=true            # => Set debug flag (subshell only)
    ./script.sh                  # => Script sees modified environment
)                                # => Subshell exits
                                 # => Parent's PATH and DEBUG unchanged

# Practical: atomic file operations
{                                # => Group all output together
    echo "BEGIN"                 # => Start marker
    process_data                 # => Main data processing
    echo "END"                   # => End marker
} | atomic_write output.txt      # => Pipe grouped output atomically
                                 # => All-or-nothing write operation

# Practical: error handling scope
(                                # => Subshell for isolated error handling
    set -e                       # => Exit on error (subshell only)
    risky_command                # => If this fails, subshell exits
    another_command              # => Only runs if risky_command succeeds
) || echo "Subshell failed"      # => ||: runs if subshell exits with error
                                 # => Parent shell continues regardless
```

**Key Takeaway**: Use `( )` for isolated environments that don't affect the parent, `{ }` for grouping commands with shared redirections, and remember that pipes create subshells that can't modify parent variables.

**Why It Matters**: Understanding subshells prevents subtle bugs where variables unexpectedly lose values in pipelines and enables safe temporary directory changes without affecting the rest of the script.

---

### Example 51: Temporary Files and Directories

Secure handling of temporary files prevents race conditions and ensures cleanup. Use `mktemp` for safe creation and traps for cleanup.

```bash
# Create temporary file
tmpfile=$(mktemp)                # => mktemp: creates secure temporary file
                                 # => XXXXXXXXXX: replaced with random chars
                                 # => Output: /tmp/tmp.AbCd1234Ef
echo "Temp file: $tmpfile"       # => Random suffix prevents collisions

# Create with template
tmpfile=$(mktemp /tmp/myapp.XXXXXX)
                                 # => Custom template: /tmp/myapp.XXXXXX
                                 # => XXXXXX: minimum 6 X's for randomness
                                 # => Output: /tmp/myapp.A1b2C3

# Create with suffix
tmpfile=$(mktemp --suffix=.txt)  # => --suffix: append extension
                                 # => Output: /tmp/tmp.AbCd1234Ef.txt
                                 # => Useful for tools that check file extensions

# Create temporary directory
tmpdir=$(mktemp -d)              # => -d: create directory instead of file
                                 # => Output: /tmp/tmp.XyZ987AbC
echo "Temp dir: $tmpdir"         # => Directory, not file

# Cleanup on exit
cleanup() {                      # => Cleanup function definition
    rm -f "$tmpfile"             # => Remove temp file (if exists)
    rm -rf "$tmpdir"             # => Remove temp directory recursively
    echo "Cleaned up temp files"
}
trap cleanup EXIT                # => trap: register cleanup for EXIT signal
                                 # => Runs on normal exit, error, or signal

# Safer pattern with trap
tmpfile=$(mktemp) || exit 1      # => Create temp file, exit if fails
trap 'rm -f "$tmpfile"' EXIT     # => Single-quote prevents early expansion
                                 # => $tmpfile expanded when trap executes
                                 # => Guarantees cleanup even on errors

# Write and read temp file
echo "data" > "$tmpfile"         # => Write to temporary file
cat "$tmpfile"                   # => Read from temporary file
                                 # => Output: data

# Use temp file for processing
sort -u inputfile > "$tmpfile"   # => sort -u: sort and remove duplicates
                                 # => Write to temp file (safe, won't corrupt input)
mv "$tmpfile" inputfile          # => mv: atomic replacement on same filesystem
                                 # => Original preserved until move completes

# Temp file for locking (file-based mutex)
lockfile="/tmp/myapp.lock"       # => Lock file path
cleanup() {                      # => Cleanup function
    rm -f "$lockfile"            # => Remove lock file
}
trap cleanup EXIT                # => Register cleanup

if ! (set -C; echo $$ > "$lockfile") 2>/dev/null; then
                                 # => set -C: noclobber (fail if file exists)
                                 # => $$: current process ID
                                 # => Atomic check-and-create operation
    echo "Script already running (PID: $(cat "$lockfile"))"
    exit 1                       # => Exit if lock exists
fi

# Practical: safe in-place edit
process_file() {                 # => Function for safe file modification
    local file="$1"              # => Input file argument
    local tmp=$(mktemp)          # => Create temp file
    trap 'rm -f "$tmp"' RETURN   # => RETURN: cleanup when function returns
                                 # => Ensures cleanup even on early return
    if process < "$file" > "$tmp"; then
                                 # => Process file to temp
                                 # => Check exit code
        mv "$tmp" "$file"        # => Success: atomic replacement
    else
        rm -f "$tmp"             # => Failure: remove temp, keep original
        return 1                 # => Propagate error
    fi
}

# Practical: multiple temp files
declare -a tmpfiles=()           # => Declare array for temp files
cleanup() {                      # => Cleanup function
    rm -f "${tmpfiles[@]}"       # => ${tmpfiles[@]}: expand all array elements
}                                # => Removes all temp files
trap cleanup EXIT                # => Register cleanup

tmpfiles+=("$(mktemp)")          # => Create first temp file, add to array
tmpfiles+=("$(mktemp)")          # => Create second temp file, add to array
# Use ${tmpfiles[0]}, ${tmpfiles[1]}...  # => Access by index

# Practical: working directory
workdir=$(mktemp -d) || exit 1   # => Create temp directory, exit on failure
trap 'rm -rf "$workdir"' EXIT    # => Register recursive cleanup
                                 # => rm -rf: removes directory and contents
cd "$workdir"                    # => Change to temp directory
# All temp operations here...    # => Work in isolated directory
# Automatic cleanup on exit      # => trap ensures removal

# DANGEROUS - don't do this!
# rm -rf /tmp/$USER/*            # => DANGER: if $USER is empty, deletes /tmp/*!
                                 # => Never use unquoted variables in rm -rf

# Safe alternative
tmpdir="/tmp/myapp_$$"           # => $$: current process ID (unique)
                                 # => Creates unique directory per process
mkdir -p "$tmpdir"               # => mkdir -p: create if doesn't exist
trap 'rm -rf "$tmpdir"' EXIT     # => Safe: specific directory, not glob
```

**Key Takeaway**: Always use `mktemp` for temporary files/directories, set up cleanup with `trap ... EXIT`, and never construct temp paths with unquoted variables - this prevents race conditions and ensures cleanup.

**Why It Matters**: Proper temporary file handling prevents security vulnerabilities (symlink attacks, race conditions), ensures cleanup on errors, and avoids filling up /tmp with orphaned files.

---

### Example 52: Lock Files and Mutex Patterns

Lock files prevent concurrent execution of scripts, essential for cron jobs, deployments, and any operation that shouldn't run in parallel.

```bash
# Basic lock file
LOCKFILE="/var/run/myapp.lock"   # => Lock file path in /var/run

# Check if already running
if [ -f "$LOCKFILE" ]; then      # => -f: check if lock file exists
    pid=$(cat "$LOCKFILE")       # => Read PID from lock file
    if kill -0 "$pid" 2>/dev/null; then
                                 # => kill -0: check if process exists (doesn't kill)
                                 # => 2>/dev/null: suppress error if PID doesn't exist
        echo "Already running (PID: $pid)"
        exit 1                   # => Exit: another instance running
    else
        echo "Stale lock file, removing"
        rm -f "$LOCKFILE"        # => Process died, remove stale lock
    fi
fi

# Create lock file
echo $$ > "$LOCKFILE"            # => $$: write current PID to lock file

# Cleanup on exit
cleanup() {                      # => Cleanup function
    rm -f "$LOCKFILE"            # => Remove our lock file
}
trap cleanup EXIT                # => Register cleanup

# Main script logic here...      # => Protected by lock file

# Using flock (recommended)
LOCKFILE="/var/lock/myapp.lock"  # => Lock file path

(                                # => Subshell for automatic lock release
    flock -n 9 || { echo "Already running"; exit 1; }
                                 # => flock -n 9: non-blocking lock on FD 9
                                 # => ||: if lock fails, print and exit
    # Main script logic here...  # => Protected section
    echo "Running with lock"     # => Lock acquired successfully
    sleep 10                     # => Do work
) 9>"$LOCKFILE"                  # => 9>: open file descriptor 9 for writing
                                 # => Lock released when subshell exits

# Flock with timeout
(                                # => Subshell
    flock -w 30 9 || { echo "Timeout waiting for lock"; exit 1; }
                                 # => -w 30: wait up to 30 seconds
                                 # => Blocks until lock available or timeout
    # Got lock after waiting up to 30 seconds
    echo "Running..."            # => Lock acquired within timeout
) 9>"$LOCKFILE"                  # => FD 9 on lock file

# Flock in function
acquire_lock() {                 # => Function to acquire lock
    local lockfile="$1"          # => Lock file path argument
    local fd=200                 # => File descriptor 200 (high number)

    eval "exec $fd>$lockfile"    # => eval: execute dynamic command
                                 # => exec: open FD 200 on lockfile
    flock -n $fd || return 1     # => Try non-blocking lock, return 1 if fails
    return 0                     # => Success
}

release_lock() {                 # => Function to release lock
    local fd=200                 # => Same FD as acquire
    flock -u $fd                 # => -u: unlock file descriptor
}

if acquire_lock "/tmp/myapp.lock"; then
    echo "Lock acquired"         # => Lock obtained
    # Do work...                 # => Protected section
    release_lock                 # => Manually release
else
    echo "Could not acquire lock"  # => Lock held by another process
    exit 1
fi

# Shared vs exclusive locks
(                                # => Subshell for reading
    flock -s 9                   # => -s: shared lock (read lock)
                                 # => Multiple readers can hold shared lock
    cat datafile                 # => Read file contents
) 9<datafile                     # => 9<: open FD 9 for reading

(                                # => Subshell for writing
    flock -x 9                   # => -x: exclusive lock (write lock)
                                 # => Only one writer, blocks readers
    echo "new data" >> datafile  # => Write to file
) 9>>datafile                    # => 9>>: open FD 9 for appending

# Practical: cron job with lock
#!/bin/bash                      # => Shebang for cron execution
# Add to crontab: */5 * * * * /path/to/job.sh  # => Run every 5 minutes
LOCKFILE="/tmp/myjob.lock"       # => Lock file path

exec 200>"$LOCKFILE"             # => exec: open FD 200, persists in script
flock -n 200 || { echo "Previous run still active"; exit 0; }
                                 # => flock -n: non-blocking lock
                                 # => exit 0: silent exit if previous run active
# Cron job logic here...         # => Protected section
process_data                     # => Data processing
send_report                      # => Send report
                                 # => Lock auto-released when script exits

# Practical: deployment lock
deploy() {                       # => Deployment function
    local lockfile="/var/run/deploy.lock"  # => Lock file

    (                            # => Subshell for atomic lock/unlock
        flock -n 9 || {          # => Try non-blocking lock
            echo "Deployment already in progress"
            exit 1               # => Fail fast if deployment running
        }

        echo "Deploying at $(date)"  # => Log start time
        git pull                 # => Update code
        npm install              # => Install dependencies
        npm run build            # => Build application
        systemctl restart app    # => Restart service
        echo "Deployment complete"  # => Log completion
    ) 9>"$lockfile"              # => Lock held for entire deployment
                                 # => Prevents concurrent deploys
}

# Practical: pid file with validation
create_pidfile() {               # => Function to create PID file with validation
    local pidfile="$1"           # => PID file path argument

    if [ -f "$pidfile" ]; then   # => Check if PID file exists
        local oldpid=$(cat "$pidfile")  # => Read old PID
        if [ -d "/proc/$oldpid" ]; then  # => /proc/$PID exists if process running
            echo "Process already running: $oldpid"
            return 1             # => Process exists, fail
        fi
        rm -f "$pidfile"         # => Stale PID file, remove it
    fi

    echo $$ > "$pidfile"         # => Write current PID
    trap "rm -f '$pidfile'" EXIT  # => Register cleanup on exit
    return 0                     # => Success
}
```

**Key Takeaway**: Use `flock` for reliable file locking with `-n` for non-blocking and `-w` for timeout, always validate stale lock files by checking if the PID still exists, and clean up locks in EXIT trap.

**Why It Matters**: Lock files prevent race conditions in cron jobs, avoid duplicate deployments, and ensure data integrity when multiple processes might access the same resources.

---

### Example 53: Signal Handling Deep Dive

Signals allow processes to respond to external events like termination requests, hangups, and user interrupts. Proper handling enables graceful shutdown and cleanup.

```mermaid
%% Signal handling flow
graph TD
    A[Signal Received] --> B{Handler Set?}
    B -->|Yes| C[Execute Handler]
    B -->|No| D[Default Action]
    C --> E{Continue?}
    E -->|Yes| F[Resume Execution]
    E -->|No| G[Cleanup & Exit]
    D --> H[Terminate/Ignore/Core]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style G fill:#CC78BC,color:#000
```

```bash
# Common signals
# SIGTERM (15) - Termination request (default kill)  # => Graceful shutdown signal
# SIGINT (2)   - Interrupt (Ctrl+C)  # => User keyboard interrupt
# SIGHUP (1)   - Hangup (terminal closed)  # => Terminal disconnection
# SIGKILL (9)  - Force kill (cannot be caught!)  # => Immediate termination
# SIGUSR1 (10) - User-defined  # => Application-specific signal 1
# SIGUSR2 (12) - User-defined  # => Application-specific signal 2

# Basic signal handler
trap 'echo "Caught SIGINT"' INT  # => trap 'command' SIGNAL: register handler
                                 # => INT: SIGINT signal name
trap 'echo "Caught SIGTERM"' TERM  # => TERM: SIGTERM signal name

# Cleanup handler
cleanup() {                      # => Cleanup function definition
    echo "Cleaning up..."        # => Log cleanup start
    rm -f "$TEMPFILE"            # => Remove temporary files
    kill $(jobs -p) 2>/dev/null  # => jobs -p: list background job PIDs
                                 # => kill: terminate background jobs
    echo "Cleanup complete"      # => Log cleanup completion
}
trap cleanup EXIT                # => EXIT: pseudo-signal for script exit
                                 # => Catches normal exit, errors, signals

# Graceful shutdown
shutdown=false                   # => Flag for shutdown state
handle_signal() {                # => Signal handler function
    echo "Received shutdown signal"
    shutdown=true                # => Set flag (doesn't exit immediately)
}
trap handle_signal SIGTERM SIGINT  # => Register handler for multiple signals

# Main loop with graceful shutdown
while [ "$shutdown" = false ]; do  # => Loop until shutdown flag set
    echo "Working..."            # => Do work
    sleep 1                      # => Brief pause
    # Check shutdown flag each iteration  # => Allows graceful completion
done
echo "Shutting down gracefully"  # => Exit after loop completes

# Ignore signal
trap '' SIGINT                   # => '': empty command ignores signal
                                 # => Ctrl+C has no effect
                                 # => Useful for critical sections

# Reset to default
trap - SIGINT                    # => -: reset to default behavior
                                 # => SIGINT will terminate script again

# Re-raise signal after cleanup
handle_term() {                  # => SIGTERM handler
    echo "Received SIGTERM"      # => Log signal receipt
    cleanup                      # => Perform cleanup
    trap - SIGTERM               # => Reset to default handler
    kill -SIGTERM $$             # => $$: current PID
                                 # => Re-send signal to self
                                 # => Propagates signal to parent process
}
trap handle_term SIGTERM         # => Register handler

# Multiple signals
trap 'echo "Signal received"; cleanup; exit 130' INT TERM HUP
                                 # => Single handler for multiple signals
                                 # => INT TERM HUP: space-separated signal list
                                 # => exit 130: standard exit code for SIGINT

# Signal in child processes
parent_handler() {               # => Parent signal handler
    echo "Parent received signal"
    # Signal all children        # => Send signal to all child processes
    kill -TERM -- -$$            # => -$$: negative PID = process group ID
                                 # => --: separator for negative PID
                                 # => Signals entire process group
}
trap parent_handler SIGTERM      # => Register parent handler

# Reload configuration (SIGHUP)
load_config() {                  # => Configuration reload function
    echo "Reloading configuration..."
    source /etc/myapp/config     # => source: re-read config file
                                 # => Updates variables in current shell
    echo "Configuration reloaded"
}
trap load_config SIGHUP          # => SIGHUP: traditionally reload signal
                                 # => kill -HUP $PID to trigger reload

# Practical: service wrapper
#!/bin/bash                      # => Service daemon script
PIDFILE="/var/run/myservice.pid"  # => PID file for service
shutdown=false                   # => Shutdown flag

cleanup() {                      # => Cleanup on exit
    rm -f "$PIDFILE"             # => Remove PID file
    echo "[$(date)] Service stopped" >> /var/log/myservice.log
                                 # => Log stop time
}

handle_shutdown() {              # => Graceful shutdown handler
    echo "Shutdown requested"    # => Log request
    shutdown=true                # => Set flag (doesn't exit immediately)
}

trap cleanup EXIT                # => Register cleanup
trap handle_shutdown SIGTERM SIGINT  # => Register shutdown handler

echo $$ > "$PIDFILE"             # => Write service PID
echo "[$(date)] Service started" >> /var/log/myservice.log
                                 # => Log start time

while [ "$shutdown" = false ]; do  # => Main service loop
    # Service work here          # => Process tasks
    process_queue                # => Handle queue items
    sleep 5                      # => Brief pause between iterations
done

echo "[$(date)] Graceful shutdown complete"  # => Log graceful exit

# Practical: timeout with signal
timeout_handler() {              # => Timeout handler
    echo "Operation timed out"   # => Log timeout
    exit 124                     # => Exit code 124: standard for timeout
}
trap timeout_handler ALRM        # => ALRM: alarm signal

# Set alarm for 30 seconds
( sleep 30; kill -ALRM $$ ) &   # => Background subshell: sleep then send ALRM
                                 # => $$: parent script PID (not subshell PID!)
alarm_pid=$!                     # => $!: PID of background alarm process

# Long operation
long_running_command             # => Operation to time-limit

# Cancel alarm if completed
kill $alarm_pid 2>/dev/null      # => Kill alarm process if still running
                                 # => 2>/dev/null: suppress error if already dead
trap - ALRM                      # => Reset ALRM handler
```

**Key Takeaway**: Use `trap 'handler' SIGNAL` to catch signals, always handle SIGTERM for graceful shutdown, use EXIT trap for guaranteed cleanup, and remember SIGKILL cannot be caught or ignored.

**Why It Matters**: Proper signal handling enables graceful shutdown that completes in-progress operations, cleans up resources, and prevents data corruption when services are stopped or restarted.

---

### Example 54: Debugging Shell Scripts

Debugging tools help identify errors, trace execution, and understand script behavior. Use `set` options, `trap DEBUG`, and strategic logging for effective debugging.

```bash
# Enable debugging modes
set -x                           # => -x: xtrace mode
                                 # => Prints each command before execution
                                 # => Shows expansions and substitutions
set -v                           # => -v: verbose mode
                                 # => Prints lines as read from script
                                 # => Shows raw input before expansion

# Disable debugging
set +x                           # => +x: disable xtrace
                                 # => Toggle debugging off

# Selective debugging
set -x                           # => Enable tracing
problematic_function             # => Trace only this function
set +x                           # => Disable tracing
                                 # => Debug specific section, not entire script

# Combine with error handling
set -euxo pipefail               # => -e: exit immediately on error
                                 # => -u: error on undefined variables
                                 # => -x: trace command execution
                                 # => -o pipefail: pipe fails if any command fails
                                 # => Strict mode for debugging

# Debug output to file
exec 2> debug.log                # => exec: redirect stderr for entire script
                                 # => 2>: file descriptor 2 (stderr)
set -x                           # => Trace output goes to stderr → debug.log
                                 # => Keeps stdout clean for normal output

# Debug only if enabled
DEBUG=${DEBUG:-false}            # => ${VAR:-default}: use false if DEBUG unset
debug() {                        # => Custom debug function
    [ "$DEBUG" = true ] && echo "[DEBUG] $*" >&2
                                 # => [ ]: test if DEBUG is true
                                 # => &&: execute if test succeeds
                                 # => >&2: write to stderr
}

debug "Variable value: $var"     # => Only outputs if DEBUG=true
                                 # => Usage: DEBUG=true ./script.sh

# Trace with line numbers
PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'
                                 # => PS4: prompt for trace output
                                 # => ${BASH_SOURCE}: script filename
                                 # => ${LINENO}: line number
                                 # => ${FUNCNAME[0]:+...}: function name if in function
set -x                           # => Enable tracing
# Now traces show file:line:function  # => Output: +(script.sh:42): main(): command

# DEBUG trap (runs before each command)
trap 'echo ">>> $BASH_COMMAND"' DEBUG
                                 # => DEBUG: pseudo-signal, fires before each command
                                 # => $BASH_COMMAND: command about to execute
# Shows each command before execution  # => Like set -x but customizable

# ERR trap (runs on errors)
trap 'echo "Error at line $LINENO: $BASH_COMMAND"' ERR
                                 # => ERR: fires when command returns non-zero
                                 # => $LINENO: line number of error
                                 # => Helps locate errors quickly

# Practical: verbose mode option
VERBOSE=false                    # => Default: quiet mode

while getopts "v" opt; do        # => getopts: parse command-line options
    case $opt in                 # => Match option letter
        v) VERBOSE=true ;;       # => -v flag sets verbose
    esac
done

log() {                          # => Conditional logging function
    [ "$VERBOSE" = true ] && echo "$*"
                                 # => Only echo if VERBOSE is true
}

log "Starting process..."        # => Only prints if -v flag used

# Practical: function call trace
call_stack() {                   # => Print function call stack
    local i                      # => Loop counter
    echo "Call stack:"           # => Header
    for ((i=${#FUNCNAME[@]}-1; i>=0; i--)); do
                                 # => ${#FUNCNAME[@]}: array length
                                 # => Loop from deepest to shallowest
        echo "  ${FUNCNAME[$i]}() at ${BASH_SOURCE[$i]}:${BASH_LINENO[$i]}"
                                 # => FUNCNAME: function name array
                                 # => BASH_SOURCE: source file array
                                 # => BASH_LINENO: line number array
    done
}

# Practical: assert function
assert() {                       # => Assert function for preconditions
    local condition="$1"         # => Condition to test
    local message="${2:-Assertion failed}"
                                 # => ${2:-default}: custom or default message

    if ! eval "$condition"; then  # => eval: execute condition string
                                 # => !: negate result
        echo "ASSERT FAILED: $message" >&2  # => Print to stderr
        echo "Condition: $condition" >&2  # => Show failed condition
        echo "Line: ${BASH_LINENO[0]}" >&2  # => Show caller line number
        exit 1                   # => Terminate script
    fi
}

assert '[ -f "config.txt" ]' "Config file missing"
                                 # => Verify file exists
assert '[ $count -gt 0 ]' "Count must be positive"
                                 # => Verify count is positive

# Practical: execution timing
time_start() {                   # => Start timing function
    _start_time=$(date +%s%N)    # => %s%N: seconds + nanoseconds
                                 # => Global variable for timing
}

time_end() {                     # => End timing function
    local end_time=$(date +%s%N)  # => Current time in nanoseconds
    local elapsed=$(( (end_time - _start_time) / 1000000 ))
                                 # => Calculate difference
                                 # => / 1000000: convert nanoseconds to milliseconds
    echo "Elapsed: ${elapsed}ms"  # => Output: Elapsed: 1234ms
}

time_start                       # => Start timer
expensive_operation              # => Operation to measure
time_end                         # => Stop timer and print duration

# Using bash debugger (bashdb)
# bashdb script.sh               # => Interactive debugger for bash scripts
# Commands: n (next), s (step), c (continue), p (print), b (breakpoint)
                                 # => Similar to gdb for C/C++
```

**Key Takeaway**: Use `set -x` for command tracing, `PS4` for detailed trace output, DEBUG trap for command interception, and conditional logging with a VERBOSE flag - combine with ERR trap for error location.

**Why It Matters**: Effective debugging techniques save hours of troubleshooting by revealing exactly where and why scripts fail, especially in complex pipelines and functions.

---

### Example 55: Environment Variables

Environment variables configure applications and scripts without code changes. Understanding scope, export, and persistence is essential for portable scripts.

```bash
# View all environment variables
env                              # => env: list all exported variables
                                 # => Shows NAME=value pairs
printenv                         # => printenv: identical to env
                                 # => Both display environment

# View specific variable
echo "$PATH"                     # => echo: print variable value
                                 # => $PATH: executable search path
printenv HOME                    # => printenv VAR: print specific variable
                                 # => HOME: user home directory

# Set variable (current shell only)
MY_VAR="value"                   # => Assignment: shell variable (not exported)
                                 # => Not inherited by child processes

# Export for child processes
export MY_VAR="value"            # => export: mark variable for inheritance
                                 # => Child processes see this variable
export PATH="$HOME/bin:$PATH"    # => Prepend directory to PATH
                                 # => $HOME/bin added to search path

# Set and export in one line
export DATABASE_URL="postgres://localhost/db"
                                 # => Assign and export in single command

# Unset variable
unset MY_VAR                     # => unset: remove variable completely
                                 # => Variable no longer defined

# Check if set
if [ -z "${MY_VAR+x}" ]; then    # => ${VAR+x}: expands to 'x' if set, empty if unset
                                 # => -z: true if string is empty
                                 # => Tests if variable is defined
    echo "MY_VAR is not set"
fi

# Default value patterns
echo "${VAR:-default}"           # => ${VAR:-default}: use default if VAR unset/empty
                                 # => VAR unchanged, just uses default for this expansion
echo "${VAR:=default}"           # => ${VAR:=default}: set VAR to default if unset
                                 # => VAR is assigned the default value
echo "${VAR:?Error message}"     # => ${VAR:?msg}: error and exit if VAR unset
                                 # => Useful for required variables
echo "${VAR:+alternate}"         # => ${VAR:+alt}: use alternate if VAR is set
                                 # => Opposite of :-

# Common environment variables
echo "User: $USER"               # => $USER: current logged-in username
echo "Home: $HOME"               # => $HOME: user's home directory path
echo "Shell: $SHELL"             # => $SHELL: user's default login shell
echo "Path: $PATH"               # => $PATH: colon-separated executable search path
echo "PWD: $PWD"                 # => $PWD: present working directory
echo "Term: $TERM"               # => $TERM: terminal type (xterm, screen, etc.)
echo "Editor: ${EDITOR:-vim}"    # => $EDITOR: preferred text editor (default: vim)

# Scope demonstration
export OUTER="outer"             # => export: mark for inheritance
INNER="inner"                    # => No export: local to current shell

bash -c 'echo "OUTER=$OUTER INNER=$INNER"'
                                 # => bash -c: spawn child shell with command
                                 # => OUTER inherited (exported)
                                 # => INNER not inherited (not exported)
                                 # => Output: OUTER=outer INNER=

# Modify for single command
MY_VAR="temp" command            # => VAR=value command: set for one command only
                                 # => MY_VAR=temp only in command's environment
                                 # => Original MY_VAR unchanged after

# Load from file
source .env                      # => source: execute file in current shell
                                 # => . .env: shorthand for source
                                 # => Variables set in .env persist

# Parse .env file (safer)
if [ -f .env ]; then             # => Check if .env exists
    export $(grep -v '^#' .env | xargs)
                                 # => grep -v '^#': exclude comments
                                 # => xargs: convert lines to arguments
                                 # => export: mark all for inheritance
fi

# Practical: environment-specific config
case "${ENVIRONMENT:-development}" in
                                 # => ${ENVIRONMENT:-development}: default to development
    production)                  # => Production configuration
        API_URL="https://api.example.com"
        DEBUG=false
        ;;
    staging)                     # => Staging configuration
        API_URL="https://staging-api.example.com"
        DEBUG=true
        ;;
    *)                           # => Default: development configuration
        API_URL="http://localhost:3000"
        DEBUG=true
        ;;
esac
export API_URL DEBUG             # => Export both variables for child processes

# Practical: secure secret handling
# Don't: echo "Password: $DB_PASSWORD"  # => NEVER log passwords
# Don't: export PASSWORD in scripts  # => Avoid exporting sensitive data

# Do: Pass directly to commands
mysql --password="$DB_PASSWORD" ...
                                 # => Pass secret as command argument
                                 # => Not visible in environment

# Do: Use files for secrets
DB_PASSWORD=$(cat /run/secrets/db_password)
                                 # => Read from secure file
                                 # => /run/secrets: Docker secrets location

# Practical: path manipulation
add_to_path() {                  # => Function to safely add to PATH
    case ":$PATH:" in            # => :$PATH:: wrap with colons for matching
        *":$1:"*) ;;             # => *":$1:"*: check if directory already in PATH
                                 # => ;;: do nothing if found
        *) export PATH="$1:$PATH" ;;
                                 # => Prepend directory if not found
                                 # => Prevents duplicates
    esac
}

add_to_path "$HOME/bin"          # => Add ~/bin to PATH if not present
add_to_path "/opt/tools/bin"     # => Add /opt/tools/bin to PATH if not present

# Practical: save and restore environment
save_env() {                     # => Function to save environment state
    export -p > /tmp/saved_env   # => export -p: print all exported variables
                                 # => Format: export VAR=value
                                 # => Save to file for later restore
}

restore_env() {                  # => Function to restore environment
    source /tmp/saved_env        # => source: execute saved export commands
                                 # => Restores all saved variables
}
```

**Key Takeaway**: Use `export` for variables needed by child processes, `${VAR:-default}` for defaults, and source `.env` files for configuration - remember unexported variables aren't inherited by subprocesses.

**Why It Matters**: Environment variables enable configuration without code changes, support different environments (dev/staging/prod), and separate secrets from source code for security.

---

## Next Steps

Continue to [Advanced](/en/learn/software-engineering/linux-platform/shell/by-example/advanced) examples for performance optimization, advanced scripting patterns, and production deployment techniques (75-95% coverage).
