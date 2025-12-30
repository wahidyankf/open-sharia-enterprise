---
title: "Advanced"
date: 2025-12-30T07:56:02+07:00
draft: false
weight: 10000003
description: "Examples 31-40: Advanced shell patterns including process management, signals, debugging, performance optimization, and production scripting (75-95% coverage)"
tags: ["linux", "shell", "bash", "tutorial", "by-example", "advanced", "production", "performance"]
---

## Advanced Level (75-95% Coverage)

This level covers advanced shell concepts through 10 self-contained examples. Each example demonstrates production-grade patterns used in system administration, DevOps automation, and enterprise scripting.

---

### Example 31: Process Management and Job Control

Shell job control manages background processes, foreground/background switching, and process monitoring using built-in shell features.

```bash
# Run command in background
sleep 60 &                      # => [1] 12345 (job number and PID)
                                # => & sends process to background

# List background jobs
jobs                            # => [1]+ Running  sleep 60 &
                                # => + indicates current job

# Bring job to foreground
fg %1                           # => Brings job 1 to foreground
                                # => Blocks until complete or Ctrl+Z

# Send foreground job to background
# First suspend with Ctrl+Z
sleep 120                       # Press Ctrl+Z
                                # => [1]+ Stopped  sleep 120
bg %1                           # => [1]+ sleep 120 &
                                # => Resumes in background

# Kill background job
kill %1                         # => Terminates job 1
                                # => %1 refers to job number, not PID

# Wait for background jobs
command1 &                      # => Start background job
command2 &                      # => Start another
wait                            # => Wait for ALL background jobs
                                # => Blocks until all complete

# Wait for specific job
command &                       # => [1] 12345
wait %1                         # => Wait only for job 1

# Disown job (continue after shell exit)
sleep 600 &                     # => [1] 12346
disown %1                       # => Remove from job table
                                # => Process continues after logout

# Run command immune to hangup (SIGHUP)
nohup ./long-running-script.sh &
                                # => Redirects stdout/stderr to nohup.out
                                # => Continues after terminal closes
```

**Key Takeaway**: Use `&` for background execution, `jobs` to list, `fg/bg` to control, `wait` to synchronize, and `nohup` for logout-immune processes. Job control is essential for parallel processing and long-running tasks.

---

### Example 32: Signal Handling and Traps

Trap handlers execute code when signals are received, enabling cleanup on exit, interrupt handling, and graceful shutdown.

```bash
#!/bin/bash
# Script with signal handling

# Define cleanup function
cleanup() {
    echo "Cleaning up..."         # => Execute on exit
    rm -f /tmp/script.$$.*        # => Remove temp files (PID-specific)
    echo "Done."
}

# Register trap for EXIT signal
trap cleanup EXIT               # => Calls cleanup() when script exits
                                # => Runs on normal exit or error

# Trap interrupt (Ctrl+C)
trap 'echo "Interrupted!"; exit 130' INT
                                # => INT = SIGINT (signal 2)
                                # => Exit code 130 = 128 + 2

# Trap termination
trap 'echo "Terminated!"; cleanup; exit 143' TERM
                                # => TERM = SIGTERM (signal 15)
                                # => Exit code 143 = 128 + 15

# Ignore signal
trap '' HUP                     # => Ignore SIGHUP (hang up)
                                # => Empty string ignores signal

# Reset trap to default
trap - INT                      # => - resets INT to default behavior

# Multiple signals in one trap
trap cleanup EXIT INT TERM      # => Same handler for 3 signals

# Temporary file with automatic cleanup
temp_file=$(mktemp)             # => Create temp file
trap "rm -f $temp_file" EXIT    # => Auto-delete on exit

# Use temp file
echo "data" > "$temp_file"      # => Write to temp file
# ... process data ...
                                # => temp_file deleted on exit automatically

# Prevent Ctrl+C during critical section
trap '' INT                     # => Ignore INT
# ... critical code ...
trap - INT                      # => Restore INT

# Debugging trap (execute before each command)
trap 'echo "Executing: $BASH_COMMAND"' DEBUG
                                # => Print each command before execution
                                # => Useful for script debugging

# Return trap (execute when function returns)
function_with_trap() {
    trap 'echo "Function exiting"' RETURN
    # ... function code ...
}                               # => RETURN trap fires when function exits
```

**Key Takeaway**: Traps ensure cleanup code runs on exit, interrupt, or termination. Use `trap 'code' SIGNAL` to register handlers, `trap '' SIGNAL` to ignore signals, and `trap - SIGNAL` to reset. Essential for production scripts that manage resources.

---

### Example 33: Advanced Parameter Expansion

Bash parameter expansion provides powerful string manipulation, default values, substring extraction, and pattern matching without external commands.

```bash
# Default values
name=${USER:-"guest"}           # => Use $USER, or "guest" if unset/empty
                                # => :- provides default value

name=${USER-"guest"}            # => Use $USER, or "guest" if unset (NOT if empty)
                                # => - checks only unset, not empty

# Assign default if unset
name=${USER:="defaultuser"}     # => Sets $name AND $USER to "defaultuser" if unset
                                # => := assigns default

# Error if unset
name=${USER:?"USER not set"}    # => Prints error and exits if $USER unset
                                # => :? for required variables

# Use alternative value
name=${USER:+"logged in"}       # => Use "logged in" if $USER is set
                                # => :+ opposite of :-

# String length
file="document.txt"
length=${#file}                 # => 12 (length of string)

# Substring extraction
path="/home/user/documents/file.txt"
${path:0:5}                     # => "/home" (from index 0, length 5)
${path:6:4}                     # => "user" (from index 6, length 4)
${path:11}                      # => "documents/file.txt" (from index 11 to end)
${path: -8}                     # => "file.txt" (last 8 characters, note space before -)

# Remove prefix pattern (shortest match)
${path#*/}                      # => "home/user/documents/file.txt" (removes "*/")
                                # => # removes shortest matching prefix

# Remove prefix pattern (longest match)
${path##*/}                     # => "file.txt" (removes everything up to last /)
                                # => ## removes longest matching prefix

# Remove suffix pattern (shortest match)
${path%/*}                      # => "/home/user/documents" (removes "/*")
                                # => % removes shortest matching suffix

# Remove suffix pattern (longest match)
${path%%/*}                     # => "" (removes everything from first /)
                                # => %% removes longest matching suffix

# Pattern replacement (first match)
file="test.txt.backup"
${file/.txt/.md}                # => "test.md.backup" (replaces first .txt)
                                # => /pattern/replacement

# Pattern replacement (all matches)
${file//.txt/.md}               # => "test.md.backup" (replaces all .txt)
                                # => //pattern/replacement (global)

# Pattern replacement at start
${file/#test/demo}              # => "demo.txt.backup" (replace at start only)
                                # => /#pattern/replacement

# Pattern replacement at end
${file/%backup/copy}            # => "test.txt.copy" (replace at end only)
                                # => /%pattern/replacement

# Case conversion (Bash 4+)
name="Alice"
${name,,}                       # => "alice" (lowercase all)
${name^^}                       # => "ALICE" (uppercase all)
${name,}                        # => "alice" (lowercase first character)
${name^}                        # => "Alice" (uppercase first character)

# Array expansion
files=(a.txt b.txt c.txt)
${files[@]}                     # => "a.txt b.txt c.txt" (all elements)
${files[*]}                     # => "a.txt b.txt c.txt" (all elements as single word)
${files[0]}                     # => "a.txt" (first element)
${#files[@]}                    # => 3 (array length)

# Indirect expansion
var="USER"
echo ${!var}                    # => Value of $USER (indirect reference)
                                # => ! dereferences variable name
```

**Key Takeaway**: Parameter expansion eliminates external commands like `sed`, `cut`, `basename`, `dirname` for string operations. Use `${var:-default}` for defaults, `${var#pattern}` for prefix removal, `${var%pattern}` for suffix removal, and `${var//pattern/replacement}` for substitution. Faster and more portable than external tools.

---

### Example 34: Process Substitution and Named Pipes

Process substitution creates temporary named pipes to use command output as file arguments, enabling advanced piping patterns.

```bash
# Compare output of two commands
diff <(ls dir1) <(ls dir2)      # => <(command) creates temporary file with output
                                # => diff reads from two "files"
                                # => Equivalent to: ls dir1 > /tmp/a; ls dir2 > /tmp/b; diff /tmp/a /tmp/b

# Process substitution as input
while read line; do
    echo "Line: $line"
done < <(find . -name "*.txt")  # => Read from command output
                                # => <(command) provides file descriptor

# Multiple input sources
paste <(seq 1 5) <(seq 10 14)   # => 1	10
                                # => 2	11
                                # => Combines two command outputs side-by-side

# Output redirection with process substitution
echo "data" > >(tee file1.txt file2.txt)
                                # => >(command) creates output file descriptor
                                # => Write to multiple destinations

# Join two sorted outputs
join <(sort file1) <(sort file2)
                                # => Sort and join in single command
                                # => No intermediate files

# Named pipes (FIFO)
mkfifo /tmp/mypipe              # => Create named pipe
                                # => Acts as file but no disk I/O

# Write to named pipe (background)
cat file.txt > /tmp/mypipe &    # => Blocks until reader connects

# Read from named pipe
cat < /tmp/mypipe               # => Read data from pipe
                                # => Unblocks writer

# Named pipe for inter-process communication
mkfifo /tmp/logpipe
tail -f /tmp/logpipe &          # => Reader in background
echo "Log message" > /tmp/logpipe  # => Writer sends message
                                # => tail receives and displays

# Cleanup named pipe
rm /tmp/mypipe                  # => Remove FIFO file

# Process substitution with tee (log and process)
command | tee >(grep ERROR > errors.log) >(grep WARN > warnings.log)
                                # => Send output to 3 destinations:
                                # => stdout, errors.log (ERRORs only), warnings.log (WARNs only)
```

**Key Takeaway**: Process substitution `<(command)` and `>(command)` treat command output/input as files. Named pipes (FIFOs) enable inter-process communication. Both eliminate temporary file creation and simplify complex pipelines.

---

### Example 35: Advanced Looping and Iteration

Bash provides multiple looping constructs beyond basic `for` and `while`, including C-style loops, field iteration, and parallel processing.

```bash
# C-style for loop
for ((i=0; i<10; i++)); do
    echo "Iteration $i"         # => Prints 0 through 9
done                            # => C-style: for ((init; condition; increment))

# Loop with step
for ((i=0; i<=100; i+=10)); do
    echo $i                     # => 0, 10, 20, ..., 100
done                            # => i+=10 increments by 10

# Loop over array indices
arr=(a b c d)
for i in "${!arr[@]}"; do
    echo "Index $i: ${arr[$i]}" # => Index 0: a, Index 1: b, etc.
done                            # => ${!arr[@]} expands to indices

# Loop until condition
count=0
until [ $count -ge 5 ]; do
    echo "Count: $count"        # => Loops while condition is FALSE
    ((count++))
done                            # => until is opposite of while

# Loop over command output (word splitting)
for word in $(cat file.txt); do
    echo "Word: $word"          # => Splits on whitespace
done                            # => WARNING: breaks on spaces in data

# Loop over lines (safe, preserves spaces)
while IFS= read -r line; do
    echo "Line: $line"          # => Reads line-by-line
done < file.txt                 # => IFS= prevents trimming, -r preserves backslashes

# Loop with custom field separator
while IFS=: read -r user pass uid gid rest; do
    echo "User: $user, UID: $uid"
done < /etc/passwd              # => IFS=: splits on colons
                                # => Reads fields into separate variables

# Infinite loop with break
while true; do
    read -p "Enter command (q to quit): " cmd
    [ "$cmd" = "q" ] && break   # => Exit loop on 'q'
    echo "You entered: $cmd"
done

# Continue to next iteration
for i in {1..10}; do
    [ $((i % 2)) -eq 0 ] && continue  # => Skip even numbers
    echo $i                     # => Prints only odd numbers
done

# Nested loops with labels (Bash 4+)
outer=0
while [ $outer -lt 3 ]; do
    inner=0
    while [ $inner -lt 3 ]; do
        if [ $inner -eq 1 ]; then
            break 2             # => Break out of 2 loops (outer)
        fi
        echo "$outer,$inner"
        ((inner++))
    done
    ((outer++))
done

# Parallel processing with background jobs
for file in *.txt; do
    process_file "$file" &      # => Each iteration in background
done
wait                            # => Wait for all background jobs

# Limit parallel jobs
max_jobs=4
for file in *.txt; do
    while [ $(jobs -r | wc -l) -ge $max_jobs ]; do
        sleep 0.1               # => Wait if at limit
    done
    process_file "$file" &      # => Start next job
done
wait
```

**Key Takeaway**: Use C-style `for ((i=0; i<n; i++))` for numeric iteration, `while IFS= read -r` for line-by-line processing, `until` for loops that run while condition is false, and background jobs with `wait` for parallel processing. Always use `-r` with `read` to preserve backslashes.

---

### Example 36: Debugging and Error Handling

Production scripts require robust error handling, debugging capabilities, and fail-fast behavior to prevent silent failures.

```bash
#!/bin/bash
# Production script template with error handling

# Exit on error
set -e                          # => Exit immediately if command fails
                                # => Prevents cascading failures

# Exit on undefined variable
set -u                          # => Error if accessing undefined variable
                                # => Catches typos and missing vars

# Fail on pipe errors
set -o pipefail                 # => Pipeline fails if ANY command fails
                                # => Without this: "false | true" succeeds

# Combined (common production pattern)
set -euo pipefail               # => Enable all three safety checks

# Debug mode (print each command before execution)
set -x                          # => Enable xtrace (print commands)
                                # => Shows variable expansion

# Conditional debug
[ "${DEBUG:-}" = "1" ] && set -x
                                # => Enable debug if DEBUG=1 set

# Disable debug for section
{ set +x; } 2>/dev/null         # => Disable xtrace, suppress trace of set +x itself
# ... code without debug ...
set -x                          # => Re-enable debug

# Error line number
trap 'echo "Error on line $LINENO"' ERR
                                # => Print line number where error occurred
                                # => $LINENO is Bash built-in variable

# Full error context
trap 'echo "Error: Command \"$BASH_COMMAND\" failed with exit code $? on line $LINENO"' ERR
                                # => $BASH_COMMAND is the failing command
                                # => $? is exit code of failed command

# Validate required commands exist
require_command() {
    command -v "$1" >/dev/null 2>&1 || {
        echo "Error: Required command '$1' not found" >&2
        exit 1
    }
}

require_command jq              # => Ensure jq is installed
require_command curl            # => Check for curl

# Validate required variables
: "${API_KEY:?Error: API_KEY not set}"
                                # => Exit with error if API_KEY unset
                                # => : is null command, only evaluates parameter

# Function with error handling
safe_operation() {
    local file=$1

    # Check preconditions
    [ -f "$file" ] || {
        echo "Error: File $file not found" >&2
        return 1
    }

    # Perform operation with error checking
    if ! grep "pattern" "$file" > /dev/null; then
        echo "Warning: Pattern not found in $file" >&2
        return 2
    fi

    return 0                    # => Explicit success
}

# Call with error handling
if ! safe_operation "data.txt"; then
    echo "Operation failed"
    exit 1
fi

# Dry-run mode
DRY_RUN=${DRY_RUN:-0}           # => Default to 0 (execute)

run_cmd() {
    if [ "$DRY_RUN" = "1" ]; then
        echo "DRY-RUN: $*"      # => Print instead of executing
    else
        "$@"                    # => Execute command
    fi
}

run_cmd rm important-file.txt   # => Safe: shows command if DRY_RUN=1

# Verbose mode
VERBOSE=${VERBOSE:-0}

log() {
    [ "$VERBOSE" = "1" ] && echo "[$(date +'%Y-%m-%d %H:%M:%S')] $*" >&2
}

log "Starting process"          # => Prints if VERBOSE=1

# Assert function
assert() {
    if ! "$@"; then
        echo "Assertion failed: $*" >&2
        exit 1
    fi
}

assert [ -d "/expected/directory" ]
                                # => Exit if directory doesn't exist

# Retry logic
retry() {
    local max_attempts=$1
    shift
    local attempt=1

    while [ $attempt -le $max_attempts ]; do
        if "$@"; then
            return 0            # => Success
        fi
        echo "Attempt $attempt failed, retrying..." >&2
        ((attempt++))
        sleep $((attempt * 2))  # => Exponential backoff
    done

    return 1                    # => All attempts failed
}

retry 3 curl -f https://api.example.com/health
                                # => Retry up to 3 times
```

**Key Takeaway**: Use `set -euo pipefail` for fail-fast behavior, trap ERR to log failures, validate preconditions with `command -v` and parameter expansion, implement retry logic for network operations, and use `DRY_RUN` for safe testing. Production scripts must fail loudly, never silently.

---

### Example 37: Performance Optimization and Benchmarking

Shell script performance matters for large-scale automation. Avoid external commands in loops, use built-in features, and benchmark critical sections.

```bash
# Benchmark time measurement
time_cmd() {
    local start=$(date +%s%N)   # => Nanosecond timestamp
    "$@"                        # => Execute command
    local end=$(date +%s%N)
    local duration=$(( (end - start) / 1000000 ))  # => Convert to milliseconds
    echo "Duration: ${duration}ms" >&2
}

time_cmd grep pattern large-file.txt

# BAD: External command in loop (SLOW)
count=0
for i in {1..1000}; do
    count=$(expr $count + 1)    # => Spawns expr process 1000 times!
done                            # => ~5 seconds

# GOOD: Arithmetic expansion (FAST)
count=0
for i in {1..1000}; do
    ((count++))                 # => Built-in arithmetic, no fork
done                            # => ~0.01 seconds (500x faster!)

# BAD: Subshell for string manipulation (SLOW)
for file in *.txt; do
    basename=$(basename "$file" .txt)  # => Fork basename command
done

# GOOD: Parameter expansion (FAST)
for file in *.txt; do
    basename=${file%.txt}       # => Built-in parameter expansion
    basename=${basename##*/}    # => Remove path
done

# BAD: Reading file line-by-line with cat (SLOW)
cat file.txt | while read line; do
    echo "$line"                # => Useless use of cat (UUOC)
done                            # => Creates unnecessary pipeline

# GOOD: Direct redirection (FAST)
while read line; do
    echo "$line"
done < file.txt                 # => No external command

# BAD: Multiple greps in sequence (SLOW)
cat file.txt | grep foo | grep bar | grep baz
                                # => 4 processes in pipeline

# GOOD: Single grep with regex (FAST)
grep 'foo.*bar.*baz' file.txt   # => Single process
                                # => Or use awk for complex logic

# Parallel processing for CPU-bound tasks
process_file() {
    # ... expensive operation ...
    sleep 1                     # => Simulate work
}

# Serial (SLOW for many files)
for file in *.txt; do
    process_file "$file"        # => One at a time
done                            # => 100 files = 100 seconds

# Parallel with GNU parallel (FAST)
parallel process_file ::: *.txt # => All cores utilized
                                # => 100 files = ~10 seconds (10 cores)

# Parallel with xargs (portable)
printf '%s\n' *.txt | xargs -P 8 -I {} sh -c 'process_file "{}"'
                                # => -P 8: 8 parallel processes
                                # => More portable than GNU parallel

# Manual parallel with background jobs
max_jobs=8
for file in *.txt; do
    while [ $(jobs -r | wc -l) -ge $max_jobs ]; do
        sleep 0.1               # => Wait for slot
    done
    process_file "$file" &      # => Background job
done
wait                            # => Wait for all to complete

# Avoid unnecessary forks
# BAD
result=$(echo $var)             # => Unnecessary fork

# GOOD
result=$var                     # => Direct assignment

# Cache repeated command output
# BAD
for i in {1..100}; do
    if [ "$(whoami)" = "root" ]; then  # => Forks whoami 100 times
        echo "Running as root"
    fi
done

# GOOD
current_user=$(whoami)          # => Cache result
for i in {1..100}; do
    if [ "$current_user" = "root" ]; then
        echo "Running as root"
    fi
done

# Use read -a for splitting (no awk/cut)
IFS=: read -ra fields <<< "a:b:c"
echo "${fields[1]}"             # => b
                                # => No external command needed

# Bulk operations instead of loops
# BAD
for file in *.txt; do
    dos2unix "$file"            # => One process per file
done

# GOOD
dos2unix *.txt                  # => Single process handles all
                                # => Many commands accept multiple files
```

**Key Takeaway**: Avoid external commands in loops, use built-in arithmetic `(())`, parameter expansion `${}`, and `read` for string operations. Parallelize CPU-bound tasks with `xargs -P`, `GNU parallel`, or background jobs. Cache repeated command output. Prefer bulk operations over loops. Profile with `time` to find bottlenecks.

---

### Example 38: Secure Scripting Practices

Production scripts must sanitize inputs, avoid injection vulnerabilities, protect secrets, and follow least-privilege principles.

```bash
#!/bin/bash
# Secure script template

# Explicit PATH to prevent $PATH poisoning
export PATH=/usr/local/bin:/usr/bin:/bin
                                # => Hardcoded PATH prevents malicious commands

# Secure temp directory
TMPDIR=$(mktemp -d)             # => Create secure temp directory
trap "rm -rf $TMPDIR" EXIT      # => Auto-cleanup on exit
chmod 700 "$TMPDIR"             # => Owner-only permissions

# Validate input (prevent injection)
validate_filename() {
    local filename=$1

    # Check for path traversal
    if [[ "$filename" =~ \.\. ]]; then
        echo "Error: Path traversal detected" >&2
        return 1
    fi

    # Check for shell metacharacters
    if [[ "$filename" =~ [';|&$`<>(){}'] ]]; then
        echo "Error: Invalid characters in filename" >&2
        return 1
    fi

    # Whitelist approach (safest)
    if [[ ! "$filename" =~ ^[a-zA-Z0-9._-]+$ ]]; then
        echo "Error: Filename contains invalid characters" >&2
        return 1
    fi

    return 0
}

# BAD: Command injection vulnerability
user_input="file.txt; rm -rf /"
cat $user_input                 # => DANGEROUS: Executes "rm -rf /"!

# GOOD: Quote variables
cat "$user_input"               # => Safe: treats as literal filename

# BAD: eval with user input (NEVER DO THIS)
user_cmd="ls"
eval $user_cmd                  # => If user_cmd="rm -rf /", you're in trouble

# GOOD: Use arrays for commands
commands=("ls" "-la" "/home")
"${commands[@]}"                # => Safe: no word splitting or injection

# Protect secrets (don't hardcode)
# BAD
API_KEY="secret123"             # => Visible in ps, process list, logs

# GOOD: Read from file with restricted permissions
[ -f ~/.secrets/api_key ] || {
    echo "Error: API key file not found" >&2
    exit 1
}
API_KEY=$(cat ~/.secrets/api_key)
                                # => File should be chmod 600

# BETTER: Use environment variables
: "${API_KEY:?Error: API_KEY environment variable not set}"
                                # => Set outside script, not in source

# Secure file creation (prevent race conditions)
# BAD: Check-then-create (TOCTOU vulnerability)
if [ ! -f /tmp/myfile ]; then
    echo "data" > /tmp/myfile   # => Attacker can create symlink between check and write
fi

# GOOD: Atomic create with exclusive open
{
    set -C                      # => noclobber: don't overwrite existing files
    echo "data" > /tmp/myfile   # => Fails if file exists
} 2>/dev/null

# BETTER: Use mktemp
temp_file=$(mktemp)             # => Creates unique file atomically

# Sanitize before logging
sanitize_log() {
    local message=$1
    # Remove potential secrets (credit cards, SSN, API keys)
    message=${message//[0-9]\{4\}-[0-9]\{4\}-[0-9]\{4\}-[0-9]\{4\}/****-****-****-****}
    echo "$message"
}

log_message "User input: $user_input"  # => BAD: May leak secrets
log_message "$(sanitize_log "$user_input")"  # => GOOD: Sanitized

# File operations with safe permissions
# Create file with restricted permissions
(umask 077 && touch secret.txt) # => Creates with 600 permissions
                                # => Subshell prevents umask from affecting rest of script

# Check for world-readable files
if [ $(($(stat -c '%a' file.txt) & 004)) -ne 0 ]; then
    echo "Warning: File is world-readable" >&2
fi

# Validate checksums before execution
expected_hash="abc123..."
actual_hash=$(sha256sum script.sh | cut -d' ' -f1)
if [ "$expected_hash" != "$actual_hash" ]; then
    echo "Error: Checksum mismatch" >&2
    exit 1
fi

# Drop privileges if running as root
if [ "$(id -u)" = "0" ]; then
    # Drop to specific user
    exec su -c "$0 $*" - nobody  # => Re-execute as 'nobody' user
fi

# Audit logging
log_audit() {
    echo "$(date +'%Y-%m-%d %H:%M:%S') [$(whoami)] $*" >> /var/log/secure-script.log
}

log_audit "Script started with args: $*"
```

**Key Takeaway**: Always quote variables, validate inputs with whitelists, never use `eval` with user input, read secrets from files with restricted permissions, use `mktemp` for temp files, set secure umask, avoid TOCTOU vulnerabilities, and sanitize logs. Security requires defense in depth.

---

### Example 39: Advanced Text Processing (awk)

AWK is a powerful text processing language built into most Unix systems, ideal for column-based data, reports, and complex transformations.

```bash
# Basic awk structure
awk 'pattern { action }' file.txt
                                # => For each line matching pattern, execute action

# Print specific columns
awk '{print $1, $3}' file.txt   # => Print columns 1 and 3
                                # => $1, $2, $3 are fields (default separator: whitespace)
                                # => $0 is entire line

# Custom field separator
awk -F: '{print $1, $7}' /etc/passwd
                                # => -F: sets field separator to colon
                                # => Prints username and shell

# Multiple patterns and actions
awk '/error/ {print "ERROR:", $0} /warning/ {print "WARN:", $0}' log.txt
                                # => Print errors and warnings with labels

# BEGIN and END blocks
awk 'BEGIN {print "Report"} {sum += $1} END {print "Total:", sum}' numbers.txt
                                # => BEGIN: before processing
                                # => END: after all lines
                                # => Useful for headers, footers, totals

# Built-in variables
awk '{print NR, NF, $0}' file.txt
                                # => NR: line number (record number)
                                # => NF: number of fields in current line
                                # => $NF: last field

# Conditionals
awk '$3 > 100 {print $1, $3}' data.txt
                                # => Print name and value if value > 100

awk '{if ($1 == "ERROR") print $0; else print "OK"}' log.txt
                                # => If-else logic

# Multiple conditions
awk '$1 > 10 && $2 < 50 {print $0}' data.txt
                                # => AND condition
awk '$1 == "foo" || $2 == "bar" {print $0}' data.txt
                                # => OR condition

# Calculations
awk '{sum += $2; count++} END {print sum/count}' numbers.txt
                                # => Calculate average of column 2

# String functions
awk '{print toupper($1)}' file.txt
                                # => Convert column 1 to uppercase

awk '{print substr($1, 1, 3)}' file.txt
                                # => Extract first 3 characters

awk '{print length($0)}' file.txt
                                # => Print line length

# Pattern matching
awk '/^[0-9]/ {print $0}' file.txt
                                # => Lines starting with digit

awk '$1 ~ /^test/ {print $0}' file.txt
                                # => Column 1 matches regex ^test

awk '$1 !~ /debug/ {print $0}' file.txt
                                # => Column 1 does NOT match "debug"

# Arrays and counting
awk '{count[$1]++} END {for (word in count) print word, count[word]}' file.txt
                                # => Count occurrences of each word in column 1
                                # => Associative arrays (hash maps)

# Multi-line processing
awk 'NR % 2 == 0 {print prev, $0} {prev = $0}' file.txt
                                # => Print pairs of lines
                                # => prev stores previous line

# Range patterns
awk '/START/,/END/ {print $0}' file.txt
                                # => Print lines between START and END (inclusive)

# Formatted output
awk '{printf "%-10s %5d\n", $1, $2}' file.txt
                                # => printf for formatted output
                                # => %-10s: left-aligned string, width 10
                                # => %5d: right-aligned integer, width 5

# Practical examples

# Log analysis: count HTTP status codes
awk '{status[$9]++} END {for (code in status) print code, status[code]}' access.log
                                # => Assumes status code in field 9

# CSV parsing
awk -F, '{gsub(/"/, "", $2); print $1, $2}' data.csv
                                # => -F, for comma separator
                                # => gsub removes quotes from field 2

# Generate report
awk 'BEGIN {print "User Report"; print "============"}
     {total += $3; print $1, $2, $3}
     END {print "------------"; print "Total:", total}' data.txt
                                # => Multi-line awk script

# Pivot data
awk '{a[$1] += $2} END {for (i in a) print i, a[i]}' transactions.txt
                                # => Sum column 2, grouped by column 1

# Join files (like SQL join)
awk 'NR==FNR {a[$1]=$2; next} $1 in a {print $0, a[$1]}' file1.txt file2.txt
                                # => NR==FNR: first file
                                # => Store key-value pairs from file1
                                # => Lookup and append from file2
```

**Key Takeaway**: AWK excels at column-based processing with built-in variables (NR, NF, $1-$n), supports conditionals and arrays, and provides string functions. Use for log analysis, CSV processing, reporting, and data aggregation. More powerful than `cut`, `grep`, `sed` for structured data. Multi-line awk scripts can replace complex pipelines.

---

### Example 40: Production Deployment Script Pattern

Real-world deployment scripts combine all advanced techniques: error handling, logging, validation, rollback, and idempotency.

```bash
#!/bin/bash
# Production deployment script template
# Deploys application with validation, rollback, and audit logging

# ====================
# Configuration
# ====================
set -euo pipefail               # Fail fast
IFS=$'\n\t'                     # Safer IFS

APP_NAME="myapp"
VERSION="${1:-}"
DEPLOY_DIR="/opt/${APP_NAME}"
BACKUP_DIR="/var/backups/${APP_NAME}"
LOG_FILE="/var/log/${APP_NAME}-deploy.log"
MAX_BACKUPS=5

# ====================
# Color output
# ====================
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'                    # No color

# ====================
# Logging functions
# ====================
log() {
    local level=$1
    shift
    local message="$*"
    local timestamp=$(date +'%Y-%m-%d %H:%M:%S')

    echo -e "${timestamp} [${level}] ${message}" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "$@"; }
log_warn() { echo -e "${YELLOW}$(log "WARN" "$@")${NC}"; }
log_error() { echo -e "${RED}$(log "ERROR" "$@")${NC}" >&2; }
log_success() { echo -e "${GREEN}$(log "SUCCESS" "$@")${NC}"; }

# ====================
# Error handling
# ====================
cleanup() {
    local exit_code=$?

    if [ $exit_code -ne 0 ]; then
        log_error "Deployment failed with exit code $exit_code"
        log_warn "Run '$0 rollback' to restore previous version"
    fi

    # Cleanup temp files
    [ -d "${TMPDIR:-}" ] && rm -rf "${TMPDIR:-}"
}

trap cleanup EXIT
trap 'log_error "Interrupted"; exit 130' INT TERM

# ====================
# Validation functions
# ====================
require_root() {
    if [ "$(id -u)" -ne 0 ]; then
        log_error "This script must be run as root"
        exit 1
    fi
}

validate_version() {
    local version=$1

    if [ -z "$version" ]; then
        log_error "Version not specified"
        echo "Usage: $0 <version>" >&2
        exit 1
    fi

    if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        log_error "Invalid version format: $version (expected: x.y.z)"
        exit 1
    fi
}

check_dependencies() {
    local deps=(curl tar systemctl)

    for cmd in "${deps[@]}"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            log_error "Required command not found: $cmd"
            exit 1
        fi
    done

    log_info "All dependencies satisfied"
}

# ====================
# Backup functions
# ====================
create_backup() {
    local backup_file="${BACKUP_DIR}/${APP_NAME}-$(date +%Y%m%d-%H%M%S).tar.gz"

    log_info "Creating backup: $backup_file"

    mkdir -p "$BACKUP_DIR"

    if [ -d "$DEPLOY_DIR" ]; then
        tar -czf "$backup_file" -C "$(dirname "$DEPLOY_DIR")" "$(basename "$DEPLOY_DIR")" 2>/dev/null || {
            log_error "Backup failed"
            return 1
        }

        log_success "Backup created: $backup_file"
    else
        log_warn "No existing installation to backup"
    fi

    # Cleanup old backups
    cleanup_old_backups
}

cleanup_old_backups() {
    local backup_count=$(find "$BACKUP_DIR" -name "${APP_NAME}-*.tar.gz" | wc -l)

    if [ "$backup_count" -gt "$MAX_BACKUPS" ]; then
        log_info "Cleaning up old backups (keeping last $MAX_BACKUPS)"

        find "$BACKUP_DIR" -name "${APP_NAME}-*.tar.gz" -type f -printf '%T@ %p\n' | \
            sort -rn | \
            tail -n +$((MAX_BACKUPS + 1)) | \
            cut -d' ' -f2 | \
            xargs rm -f

        log_info "Old backups cleaned"
    fi
}

# ====================
# Deployment functions
# ====================
download_release() {
    local version=$1
    local download_url="https://releases.example.com/${APP_NAME}/${version}/${APP_NAME}-${version}.tar.gz"
    local checksum_url="${download_url}.sha256"

    TMPDIR=$(mktemp -d)
    local archive="${TMPDIR}/${APP_NAME}.tar.gz"
    local checksum_file="${TMPDIR}/checksum.sha256"

    log_info "Downloading $APP_NAME version $version"

    # Download with retry
    local max_attempts=3
    local attempt=1

    while [ $attempt -le $max_attempts ]; do
        if curl -fsSL -o "$archive" "$download_url"; then
            break
        fi

        log_warn "Download attempt $attempt failed, retrying..."
        ((attempt++))
        sleep 2
    done

    if [ $attempt -gt $max_attempts ]; then
        log_error "Download failed after $max_attempts attempts"
        return 1
    fi

    # Verify checksum
    log_info "Verifying checksum"
    curl -fsSL -o "$checksum_file" "$checksum_url"

    cd "$TMPDIR"
    if ! sha256sum -c "$checksum_file"; then
        log_error "Checksum verification failed"
        return 1
    fi

    log_success "Download and verification complete"
}

deploy_application() {
    log_info "Deploying application"

    # Stop service
    log_info "Stopping $APP_NAME service"
    systemctl stop "${APP_NAME}.service" 2>/dev/null || true

    # Extract to deploy directory
    mkdir -p "$DEPLOY_DIR"
    tar -xzf "${TMPDIR}/${APP_NAME}.tar.gz" -C "$DEPLOY_DIR" --strip-components=1

    # Set permissions
    chown -R appuser:appgroup "$DEPLOY_DIR"
    chmod 755 "$DEPLOY_DIR"

    # Run database migrations
    if [ -x "${DEPLOY_DIR}/migrate.sh" ]; then
        log_info "Running database migrations"
        su -c "${DEPLOY_DIR}/migrate.sh" - appuser || {
            log_error "Migration failed"
            return 1
        }
    fi

    # Start service
    log_info "Starting $APP_NAME service"
    systemctl start "${APP_NAME}.service"

    # Wait for service to be healthy
    wait_for_healthy
}

wait_for_healthy() {
    local max_wait=60
    local elapsed=0

    log_info "Waiting for application to become healthy"

    while [ $elapsed -lt $max_wait ]; do
        if curl -fs http://localhost:8080/health >/dev/null 2>&1; then
            log_success "Application is healthy"
            return 0
        fi

        sleep 2
        ((elapsed += 2))
    done

    log_error "Application did not become healthy within ${max_wait}s"
    return 1
}

smoke_test() {
    log_info "Running smoke tests"

    # Test 1: HTTP endpoint
    if ! curl -fs http://localhost:8080/ >/dev/null; then
        log_error "Smoke test failed: HTTP endpoint unreachable"
        return 1
    fi

    # Test 2: Database connection
    if ! su -c "${DEPLOY_DIR}/check-db.sh" - appuser; then
        log_error "Smoke test failed: Database connection"
        return 1
    fi

    log_success "Smoke tests passed"
}

# ====================
# Rollback function
# ====================
rollback() {
    log_warn "Performing rollback"

    local latest_backup=$(find "$BACKUP_DIR" -name "${APP_NAME}-*.tar.gz" -type f -printf '%T@ %p\n' | \
        sort -rn | head -n1 | cut -d' ' -f2)

    if [ -z "$latest_backup" ]; then
        log_error "No backup found for rollback"
        exit 1
    fi

    log_info "Restoring from backup: $latest_backup"

    systemctl stop "${APP_NAME}.service" 2>/dev/null || true
    rm -rf "$DEPLOY_DIR"
    mkdir -p "$DEPLOY_DIR"
    tar -xzf "$latest_backup" -C "$(dirname "$DEPLOY_DIR")"

    systemctl start "${APP_NAME}.service"

    log_success "Rollback complete"
}

# ====================
# Main execution
# ====================
main() {
    local action="${1:-deploy}"

    case "$action" in
        deploy)
            shift
            VERSION="${1:-}"

            require_root
            validate_version "$VERSION"
            check_dependencies

            log_info "Starting deployment of $APP_NAME version $VERSION"

            create_backup || exit 1
            download_release "$VERSION" || exit 1
            deploy_application || {
                log_error "Deployment failed, initiating rollback"
                rollback
                exit 1
            }
            smoke_test || {
                log_error "Smoke tests failed, initiating rollback"
                rollback
                exit 1
            }

            log_success "Deployment of $APP_NAME $VERSION complete"
            ;;

        rollback)
            require_root
            rollback
            ;;

        *)
            echo "Usage: $0 {deploy <version>|rollback}" >&2
            exit 1
            ;;
    esac
}

main "$@"
```

**Key Takeaway**: Production scripts require comprehensive error handling (`set -euo pipefail`, traps), validation (dependencies, version format, checksums), logging (timestamped, colored), backup/rollback capability, health checks, and idempotency. This template provides a foundation for reliable automated deployments. Always test in staging before production.

---

## Summary

These 10 advanced examples cover:

1. **Process Management** - Background jobs, signals, job control
2. **Signal Handling** - Traps for cleanup and graceful shutdown
3. **Parameter Expansion** - Advanced string manipulation without external commands
4. **Process Substitution** - Named pipes and advanced piping
5. **Advanced Looping** - C-style loops, parallel processing, iteration patterns
6. **Debugging** - Error handling, logging, fail-fast behavior
7. **Performance** - Optimization techniques, benchmarking, parallelization
8. **Security** - Input validation, secret management, secure patterns
9. **AWK** - Advanced text processing and data transformation
10. **Deployment** - Production-ready deployment script with rollback

Master these patterns to write production-grade shell scripts that are **reliable**, **performant**, **secure**, and **maintainable**.
