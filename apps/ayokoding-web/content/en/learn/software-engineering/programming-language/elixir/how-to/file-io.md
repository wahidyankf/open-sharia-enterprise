---
title: "File Io"
date: 2025-12-21T17:55:00+07:00
draft: false
description: "Work with files and system in Elixir using File module, Path utilities, streams for large files, and system commands."
weight: 1000015
tags: ["elixir", "file-io", "filesystem", "streams", "system", "how-to"]
---

**Need to work with files and system efficiently?** This guide covers Elixir's File and Path modules for robust file operations, streaming large files without memory issues, and executing system commands safely.

## Prerequisites

- Basic Elixir syntax
- Understanding of binaries and strings
- Completed [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner)
- Familiarity with streams (optional but helpful)

## Problem

File I/O in many languages leads to common pitfalls: memory exhaustion from loading large files, path manipulation errors across platforms, unsafe system command execution, and improper error handling. Elixir provides elegant solutions through functional APIs and streaming.

**Challenges:**

- Reading large files without consuming excessive memory
- Cross-platform path operations
- Handling file encoding (UTF-8 vs binary)
- Safe execution of system commands
- Proper resource cleanup and error handling
- Directory traversal and manipulation

## Solution Overview

Use **File module** for file operations, **Path module** for cross-platform path manipulation, **Stream** for large files, and **System module** for system commands. All operations return tagged tuples for explicit error handling.

**Key Modules:**

- **File**: Read, write, stat, copy, move, delete files
- **Path**: Join, split, expand, basename, dirname operations
- **IO**: Low-level file handles and device operations
- **File.Stream**: Lazy file streaming
- **System**: Shell command execution

## Detailed Implementation

### 1. Basic File Operations

#### Reading Files

```elixir
case File.read("config.txt") do
  {:ok, content} ->
    IO.puts("File content: #{content}")

  {:error, :enoent} ->
    IO.puts("File not found")

  {:error, reason} ->
    IO.puts("Error: #{reason}")
end

content = File.read!("config.txt")

{:ok, lines} = File.read("data.txt") |> elem(1) |> String.split("\n")

lines =
  case File.read("data.txt") do
    {:ok, content} -> String.split(content, "\n")
    {:error, _} -> []
  end
```

#### Writing Files

```elixir
File.write("output.txt", "Hello, World!")

case File.write("output.txt", "Hello, World!") do
  :ok -> IO.puts("Write successful")
  {:error, reason} -> IO.puts("Write failed: #{reason}")
end

File.write!("output.txt", "Hello, World!")

File.write("log.txt", "New entry\n", [:append])

binary_data = <<0xFF, 0xD8, 0xFF, 0xE0>>
File.write("image.jpg", binary_data, [:binary])
```

#### File Metadata

```elixir
File.exists?("config.txt")  # true/false

{:ok, stat} = File.stat("data.txt")
stat.size        # File size in bytes
stat.type        # :regular | :directory | :symlink
stat.access      # :read | :write | :read_write | :none
stat.mtime       # Modification time (Erlang timestamp)

case File.stat("data.txt") do
  {:ok, %{size: size, mtime: mtime}} ->
    IO.puts("Size: #{size} bytes, Modified: #{inspect(mtime)}")

  {:error, :enoent} ->
    IO.puts("File does not exist")
end
```

#### File Manipulation

```elixir
File.cp("source.txt", "destination.txt")

case File.cp("source.txt", "destination.txt") do
  :ok -> IO.puts("Copied")
  {:error, :eexist} -> IO.puts("Destination already exists")
end

File.rename("old_name.txt", "new_name.txt")

File.rm("temp.txt")

case File.rm("temp.txt") do
  :ok -> IO.puts("Deleted")
  {:error, :enoent} -> IO.puts("File not found")
end

File.rmdir("empty_dir")

File.rm_rf("directory_with_contents")
```

### 2. Streaming Large Files

Streaming processes files line-by-line without loading into memory.

#### Basic File Streaming

```elixir
File.stream!("large_log.txt")
|> Stream.map(&String.trim/1)
|> Stream.filter(fn line -> String.contains?(line, "ERROR") end)
|> Enum.each(&IO.puts/1)

error_count =
  File.stream!("large_log.txt")
  |> Enum.count(fn line -> String.contains?(line, "ERROR") end)
```

#### Transform and Write

```elixir
File.stream!("input.csv")
|> Stream.map(&String.upcase/1)
|> Stream.into(File.stream!("output.csv"))
|> Stream.run()

File.stream!("data.csv")
|> Stream.drop(1)  # Skip header
|> Stream.map(fn line ->
  line
  |> String.trim()
  |> String.split(",")
  |> process_row()
end)
|> Stream.filter(&valid_row?/1)
|> Stream.map(&format_output/1)
|> Stream.into(File.stream!("cleaned.csv"))
|> Stream.run()
```

#### Custom Line Separators

```elixir
File.stream!("data.txt", [:read], :line)  # Default: \n delimiter

File.stream!("binary_data.bin", [], 1024)
|> Enum.each(fn chunk ->
  process_chunk(chunk)
end)
```

### 3. Path Operations

Cross-platform path manipulation.

#### Path Construction

```elixir
Path.join(["home", "user", "documents", "file.txt"])

Path.join(File.cwd!(), "config.txt")

Path.expand("../config.txt")

Path.expand("~/Documents")
```

#### Path Inspection

```elixir
path = "/home/user/documents/report.pdf"

Path.basename(path)           # "report.pdf"
Path.basename(path, ".pdf")   # "report"
Path.dirname(path)            # "/home/user/documents"
Path.extname(path)            # ".pdf"
Path.rootname(path)           # "/home/user/documents/report"
Path.rootname(path, ".pdf")   # "/home/user/documents/report"

Path.split(path)

Path.absname("../../config.txt")
```

#### Path Validation

```elixir
Path.absname?("/home/user/file.txt")  # true
Path.absname?("relative/path.txt")     # false

Path.relative_to("/home/user/docs/file.txt", "/home/user")

Path.type("/absolute/path")   # :absolute
Path.type("relative/path")    # :relative
Path.type("~/home")           # :relative (~ requires expansion)
```

### 4. Directory Operations

```elixir
File.mkdir("new_directory")

File.mkdir_p("path/to/nested/directory")

{:ok, files} = File.ls(".")

files = File.ls!(".")

File.dir?("path/to/directory")  # true/false

{:ok, cwd} = File.cwd()
cwd = File.cwd!()

File.cd("other_directory")

def list_files_recursive(path) do
  cond do
    File.regular?(path) ->
      [path]

    File.dir?(path) ->
      path
      |> File.ls!()
      |> Enum.map(&Path.join(path, &1))
      |> Enum.flat_map(&list_files_recursive/1)

    true ->
      []
  end
end

list_files_recursive("my_project")
```

### 5. System Commands

Execute shell commands safely.

#### Basic Command Execution

```elixir
{output, exit_code} = System.cmd("ls", ["-la"])
IO.puts("Output: #{output}")
IO.puts("Exit code: #{exit_code}")

case System.cmd("git", ["status"]) do
  {output, 0} ->
    IO.puts("Git status:\n#{output}")

  {error, code} ->
    IO.puts("Command failed with code #{code}:\n#{error}")
end

System.cmd("ls", ["-la"], cd: "/home/user")

System.cmd("echo", ["$MY_VAR"], env: [{"MY_VAR", "hello"}])
```

#### Shell Execution

```elixir
{output, 0} = System.shell("ls -la | grep txt")


System.cmd("rm", ["-rf", user_provided_path])
```

### 6. File Handles and Low-Level I/O

For more control over file operations.

```elixir
{:ok, file} = File.open("data.txt", [:read])

line1 = IO.read(file, :line)
line2 = IO.read(file, :line)

File.close(file)

result =
  File.open("data.txt", [:read], fn file ->
    IO.read(file, :all)
  end)

File.open("output.txt", [:write], fn file ->
  IO.write(file, "Line 1\n")
  IO.write(file, "Line 2\n")
end)

File.open("file.txt", [:read, :write, :utf8], fn file ->
  # Read and write operations
end)
```

## How It Works

### File Module Error Handling

File operations return tagged tuples:

- `{:ok, result}` on success
- `{:error, reason}` on failure

Common error atoms:

- `:enoent` - File/directory not found
- `:eacces` - Permission denied
- `:eisdir` - Expected file, got directory
- `:enotdir` - Expected directory, got file
- `:eexist` - File already exists

### Stream Lazy Evaluation

`File.stream!/1` returns a stream that reads lazily:

```elixir
stream = File.stream!("huge.txt")  # No I/O yet

Enum.take(stream, 5)  # Reads only first 5 lines
```

### Path Module Cross-Platform

Path operations handle platform differences:

- Unix: `/` separator
- Windows: `\` separator
- Home expansion: `~` on Unix/Mac, `%USERPROFILE%` on Windows

Path module abstracts these differences.

## Variations

### 1. Atomic File Writes

```elixir
defmodule AtomicWrite do
  def write(path, content) do
    tmp_path = path <> ".tmp"

    with :ok <- File.write(tmp_path, content),
         :ok <- File.rename(tmp_path, path) do
      :ok
    else
      {:error, reason} ->
        File.rm(tmp_path)  # Cleanup on failure
        {:error, reason}
    end
  end
end
```

### 2. File Watching

```elixir
{:ok, pid} = FileSystem.start_link(dirs: ["/path/to/watch"])

FileSystem.subscribe(pid)

receive do
  {:file_event, _watcher_pid, {path, events}} ->
    IO.puts("File changed: #{path}, events: #{inspect(events)}")
end
```

### 3. Temporary Files

```elixir
{:ok, tmp_path} = Briefly.create()
File.write!(tmp_path, "temp data")

tmp_path = Path.join(System.tmp_dir!(), "myapp_#{:rand.uniform(1000000)}")
File.write!(tmp_path, "data")
File.rm!(tmp_path)  # Cleanup
```

## Pitfalls and Best Practices

### Common Mistakes

**1. Loading Large Files into Memory**

**Bad:**

```elixir
content = File.read!("huge_log.txt")
lines = String.split(content, "\n")
Enum.filter(lines, &interesting?/1)
```

**Good:**

```elixir
File.stream!("huge_log.txt")
|> Stream.filter(&interesting?/1)
|> Enum.to_list()
```

**2. Ignoring Error Handling**

**Bad:**

```elixir
File.write!("output.txt", data)  # Raises on permission error, disk full, etc.
```

**Good:**

```elixir
case File.write("output.txt", data) do
  :ok -> :ok
  {:error, :eacces} -> {:error, "Permission denied"}
  {:error, :enospc} -> {:error, "Disk full"}
  {:error, reason} -> {:error, "Write failed: #{reason}"}
end
```

**3. Platform-Specific Paths**

**Bad:**

```elixir
path = "home/user/file.txt"  # Works on Unix, fails on Windows
path = "home\\user\\file.txt"  # Works on Windows, fails on Unix
```

**Good:**

```elixir
path = Path.join(["home", "user", "file.txt"])  # Cross-platform
```

**4. Shell Injection**

**Bad:**

```elixir
System.shell("rm #{filename}")
```

**Good:**

```elixir
System.cmd("rm", [filename])
```

**5. Forgetting to Close File Handles**

**Bad:**

```elixir
{:ok, file} = File.open("data.txt")
content = IO.read(file, :all)
File.close(file)
```

**Good:**

```elixir
File.open("data.txt", fn file ->
  IO.read(file, :all)
end)
```

### Best Practices

**1. Use Streams for Large Files**

Always prefer streaming for files > 100MB or unknown size.

**2. Handle Errors Explicitly**

Use pattern matching on `{:ok, result}` and `{:error, reason}`.

**3. Use Path Module for Cross-Platform Code**

Never hardcode `/` or `\` separators.

**4. Validate User Input**

Sanitize file paths from user input to prevent directory traversal:

```elixir
defmodule SafePath do
  def validate(user_path, base_dir) do
    expanded = Path.expand(user_path, base_dir)

    if String.starts_with?(expanded, base_dir) do
      {:ok, expanded}
    else
      {:error, :invalid_path}
    end
  end
end
```

**5. Use Temporary Directories**

```elixir
tmp_dir = System.tmp_dir!()
tmp_file = Path.join(tmp_dir, "myapp_#{:erlang.unique_integer([:positive])}")
```

## Related Resources

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/elixir/tutorials/beginner) - Strings and binaries
- [Cookbook](/en/learn/software-engineering/programming-language/elixir/how-to/cookbook) - File processing recipes
- [Strings and Binaries Guide](/en/learn/software-engineering/programming-language/elixir/how-to/strings-binaries) - Text processing
- [CLI Applications Guide](/en/learn/software-engineering/programming-language/elixir/how-to/cli-applications) - Building file-based tools
