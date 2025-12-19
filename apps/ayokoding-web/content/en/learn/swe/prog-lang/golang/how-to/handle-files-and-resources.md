---
title: "How to Handle Files and Resources"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 609
description: "Master Go file I/O with defer, os package, proper resource cleanup, and modern file operations"
tags: ["golang", "files", "io", "resources", "defer"]
---

## Problem

File handling in Go requires explicit resource management without automatic garbage collection for open files. Forgotten file.Close() calls leak file descriptors, eventually causing "too many open files" errors. Poor error handling leads to partial writes and corrupted data.

This guide shows effective file handling patterns in Go.

## Defer for Resource Cleanup

### Automatic Cleanup Pattern

```go
// ✅ defer ensures cleanup even with errors
func ReadConfig(filename string) ([]byte, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, fmt.Errorf("opening file: %w", err)
    }
    defer file.Close() // Always closes, even if panic or error

    data, err := io.ReadAll(file)
    if err != nil {
        return nil, fmt.Errorf("reading file: %w", err)
    }

    return data, nil
}

// ❌ Manual cleanup - error-prone
func ReadConfigBad(filename string) ([]byte, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, err
    }

    data, err := io.ReadAll(file)
    if err != nil {
        file.Close() // Easy to forget!
        return nil, err
    }

    file.Close()
    return data, nil
}
```

**Why defer matters**: defer executes when function returns, regardless of return path. Handles panic recovery. Write cleanup code immediately after resource acquisition, reducing chance of forgetting.

### Multiple defer Statements

```go
// ✅ Multiple resources - deferred in reverse order
func CopyFile(src, dst string) error {
    source, err := os.Open(src)
    if err != nil {
        return fmt.Errorf("open source: %w", err)
    }
    defer source.Close() // Closes second

    destination, err := os.Create(dst)
    if err != nil {
        return fmt.Errorf("create destination: %w", err)
    }
    defer destination.Close() // Closes first (LIFO)

    _, err = io.Copy(destination, source)
    if err != nil {
        return fmt.Errorf("copying: %w", err)
    }

    return nil
}
```

**Defer order**: Last defer executes first (stack order). Ensures dependent resources close in correct order.

## Reading Files

### Reading Entire File

```go
// ✅ Read entire file into memory (small files)
func ReadSmallFile(filename string) (string, error) {
    data, err := os.ReadFile(filename)
    if err != nil {
        return "", fmt.Errorf("reading file: %w", err)
    }
    return string(data), nil
}

// ✅ Read file line by line (large files)
func ProcessLargeFile(filename string) error {
    file, err := os.Open(filename)
    if err != nil {
        return fmt.Errorf("opening file: %w", err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        processLine(line) // Process one line at a time
    }

    if err := scanner.Err(); err != nil {
        return fmt.Errorf("scanning file: %w", err)
    }

    return nil
}

func processLine(line string) {
    // Process line...
}
```

### Buffered Reading

```go
// ✅ Buffered reading for performance
func ReadWithBuffer(filename string) error {
    file, err := os.Open(filename)
    if err != nil {
        return err
    }
    defer file.Close()

    reader := bufio.NewReader(file)
    buffer := make([]byte, 4096)

    for {
        n, err := reader.Read(buffer)
        if err == io.EOF {
            break
        }
        if err != nil {
            return fmt.Errorf("reading: %w", err)
        }

        processChunk(buffer[:n])
    }

    return nil
}

func processChunk(data []byte) {
    // Process chunk...
}
```

## Writing Files

### Writing Text

```go
// ✅ Write string to file (overwrites)
func WriteFile(filename, content string) error {
    err := os.WriteFile(filename, []byte(content), 0644)
    if err != nil {
        return fmt.Errorf("writing file: %w", err)
    }
    return nil
}

// ✅ Append to file
func AppendToFile(filename, content string) error {
    file, err := os.OpenFile(filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
    if err != nil {
        return fmt.Errorf("opening file: %w", err)
    }
    defer file.Close()

    if _, err := file.WriteString(content); err != nil {
        return fmt.Errorf("writing: %w", err)
    }

    return nil
}

// ✅ Buffered writing for performance
func WriteWithBuffer(filename string, lines []string) error {
    file, err := os.Create(filename)
    if err != nil {
        return err
    }
    defer file.Close()

    writer := bufio.NewWriter(file)
    defer writer.Flush() // Ensure buffer is written

    for _, line := range lines {
        if _, err := writer.WriteString(line + "\n"); err != nil {
            return fmt.Errorf("writing line: %w", err)
        }
    }

    return nil
}
```

### Atomic Writes

```go
// ✅ Atomic write - prevents partial corruption
func WriteFileAtomically(filename, content string) error {
    tmpFile := filename + ".tmp"

    // Write to temporary file
    if err := os.WriteFile(tmpFile, []byte(content), 0644); err != nil {
        return fmt.Errorf("writing temp file: %w", err)
    }

    // Atomic rename
    if err := os.Rename(tmpFile, filename); err != nil {
        os.Remove(tmpFile) // Cleanup on failure
        return fmt.Errorf("renaming file: %w", err)
    }

    return nil
}
```

**Why atomic writes**: Prevents leaving corrupted partial files. Either old version remains or new version fully written. Critical for config files and data stores.

## Working with Directories

### Directory Operations

```go
// ✅ Create directory
func CreateDir(path string) error {
    if err := os.Mkdir(path, 0755); err != nil {
        return fmt.Errorf("creating directory: %w", err)
    }
    return nil
}

// ✅ Create directory with parents
func CreateDirAll(path string) error {
    if err := os.MkdirAll(path, 0755); err != nil {
        return fmt.Errorf("creating directories: %w", err)
    }
    return nil
}

// ✅ List directory contents
func ListFiles(dir string) ([]string, error) {
    entries, err := os.ReadDir(dir)
    if err != nil {
        return nil, fmt.Errorf("reading directory: %w", err)
    }

    var files []string
    for _, entry := range entries {
        if !entry.IsDir() {
            files = append(files, entry.Name())
        }
    }

    return files, nil
}

// ✅ Walk directory tree
func WalkDirectory(root string) error {
    return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        if !info.IsDir() {
            fmt.Printf("File: %s, Size: %d bytes\n", path, info.Size())
        }

        return nil
    })
}
```

### Finding Files

```go
// ✅ Find files by extension
func FindFilesByExt(root, ext string) ([]string, error) {
    var files []string

    err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        if !info.IsDir() && filepath.Ext(path) == ext {
            files = append(files, path)
        }

        return nil
    })

    return files, err
}

// ✅ Glob pattern matching
func FindByPattern(pattern string) ([]string, error) {
    matches, err := filepath.Glob(pattern)
    if err != nil {
        return nil, fmt.Errorf("glob pattern: %w", err)
    }
    return matches, nil
}

// Usage:
// matches, _ := FindByPattern("data/*.json")
// matches, _ := FindByPattern("logs/2025-*-*.log")
```

## File Operations

### Copy, Move, Delete

```go
// ✅ Copy file
func CopyFile(src, dst string) error {
    source, err := os.Open(src)
    if err != nil {
        return fmt.Errorf("open source: %w", err)
    }
    defer source.Close()

    destination, err := os.Create(dst)
    if err != nil {
        return fmt.Errorf("create destination: %w", err)
    }
    defer destination.Close()

    if _, err := io.Copy(destination, source); err != nil {
        return fmt.Errorf("copying: %w", err)
    }

    // Sync to ensure data is written
    return destination.Sync()
}

// ✅ Move file (rename)
func MoveFile(src, dst string) error {
    if err := os.Rename(src, dst); err != nil {
        return fmt.Errorf("renaming: %w", err)
    }
    return nil
}

// ✅ Delete file
func DeleteFile(filename string) error {
    if err := os.Remove(filename); err != nil {
        return fmt.Errorf("removing file: %w", err)
    }
    return nil
}

// ✅ Delete directory recursively
func DeleteDirectory(dir string) error {
    if err := os.RemoveAll(dir); err != nil {
        return fmt.Errorf("removing directory: %w", err)
    }
    return nil
}
```

### File Metadata

```go
// ✅ Check file existence
func FileExists(filename string) bool {
    _, err := os.Stat(filename)
    return !os.IsNotExist(err)
}

// ✅ Get file size
func GetFileSize(filename string) (int64, error) {
    info, err := os.Stat(filename)
    if err != nil {
        return 0, fmt.Errorf("stat file: %w", err)
    }
    return info.Size(), nil
}

// ✅ Check if path is directory
func IsDirectory(path string) (bool, error) {
    info, err := os.Stat(path)
    if err != nil {
        return false, err
    }
    return info.IsDir(), nil
}

// ✅ Get modification time
func GetModTime(filename string) (time.Time, error) {
    info, err := os.Stat(filename)
    if err != nil {
        return time.Time{}, err
    }
    return info.ModTime(), nil
}
```

## CSV and JSON

### Reading CSV

```go
import "encoding/csv"

// ✅ Read CSV file
func ReadCSV(filename string) ([][]string, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, fmt.Errorf("opening CSV: %w", err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        return nil, fmt.Errorf("reading CSV: %w", err)
    }

    return records, nil
}

// ✅ Parse CSV with header
type User struct {
    Name  string
    Email string
    Age   int
}

func ReadUsersCSV(filename string) ([]User, error) {
    file, err := os.Open(filename)
    if err != nil {
        return nil, err
    }
    defer file.Close()

    reader := csv.NewReader(file)

    // Skip header
    if _, err := reader.Read(); err != nil {
        return nil, fmt.Errorf("reading header: %w", err)
    }

    var users []User
    for {
        record, err := reader.Read()
        if err == io.EOF {
            break
        }
        if err != nil {
            return nil, fmt.Errorf("reading record: %w", err)
        }

        age, _ := strconv.Atoi(record[2])
        users = append(users, User{
            Name:  record[0],
            Email: record[1],
            Age:   age,
        })
    }

    return users, nil
}

// ✅ Write CSV
func WriteCSV(filename string, data [][]string) error {
    file, err := os.Create(filename)
    if err != nil {
        return err
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    for _, record := range data {
        if err := writer.Write(record); err != nil {
            return fmt.Errorf("writing record: %w", err)
        }
    }

    return nil
}
```

### JSON Operations

```go
import "encoding/json"

type Config struct {
    DatabaseURL string `json:"database_url"`
    Port        int    `json:"port"`
    Debug       bool   `json:"debug"`
}

// ✅ Read JSON file
func ReadJSON(filename string, v interface{}) error {
    data, err := os.ReadFile(filename)
    if err != nil {
        return fmt.Errorf("reading file: %w", err)
    }

    if err := json.Unmarshal(data, v); err != nil {
        return fmt.Errorf("unmarshaling JSON: %w", err)
    }

    return nil
}

// ✅ Write JSON file
func WriteJSON(filename string, v interface{}) error {
    data, err := json.MarshalIndent(v, "", "  ")
    if err != nil {
        return fmt.Errorf("marshaling JSON: %w", err)
    }

    if err := os.WriteFile(filename, data, 0644); err != nil {
        return fmt.Errorf("writing file: %w", err)
    }

    return nil
}

// Usage
func ExampleJSON() error {
    // Read config
    var config Config
    if err := ReadJSON("config.json", &config); err != nil {
        return err
    }

    // Modify and write back
    config.Debug = true
    if err := WriteJSON("config.json", config); err != nil {
        return err
    }

    return nil
}
```

## Embedded Files (Go 1.16+)

### Embed Files in Binary

```go
import "embed"

// ✅ Embed single file
//go:embed templates/index.html
var indexHTML string

// ✅ Embed multiple files
//go:embed templates/*.html
var templates embed.FS

// ✅ Embed directory
//go:embed static
var staticFiles embed.FS

func LoadTemplate(name string) (string, error) {
    data, err := templates.ReadFile("templates/" + name)
    if err != nil {
        return "", err
    }
    return string(data), nil
}

func ServeStatic(w http.ResponseWriter, r *http.Request) {
    http.FileServer(http.FS(staticFiles)).ServeHTTP(w, r)
}
```

**Why embed**: Bundle assets in binary, no external file dependencies, simplifies deployment.

## Summary

Effective file handling in Go centers on defer for guaranteed cleanup. defer file.Close() immediately after os.Open() ensures files close regardless of return path. Multiple defer statements execute in reverse order, handling dependent resources correctly.

Reading files uses os.ReadFile for small files loaded entirely into memory, bufio.Scanner for line-by-line processing of large files, and bufio.Reader for chunk-based processing. Choose based on file size and processing needs.

Writing files with os.WriteFile overwrites atomically for small content. Buffered writing with bufio.Writer improves performance for large writes. Remember writer.Flush() to ensure buffered data writes to disk. Atomic writes through temp files and renames prevent corruption.

Directory operations use os.Mkdir for single directories, os.MkdirAll for creating parent directories, os.ReadDir for listing contents, and filepath.Walk for recursive traversal. Glob patterns with filepath.Glob match files by pattern.

File operations include io.Copy for efficient file copying, os.Rename for moving files, os.Remove for deletion, and os.RemoveAll for recursive directory deletion. os.Stat provides file metadata - size, modification time, permissions.

CSV handling through encoding/csv provides Reader and Writer. Read headers separately, parse records into structs. JSON uses encoding/json with Marshal/Unmarshal. Indent JSON output with MarshalIndent for human-readable files.

Embedded files with embed.FS compile static files into binaries. Templates, configuration files, and static assets bundle with executables, eliminating external dependencies and simplifying deployment.

Error handling uses fmt.Errorf with %w to wrap errors, preserving error chains. Check for specific errors like io.EOF to distinguish end-of-file from real errors. Return wrapped errors with context about which operation failed.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/golang/how-to/write-effective-tests)
