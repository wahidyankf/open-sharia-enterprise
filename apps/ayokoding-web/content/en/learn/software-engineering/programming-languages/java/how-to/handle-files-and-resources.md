---
title: "Handle Files and Resources"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000006
description: "Master Java file I/O with Path API, try-with-resources, and modern file operations"
tags: ["java", "files", "io", "nio", "resources"]
categories: ["learn"]
---

## Problem

File handling in Java has evolved significantly. Old code uses File class with manual resource management, while modern Java prefers Path API with try-with-resources. Poor file handling leads to resource leaks, platform-specific bugs, and difficult-to-read code.

This guide shows modern file handling patterns in Java.

## Try-with-Resources Pattern

### Automatic Resource Management

Try-with-resources automatically closes resources, eliminating finally blocks and preventing leaks.

```java
// ❌ Old style - manual cleanup
public void processFile(String path) throws IOException {
  BufferedReader reader = null;
  try {
    reader = new BufferedReader(new FileReader(path));
    String line;
    while ((line = reader.readLine()) != null) {
      processLine(line);
    }
  } finally {
    if (reader != null) {
      try {
        reader.close();
      } catch (IOException e) {
        // What to do with this exception?
      }
    }
  }
}

// ✅ Modern style - automatic cleanup
public void processFile(String path) throws IOException {
  try (BufferedReader reader = new BufferedReader(new FileReader(path))) {
    String line;
    while ((line = reader.readLine()) != null) {
      processLine(line);
    }
  } // reader.close() called automatically
}
```

**Why it matters**: Try-with-resources guarantees cleanup even when exceptions occur. No more forgotten close() calls or complex finally blocks. Resources close in reverse order of creation, handling dependencies correctly.

### Multiple Resources

```java
// ✅ Multiple resources - both closed automatically
public void copyFile(String source, String dest) throws IOException {
  try (
    InputStream in = new FileInputStream(source);
    OutputStream out = new FileOutputStream(dest)
  ) {
    byte[] buffer = new byte[8192];
    int bytesRead;
    while ((bytesRead = in.read(buffer)) != -1) {
      out.write(buffer, 0, bytesRead);
    }
  } // Both streams closed, even if exception occurs
}

// ✅ Nested try-with-resources for independent resources
public void processMultipleFiles(String config, String data) throws IOException {
  try (BufferedReader configReader = Files.newBufferedReader(Paths.get(config))) {
    String configLine = configReader.readLine();

    try (BufferedReader dataReader = Files.newBufferedReader(Paths.get(data))) {
      String dataLine;
      while ((dataLine = dataReader.readLine()) != null) {
        processDataLine(dataLine, configLine);
      }
    }
  }
}
```

## Path API vs File

Use Path API instead of File for modern file operations.

```java
// ❌ Old File API
File file = new File("data/users.txt");
File parent = file.getParentFile();
String name = file.getName();

if (file.exists()) {
  file.delete();
}

// ✅ Modern Path API
Path path = Paths.get("data", "users.txt");
Path parent = path.getParent();
String name = path.getFileName().toString();

if (Files.exists(path)) {
  Files.delete(path);
}

// ✅ Path is immutable and composable
Path base = Paths.get("data");
Path usersFile = base.resolve("users.txt");
Path configFile = base.resolve("config.json");

// ✅ Platform-independent path handling
Path path = Paths.get("data", "users", "alice.json");
// Windows: data\users\alice.json
// Unix: data/users/alice.json
```

**Benefits of Path API:**

- Immutable and thread-safe
- Platform-independent
- Better exception handling
- More operations available through Files utility class
- Supports file systems beyond disk (zip, memory, etc.)

## Reading Files

### Reading All Lines

```java
// ✅ Read all lines at once (small files)
public List<String> readUsernames(String path) throws IOException {
  return Files.readAllLines(Paths.get(path));
}

// ✅ With specific encoding
public List<String> readUsernames(String path) throws IOException {
  return Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8);
}

// ✅ Process large files line by line (memory efficient)
public void processLargeFile(String path) throws IOException {
  try (Stream<String> lines = Files.lines(Paths.get(path))) {
    lines
      .filter(line -> !line.isEmpty())
      .map(String::trim)
      .forEach(this::processLine);
  }
}
```

### Reading Text Content

```java
// ✅ Read entire file as string (small files)
public String readConfig(String path) throws IOException {
  return Files.readString(Paths.get(path));
}

// ✅ With specific encoding
public String readConfig(String path) throws IOException {
  return Files.readString(Paths.get(path), StandardCharsets.UTF_8);
}

// ✅ Using BufferedReader for manual control
public String readFile(String path) throws IOException {
  StringBuilder content = new StringBuilder();

  try (BufferedReader reader = Files.newBufferedReader(Paths.get(path))) {
    String line;
    while ((line = reader.readLine()) != null) {
      content.append(line).append("\n");
    }
  }

  return content.toString();
}
```

### Reading Binary Files

```java
// ✅ Read entire binary file
public byte[] readImageFile(String path) throws IOException {
  return Files.readAllBytes(Paths.get(path));
}

// ✅ Read binary file with InputStream (large files)
public void processBinaryFile(String path) throws IOException {
  try (InputStream in = Files.newInputStream(Paths.get(path))) {
    byte[] buffer = new byte[8192];
    int bytesRead;
    while ((bytesRead = in.read(buffer)) != -1) {
      processBinaryData(buffer, bytesRead);
    }
  }
}

// ✅ Read with buffering for performance
public void readLargeBinaryFile(String path) throws IOException {
  try (InputStream in = new BufferedInputStream(
         Files.newInputStream(Paths.get(path)))) {
    // BufferedInputStream adds internal buffering
    int data;
    while ((data = in.read()) != -1) {
      processByte((byte) data);
    }
  }
}
```

## Writing Files

### Writing Text Content

```java
// ✅ Write string to file (overwrites)
public void saveConfig(String path, String content) throws IOException {
  Files.writeString(Paths.get(path), content);
}

// ✅ Write with specific encoding
public void saveConfig(String path, String content) throws IOException {
  Files.writeString(Paths.get(path), content, StandardCharsets.UTF_8);
}

// ✅ Append to existing file
public void appendToLog(String path, String message) throws IOException {
  Files.writeString(
    Paths.get(path),
    message + "\n",
    StandardCharsets.UTF_8,
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND
  );
}

// ✅ Write list of lines
public void saveUsernames(String path, List<String> usernames) throws IOException {
  Files.write(Paths.get(path), usernames);
}

// ✅ Using BufferedWriter for manual control
public void writeReport(String path, List<ReportLine> lines) throws IOException {
  try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(path))) {
    for (ReportLine line : lines) {
      writer.write(line.format());
      writer.newLine();
    }
  }
}
```

### Writing Binary Files

```java
// ✅ Write byte array
public void saveImage(String path, byte[] imageData) throws IOException {
  Files.write(Paths.get(path), imageData);
}

// ✅ Write with OutputStream
public void writeBinaryData(String path, byte[] data) throws IOException {
  try (OutputStream out = Files.newOutputStream(Paths.get(path))) {
    out.write(data);
  }
}

// ✅ Write with buffering for performance
public void writeLargeBinaryFile(String path, byte[] data) throws IOException {
  try (OutputStream out = new BufferedOutputStream(
         Files.newOutputStream(Paths.get(path)))) {
    out.write(data);
  }
}
```

### Atomic Writes

```java
// ✅ Atomic write (all-or-nothing)
public void saveConfigAtomically(String path, String content) throws IOException {
  Path targetPath = Paths.get(path);
  Path tempPath = Paths.get(path + ".tmp");

  // Write to temp file
  Files.writeString(tempPath, content);

  // Atomic move (overwrites target)
  Files.move(tempPath, targetPath, StandardCopyOption.REPLACE_EXISTING,
    StandardCopyOption.ATOMIC_MOVE);
}
```

**Why it matters**: Atomic writes prevent partial file corruption. If process crashes during write, either old content remains or new content is fully written. Never leaves half-written data.

## Working with Directories

### Creating Directories

```java
// ✅ Create single directory
public void createDirectory(String path) throws IOException {
  Files.createDirectory(Paths.get(path));
  // Throws exception if parent doesn't exist
}

// ✅ Create directory with parents
public void createDirectoryTree(String path) throws IOException {
  Files.createDirectories(Paths.get(path));
  // Creates all missing parent directories
}

// ✅ Create temporary directory
public Path createTempDirectory() throws IOException {
  return Files.createTempDirectory("myapp-");
  // Creates with unique name like "myapp-1234567890"
}
```

### Listing Directory Contents

```java
// ✅ List immediate children
public List<Path> listFiles(String directory) throws IOException {
  try (Stream<Path> paths = Files.list(Paths.get(directory))) {
    return paths.collect(Collectors.toList());
  }
}

// ✅ Filter by file type
public List<Path> listTextFiles(String directory) throws IOException {
  try (Stream<Path> paths = Files.list(Paths.get(directory))) {
    return paths
      .filter(path -> path.toString().endsWith(".txt"))
      .collect(Collectors.toList());
  }
}

// ✅ Walk entire directory tree
public List<Path> findAllJsonFiles(String root) throws IOException {
  try (Stream<Path> paths = Files.walk(Paths.get(root))) {
    return paths
      .filter(Files::isRegularFile)
      .filter(path -> path.toString().endsWith(".json"))
      .collect(Collectors.toList());
  }
}

// ✅ Walk with depth limit
public List<Path> findFilesInSubdirectories(String root, int maxDepth)
    throws IOException {
  try (Stream<Path> paths = Files.walk(Paths.get(root), maxDepth)) {
    return paths
      .filter(Files::isRegularFile)
      .collect(Collectors.toList());
  }
}
```

### Directory Operations

```java
// ✅ Copy directory recursively
public void copyDirectory(String source, String dest) throws IOException {
  Path sourcePath = Paths.get(source);
  Path destPath = Paths.get(dest);

  try (Stream<Path> paths = Files.walk(sourcePath)) {
    paths.forEach(path -> {
      try {
        Path targetPath = destPath.resolve(sourcePath.relativize(path));
        if (Files.isDirectory(path)) {
          Files.createDirectories(targetPath);
        } else {
          Files.copy(path, targetPath, StandardCopyOption.REPLACE_EXISTING);
        }
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    });
  }
}

// ✅ Delete directory recursively
public void deleteDirectory(String directory) throws IOException {
  Path path = Paths.get(directory);

  if (Files.exists(path)) {
    try (Stream<Path> paths = Files.walk(path)) {
      paths
        .sorted(Comparator.reverseOrder()) // Delete files before directories
        .forEach(p -> {
          try {
            Files.delete(p);
          } catch (IOException e) {
            throw new UncheckedIOException(e);
          }
        });
    }
  }
}
```

## File Operations

### Copy and Move

```java
// ✅ Copy file
public void copyFile(String source, String dest) throws IOException {
  Files.copy(Paths.get(source), Paths.get(dest));
}

// ✅ Copy with options
public void copyFileReplace(String source, String dest) throws IOException {
  Files.copy(
    Paths.get(source),
    Paths.get(dest),
    StandardCopyOption.REPLACE_EXISTING,
    StandardCopyOption.COPY_ATTRIBUTES
  );
}

// ✅ Move file
public void moveFile(String source, String dest) throws IOException {
  Files.move(Paths.get(source), Paths.get(dest));
}

// ✅ Atomic move
public void moveFileAtomically(String source, String dest) throws IOException {
  Files.move(
    Paths.get(source),
    Paths.get(dest),
    StandardCopyOption.ATOMIC_MOVE
  );
}
```

### Delete Files

```java
// ✅ Delete file (throws if doesn't exist)
public void deleteFile(String path) throws IOException {
  Files.delete(Paths.get(path));
}

// ✅ Delete if exists (no exception if missing)
public boolean deleteFileIfExists(String path) throws IOException {
  return Files.deleteIfExists(Paths.get(path));
}
```

### File Metadata

```java
// ✅ Check file existence
public boolean fileExists(String path) {
  return Files.exists(Paths.get(path));
}

// ✅ Check if regular file or directory
public boolean isFile(String path) {
  return Files.isRegularFile(Paths.get(path));
}

public boolean isDirectory(String path) {
  return Files.isDirectory(Paths.get(path));
}

// ✅ File size
public long getFileSize(String path) throws IOException {
  return Files.size(Paths.get(path));
}

// ✅ Last modified time
public FileTime getLastModifiedTime(String path) throws IOException {
  return Files.getLastModifiedTime(Paths.get(path));
}

// ✅ Check if readable/writable
public boolean canRead(String path) {
  return Files.isReadable(Paths.get(path));
}

public boolean canWrite(String path) {
  return Files.isWritable(Paths.get(path));
}
```

## Reading CSV and JSON

### CSV with BufferedReader

```java
// ✅ Read CSV file
public List<User> readUsersFromCsv(String path) throws IOException {
  List<User> users = new ArrayList<>();

  try (BufferedReader reader = Files.newBufferedReader(Paths.get(path))) {
    String line = reader.readLine(); // Skip header
    while ((line = reader.readLine()) != null) {
      String[] parts = line.split(",");
      if (parts.length >= 3) {
        users.add(new User(
          parts[0].trim(),
          Integer.parseInt(parts[1].trim()),
          parts[2].trim()
        ));
      }
    }
  }

  return users;
}

// ✅ Write CSV file
public void writeUsersToCsv(String path, List<User> users) throws IOException {
  try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(path))) {
    writer.write("Name,Age,Email");
    writer.newLine();

    for (User user : users) {
      writer.write(String.format("%s,%d,%s",
        user.getName(), user.getAge(), user.getEmail()));
      writer.newLine();
    }
  }
}
```

### JSON with Jackson

```java
import com.fasterxml.jackson.databind.ObjectMapper;

// ✅ Read JSON file
public User readUserFromJson(String path) throws IOException {
  ObjectMapper mapper = new ObjectMapper();
  return mapper.readValue(Paths.get(path).toFile(), User.class);
}

// ✅ Read JSON array
public List<User> readUsersFromJson(String path) throws IOException {
  ObjectMapper mapper = new ObjectMapper();
  return mapper.readValue(
    Paths.get(path).toFile(),
    mapper.getTypeFactory().constructCollectionType(List.class, User.class)
  );
}

// ✅ Write JSON file
public void writeUserToJson(String path, User user) throws IOException {
  ObjectMapper mapper = new ObjectMapper();
  mapper.writerWithDefaultPrettyPrinter()
    .writeValue(Paths.get(path).toFile(), user);
}
```

## Resource Management Best Practices

### Custom AutoCloseable

```java
// ✅ Implement AutoCloseable for custom resources
public class DatabaseConnection implements AutoCloseable {
  private final Connection connection;
  private boolean closed = false;

  public DatabaseConnection(String url) throws SQLException {
    this.connection = DriverManager.getConnection(url);
  }

  public void executeQuery(String sql) throws SQLException {
    if (closed) {
      throw new IllegalStateException("Connection closed");
    }
    // Execute query
  }

  @Override
  public void close() throws SQLException {
    if (!closed) {
      connection.close();
      closed = true;
    }
  }
}

// Usage with try-with-resources
try (DatabaseConnection db = new DatabaseConnection(url)) {
  db.executeQuery("SELECT * FROM users");
} // Automatically closed
```

### Suppressed Exceptions

```java
public class ResourceExample implements AutoCloseable {
  @Override
  public void close() throws IOException {
    throw new IOException("Close failed");
  }

  public void doWork() throws IOException {
    throw new IOException("Work failed");
  }
}

// ✅ Try-with-resources preserves both exceptions
try (ResourceExample resource = new ResourceExample()) {
  resource.doWork(); // Throws IOException
} catch (IOException e) {
  System.out.println("Main exception: " + e.getMessage());
  // "Work failed"

  Throwable[] suppressed = e.getSuppressed();
  if (suppressed.length > 0) {
    System.out.println("Suppressed: " + suppressed[0].getMessage());
    // "Close failed"
  }
}
```

## Summary

Modern Java file handling centers on the Path API and try-with-resources pattern. Path provides immutable, platform-independent file and directory references that work consistently across Windows, Linux, and macOS. Use Paths.get() to create paths and Files utility class for operations - reading, writing, copying, moving, and deleting.

Try-with-resources eliminates manual resource management. Declare resources in the try parentheses and Java automatically closes them when the block exits, even when exceptions occur. Multiple resources close in reverse order of creation. Any class implementing AutoCloseable works with try-with-resources.

Read small files entirely with Files.readString() or Files.readAllLines(). Process large files efficiently with Files.lines() which returns a stream, preventing entire file loading into memory. Always specify charset encoding explicitly - StandardCharsets.UTF_8 for text files.

Write text with Files.writeString() or Files.write(). For binary data use Files.readAllBytes() and Files.write(). Buffered readers and writers provide manual control when needed. Atomic writes through temporary files prevent corruption - write to temp file then move atomically to target.

Directory operations use Files.walk() to traverse trees recursively. Filter streams to find specific files. Walking returns a stream that must be closed - use try-with-resources. Delete directories by walking in reverse order, removing files before parent directories.

CSV parsing splits lines on commas, handling headers and data rows separately. JSON requires a library like Jackson. ObjectMapper reads and writes JSON to/from Java objects. Always handle encoding explicitly and close resources properly.

Implement AutoCloseable for custom resources that need cleanup. Close method runs automatically with try-with-resources. Mark resource as closed and throw IllegalStateException when operations attempt to use closed resources. This pattern works for database connections, network sockets, or any resource needing cleanup.

## Related Content

- [Java Best Practices and Design Principles](/en/learn/software-engineering/programming-languages/java/explanation/best-practices)
- [How to Implement Proper Exception Handling](/en/learn/software-engineering/programming-languages/java/how-to/exception-handling)
- [How to Use Java Collections Effectively](/en/learn/software-engineering/programming-languages/java/how-to/use-collections-effectively)
