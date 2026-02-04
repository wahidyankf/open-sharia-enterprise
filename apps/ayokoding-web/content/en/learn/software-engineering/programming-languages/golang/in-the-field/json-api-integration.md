---
title: "JSON API Integration"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "JSON encoding/decoding with encoding/json, struct tags, API client patterns, and performance optimization with jsoniter"
weight: 10000044
tags: ["golang", "json", "api", "rest", "encoding", "jsoniter", "production"]
---

## Why JSON & API Integration Matters

JSON is the universal data format for REST APIs and microservices. Go's `encoding/json` package provides zero-dependency JSON handling used across the ecosystem. Understanding struct tags, custom marshaling, and performance characteristics prevents common pitfalls like silent field omission and unnecessary heap allocations.

**Core benefits**:

- **Zero dependencies**: Production-ready JSON in standard library
- **Type-safe marshaling**: Compile-time checks prevent runtime errors
- **Struct tags control encoding**: Precise control over JSON representation
- **Reflection-based**: Works with any struct without code generation

**Problem**: Developers often encounter silent field drops (unexported fields ignored), poor performance (excessive allocations), or incorrect JSON structure (wrong tag configuration) when working with APIs.

**Solution**: Master `encoding/json` fundamentals first, understand limitations (performance, streaming), then adopt `jsoniter` for high-throughput services where performance matters.

## Standard Library First: encoding/json

Go's `encoding/json` package provides `Marshal`/`Unmarshal` for byte slices and `Encoder`/`Decoder` for streaming. Struct tags control field mapping and behavior.

**Basic marshaling pattern**:

```go
package main

import (
    "encoding/json"
    // => Standard library for JSON encoding/decoding
    // => Includes Marshal, Unmarshal, Encoder, Decoder
    "fmt"
    "log"
)

type User struct {
    ID   int    `json:"id"`
    // => json:"id" sets JSON field name
    // => Exported field (capitalized) required for marshaling
    // => JSON output: "id" (lowercase)

    Name string `json:"name"`
    // => Maps Name to "name" in JSON

    Email string `json:"email,omitempty"`
    // => omitempty: omit field if zero value
    // => Zero value for string is ""
    // => If Email == "", field not in JSON

    Password string `json:"-"`
    // => json:"-" excludes field from JSON
    // => Never marshaled or unmarshaled
    // => Useful for sensitive fields

    age int
    // => Unexported field (lowercase)
    // => NEVER marshaled (ignored by encoding/json)
    // => Common mistake: forgetting to capitalize
}

func main() {
    user := User{
        ID:       1,
        Name:     "Alice",
        Email:    "alice@example.com",
        Password: "secret123",
        age:      30,
    }
    // => user populated with data

    data, err := json.Marshal(user)
    // => json.Marshal converts user to []byte
    // => data is JSON bytes
    // => err is non-nil if marshaling fails (rare for simple structs)
    // => Returns: {"id":1,"name":"Alice","email":"alice@example.com"}
    // => Password omitted (json:"-")
    // => age omitted (unexported)

    if err != nil {
        log.Fatalf("Marshal failed: %v", err)
    }

    fmt.Printf("JSON: %s\n", data)
    // => Output: JSON: {"id":1,"name":"Alice","email":"alice@example.com"}

    fmt.Printf("Length: %d bytes\n", len(data))
    // => Output: Length: 52 bytes
}
```

**Unmarshaling pattern**:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID    int    `json:"id"`
    Name  string `json:"name"`
    Email string `json:"email,omitempty"`
}

func main() {
    jsonData := []byte(`{"id":1,"name":"Alice","email":"alice@example.com","age":30}`)
    // => jsonData is JSON bytes
    // => age field present in JSON but not in struct
    // => Extra fields in JSON ignored during unmarshaling

    var user User
    // => user is zero-value User

    err := json.Unmarshal(jsonData, &user)
    // => json.Unmarshal parses JSON into user
    // => &user is pointer (required for unmarshaling)
    // => err is non-nil if JSON invalid or type mismatch
    // => Modifies user in place

    if err != nil {
        log.Fatalf("Unmarshal failed: %v", err)
    }

    fmt.Printf("User: %+v\n", user)
    // => Output: User: {ID:1 Name:Alice Email:alice@example.com}
    // => %+v prints field names
    // => age field ignored (not in struct)
}
```

**Nested structs pattern**:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Address struct {
    Street  string `json:"street"`
    City    string `json:"city"`
    Country string `json:"country"`
}

type User struct {
    ID      int     `json:"id"`
    Name    string  `json:"name"`
    Address Address `json:"address"`
    // => Address is nested struct
    // => Marshaled as nested JSON object
}

func main() {
    user := User{
        ID:   1,
        Name: "Alice",
        Address: Address{
            Street:  "123 Main St",
            City:    "Springfield",
            Country: "USA",
        },
    }

    data, err := json.Marshal(user)
    if err != nil {
        log.Fatalf("Marshal failed: %v", err)
    }

    fmt.Printf("JSON: %s\n", data)
    // => Output: JSON: {"id":1,"name":"Alice","address":{"street":"123 Main St","city":"Springfield","country":"USA"}}
    // => address is nested object
}
```

**Slices and maps pattern**:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Post struct {
    ID      int               `json:"id"`
    Title   string            `json:"title"`
    Tags    []string          `json:"tags"`
    // => Tags is slice (JSON array)
    // => Empty slice: [], nil: null

    Metadata map[string]string `json:"metadata"`
    // => Metadata is map (JSON object)
    // => Keys must be strings
    // => Empty map: {}, nil: null
}

func main() {
    post := Post{
        ID:    1,
        Title: "Hello World",
        Tags:  []string{"go", "json", "tutorial"},
        // => Tags becomes JSON array

        Metadata: map[string]string{
            "author": "Alice",
            "status": "published",
        },
        // => Metadata becomes JSON object
    }

    data, err := json.Marshal(post)
    if err != nil {
        log.Fatalf("Marshal failed: %v", err)
    }

    fmt.Printf("JSON: %s\n", data)
    // => Output: JSON: {"id":1,"title":"Hello World","tags":["go","json","tutorial"],"metadata":{"author":"Alice","status":"published"}}
    // => tags is array, metadata is object
}
```

**Streaming with Encoder/Decoder**:

```go
package main

import (
    "encoding/json"
    "os"
    // => Standard library for file operations
    "log"
)

type User struct {
    ID   int    `json:"id"`
    Name string `json:"name"`
}

func writeUsers(users []User) error {
    // => Writes users to file using streaming encoder

    file, err := os.Create("users.json")
    // => Creates users.json file
    // => Truncates if exists
    if err != nil {
        return err
    }
    defer file.Close()

    encoder := json.NewEncoder(file)
    // => json.NewEncoder(file) creates encoder writing to file
    // => Encoder writes JSON directly to io.Writer
    // => More efficient than Marshal + Write for large data

    encoder.SetIndent("", "  ")
    // => SetIndent formats JSON with indentation
    // => First arg: prefix (usually "")
    // => Second arg: indent string (usually "  " or "\t")
    // => Production: omit for compact JSON

    for _, user := range users {
        // => Encode each user separately
        // => Output: one JSON object per Encode call

        if err := encoder.Encode(user); err != nil {
            // => encoder.Encode writes user to file
            // => Returns error if write fails
            return err
        }
        // => Each Encode call writes complete JSON + newline
    }

    return nil
}

func readUsers() ([]User, error) {
    // => Reads users from file using streaming decoder

    file, err := os.Open("users.json")
    // => Opens users.json for reading
    if err != nil {
        return nil, err
    }
    defer file.Close()

    decoder := json.NewDecoder(file)
    // => json.NewDecoder(file) creates decoder reading from file
    // => Decoder reads JSON from io.Reader
    // => Handles newline-delimited JSON (one object per line)

    var users []User
    // => users accumulates decoded users

    for decoder.More() {
        // => decoder.More() returns true if more objects available
        // => Returns false at EOF

        var user User
        if err := decoder.Decode(&user); err != nil {
            // => decoder.Decode reads next JSON object
            // => Parses into user
            return nil, err
        }

        users = append(users, user)
        // => Accumulate decoded user
    }

    return users, nil
}

func main() {
    users := []User{
        {ID: 1, Name: "Alice"},
        {ID: 2, Name: "Bob"},
    }

    if err := writeUsers(users); err != nil {
        log.Fatalf("Write failed: %v", err)
    }

    readUsers, err := readUsers()
    if err != nil {
        log.Fatalf("Read failed: %v", err)
    }

    log.Printf("Read %d users", len(readUsers))
    // => Output: Read 2 users
}
```

**API client pattern with error handling**:

```go
package main

import (
    "bytes"
    // => Standard library for buffer operations
    "encoding/json"
    "fmt"
    "io"
    "net/http"
    "time"
    // => Standard library for time operations
)

type CreateUserRequest struct {
    Name  string `json:"name"`
    Email string `json:"email"`
}

type CreateUserResponse struct {
    ID        int       `json:"id"`
    Name      string    `json:"name"`
    Email     string    `json:"email"`
    CreatedAt time.Time `json:"created_at"`
    // => time.Time marshaled as RFC3339 string
    // => Example: "2024-01-15T10:30:00Z"
}

type ErrorResponse struct {
    Error   string `json:"error"`
    Message string `json:"message"`
}

func createUser(baseURL string, req CreateUserRequest) (*CreateUserResponse, error) {
    // => Returns pointer to response and error

    jsonData, err := json.Marshal(req)
    // => Marshal request to JSON bytes
    if err != nil {
        return nil, fmt.Errorf("marshal request: %w", err)
    }

    httpReq, err := http.NewRequest(
        http.MethodPost,
        baseURL+"/users",
        bytes.NewReader(jsonData),
        // => bytes.NewReader wraps []byte as io.Reader
        // => http.NewRequest requires io.Reader for body
    )
    if err != nil {
        return nil, fmt.Errorf("create request: %w", err)
    }

    httpReq.Header.Set("Content-Type", "application/json")
    // => Set Content-Type header
    // => Tells server request body is JSON
    // => Required for most JSON APIs

    client := &http.Client{Timeout: 10 * time.Second}
    // => Create client with 10-second timeout
    // => Prevents hanging indefinitely
    // => Production: reuse client (connection pooling)

    resp, err := client.Do(httpReq)
    // => client.Do executes request
    // => resp is *http.Response
    if err != nil {
        return nil, fmt.Errorf("request failed: %w", err)
    }
    defer resp.Body.Close()

    if resp.StatusCode != http.StatusCreated {
        // => Check status code before parsing body
        // => http.StatusCreated is 201

        var errResp ErrorResponse
        if err := json.NewDecoder(resp.Body).Decode(&errResp); err != nil {
            // => Try to parse error response
            // => May fail if error not JSON
            return nil, fmt.Errorf("status %d, decode error: %w", resp.StatusCode, err)
        }

        return nil, fmt.Errorf("status %d: %s", resp.StatusCode, errResp.Message)
        // => Return descriptive error
    }

    var user CreateUserResponse
    if err := json.NewDecoder(resp.Body).Decode(&user); err != nil {
        // => Parse success response
        return nil, fmt.Errorf("decode response: %w", err)
    }

    return &user, nil
}

func main() {
    req := CreateUserRequest{
        Name:  "Alice",
        Email: "alice@example.com",
    }

    user, err := createUser("https://api.example.com", req)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }

    fmt.Printf("Created user %d: %s\n", user.ID, user.Name)
}
```

**Custom marshaling pattern**:

```go
package main

import (
    "encoding/json"
    "fmt"
    "time"
)

type Event struct {
    Name string
    Time time.Time
}

// MarshalJSON implements json.Marshaler interface
// => Called by json.Marshal instead of default marshaling
// => Returns JSON representation as []byte
func (e Event) MarshalJSON() ([]byte, error) {
    // => Custom marshaling logic

    type Alias Event
    // => Alias prevents infinite recursion
    // => Marshaling Alias uses default marshaling
    // => Without alias, marshaling Event calls MarshalJSON → infinite loop

    return json.Marshal(&struct {
        Alias
        Timestamp int64 `json:"timestamp"`
        // => Add computed field to JSON
    }{
        Alias:     Alias(e),
        // => Include original fields
        Timestamp: e.Time.Unix(),
        // => Unix timestamp (seconds since epoch)
        // => Adds "timestamp" field alongside default fields
    })
}

// UnmarshalJSON implements json.Unmarshaler interface
// => Called by json.Unmarshal instead of default unmarshaling
func (e *Event) UnmarshalJSON(data []byte) error {
    // => e is pointer (required to modify receiver)

    type Alias Event
    aux := &struct {
        *Alias
        Timestamp int64 `json:"timestamp"`
    }{
        Alias: (*Alias)(e),
        // => Unmarshal into original struct
    }

    if err := json.Unmarshal(data, &aux); err != nil {
        return err
    }

    e.Time = time.Unix(aux.Timestamp, 0)
    // => Convert timestamp to time.Time
    // => time.Unix creates time from Unix timestamp

    return nil
}

func main() {
    event := Event{
        Name: "Go Conference",
        Time: time.Now(),
    }

    data, _ := json.Marshal(event)
    fmt.Printf("JSON: %s\n", data)
    // => Output: JSON: {"Name":"Go Conference","timestamp":1707048000}
    // => timestamp added by custom MarshalJSON
}
```

**Limitations for production**:

- **Performance**: Reflection-based (slower than code generation)
- **No streaming by default**: Marshal/Unmarshal load entire structure into memory
- **Limited customization**: Custom marshaling requires separate methods
- **No schema validation**: Invalid JSON structures accepted (missing required fields)
- **Case-sensitive**: Field names case-sensitive (common API integration issue)

## Production Enhancement: jsoniter for Performance

`jsoniter` is a drop-in replacement for `encoding/json` with significantly better performance (2-3x faster) through code generation and optimizations. Use for high-throughput services where JSON performance is bottleneck.

**Installing jsoniter**:

```bash
go get github.com/json-iterator/go
# => Downloads jsoniter package
# => Drop-in replacement for encoding/json
```

**jsoniter usage (identical API)**:

```go
package main

import (
    jsoniter "github.com/json-iterator/go"
    // => Import as jsoniter alias
    // => API identical to encoding/json
    "fmt"
)

var json = jsoniter.ConfigCompatibleWithStandardLibrary
// => Create JSON instance with standard library compatibility
// => json.Marshal, json.Unmarshal behave like encoding/json
// => Alternative: jsoniter.ConfigFastest (less compatible, faster)

type User struct {
    ID   int    `json:"id"`
    Name string `json:"name"`
}

func main() {
    user := User{ID: 1, Name: "Alice"}

    data, err := json.Marshal(user)
    // => Identical API to encoding/json
    // => 2-3x faster than encoding/json
    // => Same []byte output
    if err != nil {
        panic(err)
    }

    fmt.Printf("JSON: %s\n", data)
    // => Output: JSON: {"id":1,"name":"Alice"}

    var decoded User
    err = json.Unmarshal(data, &decoded)
    // => Identical API, faster performance
    if err != nil {
        panic(err)
    }

    fmt.Printf("Decoded: %+v\n", decoded)
    // => Output: Decoded: {ID:1 Name:Alice}
}
```

**When to use jsoniter**:

- High-throughput APIs (>10K req/sec)
- Large JSON payloads (>1MB)
- CPU profiling shows JSON as bottleneck
- After optimizing other performance issues first

**When to stick with encoding/json**:

- Low-medium throughput (<10K req/sec)
- Small JSON payloads (<100KB)
- Zero-dependency requirement
- JSON not performance bottleneck

## API Client Best Practices

**Structured error handling**:

```go
type APIError struct {
    StatusCode int
    Message    string
}

func (e *APIError) Error() string {
    return fmt.Sprintf("API error %d: %s", e.StatusCode, e.Message)
}

func makeRequest(url string) error {
    resp, err := http.Get(url)
    if err != nil {
        return fmt.Errorf("request failed: %w", err)
    }
    defer resp.Body.Close()

    if resp.StatusCode != http.StatusOK {
        body, _ := io.ReadAll(resp.Body)
        return &APIError{
            StatusCode: resp.StatusCode,
            Message:    string(body),
        }
    }

    return nil
}
```

**Retry logic with exponential backoff**:

```go
func retryRequest(url string, maxRetries int) (*http.Response, error) {
    var resp *http.Response
    var err error

    for i := 0; i < maxRetries; i++ {
        resp, err = http.Get(url)
        if err == nil && resp.StatusCode < 500 {
            // => Success or client error (don't retry)
            return resp, nil
        }

        if resp != nil {
            resp.Body.Close()
        }

        waitTime := time.Duration(1<<uint(i)) * time.Second
        // => Exponential backoff: 1s, 2s, 4s, 8s...
        time.Sleep(waitTime)
    }

    return nil, fmt.Errorf("max retries exceeded: %w", err)
}
```

**Request context for cancellation**:

```go
func fetchWithContext(ctx context.Context, url string) error {
    req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
    // => NewRequestWithContext attaches context
    // => Request cancelled if context cancelled
    if err != nil {
        return err
    }

    client := &http.Client{Timeout: 5 * time.Second}
    resp, err := client.Do(req)
    if err != nil {
        return err
    }
    defer resp.Body.Close()

    // Process response...
    return nil
}
```

## Trade-offs Comparison

| Aspect                 | encoding/json                        | jsoniter                             |
| ---------------------- | ------------------------------------ | ------------------------------------ |
| **Dependencies**       | None (stdlib)                        | 1 package (jsoniter)                 |
| **Performance**        | Baseline (reflection-based)          | 2-3x faster (optimized)              |
| **Marshal Speed**      | ~1000 ns/op (medium structs)         | ~300 ns/op (medium structs)          |
| **Memory Allocations** | Higher (reflection overhead)         | Lower (code generation)              |
| **API Compatibility**  | Standard library                     | Drop-in replacement                  |
| **Customization**      | MarshalJSON/UnmarshalJSON interfaces | Same + additional APIs               |
| **Validation**         | None                                 | None (both need separate validation) |
| **Community**          | Huge (stdlib)                        | Growing                              |
| **When to Use**        | Default choice, low-medium traffic   | High traffic, JSON bottleneck        |

## Best Practices

**Struct tag best practices**:

1. **Use `json:"name"`** for field mapping (consistent lowercase)
2. **Use `omitempty`** for optional fields to reduce JSON size
3. **Use `json:"-"`** to exclude sensitive fields (passwords, tokens)
4. **Validate field presence** manually or with validation library
5. **Document required vs optional fields** in struct comments

**Marshaling best practices**:

1. **Use `json.Marshal` for small data** (<1MB)
2. **Use `json.Encoder` for streaming** large data or file output
3. **Check errors** always (even though rare for simple structs)
4. **Capitalize struct fields** (unexported fields silently ignored)
5. **Test JSON output** with table-driven tests

**Unmarshaling best practices**:

1. **Validate input** before unmarshaling (length limits, content type)
2. **Use pointers** for optional fields (`*string` vs `string`)
3. **Check for unknown fields** if strict validation needed
4. **Limit input size** to prevent DoS (use `io.LimitReader`)
5. **Handle missing fields** gracefully (provide defaults)

**API client best practices**:

1. **Set timeouts** on http.Client (default: no timeout)
2. **Reuse http.Client** for connection pooling
3. **Close response bodies** always (`defer resp.Body.Close()`)
4. **Check status codes** before unmarshaling
5. **Parse error responses** with separate struct
6. **Implement retries** with exponential backoff
7. **Use context** for request cancellation
8. **Log requests** for debugging (sanitize sensitive data)
