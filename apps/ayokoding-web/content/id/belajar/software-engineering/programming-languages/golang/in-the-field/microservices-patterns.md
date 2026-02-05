---
title: "Microservices Patterns"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Pola desain service, komunikasi, dan resiliensi untuk microservices Go"
weight: 1000064
tags: ["golang", "microservices", "distributed-systems", "arsitektur", "resiliensi"]
---

## Mengapa Microservices Patterns Penting

Microservices menguraikan sistem menjadi service yang dapat di-deploy secara independen, memerlukan perhatian cermat terhadap komunikasi, resiliensi, dan discovery. Dalam sistem keuangan seperti payment processing atau accounting service, microservices memungkinkan scaling independen, otonomi tim, dan isolasi fault, tetapi menimbulkan tantangan distributed system.

**Manfaat utama**:

- **Independent deployment**: Update payment service tanpa menyentuh invoicing
- **Fault isolation**: Kegagalan kalkulator zakat tidak crash seluruh sistem
- **Technology flexibility**: Gunakan Go untuk high-throughput service, Python untuk ML
- **Team autonomy**: Setiap service dimiliki oleh tim berbeda

**Masalah**: Tanpa pola yang tepat, microservices menciptakan cascading failure, request hilang, chaos konfigurasi, dan debugging nightmare di seluruh log terdistribusi.

**Solusi**: Terapkan service discovery, circuit breaker, rate limiting, retry dengan backoff, health check, dan graceful shutdown pattern menggunakan standard library Go dan library production-grade.

## Pendekatan Standard Library: HTTP Service dengan Timeout

Standard library Go menyediakan http.Server, http.Client, dan context untuk membangun resilient HTTP service tanpa dependensi eksternal.

### Service dengan Health Check

```go
package main

import (
    "context"
    "encoding/json"
    // => Standard library JSON encoding
    "fmt"
    "log"
    "net/http"
    // => Standard library HTTP server
    "os"
    "os/signal"
    // => Signal handling untuk graceful shutdown
    "syscall"
    "time"
)

// ZakatService merepresentasikan business service
// => Komponen microservice
type ZakatService struct {
    startTime time.Time
    // => Service start time untuk health check
}

// NewZakatService membuat service instance
func NewZakatService() *ZakatService {
    return &ZakatService{
        startTime: time.Now(),
    }
}

// HealthCheck return status health service
// => Pola endpoint health check
func (s *ZakatService) HealthCheck(w http.ResponseWriter, r *http.Request) {
    // => Signature HTTP handler
    // => w menulis response, r berisi request

    health := map[string]interface{}{
        "status": "healthy",
        // => Status service (healthy/unhealthy/degraded)
        "uptime": time.Since(s.startTime).String(),
        // => Uptime untuk monitoring
        "timestamp": time.Now().Unix(),
        // => Timestamp saat ini
    }
    // => Struktur response health check

    w.Header().Set("Content-Type", "application/json")
    // => Set response content type
    w.WriteHeader(http.StatusOK)
    // => 200 status code (healthy)
    json.NewEncoder(w).Encode(health)
    // => Tulis JSON response
    // => Standard library JSON encoder
}

// Calculate menangani request kalkulasi zakat
// => Endpoint bisnis
func (s *ZakatService) Calculate(w http.ResponseWriter, r *http.Request) {
    // => HTTP POST handler

    if r.Method != http.MethodPost {
        // => Validasi HTTP method
        http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
        // => 405 status code
        return
    }

    var req struct {
        Wealth float64 `json:"wealth"`
        Nisab  float64 `json:"nisab"`
    }
    // => Struktur request

    err := json.NewDecoder(r.Body).Decode(&req)
    // => Decode JSON request body
    // => Standard library JSON decoder
    if err != nil {
        // => JSON invalid
        http.Error(w, "invalid request", http.StatusBadRequest)
        // => 400 status code
        return
    }

    // Logika bisnis (disederhanakan)
    zakatDue := 0.0
    if req.Wealth >= req.Nisab {
        zakatDue = (req.Wealth - req.Nisab) * 0.025
    }

    response := map[string]interface{}{
        "wealth":      req.Wealth,
        "nisab":       req.Nisab,
        "zakat_due":   zakatDue,
        "is_eligible": req.Wealth >= req.Nisab,
    }

    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(http.StatusOK)
    json.NewEncoder(w).Encode(response)
}

func main() {
    service := NewZakatService()
    // => Buat service instance

    mux := http.NewServeMux()
    // => Standard library router
    mux.HandleFunc("/health", service.HealthCheck)
    // => Endpoint health check
    mux.HandleFunc("/calculate", service.Calculate)
    // => Endpoint bisnis

    server := &http.Server{
        Addr:         ":8080",
        // => Listen di port 8080
        Handler:      mux,
        // => Request router
        ReadTimeout:  10 * time.Second,
        // => Cegah slow client attack
        WriteTimeout: 10 * time.Second,
        // => Cegah slow response attack
        IdleTimeout:  60 * time.Second,
        // => Keep-alive timeout
    }
    // => Konfigurasi HTTP server dengan timeout

    // Graceful shutdown handling
    // => Pola shutdown untuk production service
    go func() {
        // => Goroutine untuk signal handling
        sigint := make(chan os.Signal, 1)
        // => Buffered channel untuk signal
        signal.Notify(sigint, os.Interrupt, syscall.SIGTERM)
        // => Register signal handler
        // => SIGINT (Ctrl+C), SIGTERM (kill)

        <-sigint
        // => Block hingga signal diterima
        log.Println("shutting down server...")

        ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
        // => Shutdown timeout context
        // => 30 detik untuk selesaikan in-flight request
        defer cancel()

        if err := server.Shutdown(ctx); err != nil {
            // => Graceful shutdown
            // => Berhenti menerima request baru
            // => Tunggu in-flight request selesai
            log.Printf("server shutdown error: %v", err)
        }
        log.Println("server stopped")
    }()

    log.Printf("starting server on %s", server.Addr)
    if err := server.ListenAndServe(); err != http.ErrServerClosed {
        // => Start HTTP server
        // => Block hingga shutdown
        log.Fatalf("server error: %v", err)
    }
}
```

### HTTP Client dengan Retry

```go
package main

import (
    "bytes"
    "context"
    "encoding/json"
    "errors"
    "fmt"
    "net/http"
    "time"
)

// InvoiceClient memanggil invoice microservice
// => Client untuk komunikasi inter-service
type InvoiceClient struct {
    baseURL string
    // => URL service target
    client  *http.Client
    // => HTTP client yang dapat digunakan ulang (connection pooling)
}

// NewInvoiceClient membuat configured client
func NewInvoiceClient(baseURL string) *InvoiceClient {
    return &InvoiceClient{
        baseURL: baseURL,
        client: &http.Client{
            Timeout: 5 * time.Second,
            // => Total request timeout
            // => Cegah hanging request
        },
    }
}

// CreateInvoice mengirim request ke invoice service dengan retry
// => Pola retry dengan exponential backoff
func (c *InvoiceClient) CreateInvoice(ctx context.Context, invoice map[string]interface{}) error {
    // => context.Context untuk pembatalan
    // => Propagasi deadline lintas service

    maxRetries := 3
    // => Maksimum percobaan retry
    backoff := 100 * time.Millisecond
    // => Durasi backoff awal

    var lastErr error

    for attempt := 0; attempt < maxRetries; attempt++ {
        // => Loop retry

        if attempt > 0 {
            // => Tunggu sebelum retry (tidak pada percobaan pertama)
            select {
            case <-time.After(backoff):
                // => Tunggu untuk durasi backoff
            case <-ctx.Done():
                // => Context dibatalkan (timeout, cancellation)
                return ctx.Err()
            }

            backoff *= 2
            // => Exponential backoff
            // => 100ms → 200ms → 400ms
        }

        err := c.doRequest(ctx, invoice)
        // => Coba request
        if err == nil {
            // => Sukses
            return nil
        }

        lastErr = err
        // => Simpan error untuk percobaan terakhir

        if !isRetryable(err) {
            // => Cek apakah error dapat di-retry
            // => Jangan retry error 4xx
            return err
        }

        log.Printf("attempt %d failed: %v, retrying...", attempt+1, err)
    }

    return fmt.Errorf("max retries exceeded: %w", lastErr)
    // => Semua retry gagal
}

// doRequest melakukan single HTTP request
// => Dipisah untuk logika retry
func (c *InvoiceClient) doRequest(ctx context.Context, invoice map[string]interface{}) error {
    // => Percobaan request tunggal

    body, err := json.Marshal(invoice)
    // => Encode request body
    if err != nil {
        return err
    }

    req, err := http.NewRequestWithContext(ctx, "POST", c.baseURL+"/invoices", bytes.NewReader(body))
    // => Buat request dengan context
    // => Context memungkinkan pembatalan
    if err != nil {
        return err
    }

    req.Header.Set("Content-Type", "application/json")
    // => Set content type header

    resp, err := c.client.Do(req)
    // => Eksekusi HTTP request
    // => Reuse connection pool
    if err != nil {
        return fmt.Errorf("request failed: %w", err)
    }
    defer resp.Body.Close()
    // => Selalu tutup response body

    if resp.StatusCode >= 500 {
        // => Server error (dapat di-retry)
        return fmt.Errorf("server error: %d", resp.StatusCode)
    }

    if resp.StatusCode >= 400 {
        // => Client error (tidak dapat di-retry)
        return fmt.Errorf("client error: %d", resp.StatusCode)
    }

    return nil
    // => Sukses (status 2xx)
}

// isRetryable menentukan apakah error harus di-retry
// => Strategi retry
func isRetryable(err error) bool {
    // Cek server error (5xx) atau network error
    // Jangan retry client error (4xx)
    // => Network error: connection refused, timeout
    // => Server error: 500, 502, 503, 504
    return true // Disederhanakan untuk contoh
}
```

**Keterbatasan pendekatan standard library**:

- Logika retry manual (boilerplate)
- Tanpa circuit breaker (kegagalan cascade)
- Tanpa rate limiting (dapat kewalahan service)
- Service discovery manual (URL hardcoded)
- Tanpa distributed tracing (debugging sulit)

## Pola Produksi: Circuit Breaker, Rate Limiting, Service Discovery

Microservice produksi menggunakan library khusus untuk pola resiliensi.

### Circuit Breaker dengan gobreaker

```bash
go get github.com/sony/gobreaker
# => Library circuit breaker
# => Cegah cascading failure
```

```go
package main

import (
    "context"
    "errors"
    "fmt"
    "github.com/sony/gobreaker"
    // => Library circuit breaker
    "net/http"
    "time"
)

// ResilientInvoiceClient membungkus client dengan circuit breaker
// => Pola komunikasi resilient
type ResilientInvoiceClient struct {
    baseURL string
    client  *http.Client
    cb      *gobreaker.CircuitBreaker
    // => State machine circuit breaker
}

// NewResilientInvoiceClient membuat client dengan circuit breaker
func NewResilientInvoiceClient(baseURL string) *ResilientInvoiceClient {
    settings := gobreaker.Settings{
        Name:        "invoice-service",
        // => Nama circuit breaker (untuk metrik)
        MaxRequests: 3,
        // => Max request di state half-open
        // => Test apakah service pulih
        Interval:    10 * time.Second,
        // => Reset interval failure count
        Timeout:     30 * time.Second,
        // => Timeout state open sebelum half-open
        // => Tunggu sebelum retry
        ReadyToTrip: func(counts gobreaker.Counts) bool {
            // => Menentukan kapan buka circuit
            failureRatio := float64(counts.TotalFailures) / float64(counts.Requests)
            // => Hitung failure rate
            return counts.Requests >= 3 && failureRatio >= 0.6
            // => Buka jika ≥3 request dan ≥60% failure rate
        },
    }

    return &ResilientInvoiceClient{
        baseURL: baseURL,
        client: &http.Client{
            Timeout: 5 * time.Second,
        },
        cb: gobreaker.NewCircuitBreaker(settings),
        // => Buat circuit breaker dengan setting
    }
}

// CreateInvoice mengirim request melalui circuit breaker
// => Circuit breaker cegah cascading failure
func (c *ResilientInvoiceClient) CreateInvoice(ctx context.Context, invoice map[string]interface{}) error {
    // => Bungkus request di circuit breaker

    _, err := c.cb.Execute(func() (interface{}, error) {
        // => Circuit breaker eksekusi fungsi
        // => Track success/failure
        return nil, c.doRequest(ctx, invoice)
        // => HTTP request aktual
    })

    if err != nil {
        // => Request gagal atau circuit open
        if errors.Is(err, gobreaker.ErrOpenState) {
            // => Circuit breaker open (terlalu banyak kegagalan)
            return fmt.Errorf("invoice service unavailable (circuit open): %w", err)
        }
        return err
    }

    return nil
}

func (c *ResilientInvoiceClient) doRequest(ctx context.Context, invoice map[string]interface{}) error {
    // => Implementasi sama seperti sebelumnya
    // => HTTP request dengan context
    return nil // Disederhanakan
}
```

**State circuit breaker**:

- **Closed**: Operasi normal, request lewat
- **Open**: Terlalu banyak kegagalan, request ditolak langsung (fail fast)
- **Half-Open**: Test apakah service pulih, request terbatas diizinkan

### Rate Limiting dengan golang.org/x/time/rate

```bash
go get golang.org/x/time/rate
# => Rate limiter dari Go extended library
# => Algoritma token bucket
```

```go
package main

import (
    "context"
    "fmt"
    "golang.org/x/time/rate"
    // => Rate limiter (token bucket)
    "net/http"
)

// RateLimitedClient membatasi rate request keluar
// => Cegah kewalahan downstream service
type RateLimitedClient struct {
    baseURL string
    client  *http.Client
    limiter *rate.Limiter
    // => Rate limiter token bucket
}

// NewRateLimitedClient membuat client dengan rate limit
func NewRateLimitedClient(baseURL string, requestsPerSecond float64) *RateLimitedClient {
    return &RateLimitedClient{
        baseURL: baseURL,
        client:  &http.Client{},
        limiter: rate.NewLimiter(rate.Limit(requestsPerSecond), 1),
        // => rate.Limit: request per detik
        // => 1: burst size (max token)
        // => Token bucket: 10 req/sec, burst 1
    }
}

// CreateInvoice mengirim rate-limited request
// => Tunggu token sebelum mengirim
func (c *RateLimitedClient) CreateInvoice(ctx context.Context, invoice map[string]interface{}) error {
    // => Request dengan rate-limited

    err := c.limiter.Wait(ctx)
    // => Tunggu token (block hingga tersedia)
    // => Hormati pembatalan context
    // => Token dikonsumsi saat return
    if err != nil {
        // => Context dibatalkan atau deadline terlewati
        return fmt.Errorf("rate limit wait failed: %w", err)
    }

    // Lanjutkan dengan request
    // => Token didapat, aman mengirim request
    return c.doRequest(ctx, invoice)
}

func (c *RateLimitedClient) doRequest(ctx context.Context, invoice map[string]interface{}) error {
    // => Implementasi HTTP request
    return nil // Disederhanakan
}
```

### Service Discovery dengan Consul

```bash
go get github.com/hashicorp/consul/api
# => Library client Consul
# => Service registry dan discovery
```

```go
package main

import (
    "fmt"
    consulapi "github.com/hashicorp/consul/api"
    // => Consul API client
)

// ServiceRegistry menangani registrasi dan discovery service
// => Lokasi service dinamis
type ServiceRegistry struct {
    client *consulapi.Client
    // => Consul client
}

// NewServiceRegistry membuat Consul registry client
func NewServiceRegistry(consulAddr string) (*ServiceRegistry, error) {
    config := consulapi.DefaultConfig()
    // => Konfigurasi Consul default
    config.Address = consulAddr
    // => Alamat Consul agent (localhost:8500)

    client, err := consulapi.NewClient(config)
    // => Buat Consul client
    if err != nil {
        return nil, fmt.Errorf("consul client creation failed: %w", err)
    }

    return &ServiceRegistry{client: client}, nil
}

// Register mendaftar service dengan Consul
// => Service mengumumkan diri saat startup
func (r *ServiceRegistry) Register(serviceID, serviceName, address string, port int) error {
    registration := &consulapi.AgentServiceRegistration{
        ID:      serviceID,
        // => ID instance service unik
        Name:    serviceName,
        // => Nama service (misal, "zakat-service")
        Address: address,
        // => IP address service
        Port:    port,
        // => Port service
        Check: &consulapi.AgentServiceCheck{
            HTTP:     fmt.Sprintf("http://%s:%d/health", address, port),
            // => Endpoint health check
            Interval: "10s",
            // => Cek setiap 10 detik
            Timeout:  "2s",
            // => Timeout health check
        },
        // => Konfigurasi Consul health check
    }

    err := r.client.Agent().ServiceRegister(registration)
    // => Daftar dengan Consul agent
    // => Service terlihat oleh service lain
    if err != nil {
        return fmt.Errorf("service registration failed: %w", err)
    }

    return nil
}

// Discover menemukan instance service healthy
// => Lokasi service dinamis
func (r *ServiceRegistry) Discover(serviceName string) (string, error) {
    services, _, err := r.client.Health().Service(serviceName, "", true, nil)
    // => Query instance healthy
    // => true: hanya passing health check
    if err != nil {
        return "", fmt.Errorf("service discovery failed: %w", err)
    }

    if len(services) == 0 {
        // => Tidak ada instance healthy
        return "", fmt.Errorf("no healthy instances of %s", serviceName)
    }

    // Load balancing sederhana: instance healthy pertama
    service := services[0]
    // => Produksi: round-robin, least connection
    address := fmt.Sprintf("http://%s:%d", service.Service.Address, service.Service.Port)
    // => Konstruksi URL service
    return address, nil
}

// Deregister menghapus service dari Consul
// => Dipanggil saat graceful shutdown
func (r *ServiceRegistry) Deregister(serviceID string) error {
    err := r.client.Agent().ServiceDeregister(serviceID)
    // => Hapus dari registry
    if err != nil {
        return fmt.Errorf("service deregistration failed: %w", err)
    }
    return nil
}
```

**Tabel trade-off**:

| Aspek                 | Standard Library (HTTP + Context) | Produksi (Circuit Breaker + Rate Limit + Discovery) |
| --------------------- | --------------------------------- | --------------------------------------------------- |
| **Resiliensi**        | Retry manual saja                 | Circuit breaker cegah cascading failure             |
| **Rate limiting**     | Tanpa (dapat kewalahan service)   | Token bucket batasi request rate                    |
| **Service discovery** | URL hardcoded                     | Discovery dinamis dengan health check               |
| **Kompleksitas**      | Rendah (HTTP + context)           | Sedang (multiple library)                           |
| **Observability**     | Logging manual                    | Integrasi metrik library                            |
| **Kapan digunakan**   | Single service                    | Microservice (>3 service)                           |

## Best Practice

1. **Health check mandatory**: Setiap service harus ekspos endpoint /health
2. **Graceful shutdown**: Handle SIGTERM, selesaikan in-flight request
3. **Propagasi context**: Teruskan context.Context melalui semua service call
4. **Circuit breaker untuk external call**: Bungkus semua inter-service HTTP call
5. **Rate limiting outbound**: Lindungi downstream service dari overload
6. **Timeout di mana-mana**: Set read, write, idle, dan request timeout
7. **Structured logging**: Gunakan JSON log untuk agregasi (ELK, Splunk)

## Contoh Real-World: Payment Processing Microservice

```go
// Payment service dengan pola resiliensi lengkap
type PaymentService struct {
    invoiceClient    *ResilientInvoiceClient    // Circuit breaker
    accountingClient *RateLimitedClient         // Rate limiting
    registry         *ServiceRegistry           // Service discovery
}

func (s *PaymentService) ProcessPayment(ctx context.Context, payment Payment) error {
    // 1. Buat invoice (dengan circuit breaker)
    err := s.invoiceClient.CreateInvoice(ctx, payment.Invoice())
    if err != nil {
        return fmt.Errorf("invoice creation failed: %w", err)
    }

    // 2. Rekam transaksi (dengan rate limiting)
    err = s.accountingClient.RecordTransaction(ctx, payment.Transaction())
    if err != nil {
        // Compensating transaction (rollback invoice)
        return fmt.Errorf("accounting failed: %w", err)
    }

    return nil
}
```

**Pola microservice yang ditunjukkan**:

- Circuit breaker isolasi kegagalan invoice service
- Rate limiting lindungi accounting service
- Service discovery memungkinkan routing dinamis
- Propagasi context memungkinkan pembatalan request
- Graceful shutdown cegah kehilangan data
- Health check memungkinkan recovery otomatis
