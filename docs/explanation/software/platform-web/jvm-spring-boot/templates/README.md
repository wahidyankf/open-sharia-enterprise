# Spring Boot Code Templates

Production-ready code templates for Spring Boot applications in the open-sharia-enterprise platform. These templates provide complete, type-safe boilerplate code following OSE Platform conventions and best practices.

## 📋 Available Templates

### 1. REST Controller Template

**File**: [ex-so-plwe-jvsb-te\_\_rest-controller-template.md](./ex-so-plwe-jvsb-te__rest-controller-template.md)

Complete REST controller template with:

- CRUD endpoints (GET, POST, PUT, DELETE)
- Request/Response DTOs with validation
- Error handling with `@ExceptionHandler`
- Authentication with `@PreAuthorize`
- Pagination and filtering
- OpenAPI documentation
- OSE Platform examples (Zakat, Murabaha, Waqf)

**Use when**: Creating REST API endpoints for domain entities

**Example scenarios**:

- Zakat calculation API endpoints
- Murabaha contract management endpoints
- Waqf project donation endpoints

### 2. Service Layer Template

**File**: [ex-so-plwe-jvsb-te\_\_service-template.md](./ex-so-plwe-jvsb-te__service-template.md)

Service layer template with:

- Business logic implementation
- Transaction management with `@Transactional`
- Domain event publishing
- Error handling and validation
- Repository interaction
- Metrics and logging

**Use when**: Creating service classes for business logic

**Example scenarios**:

- ZakatCalculationService for zakat calculations
- MurabahaApplicationService for contract processing
- WaqfDonationService for donation management

### 3. Repository Template

**File**: [ex-so-plwe-jvsb-te\_\_repository-template.md](./ex-so-plwe-jvsb-te__repository-template.md)

Repository template with:

- Spring Data JPA interface
- Custom query methods
- `@Query` annotations for complex queries
- Projections for optimized queries
- Pagination and sorting
- `@EntityGraph` for fetch optimization

**Use when**: Creating repository interfaces for data access

**Example scenarios**:

- ZakatCalculationRepository for zakat data
- MurabahaContractRepository for contract data
- WaqfProjectRepository for project data

### 4. Configuration Properties Template

**File**: [ex-so-plwe-jvsb-te\_\_config-properties-template.md](./ex-so-plwe-jvsb-te__config-properties-template.md)

Configuration properties template with:

- Type-safe configuration with `@ConfigurationProperties`
- Validation with Jakarta Bean Validation
- Nested configuration classes
- Default values
- Documentation comments

**Use when**: Creating configuration classes for externalized settings

**Example scenarios**:

- ZakatProperties for zakat calculation settings
- MurabahaProperties for contract configuration
- WaqfProperties for donation settings

### 5. Exception Handler Template

**File**: [ex-so-plwe-jvsb-te\_\_exception-handler-template.md](./ex-so-plwe-jvsb-te__exception-handler-template.md)

Global exception handler template with:

- `@RestControllerAdvice` for centralized error handling
- Custom exception handling methods
- Validation error handling
- Generic exception handling
- Structured error responses
- HTTP status code mapping

**Use when**: Creating global exception handlers for consistent error responses

**Example scenarios**:

- Global error handling for all REST endpoints
- Custom exception handling for domain exceptions
- Validation error formatting

## 🚀 Quick Start

### Using a Template

1. **Choose appropriate template** based on what you're creating (controller, service, repository, configuration, or exception handler)

2. **Copy template code** from the template file

3. **Replace placeholders**:
   - `[Entity]` → Your domain entity name (e.g., `ZakatCalculation`, `MurabahaContract`)
   - `[entity]` → Lowercase entity name (e.g., `zakatCalculation`, `murabahaContract`)
   - `[entities]` → Plural entity name (e.g., `zakatCalculations`, `murabahaContracts`)
   - Package names, imports, and logic

4. **Test thoroughly**:
   - Unit tests for service logic
   - Integration tests for controllers and repositories
   - Validation edge cases
   - Error scenarios

### Example: Creating a Zakat Calculation Service

**Step 1**: Copy service template from `ex-so-plwe-jvsb-te__service-template.md`

**Step 2**: Replace placeholders:

```java
// From template:
@Service
@Transactional
public class [Entity]Service {
    // ...
}

// Your code:
@Service
@Transactional
public class ZakatCalculationService {
    // ...
}
```

**Step 3**: Customize business logic:

```java
@Service
@Slf4j
@Transactional
public class ZakatCalculationService {

    private final ZakatCalculationRepository repository;
    private final NisabThresholdService nisabService;
    private final ApplicationEventPublisher eventPublisher;

    public ZakatCalculationService(
        ZakatCalculationRepository repository,
        NisabThresholdService nisabService,
        ApplicationEventPublisher eventPublisher
    ) {
        this.repository = repository;
        this.nisabService = nisabService;
        this.eventPublisher = eventPublisher;
    }

    public ZakatCalculationResponse calculate(CreateZakatRequest request, String userId) {
        // Get nisab threshold
        Money nisabThreshold = nisabService.getCurrentNisab(
            request.currency(),
            request.calculationDate()
        );

        // Calculate zakat
        ZakatCalculation calculation = ZakatCalculation.calculate(
            new Money(request.wealth(), request.currency()),
            nisabThreshold,
            userId,
            request.calculationDate()
        );

        // Save to database
        ZakatCalculation saved = repository.save(calculation);

        // Publish domain event
        if (saved.isZakatDue()) {
            eventPublisher.publishEvent(
                new ZakatDueEvent(saved.getId(), userId, saved.getZakatAmount())
            );
        }

        log.info("Zakat calculation completed - id: {}, zakatDue: {}",
            saved.getId(), saved.isZakatDue());

        return ZakatCalculationMapper.toResponse(saved);
    }
}
```

**Step 4**: Write tests:

```java
@SpringBootTest
@Transactional
class ZakatCalculationServiceTest {

    @Autowired
    private ZakatCalculationService service;

    @MockBean
    private NisabThresholdService nisabService;

    @Test
    void calculate_wealthAboveNisab_returnsZakatDue() {
        // Arrange
        when(nisabService.getCurrentNisab("USD", LocalDate.now()))
            .thenReturn(new Money(new BigDecimal("5000"), "USD"));

        CreateZakatRequest request = new CreateZakatRequest(
            new BigDecimal("10000"),
            "USD",
            LocalDate.now()
        );

        // Act
        ZakatCalculationResponse response = service.calculate(request, "user-123");

        // Assert
        assertThat(response.zakatDue()).isTrue();
        assertThat(response.zakatAmount()).isEqualByComparingTo("250.00");
    }
}
```

**Step 5**: Test and deploy

## 📝 Template Conventions

All templates follow these conventions:

### Java & Spring Boot

- **Constructor injection**: Use constructor injection for dependencies (no `@Autowired`)
- **Immutable DTOs**: Use Java records for request/response DTOs
- **Validation**: Use Jakarta Bean Validation (`@Valid`, `@NotNull`, `@Min`)
- **Logging**: Use SLF4J with Lombok `@Slf4j`

### Error Handling

- **Custom exceptions**: Domain exceptions extend RuntimeException
- **Global handler**: `@RestControllerAdvice` for centralized error handling
- **Structured responses**: Consistent error response format
- **HTTP status codes**: Appropriate status codes (400, 404, 500)

### Transactions

- **Service layer**: `@Transactional` on service methods
- **Read-only**: Use `@Transactional(readOnly = true)` for queries
- **Isolation**: Consider isolation levels for concurrent operations
- **Rollback**: Automatic rollback on unchecked exceptions

### Authentication & Authorization

- **Method security**: Use `@PreAuthorize` for role-based access
- **Spring Security**: Leverage Spring Security for authentication
- **Principal injection**: Inject `Principal` or `Authentication` for user context
- **Authorization checks**: Verify permissions before business operations

### Data Access

- **Spring Data JPA**: Use repository interfaces
- **Query methods**: Derive queries from method names
- **Custom queries**: Use `@Query` for complex queries
- **Fetch optimization**: Use `@EntityGraph` to prevent N+1 queries

### API Documentation

- **OpenAPI**: Document APIs with `@Operation`, `@ApiResponse`
- **Swagger UI**: Enable Swagger UI for API exploration
- **Examples**: Provide request/response examples
- **Descriptions**: Clear, concise endpoint descriptions

## 🎨 OSE Platform Context

Templates include examples from the OSE Platform Islamic finance domain:

### Zakat (Obligatory Charity)

- **Calculation service**: Calculate zakat based on wealth and nisab threshold
- **Nisab threshold**: External API integration for gold/silver prices
- **Historical tracking**: Annual zakat calculations for users
- **Notifications**: Email notifications when zakat is due

**Domain concepts**:

- Nisab: Minimum wealth threshold (85g gold or 595g silver)
- Hawal: Lunar year (354 days) for wealth calculation
- Zakat rate: 2.5% of qualifying wealth

### Murabaha (Cost-Plus Financing)

- **Application service**: Process financing applications
- **Credit check**: External API integration for credit scores
- **Risk assessment**: Calculate risk level and markup
- **Installment scheduling**: Generate payment schedules
- **Contract management**: Manage active contracts and payments

**Domain concepts**:

- Purchase price: Original asset cost
- Markup: Profit margin (percentage of purchase price)
- Installments: Monthly payments over contract period
- Grace period: Days after due date before penalty

### Waqf (Endowment)

- **Donation service**: Process one-time and recurring donations
- **Project management**: Create and manage charitable projects
- **Impact tracking**: Track donations and project outcomes
- **Donor management**: Manage donor profiles and history

**Domain concepts**:

- Waqf project: Charitable project with funding goal
- Donation: One-time or recurring contribution
- Impact report: Quarterly/annual reports on project outcomes
- Donor receipt: Tax-deductible donation receipt

## 🔒 Security Best Practices

Templates incorporate security measures:

- **Input validation**: Jakarta Bean Validation on all DTOs
- **SQL injection prevention**: Spring Data JPA parameterized queries
- **Authentication**: Spring Security integration
- **Authorization**: Role-based access control with `@PreAuthorize`
- **CSRF protection**: Spring Security CSRF tokens
- **Rate limiting**: Bucket4j integration for API rate limiting
- **Secrets management**: Externalized configuration with Vault

### Example: Secured Controller

```java
@RestController
@RequestMapping("/api/v1/zakat/calculations")
@PreAuthorize("hasRole('USER')")
public class ZakatCalculationController {

    @PostMapping
    @PreAuthorize("hasAuthority('ZAKAT:WRITE')")
    public ResponseEntity<ZakatCalculationResponse> create(
        @Valid @RequestBody CreateZakatRequest request,
        Principal principal
    ) {
        String userId = principal.getName();
        ZakatCalculationResponse response = service.calculate(request, userId);
        return ResponseEntity.created(/*...*/).body(response);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAuthority('ZAKAT:READ')")
    public ResponseEntity<ZakatCalculationResponse> getById(
        @PathVariable String id,
        Principal principal
    ) {
        // Verify user owns this calculation
        ZakatCalculation calculation = repository.findById(id)
            .orElseThrow(() -> new CalculationNotFoundException(id));

        if (!calculation.getUserId().equals(principal.getName())) {
            throw new ForbiddenException("Access denied");
        }

        return ResponseEntity.ok(ZakatCalculationMapper.toResponse(calculation));
    }
}
```

## 🧪 Testing Guidelines

Test templates thoroughly:

### Unit Tests (Service Layer)

```java
@ExtendWith(MockitoExtension.class)
class ZakatCalculationServiceTest {

    @Mock
    private ZakatCalculationRepository repository;

    @Mock
    private NisabThresholdService nisabService;

    @InjectMocks
    private ZakatCalculationService service;

    @Test
    void calculate_wealthAboveNisab_returnsZakatDue() {
        // Arrange
        Money nisabThreshold = new Money(new BigDecimal("5000"), "USD");
        when(nisabService.getCurrentNisab("USD", LocalDate.now()))
            .thenReturn(nisabThreshold);

        CreateZakatRequest request = new CreateZakatRequest(
            new BigDecimal("10000"),
            "USD",
            LocalDate.now()
        );

        ZakatCalculation saved = new ZakatCalculation(/*...*/);
        when(repository.save(any())).thenReturn(saved);

        // Act
        ZakatCalculationResponse response = service.calculate(request, "user-123");

        // Assert
        assertThat(response.zakatDue()).isTrue();
        assertThat(response.zakatAmount()).isEqualByComparingTo("250.00");

        verify(repository).save(any(ZakatCalculation.class));
        verify(nisabService).getCurrentNisab("USD", LocalDate.now());
    }
}
```

### Integration Tests (Controller + Service + Repository)

```java
@SpringBootTest
@AutoConfigureMockMvc
@Transactional
class ZakatCalculationControllerIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ZakatCalculationRepository repository;

    @Test
    @WithMockUser(username = "user-123", authorities = {"ZAKAT:WRITE"})
    void create_validRequest_returns201Created() throws Exception {
        String requestJson = """
            {
                "wealth": 10000,
                "currency": "USD",
                "calculationDate": "2026-01-26"
            }
            """;

        mockMvc.perform(post("/api/v1/zakat/calculations")
                .contentType(MediaType.APPLICATION_JSON)
                .content(requestJson))
            .andExpect(status().isCreated())
            .andExpect(header().exists("Location"))
            .andExpect(jsonPath("$.wealth").value(10000))
            .andExpect(jsonPath("$.zakatDue").value(true))
            .andExpect(jsonPath("$.zakatAmount").value(250.00));

        // Verify database persistence
        List<ZakatCalculation> calculations = repository.findAll();
        assertThat(calculations).hasSize(1);
        assertThat(calculations.get(0).getWealth().amount())
            .isEqualByComparingTo(new BigDecimal("10000"));
    }
}
```

### Repository Tests (Data Access)

```java
@DataJpaTest
class ZakatCalculationRepositoryTest {

    @Autowired
    private ZakatCalculationRepository repository;

    @Test
    void findByUserIdAndDateRange_existingCalculations_returnsResults() {
        // Arrange
        ZakatCalculation calc1 = new ZakatCalculation(/*...*/);
        ZakatCalculation calc2 = new ZakatCalculation(/*...*/);
        repository.saveAll(List.of(calc1, calc2));

        // Act
        List<ZakatCalculation> results = repository.findByUserIdAndDateRange(
            "user-123",
            LocalDate.of(2026, 1, 1),
            LocalDate.of(2026, 12, 31)
        );

        // Assert
        assertThat(results).hasSize(2);
    }
}
```

---

**Last Updated**: 2026-01-26
**Spring Boot Version**: 3.x
