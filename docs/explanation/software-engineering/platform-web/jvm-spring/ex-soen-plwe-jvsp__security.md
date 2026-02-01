---
title: Spring Framework Security
description: Spring Security fundamentals covering architecture, authentication, authorization, filter chain, method security, password encoding, CSRF, session management, and security testing
category: explanation
subcategory: platform-web
tags:
  - spring-framework
  - spring-security
  - authentication
  - authorization
  - java
  - kotlin
principles:
  - explicit-over-implicit
created: 2026-01-29
updated: 2026-01-29
---

# Spring Framework Security

**Understanding-oriented documentation** for Spring Security fundamentals with Spring Framework.

## Overview

Spring Security provides comprehensive security services for Java applications, including authentication, authorization, and protection against common exploits. This document covers Spring Security basics for Islamic finance applications.

**Version**: Spring Framework 6.1+, Spring Security 6.1+ (Java 17+, Kotlin 1.9+)

## Quick Reference

**Jump to:**

- [Spring Security Architecture](#spring-security-architecture)
- [Authentication](#authentication)
- [Authorization](#authorization)
- [Security Filter Chain](#security-filter-chain)
- [Method Security](#method-security)
- [Password Encoding](#password-encoding)
- [CSRF Protection](#csrf-protection)
- [Session Management](#session-management)

## Spring Security Architecture

### Enable Security

**Java Example**:

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .authorizeHttpRequests(auth -> auth
        .requestMatchers("/api/public/**").permitAll()
        .requestMatchers("/api/admin/**").hasRole("ADMIN")
        .anyRequest().authenticated()
      )
      .formLogin(Customizer.withDefaults())
      .httpBasic(Customizer.withDefaults());

    return http.build();
  }
}
```

**Kotlin Example**:

```kotlin
@Configuration
@EnableWebSecurity
class SecurityConfig {

  @Bean
  fun filterChain(http: HttpSecurity): SecurityFilterChain {
    http {
      authorizeHttpRequests {
        authorize("/api/public/**", permitAll)
        authorize("/api/admin/**", hasRole("ADMIN"))
        authorize(anyRequest, authenticated)
      }
      formLogin {}
      httpBasic {}
    }

    return http.build()
  }
}
```

## Authentication

### UserDetailsService

**Java Example** (Donor Authentication):

```java
@Service
public class DonorUserDetailsService implements UserDetailsService {
  private final DonorRepository donorRepository;

  public DonorUserDetailsService(DonorRepository donorRepository) {
    this.donorRepository = donorRepository;
  }

  @Override
  public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
    Donor donor = donorRepository.findByEmail(email)
      .orElseThrow(() -> new UsernameNotFoundException("Donor not found: " + email));

    return User.builder()
      .username(donor.getEmail())
      .password(donor.getPasswordHash())
      .roles(donor.getRoles().toArray(new String[0]))
      .accountLocked(!donor.isActive())
      .build();
  }
}
```

**Kotlin Example**:

```kotlin
@Service
class DonorUserDetailsService(
  private val donorRepository: DonorRepository
) : UserDetailsService {

  override fun loadUserByUsername(email: String): UserDetails {
    val donor = donorRepository.findByEmail(email)
      ?: throw UsernameNotFoundException("Donor not found: $email")

    return User.builder()
      .username(donor.email)
      .password(donor.passwordHash)
      .roles(*donor.roles.toTypedArray())
      .accountLocked(!donor.isActive)
      .build()
  }
}
```

### Custom Authentication Provider

**Java Example**:

```java
@Component
public class DonorAuthenticationProvider implements AuthenticationProvider {
  private final DonorUserDetailsService userDetailsService;
  private final PasswordEncoder passwordEncoder;

  public DonorAuthenticationProvider(
    DonorUserDetailsService userDetailsService,
    PasswordEncoder passwordEncoder
  ) {
    this.userDetailsService = userDetailsService;
    this.passwordEncoder = passwordEncoder;
  }

  @Override
  public Authentication authenticate(Authentication authentication) throws AuthenticationException {
    String email = authentication.getName();
    String password = authentication.getCredentials().toString();

    UserDetails user = userDetailsService.loadUserByUsername(email);

    if (!passwordEncoder.matches(password, user.getPassword())) {
      throw new BadCredentialsException("Invalid password");
    }

    return new UsernamePasswordAuthenticationToken(
      user,
      password,
      user.getAuthorities()
    );
  }

  @Override
  public boolean supports(Class<?> authentication) {
    return UsernamePasswordAuthenticationToken.class.isAssignableFrom(authentication);
  }
}
```

## Authorization

### URL-Based Authorization

**Java Example**:

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http.authorizeHttpRequests(auth -> auth
      // Public endpoints
      .requestMatchers("/api/public/**").permitAll()
      .requestMatchers("/api/zakat/nisab/current").permitAll()

      // Donor-only endpoints
      .requestMatchers("/api/donations/**").hasRole("DONOR")
      .requestMatchers("/api/zakat/calculate").hasRole("DONOR")

      // Admin-only endpoints
      .requestMatchers("/api/admin/**").hasRole("ADMIN")
      .requestMatchers("/api/reports/**").hasRole("ADMIN")

      // Authenticated users
      .anyRequest().authenticated()
    );

    return http.build();
  }
}
```

**Kotlin Example**:

```kotlin
@Configuration
@EnableWebSecurity
class SecurityConfig {

  @Bean
  fun filterChain(http: HttpSecurity): SecurityFilterChain {
    http {
      authorizeHttpRequests {
        // Public endpoints
        authorize("/api/public/**", permitAll)
        authorize("/api/zakat/nisab/current", permitAll)

        // Donor-only endpoints
        authorize("/api/donations/**", hasRole("DONOR"))
        authorize("/api/zakat/calculate", hasRole("DONOR"))

        // Admin-only endpoints
        authorize("/api/admin/**", hasRole("ADMIN"))
        authorize("/api/reports/**", hasRole("ADMIN"))

        // Authenticated users
        authorize(anyRequest, authenticated)
      }
    }

    return http.build()
  }
}
```

## Security Filter Chain

### Custom Security Filter

**Java Example** (API Key Authentication):

```java
public class ApiKeyAuthenticationFilter extends OncePerRequestFilter {
  private static final String API_KEY_HEADER = "X-API-Key";
  private final ApiKeyService apiKeyService;

  public ApiKeyAuthenticationFilter(ApiKeyService apiKeyService) {
    this.apiKeyService = apiKeyService;
  }

  @Override
  protected void doFilterInternal(
    HttpServletRequest request,
    HttpServletResponse response,
    FilterChain filterChain
  ) throws ServletException, IOException {
    String apiKey = request.getHeader(API_KEY_HEADER);

    if (apiKey != null && apiKeyService.isValid(apiKey)) {
      ApiKeyAuthentication authentication = new ApiKeyAuthentication(apiKey, true);
      SecurityContextHolder.getContext().setAuthentication(authentication);
    }

    filterChain.doFilter(request, response);
  }
}

@Configuration
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http, ApiKeyService apiKeyService) throws Exception {
    http.addFilterBefore(
      new ApiKeyAuthenticationFilter(apiKeyService),
      UsernamePasswordAuthenticationFilter.class
    );

    return http.build();
  }
}
```

## Method Security

### @PreAuthorize and @PostAuthorize

**Java Example** (Donation Service):

```java
@Service
@EnableMethodSecurity
public class DonationService {
  private final DonationRepository repository;

  public DonationService(DonationRepository repository) {
    this.repository = repository;
  }

  @PreAuthorize("hasRole('DONOR')")
  public DonationResponse createDonation(CreateDonationRequest request) {
    // Only donors can create donations
    Donation donation = Donation.create(
      request.amount(),
      request.category(),
      getCurrentDonorId(),
      LocalDate.now()
    );

    repository.save(donation);
    return toResponse(donation);
  }

  @PreAuthorize("hasRole('DONOR') and #donorId == authentication.principal.username")
  public List<DonationResponse> getDonationsByDonor(String donorId) {
    // Donors can only view their own donations
    return repository.findByDonorId(donorId).stream()
      .map(this::toResponse)
      .toList();
  }

  @PreAuthorize("hasRole('ADMIN')")
  public List<DonationResponse> getAllDonations() {
    // Only admins can view all donations
    return repository.findAll().stream()
      .map(this::toResponse)
      .toList();
  }

  @PostAuthorize("returnObject.donorId == authentication.principal.username or hasRole('ADMIN')")
  public DonationResponse getDonation(String id) {
    // Verify after retrieval that user owns donation or is admin
    return repository.findById(id)
      .map(this::toResponse)
      .orElseThrow(() -> new DonationNotFoundException(id));
  }

  private String getCurrentDonorId() {
    return SecurityContextHolder.getContext().getAuthentication().getName();
  }

  private DonationResponse toResponse(Donation donation) {
    return new DonationResponse(
      donation.getId().getValue(),
      donation.getAmount(),
      donation.getCategory(),
      donation.getDonorId(),
      donation.getDonationDate()
    );
  }
}
```

**Kotlin Example**:

```kotlin
@Service
@EnableMethodSecurity
class DonationService(private val repository: DonationRepository) {

  @PreAuthorize("hasRole('DONOR')")
  fun createDonation(request: CreateDonationRequest): DonationResponse {
    // Only donors can create donations
    val donation = Donation.create(
      request.amount,
      request.category,
      getCurrentDonorId(),
      LocalDate.now()
    )

    repository.save(donation)
    return donation.toResponse()
  }

  @PreAuthorize("hasRole('DONOR') and #donorId == authentication.principal.username")
  fun getDonationsByDonor(donorId: String): List<DonationResponse> {
    // Donors can only view their own donations
    return repository.findByDonorId(donorId)
      .map { it.toResponse() }
  }

  @PreAuthorize("hasRole('ADMIN')")
  fun getAllDonations(): List<DonationResponse> {
    // Only admins can view all donations
    return repository.findAll()
      .map { it.toResponse() }
  }

  private fun getCurrentDonorId(): String =
    SecurityContextHolder.getContext().authentication.name

  private fun Donation.toResponse(): DonationResponse = DonationResponse(
    id.value,
    amount,
    category,
    donorId,
    donationDate
  )
}
```

## Password Encoding

**Java Example**:

```java
@Configuration
public class SecurityConfig {

  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder(12);  // Strength factor 12
  }
}

@Service
public class DonorRegistrationService {
  private final DonorRepository donorRepository;
  private final PasswordEncoder passwordEncoder;

  public DonorRegistrationService(
    DonorRepository donorRepository,
    PasswordEncoder passwordEncoder
  ) {
    this.donorRepository = donorRepository;
    this.passwordEncoder = passwordEncoder;
  }

  public DonorResponse registerDonor(RegisterDonorRequest request) {
    // Encode password before storing
    String encodedPassword = passwordEncoder.encode(request.password());

    Donor donor = Donor.create(
      request.email(),
      encodedPassword,
      request.name()
    );

    donorRepository.save(donor);
    return toResponse(donor);
  }

  private DonorResponse toResponse(Donor donor) {
    return new DonorResponse(
      donor.getId(),
      donor.getEmail(),
      donor.getName(),
      donor.getCreatedAt()
    );
  }
}
```

**Kotlin Example**:

```kotlin
@Configuration
class SecurityConfig {

  @Bean
  fun passwordEncoder(): PasswordEncoder = BCryptPasswordEncoder(12)  // Strength factor 12
}

@Service
class DonorRegistrationService(
  private val donorRepository: DonorRepository,
  private val passwordEncoder: PasswordEncoder
) {

  fun registerDonor(request: RegisterDonorRequest): DonorResponse {
    // Encode password before storing
    val encodedPassword = passwordEncoder.encode(request.password)

    val donor = Donor.create(
      request.email,
      encodedPassword,
      request.name
    )

    donorRepository.save(donor)
    return donor.toResponse()
  }

  private fun Donor.toResponse(): DonorResponse = DonorResponse(
    id,
    email,
    name,
    createdAt
  )
}
```

## CSRF Protection

**Java Example**:

```java
@Configuration
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .csrf(csrf -> csrf
        .csrfTokenRepository(CookieCsrfTokenRepository.withHttpOnlyFalse())
      )
      .authorizeHttpRequests(auth -> auth
        .anyRequest().authenticated()
      );

    return http.build();
  }
}

// For stateless REST APIs, disable CSRF
@Configuration
public class RestApiSecurityConfig {

  @Bean
  public SecurityFilterChain apiFilterChain(HttpSecurity http) throws Exception {
    http
      .csrf(csrf -> csrf.disable())  // Disable for stateless APIs
      .authorizeHttpRequests(auth -> auth
        .anyRequest().authenticated()
      );

    return http.build();
  }
}
```

## Session Management

**Java Example**:

```java
@Configuration
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .sessionManagement(session -> session
        .sessionCreationPolicy(SessionCreationPolicy.IF_REQUIRED)
        .maximumSessions(1)
        .maxSessionsPreventsLogin(true)
      )
      .authorizeHttpRequests(auth -> auth
        .anyRequest().authenticated()
      );

    return http.build();
  }
}

// For stateless REST APIs
@Configuration
public class RestApiSecurityConfig {

  @Bean
  public SecurityFilterChain apiFilterChain(HttpSecurity http) throws Exception {
    http
      .sessionManagement(session -> session
        .sessionCreationPolicy(SessionCreationPolicy.STATELESS)  // No sessions for REST APIs
      )
      .authorizeHttpRequests(auth -> auth
        .anyRequest().authenticated()
      );

    return http.build();
  }
}
```

## Security Testing

**Java Example**:

```java
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {SecurityConfig.class, TestConfig.class})
@WebAppConfiguration
class DonationControllerSecurityTest {

  @Autowired
  private WebApplicationContext context;

  private MockMvc mockMvc;

  @BeforeEach
  void setup() {
    mockMvc = MockMvcBuilders.webAppContextSetup(context)
      .apply(springSecurity())
      .build();
  }

  @Test
  @WithMockUser(roles = "DONOR")
  void createDonation_authenticatedDonor_succeeds() throws Exception {
    String requestBody = """
      {
        "amount": 100.00,
        "category": "ZAKAT",
        "donorId": "donor-123",
        "donationDate": "2026-01-29"
      }
      """;

    mockMvc.perform(post("/api/v1/donations")
        .contentType(MediaType.APPLICATION_JSON)
        .content(requestBody))
      .andExpect(status().isCreated());
  }

  @Test
  void createDonation_unauthenticated_returns401() throws Exception {
    mockMvc.perform(post("/api/v1/donations")
        .contentType(MediaType.APPLICATION_JSON)
        .content("{}"))
      .andExpect(status().isUnauthorized());
  }

  @Test
  @WithMockUser(roles = "USER")
  void getAllDonations_nonAdmin_returns403() throws Exception {
    mockMvc.perform(get("/api/v1/donations/all"))
      .andExpect(status().isForbidden());
  }

  @Test
  @WithMockUser(roles = "ADMIN")
  void getAllDonations_admin_succeeds() throws Exception {
    mockMvc.perform(get("/api/v1/donations/all"))
      .andExpect(status().isOk());
  }
}
```

## Related Documentation

### Core Spring Framework Documentation

- **[Spring Framework README](./README.md)** - Framework overview
- **[Web MVC](./ex-soen-plwe-jvsp__web-mvc.md)** - Web layer
- **[REST APIs](./ex-soen-plwe-jvsp__rest-apis.md)** - RESTful services

---

**Last Updated**: 2026-01-29
**Spring Framework Version**: 6.1+, Spring Security 6.1+ (Java 17+, Kotlin 1.9+)
**Maintainers**: Platform Documentation Team
