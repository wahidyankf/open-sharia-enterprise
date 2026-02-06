---
title: "Spring Boot Web Applications"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000030
description: "Manual DispatcherServlet configuration → auto-configured Spring MVC with embedded server in Spring Boot"
tags: ["spring-boot", "in-the-field", "production", "web", "spring-mvc"]
---

## Why Spring Boot Web Matters

Spring Boot's auto-configured web eliminates 100+ lines of DispatcherServlet, ViewResolver, and MessageConverter configuration. In production REST APIs serving millions of requests, Boot's spring-boot-starter-web provides production-ready Jackson JSON serialization, content negotiation, and exception handling out-of-the-box—enabling teams to implement controllers immediately without infrastructure setup.

**Problem**: Manual Spring MVC setup requires explicit DispatcherServlet, ViewResolver, MessageConverter configuration.

**Solution**: Spring Boot auto-configures Spring MVC with sensible defaults via spring-boot-starter-web.

## Manual Spring MVC Configuration

```java
@Configuration
@EnableWebMvc
public class WebConfig implements WebMvcConfigurer {

    @Override
    public void configureMessageConverters(List<HttpMessageConverter<?>> converters) {
        // => Manually configure Jackson for JSON
        MappingJackson2HttpMessageConverter jsonConverter =
            new MappingJackson2HttpMessageConverter();
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        jsonConverter.setObjectMapper(mapper);
        converters.add(jsonConverter);
    }

    @Bean
    public ViewResolver viewResolver() {
        InternalResourceViewResolver resolver = new InternalResourceViewResolver();
        resolver.setPrefix("/WEB-INF/views/");
        resolver.setSuffix(".jsp");
        return resolver;
    }
}
```

**Limitations**: Verbose configuration, easy to miss MessageConverters, no production defaults.

## Spring Boot Auto-Configuration

```java
@SpringBootApplication  // => Includes @EnableAutoConfiguration
public class ZakatApplication {
    public static void main(String[] args) {
        SpringApplication.run(ZakatApplication.class, args);
        // => Auto-configures: DispatcherServlet, Jackson, embedded Tomcat
    }
}

@RestController
@RequestMapping("/api/donations")
public class DonationController {

    @GetMapping("/{id}")
    public DonationResponse getDonation(@PathVariable Long id) {
        // => Jackson automatically serializes DonationResponse to JSON
        // => Content-Type: application/json (auto-negotiated)
        return donationService.findById(id);
    }

    @PostMapping
    public ResponseEntity<DonationResponse> createDonation(
            @RequestBody @Valid DonationRequest request) {
        // => Jackson deserializes JSON to DonationRequest
        // => @Valid triggers Hibernate Validator (auto-configured)
        DonationResponse response = donationService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
```

**What Spring Boot auto-configures**:

- DispatcherServlet at / (no web.xml)
- Jackson for JSON (MappingJackson2HttpMessageConverter)
- Content negotiation (Accept header → JSON/XML)
- Static content serving (/static, /public, /resources)
- Error handling (/error endpoint)
- Embedded Tomcat on port 8080

## Production Patterns

**Custom Jackson configuration**:

```yaml
spring:
  jackson:
    serialization:
      write-dates-as-timestamps: false # => ISO 8601 dates
      indent-output: false # => Compact JSON in production
    deserialization:
      fail-on-unknown-properties: false # => Ignore unknown fields
    default-property-inclusion: non_null # => Exclude null fields
```

**CORS configuration**:

```java
@Configuration
public class CorsConfig {
    @Bean
    public WebMvcConfigurer corsConfigurer() {
        return new WebMvcConfigurer() {
            @Override
            public void addCorsMappings(CorsRegistry registry) {
                registry.addMapping("/api/**")
                    .allowedOrigins("https://zakatfoundation.org")
                    .allowedMethods("GET", "POST", "PUT", "DELETE")
                    .allowedHeaders("*")
                    .allowCredentials(true);
            }
        };
    }
}
```

**Trade-offs**: Spring Boot covers 95% REST API use cases. Custom WebMvcConfigurer for advanced scenarios.

## Next Steps

- [REST API Development](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/rest-api-development) - @RestController patterns
- [Validation](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/validation) - Bean Validation integration
- [Error Handling](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/error-handling) - @ControllerAdvice patterns
