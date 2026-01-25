---
title: Configuration Properties Template
description: Template for type-safe configuration properties
category: explanation
subcategory: platform-web-templates
tags:
  - spring-boot
  - template
  - configuration
  - properties
last_updated: 2026-01-25
---

# Configuration Properties Template

```java
package com.oseplatform.[domain].config;

import jakarta.validation.constraints.*;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import java.math.BigDecimal;
import java.time.Duration;
import java.util.List;
import java.util.Map;

/**
 * Configuration properties for [Domain] module.
 *
 * <p>Binds to application.yml properties under ose.[domain].
 * All properties are validated at startup.
 */
@ConfigurationProperties(prefix = "ose.[domain]")
@Validated
public class [Domain]Properties {

    /**
     * Enable/disable [domain] module.
     */
    private boolean enabled = true;

    /**
     * Default rate configuration.
     */
    @NotNull
    @DecimalMin(value = "0.0", inclusive = false)
    @DecimalMax(value = "1.0")
    private BigDecimal defaultRate = new BigDecimal("0.025");

    /**
     * Processing timeout for [domain] operations.
     */
    @NotNull
    private Duration processingTimeout = Duration.ofSeconds(30);

    /**
     * Maximum items per batch.
     */
    @Min(1)
    @Max(1000)
    private int batchSize = 100;

    /**
     * Allowed status values.
     */
    @NotEmpty
    private List<String> allowedStatuses = List.of(
        "PENDING", "PROCESSING", "COMPLETED", "FAILED"
    );

    /**
     * External API configuration.
     */
    private ApiConfig api = new ApiConfig();

    /**
     * Feature flags.
     */
    private Features features = new Features();

    // Getters and setters

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public BigDecimal getDefaultRate() {
        return defaultRate;
    }

    public void setDefaultRate(BigDecimal defaultRate) {
        this.defaultRate = defaultRate;
    }

    public Duration getProcessingTimeout() {
        return processingTimeout;
    }

    public void setProcessingTimeout(Duration processingTimeout) {
        this.processingTimeout = processingTimeout;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }

    public List<String> getAllowedStatuses() {
        return allowedStatuses;
    }

    public void setAllowedStatuses(List<String> allowedStatuses) {
        this.allowedStatuses = allowedStatuses;
    }

    public ApiConfig getApi() {
        return api;
    }

    public void setApi(ApiConfig api) {
        this.api = api;
    }

    public Features getFeatures() {
        return features;
    }

    public void setFeatures(Features features) {
        this.features = features;
    }

    /**
     * Nested configuration for external API.
     */
    public static class ApiConfig {

        @NotBlank
        private String baseUrl = "https://api.example.com";

        @NotBlank
        private String apiKey;

        @NotNull
        private Duration connectTimeout = Duration.ofSeconds(10);

        @NotNull
        private Duration readTimeout = Duration.ofSeconds(30);

        @Min(1)
        @Max(10)
        private int maxRetries = 3;

        // Getters and setters

        public String getBaseUrl() {
            return baseUrl;
        }

        public void setBaseUrl(String baseUrl) {
            this.baseUrl = baseUrl;
        }

        public String getApiKey() {
            return apiKey;
        }

        public void setApiKey(String apiKey) {
            this.apiKey = apiKey;
        }

        public Duration getConnectTimeout() {
            return connectTimeout;
        }

        public void setConnectTimeout(Duration connectTimeout) {
            this.connectTimeout = connectTimeout;
        }

        public Duration getReadTimeout() {
            return readTimeout;
        }

        public void setReadTimeout(Duration readTimeout) {
            this.readTimeout = readTimeout;
        }

        public int getMaxRetries() {
            return maxRetries;
        }

        public void setMaxRetries(int maxRetries) {
            this.maxRetries = maxRetries;
        }
    }

    /**
     * Feature flags configuration.
     */
    public static class Features {

        private boolean asyncProcessing = false;

        private boolean advancedValidation = true;

        private boolean notificationsEnabled = true;

        // Getters and setters

        public boolean isAsyncProcessing() {
            return asyncProcessing;
        }

        public void setAsyncProcessing(boolean asyncProcessing) {
            this.asyncProcessing = asyncProcessing;
        }

        public boolean isAdvancedValidation() {
            return advancedValidation;
        }

        public void setAdvancedValidation(boolean advancedValidation) {
            this.advancedValidation = advancedValidation;
        }

        public boolean isNotificationsEnabled() {
            return notificationsEnabled;
        }

        public void setNotificationsEnabled(boolean notificationsEnabled) {
            this.notificationsEnabled = notificationsEnabled;
        }
    }
}
```

**Corresponding application.yml**:

```yaml
ose:
  [domain]:
    enabled: true
    default-rate: 0.025
    processing-timeout: 30s
    batch-size: 100
    allowed-statuses:
      - PENDING
      - PROCESSING
      - COMPLETED
      - FAILED
    api:
      base-url: ${OSE_API_URL:https://api.example.com}
      api-key: ${OSE_API_KEY}
      connect-timeout: 10s
      read-timeout: 30s
      max-retries: 3
    features:
      async-processing: false
      advanced-validation: true
      notifications-enabled: true
```

**Enable Configuration Properties**:

```java
package com.oseplatform.[domain].config;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableConfigurationProperties([Domain]Properties.class)
public class [Domain]Config {
    // Additional beans if needed
}
```

**Usage in Components**:

```java
@Service
public class [Domain]Service {

    private final [Domain]Properties properties;

    public [Domain]Service([Domain]Properties properties) {
        this.properties = properties;
    }

    public void processWithConfig() {
        if (!properties.isEnabled()) {
            return;
        }

        BigDecimal rate = properties.getDefaultRate();
        Duration timeout = properties.getProcessingTimeout();
        // Use configuration...
    }
}
```

**Replace**:

- `[domain]` - Your bounded context (e.g., `zakat`, `murabaha`)
- `[Domain]` - Domain name PascalCase (e.g., `Zakat`, `Murabaha`)

**Best Practices**:

- Always use @Validated for validation
- Provide sensible defaults for optional properties
- Use Duration for timeouts instead of integers
- Group related properties in nested classes
- Document each property with JavaDoc
- Use environment variables for secrets (${ENV_VAR})
- Validate at startup (fail-fast principle)

---

**Last Updated**: 2026-01-25
