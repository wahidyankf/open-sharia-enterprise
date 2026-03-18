package com.demobejasb.integration;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;
import org.jspecify.annotations.Nullable;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * Stores the result of the most recent "When" step as an HTTP-like response (status code + JSON
 * body). Step definitions populate this by calling service methods directly and mapping outcomes
 * to the equivalent HTTP status codes, matching the behaviour that controllers produce.
 */
@Component
@Scope("cucumber-glue")
public class ResponseStore {

    private static final ObjectMapper MAPPER;

    static {
        ObjectMapper m = new ObjectMapper();
        // Register JSR310 module if available (Spring Boot includes it transitively)
        try {
            Class<?> moduleClass = Class.forName("com.fasterxml.jackson.datatype.jsr310.JavaTimeModule");
            m.registerModule((com.fasterxml.jackson.databind.Module) moduleClass.getDeclaredConstructor().newInstance());
            m.disable(com.fasterxml.jackson.databind.SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        } catch (Exception ignored) {
            // JSR310 module not available — fall back to default serialization
        }
        MAPPER = m;
    }

    private int statusCode;
    @Nullable
    private String body;

    public void setResponse(final int statusCode, final @Nullable Object bodyObject) {
        this.statusCode = statusCode;
        if (bodyObject == null) {
            this.body = null;
        } else if (bodyObject instanceof String s) {
            this.body = s;
        } else {
            try {
                this.body = MAPPER.writeValueAsString(bodyObject);
            } catch (Exception e) {
                throw new RuntimeException("Failed to serialise response body", e);
            }
        }
    }

    public void setResponse(final int statusCode) {
        this.statusCode = statusCode;
        this.body = null;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public String getBody() {
        final String b = body;
        if (b == null) {
            return "{}";
        }
        return b;
    }

    /**
     * Returns the response body parsed as a JSON object map.
     *
     * @return map of JSON fields
     */
    public Map<String, Object> getBodyAsMap() {
        try {
            String b = body;
            if (b == null || b.isBlank()) {
                return Map.of();
            }
            return MAPPER.readValue(b, new TypeReference<Map<String, Object>>() {});
        } catch (Exception e) {
            throw new RuntimeException("Failed to parse response body as map: " + body, e);
        }
    }

    public void clear() {
        this.statusCode = 0;
        this.body = null;
    }
}
