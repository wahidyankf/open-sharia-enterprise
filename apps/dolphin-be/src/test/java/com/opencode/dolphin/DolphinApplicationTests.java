package com.opencode.dolphin;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.resttestclient.autoconfigure.AutoConfigureTestRestTemplate;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.resttestclient.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for Dolphin Application.
 *
 * Tests verify that the Spring Boot application context loads successfully
 * and that actuator endpoints are accessible.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureTestRestTemplate
@ActiveProfiles("dev")
class DolphinApplicationTests {

    @LocalServerPort
    private int port;

    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * Test that the Spring Boot application context loads successfully.
     */
    @Test
    void contextLoads() {
        // This test will fail if the application context cannot be loaded
        assertThat(restTemplate).isNotNull();
    }

    /**
     * Test that the health check endpoint returns HTTP 200 OK with status "UP".
     */
    @Test
    void healthCheckEndpointReturnsOk() {
        String url = "http://localhost:" + port + "/actuator/health";
        ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody()).contains("\"status\":\"UP\"");
    }

    /**
     * Test that the health check endpoint includes groups (Spring Boot 4.0 format).
     */
    @Test
    void healthCheckIncludesGroups() {
        String url = "http://localhost:" + port + "/actuator/health";
        ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);

        assertThat(response.getBody()).contains("\"groups\"");
        assertThat(response.getBody()).contains("\"status\":\"UP\"");
    }

    /**
     * Test that the info endpoint is accessible.
     */
    @Test
    void infoEndpointIsAccessible() {
        String url = "http://localhost:" + port + "/actuator/info";
        ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }
}
