package com.organiclever.be.integration.steps;

import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.testcontainers.postgresql.PostgreSQLContainer;

@Configuration
public class TestcontainersConfig {

    @Bean
    @ServiceConnection
    @SuppressWarnings("resource")
    static PostgreSQLContainer postgresContainer() {
        return new PostgreSQLContainer("postgres:17-alpine");
    }
}
