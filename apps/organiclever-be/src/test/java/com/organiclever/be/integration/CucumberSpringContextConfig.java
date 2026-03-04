package com.organiclever.be.integration;

import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.MOCK)
@ActiveProfiles("test")
@Import(CucumberSpringContextConfig.MockMvcConfig.class)
public class CucumberSpringContextConfig {

    /**
     * Provides MockMvc bean since @AutoConfigureMockMvc was removed in Spring Boot 4.0.
     * Uses the MOCK web environment's WebApplicationContext to build MockMvc.
     */
    @TestConfiguration
    static class MockMvcConfig {

        @Autowired
        private WebApplicationContext webApplicationContext;

        @Bean
        public MockMvc mockMvc() {
            return MockMvcBuilders.webAppContextSetup(webApplicationContext).build();
        }
    }
}
