package com.opencode.dolphin;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Main application class for Dolphin Learning Management System Backend.
 *
 * This application provides backend services for the LMS platform,
 * including user management, course management, and progress tracking.
 */
@SpringBootApplication
public class DolphinApplication {

    public static void main(String[] args) {
        SpringApplication.run(DolphinApplication.class, args);
    }
}
