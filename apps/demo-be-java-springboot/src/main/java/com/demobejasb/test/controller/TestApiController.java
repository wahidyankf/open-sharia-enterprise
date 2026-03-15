package com.demobejasb.test.controller;

import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import java.util.Map;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Test-only API controller for database reset and user promotion.
 *
 * <p>Only active when {@code app.test-api.enabled=true} (set via the {@code ENABLE_TEST_API}
 * environment variable). These endpoints must never be exposed in production.
 */
@RestController
@RequestMapping("/api/v1/test")
@ConditionalOnProperty(name = "app.test-api.enabled", havingValue = "true")
public class TestApiController {

    private final JdbcTemplate jdbcTemplate;
    private final UserRepository userRepository;

    public TestApiController(
            final JdbcTemplate jdbcTemplate, final UserRepository userRepository) {
        this.jdbcTemplate = jdbcTemplate;
        this.userRepository = userRepository;
    }

    /**
     * Deletes all user-created data from the database.
     *
     * <p>Deletion order respects foreign-key constraints:
     * attachments → expenses → refresh_tokens → revoked_tokens → users.
     *
     * @return 200 OK with a confirmation message
     */
    @PostMapping("/reset-db")
    public ResponseEntity<Map<String, String>> resetDb() {
        jdbcTemplate.execute("DELETE FROM attachments");
        jdbcTemplate.execute("DELETE FROM expenses");
        jdbcTemplate.execute("DELETE FROM refresh_tokens");
        jdbcTemplate.execute("DELETE FROM revoked_tokens");
        jdbcTemplate.execute("DELETE FROM users");
        return ResponseEntity.ok(Map.of("message", "Database reset successful"));
    }

    /**
     * Promotes an existing user to the {@code ADMIN} role.
     *
     * @param body request body containing {@code username}
     * @return 200 OK with a confirmation message, or 404 if the user is not found
     */
    @PostMapping("/promote-admin")
    public ResponseEntity<Map<String, String>> promoteAdmin(
            @RequestBody final Map<String, String> body) {
        String username = body.getOrDefault("username", "");
        User user =
                userRepository
                        .findByUsername(username)
                        .orElseThrow(
                                () -> new UserNotFoundException("User not found: " + username));
        user.setRole("ADMIN");
        userRepository.save(user);
        return ResponseEntity.ok(Map.of("message", "User " + username + " promoted to ADMIN"));
    }
}
