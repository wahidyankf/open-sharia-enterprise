package com.organiclever.be.integration;

import com.organiclever.be.auth.model.User;
import com.organiclever.be.config.JpaAuditingConfig;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.AuditorAware;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Covers branches that Cucumber scenarios cannot reach:
 * - User protected no-arg JPA constructor + nullable soft-delete getters
 * - JpaAuditingConfig.auditorProvider() authenticated branch
 * - UserDetailsServiceImpl.loadUserByUsername() UsernameNotFoundException branch
 */
@SpringBootTest
@ActiveProfiles("test")
@TestPropertySource(
    properties = {
        "spring.datasource.url="
            + "jdbc:h2:mem:testdb_coverit;DB_CLOSE_DELAY=-1;"
            + "MODE=PostgreSQL;DATABASE_TO_UPPER=false"
    })
class CoverageIT {

    @Autowired
    private UserDetailsService userDetailsService;

    @Test
    void userNoArgConstructorAndNullableGetters() {
        // Subclass to reach the protected no-arg constructor required by JPA.
        class TestUser extends User {}
        User user = new TestUser();
        assertThat(user.getDeletedAt()).isNull();
        assertThat(user.getDeletedBy()).isNull();
    }

    @Test
    void auditorProvider_returnsUsername_whenAuthenticated() {
        JpaAuditingConfig config = new JpaAuditingConfig();
        AuditorAware<String> auditor = config.auditorProvider();
        UsernamePasswordAuthenticationToken auth =
                new UsernamePasswordAuthenticationToken("coveruser", null, List.of());
        SecurityContextHolder.getContext().setAuthentication(auth);
        try {
            assertThat(auditor.getCurrentAuditor()).contains("coveruser");
        } finally {
            SecurityContextHolder.clearContext();
        }
    }

    @Test
    void loadUserByUsername_throwsUsernameNotFoundException_whenUserNotFound() {
        assertThatThrownBy(() -> userDetailsService.loadUserByUsername("nosuchuser"))
                .isInstanceOf(UsernameNotFoundException.class);
    }
}
