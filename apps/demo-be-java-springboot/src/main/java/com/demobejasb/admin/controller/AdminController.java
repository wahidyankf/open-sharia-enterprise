package com.demobejasb.admin.controller;

import com.demobejasb.admin.dto.AdminPasswordResetResponse;
import com.demobejasb.admin.dto.AdminUserListResponse;
import com.demobejasb.admin.dto.AdminUserResponse;
import com.demobejasb.admin.dto.DisableUserRequest;
import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import java.util.List;
import java.util.UUID;
import org.jspecify.annotations.Nullable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/admin")
public class AdminController {

    private final UserRepository userRepository;

    public AdminController(final UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @GetMapping("/users")
    public ResponseEntity<AdminUserListResponse> listUsers(
            @RequestParam(required = false) @Nullable String search,
            @RequestParam(defaultValue = "0") final int page,
            @RequestParam(defaultValue = "20") final int size) {
        PageRequest pageRequest = PageRequest.of(page, size, Sort.by("createdAt"));
        Page<User> users;
        if (search != null && !search.isBlank()) {
            users = userRepository.findAllByEmailOrUsernameContaining(search, pageRequest);
        } else {
            users = userRepository.findAll(pageRequest);
        }
        List<AdminUserResponse> data =
                users.getContent().stream().map(AdminUserResponse::from).toList();
        return ResponseEntity.ok(
                new AdminUserListResponse(data, users.getTotalElements(), users.getNumber()));
    }

    @PostMapping("/users/{id}/disable")
    public ResponseEntity<AdminUserResponse> disableUser(
            @PathVariable final UUID id,
            @RequestBody final DisableUserRequest request) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        user.setStatus("DISABLED");
        User saved = userRepository.save(user);
        return ResponseEntity.ok(AdminUserResponse.from(saved));
    }

    @PostMapping("/users/{id}/enable")
    public ResponseEntity<AdminUserResponse> enableUser(@PathVariable final UUID id) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        user.setStatus("ACTIVE");
        User saved = userRepository.save(user);
        return ResponseEntity.ok(AdminUserResponse.from(saved));
    }

    @PostMapping("/users/{id}/unlock")
    public ResponseEntity<AdminUserResponse> unlockUser(@PathVariable final UUID id) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        user.setStatus("ACTIVE");
        user.setFailedLoginAttempts(0);
        User saved = userRepository.save(user);
        return ResponseEntity.ok(AdminUserResponse.from(saved));
    }

    @PostMapping("/users/{id}/force-password-reset")
    public ResponseEntity<AdminPasswordResetResponse> forcePasswordReset(
            @PathVariable final UUID id) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        String resetToken = UUID.randomUUID().toString();
        user.setPasswordResetToken(resetToken);
        userRepository.save(user);
        return ResponseEntity.ok(new AdminPasswordResetResponse(resetToken));
    }
}
