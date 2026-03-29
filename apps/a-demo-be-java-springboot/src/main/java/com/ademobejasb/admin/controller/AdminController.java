package com.ademobejasb.admin.controller;

import com.ademobejasb.auth.model.User;
import com.ademobejasb.auth.repository.UserRepository;
import com.ademobejasb.auth.service.AuthService;
import com.ademobejasb.contracts.DisableRequest;
import com.ademobejasb.contracts.PasswordResetResponse;
import com.ademobejasb.contracts.UserListResponse;
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
    public ResponseEntity<UserListResponse> listUsers(
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
        List<com.ademobejasb.contracts.User> data =
                users.getContent().stream().map(AuthService::buildUserResponse).toList();
        UserListResponse response = new UserListResponse();
        response.setContent(data);
        response.setTotalElements((int) users.getTotalElements());
        response.setTotalPages(users.getTotalPages());
        response.setPage(users.getNumber());
        response.setSize(size);
        return ResponseEntity.ok(response);
    }

    @PostMapping("/users/{id}/disable")
    public ResponseEntity<com.ademobejasb.contracts.User> disableUser(
            @PathVariable final UUID id,
            @RequestBody final DisableRequest request) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        user.setStatus("DISABLED");
        User saved = userRepository.save(user);
        return ResponseEntity.ok(AuthService.buildUserResponse(saved));
    }

    @PostMapping("/users/{id}/enable")
    public ResponseEntity<com.ademobejasb.contracts.User> enableUser(@PathVariable final UUID id) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        user.setStatus("ACTIVE");
        User saved = userRepository.save(user);
        return ResponseEntity.ok(AuthService.buildUserResponse(saved));
    }

    @PostMapping("/users/{id}/unlock")
    public ResponseEntity<com.ademobejasb.contracts.User> unlockUser(@PathVariable final UUID id) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        user.setStatus("ACTIVE");
        user.setFailedLoginAttempts(0);
        User saved = userRepository.save(user);
        return ResponseEntity.ok(AuthService.buildUserResponse(saved));
    }

    @PostMapping("/users/{id}/force-password-reset")
    public ResponseEntity<PasswordResetResponse> forcePasswordReset(
            @PathVariable final UUID id) {
        User user =
                userRepository
                        .findById(id)
                        .orElseThrow(() -> new RuntimeException("User not found"));
        String resetToken = UUID.randomUUID().toString();
        user.setPasswordResetToken(resetToken);
        userRepository.save(user);
        PasswordResetResponse response = new PasswordResetResponse();
        response.setToken(resetToken);
        return ResponseEntity.ok(response);
    }
}
