package com.demobejasb.user.controller;

import com.demobejasb.auth.model.User;
import com.demobejasb.auth.repository.UserRepository;
import com.demobejasb.auth.service.AuthService;
import com.demobejasb.auth.service.InvalidCredentialsException;
import com.demobejasb.contracts.ChangePasswordRequest;
import com.demobejasb.contracts.UpdateProfileRequest;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/users")
public class UserController {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;

    public UserController(
            final UserRepository userRepository, final PasswordEncoder passwordEncoder) {
        this.userRepository = userRepository;
        this.passwordEncoder = passwordEncoder;
    }

    @GetMapping("/me")
    public ResponseEntity<com.demobejasb.contracts.User> getProfile(
            @AuthenticationPrincipal final UserDetails userDetails) {
        User user = getUser(userDetails);
        return ResponseEntity.ok(AuthService.buildUserResponse(user));
    }

    @PatchMapping("/me")
    public ResponseEntity<com.demobejasb.contracts.User> updateProfile(
            @AuthenticationPrincipal final UserDetails userDetails,
            @Valid @RequestBody final UpdateProfileRequest request) {
        User user = getUser(userDetails);
        if (request.getDisplayName() != null) {
            user.setDisplayName(request.getDisplayName());
        }
        userRepository.save(user);
        return ResponseEntity.ok(AuthService.buildUserResponse(user));
    }

    @PostMapping("/me/password")
    public ResponseEntity<Void> changePassword(
            @AuthenticationPrincipal final UserDetails userDetails,
            @Valid @RequestBody final ChangePasswordRequest request)
            throws InvalidCredentialsException {
        User user = getUser(userDetails);
        if (!passwordEncoder.matches(request.getOldPassword(), user.getPasswordHash())) {
            throw new InvalidCredentialsException();
        }
        user.setPasswordHash(java.util.Objects.requireNonNull(passwordEncoder.encode(request.getNewPassword())));
        userRepository.save(user);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/me/deactivate")
    public ResponseEntity<Void> deactivate(
            @AuthenticationPrincipal final UserDetails userDetails) {
        User user = getUser(userDetails);
        user.setStatus("DISABLED");
        userRepository.save(user);
        return ResponseEntity.ok().build();
    }

    private User getUser(final UserDetails userDetails) {
        return userRepository
                .findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new RuntimeException("User not found"));
    }
}
