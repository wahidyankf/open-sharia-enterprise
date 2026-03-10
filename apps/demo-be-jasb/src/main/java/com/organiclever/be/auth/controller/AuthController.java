package com.organiclever.be.auth.controller;

import com.organiclever.be.auth.dto.AuthResponse;
import com.organiclever.be.auth.dto.LoginRequest;
import com.organiclever.be.auth.dto.RefreshRequest;
import com.organiclever.be.auth.dto.RegisterRequest;
import com.organiclever.be.auth.dto.RegisterResponse;
import com.organiclever.be.auth.service.AccountNotActiveException;
import com.organiclever.be.auth.service.AuthService;
import com.organiclever.be.auth.service.InvalidCredentialsException;
import com.organiclever.be.auth.service.InvalidTokenException;
import com.organiclever.be.auth.service.UsernameAlreadyExistsException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/auth")
public class AuthController {

    private final AuthService authService;

    public AuthController(final AuthService authService) {
        this.authService = authService;
    }

    @PostMapping("/register")
    public ResponseEntity<RegisterResponse> register(
            @Valid @RequestBody final RegisterRequest request)
            throws UsernameAlreadyExistsException {
        return ResponseEntity.status(HttpStatus.CREATED).body(authService.register(request));
    }

    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(@Valid @RequestBody final LoginRequest request)
            throws InvalidCredentialsException, AccountNotActiveException {
        return ResponseEntity.ok(authService.login(request));
    }

    @PostMapping("/refresh")
    public ResponseEntity<AuthResponse> refresh(@Valid @RequestBody final RefreshRequest request)
            throws InvalidTokenException, AccountNotActiveException {
        return ResponseEntity.ok(authService.refresh(request.refreshToken()));
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(
            final HttpServletRequest request,
            @AuthenticationPrincipal final UserDetails userDetails) {
        String token = extractToken(request);
        if (token != null) {
            authService.logout(token);
        }
        return ResponseEntity.ok().build();
    }

    @PostMapping("/logout-all")
    public ResponseEntity<Void> logoutAll(
            final HttpServletRequest request,
            @AuthenticationPrincipal final UserDetails userDetails) {
        String token = extractToken(request);
        if (token != null && userDetails != null) {
            authService.logoutAll(token, userDetails.getUsername());
        }
        return ResponseEntity.ok().build();
    }

    private String extractToken(final HttpServletRequest request) {
        String header = request.getHeader("Authorization");
        if (header != null && header.startsWith("Bearer ")) {
            return header.substring(7);
        }
        return null;
    }
}
