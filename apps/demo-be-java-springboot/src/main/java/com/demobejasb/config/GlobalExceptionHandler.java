package com.demobejasb.config;

import com.demobejasb.attachment.FileSizeLimitExceededException;
import com.demobejasb.auth.service.AccountNotActiveException;
import com.demobejasb.auth.service.InvalidCredentialsException;
import com.demobejasb.auth.service.InvalidTokenException;
import com.demobejasb.auth.service.TokenExpiredException;
import com.demobejasb.auth.service.UsernameAlreadyExistsException;
import com.demobejasb.test.controller.UserNotFoundException;
import java.util.Map;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.multipart.MaxUploadSizeExceededException;
import org.springframework.web.server.ResponseStatusException;

@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(UsernameAlreadyExistsException.class)
    public ResponseEntity<Map<String, String>> handleDuplicate(
            final UsernameAlreadyExistsException ex) {
        return ResponseEntity.status(HttpStatus.CONFLICT)
                .body(Map.of("message", ex.getMessage()));
    }

    @ExceptionHandler(InvalidCredentialsException.class)
    public ResponseEntity<Map<String, String>> handleInvalidCredentials(
            final InvalidCredentialsException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("message", ex.getMessage()));
    }

    @ExceptionHandler(AccountNotActiveException.class)
    public ResponseEntity<Map<String, String>> handleAccountNotActive(
            final AccountNotActiveException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("message", "Account is deactivated or not active"));
    }

    @ExceptionHandler(TokenExpiredException.class)
    public ResponseEntity<Map<String, String>> handleTokenExpired(
            final TokenExpiredException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("message", "Token has expired"));
    }

    @ExceptionHandler(InvalidTokenException.class)
    public ResponseEntity<Map<String, String>> handleInvalidToken(
            final InvalidTokenException ex) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("message", "Invalid token"));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, String>> handleValidation(
            final MethodArgumentNotValidException ex) {
        String field = ex.getBindingResult().getFieldErrors().isEmpty()
                ? "unknown"
                : ex.getBindingResult().getFieldErrors().get(0).getField();
        return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body(Map.of("message", "Validation failed for field: " + field));
    }

    @ExceptionHandler(MaxUploadSizeExceededException.class)
    public ResponseEntity<Map<String, String>> handleMaxUploadSize(
            final MaxUploadSizeExceededException ex) {
        return ResponseEntity.status(HttpStatus.CONTENT_TOO_LARGE)
                .body(Map.of("message", "File size exceeds the maximum allowed limit"));
    }

    @ExceptionHandler(FileSizeLimitExceededException.class)
    public ResponseEntity<Map<String, String>> handleFileSizeLimit(
            final FileSizeLimitExceededException ex) {
        return ResponseEntity.status(HttpStatus.CONTENT_TOO_LARGE)
                .body(Map.of("message", "File size exceeds the maximum allowed limit"));
    }

    @ExceptionHandler(UserNotFoundException.class)
    public ResponseEntity<Map<String, String>> handleUserNotFound(
            final UserNotFoundException ex) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .body(Map.of("message", ex.getMessage()));
    }

    @ExceptionHandler(ResponseStatusException.class)
    public ResponseEntity<Map<String, String>> handleResponseStatus(
            final ResponseStatusException ex) {
        return ResponseEntity.status(ex.getStatusCode())
                .body(Map.of("message", ex.getReason() != null ? ex.getReason() : "Error"));
    }
}
