package com.organiclever.be.attachment.controller;

import com.organiclever.be.attachment.dto.AttachmentListResponse;
import com.organiclever.be.attachment.dto.AttachmentResponse;
import com.organiclever.be.attachment.model.Attachment;
import com.organiclever.be.attachment.repository.AttachmentRepository;
import com.organiclever.be.auth.model.User;
import com.organiclever.be.auth.repository.UserRepository;
import com.organiclever.be.expense.model.Expense;
import com.organiclever.be.expense.repository.ExpenseRepository;
import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;

@RestController
@RequestMapping("/api/v1/expenses/{expenseId}/attachments")
public class AttachmentController {

    private static final Set<String> ALLOWED_TYPES =
            Set.of("image/jpeg", "image/png", "application/pdf");
    private static final long MAX_SIZE = 10L * 1024 * 1024;

    private final AttachmentRepository attachmentRepository;
    private final ExpenseRepository expenseRepository;
    private final UserRepository userRepository;

    public AttachmentController(
            final AttachmentRepository attachmentRepository,
            final ExpenseRepository expenseRepository,
            final UserRepository userRepository) {
        this.attachmentRepository = attachmentRepository;
        this.expenseRepository = expenseRepository;
        this.userRepository = userRepository;
    }

    @PostMapping
    public ResponseEntity<AttachmentResponse> upload(
            @AuthenticationPrincipal final UserDetails userDetails,
            @PathVariable final UUID expenseId,
            @RequestParam("file") final MultipartFile file)
            throws IOException {
        String contentType = file.getContentType();
        if (contentType == null || !ALLOWED_TYPES.contains(contentType)) {
            throw new ResponseStatusException(
                    HttpStatus.UNSUPPORTED_MEDIA_TYPE, "Unsupported file type");
        }
        if (file.getSize() > MAX_SIZE) {
            throw new com.organiclever.be.attachment.FileSizeLimitExceededException();
        }

        Expense expense = getExpense(expenseId);
        User currentUser = getUser(userDetails);
        if (!expense.getUser().getId().equals(currentUser.getId())) {
            throw new ResponseStatusException(HttpStatus.FORBIDDEN, "Access denied");
        }

        String filename = file.getOriginalFilename() != null ? file.getOriginalFilename() : "file";
        Attachment attachment =
                new Attachment(expense, filename, contentType, file.getSize(), file.getBytes());
        Attachment saved = attachmentRepository.save(attachment);
        return ResponseEntity.status(HttpStatus.CREATED).body(AttachmentResponse.from(saved));
    }

    @GetMapping
    public ResponseEntity<AttachmentListResponse> list(
            @AuthenticationPrincipal final UserDetails userDetails,
            @PathVariable final UUID expenseId) {
        Expense expense = getExpense(expenseId);
        User currentUser = getUser(userDetails);
        if (!expense.getUser().getId().equals(currentUser.getId())) {
            throw new ResponseStatusException(HttpStatus.FORBIDDEN, "Access denied");
        }
        List<AttachmentResponse> attachments =
                attachmentRepository.findAllByExpense(expense).stream()
                        .map(AttachmentResponse::from)
                        .toList();
        return ResponseEntity.ok(new AttachmentListResponse(attachments));
    }

    @DeleteMapping("/{attachmentId}")
    public ResponseEntity<Void> delete(
            @AuthenticationPrincipal final UserDetails userDetails,
            @PathVariable final UUID expenseId,
            @PathVariable final UUID attachmentId) {
        Expense expense = getExpense(expenseId);
        User currentUser = getUser(userDetails);
        if (!expense.getUser().getId().equals(currentUser.getId())) {
            throw new ResponseStatusException(HttpStatus.FORBIDDEN, "Access denied");
        }
        Attachment attachment =
                attachmentRepository
                        .findByIdAndExpense(attachmentId, expense)
                        .orElseThrow(
                                () ->
                                        new ResponseStatusException(
                                                HttpStatus.NOT_FOUND, "Attachment not found"));
        attachmentRepository.delete(attachment);
        return ResponseEntity.noContent().build();
    }

    private Expense getExpense(final UUID expenseId) {
        return expenseRepository
                .findById(expenseId)
                .orElseThrow(
                        () -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Expense not found"));
    }

    private User getUser(final UserDetails userDetails) {
        return userRepository
                .findByUsername(userDetails.getUsername())
                .orElseThrow(() -> new RuntimeException("User not found"));
    }
}
