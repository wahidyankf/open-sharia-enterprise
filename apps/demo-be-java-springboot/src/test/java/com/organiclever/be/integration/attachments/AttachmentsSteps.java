package com.organiclever.be.integration.attachments;

import com.organiclever.be.attachment.dto.AttachmentListResponse;
import com.organiclever.be.attachment.dto.AttachmentResponse;
import com.organiclever.be.attachment.model.Attachment;
import com.organiclever.be.attachment.repository.AttachmentRepository;
import com.organiclever.be.auth.dto.AuthResponse;
import com.organiclever.be.auth.model.User;
import com.organiclever.be.auth.repository.UserRepository;
import com.organiclever.be.auth.service.AccountNotActiveException;
import com.organiclever.be.auth.service.AuthService;
import com.organiclever.be.auth.service.InvalidCredentialsException;
import com.organiclever.be.expense.model.Expense;
import com.organiclever.be.expense.repository.ExpenseRepository;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.AuthSteps;
import com.organiclever.be.integration.steps.ExpenseStepHelper;
import com.organiclever.be.integration.steps.TokenStore;
import com.organiclever.be.security.JwtUtil;
import com.jayway.jsonpath.JsonPath;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;

import static org.assertj.core.api.Assertions.assertThat;

@Scope("cucumber-glue")
public class AttachmentsSteps {

    private static final Set<String> ALLOWED_TYPES =
            Set.of("image/jpeg", "image/png", "application/pdf");
    private static final long MAX_SIZE = 10L * 1024 * 1024;

    @Autowired
    private AttachmentRepository attachmentRepository;

    @Autowired
    private ExpenseRepository expenseRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AuthService authService;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Autowired
    private JwtUtil jwtUtil;

    @Autowired
    private ExpenseStepHelper expenseHelper;

    @Autowired
    private AuthSteps authSteps;

    @Given("^alice has created an entry with body (.*)$")
    public void aliceHasCreatedAnEntryWithBody(final String body) {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        UUID id = expenseHelper.createExpenseOrFail(token, body);
        tokenStore.setExpenseId(id);
    }

    @When("^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceUploadsFileToExpense(final String filename, final String contentType) {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        byte[] fileContent = ("dummy content for " + filename).getBytes();
        UUID savedId = uploadAttachment(token, expenseId, filename, contentType, fileContent);
        if (savedId != null) {
            tokenStore.setAttachmentId(savedId);
        }
    }

    @Given("alice has uploaded file {string} with content type {string} to the entry")
    public void aliceHasUploadedFileToEntry(final String filename, final String contentType) {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        byte[] fileContent = ("dummy content for " + filename).getBytes();
        UUID savedId = uploadAttachmentOrFail(token, expenseId, filename, contentType, fileContent);
        tokenStore.setAttachmentId(savedId);
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceSendsGetAttachments() {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        listAttachments(token, expenseId);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{attachmentId\\}$")
    public void aliceDeletesAttachment() {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        UUID attachmentId = tokenStore.getAttachmentId();
        if (token == null || expenseId == null || attachmentId == null) {
            throw new IllegalStateException("Token, expense ID, or attachment ID not stored");
        }
        deleteAttachment(token, expenseId, attachmentId);
    }

    @When("^alice uploads an oversized file to POST /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceUploadsOversizedFile() {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        byte[] largeContent = new byte[11 * 1024 * 1024]; // 11 MB
        java.util.Arrays.fill(largeContent, (byte) 'x');
        uploadAttachment(token, expenseId, "large.jpg", "image/jpeg", largeContent);
    }

    @Then("the response body should contain {int} items in the {string} array")
    public void theResponseBodyShouldContainItemsInArray(final int count, final String field) {
        Map<String, Object> body = responseStore.getBodyAsMap();
        assertThat(body).containsKey(field);
        Object value = body.get(field);
        assertThat(value).isInstanceOf(List.class);
        List<?> list = (List<?>) value;
        assertThat(list).hasSize(count);
    }

    @Then("the response body should contain an attachment with {string} equal to {string}")
    public void theResponseBodyShouldContainAttachmentWithFieldEqual(
            final String field, final String value) {
        String bodyJson = responseStore.getBody();
        List<String> values = JsonPath.read(bodyJson, "$.attachments[*]." + field);
        assertThat(values).contains(value);
    }

    @Then("the response body should contain an error message about file size")
    public void theResponseBodyShouldContainErrorMessageAboutFileSize() {
        assertThat(responseStore.getBody()).containsIgnoringCase("size");
    }

    @Given("^bob has created an entry with body (.*)$")
    public void bobHasCreatedAnEntryWithBody(final String body) {
        // Register bob if not already registered
        if (userRepository.findByUsername("bob").isEmpty()) {
            authSteps.registerOrFail("bob", "bob@example.com", "Str0ng#Pass2");
        }
        // Login bob
        AuthResponse bobAuth;
        try {
            bobAuth = authService.login(
                    new com.organiclever.be.auth.dto.LoginRequest("bob", "Str0ng#Pass2"));
        } catch (InvalidCredentialsException | AccountNotActiveException e) {
            throw new RuntimeException("Failed to login as bob: " + e.getMessage(), e);
        }
        // Create expense as bob
        UUID id = expenseHelper.createExpenseOrFail(bobAuth.accessToken(), body);
        tokenStore.setBobExpenseId(id);
    }

    @When("^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    public void aliceUploadsFileToBobsExpense(final String filename, final String contentType) {
        String token = tokenStore.getToken();
        UUID bobExpenseId = tokenStore.getBobExpenseId();
        if (token == null || bobExpenseId == null) {
            throw new IllegalStateException("Token or bob expense ID not stored");
        }
        byte[] fileContent = ("content for " + filename).getBytes();
        uploadAttachment(token, bobExpenseId, filename, contentType, fileContent);
    }

    @When("^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    public void aliceSendsGetAttachmentsOnBobsExpense() {
        String token = tokenStore.getToken();
        UUID bobExpenseId = tokenStore.getBobExpenseId();
        if (token == null || bobExpenseId == null) {
            throw new IllegalStateException("Token or bob expense ID not stored");
        }
        listAttachments(token, bobExpenseId);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$")
    public void aliceDeletesAttachmentOnBobsExpense() {
        String token = tokenStore.getToken();
        UUID bobExpenseId = tokenStore.getBobExpenseId();
        UUID attachmentId = tokenStore.getAttachmentId();
        if (token == null || bobExpenseId == null || attachmentId == null) {
            throw new IllegalStateException("Token, bob expense ID, or attachment ID not stored");
        }
        deleteAttachment(token, bobExpenseId, attachmentId);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{randomAttachmentId\\}$")
    public void aliceDeletesNonExistentAttachment() {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        UUID randomId = UUID.randomUUID();
        deleteAttachment(token, expenseId, randomId);
    }

    // ============================================================
    // Internal helpers
    // ============================================================

    /**
     * Uploads an attachment and stores the result in ResponseStore. Returns the attachment ID on
     * success, or null on failure.
     */
    private UUID uploadAttachment(
            final String token, final UUID expenseId,
            final String filename, final String contentType, final byte[] fileContent) {
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return null;
        }
        String username = jwtUtil.extractUsername(token);
        User currentUser = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));

        Expense expense = expenseRepository.findById(expenseId).orElse(null);
        if (expense == null) {
            responseStore.setResponse(404, Map.of("message", "Expense not found"));
            return null;
        }
        if (!expense.getUser().getId().equals(currentUser.getId())) {
            responseStore.setResponse(403, Map.of("message", "Access denied"));
            return null;
        }
        if (!ALLOWED_TYPES.contains(contentType)) {
            responseStore.setResponse(415, Map.of("message", "Unsupported file type"));
            return null;
        }
        if (fileContent.length > MAX_SIZE) {
            responseStore.setResponse(413, Map.of("message", "File size exceeds the maximum allowed limit"));
            return null;
        }
        Attachment attachment = new Attachment(expense, filename, contentType, fileContent.length, fileContent);
        Attachment saved = attachmentRepository.save(attachment);
        responseStore.setResponse(201, AttachmentResponse.from(saved));
        return saved.getId();
    }

    /**
     * Uploads an attachment and throws if it fails (used in Given steps).
     */
    private UUID uploadAttachmentOrFail(
            final String token, final UUID expenseId,
            final String filename, final String contentType, final byte[] fileContent) {
        UUID id = uploadAttachment(token, expenseId, filename, contentType, fileContent);
        if (id == null) {
            throw new RuntimeException(
                    "Unexpected attachment upload failure: " + responseStore.getBody());
        }
        return id;
    }

    private void listAttachments(final String token, final UUID expenseId) {
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User currentUser = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));

        Expense expense = expenseRepository.findById(expenseId).orElse(null);
        if (expense == null) {
            responseStore.setResponse(404, Map.of("message", "Expense not found"));
            return;
        }
        if (!expense.getUser().getId().equals(currentUser.getId())) {
            responseStore.setResponse(403, Map.of("message", "Access denied"));
            return;
        }
        List<AttachmentResponse> attachments = attachmentRepository.findAllByExpense(expense)
                .stream()
                .map(AttachmentResponse::from)
                .toList();
        responseStore.setResponse(200, new AttachmentListResponse(attachments));
    }

    private void deleteAttachment(
            final String token, final UUID expenseId, final UUID attachmentId) {
        if (!jwtUtil.isTokenValid(token)) {
            responseStore.setResponse(401, Map.of("message", "Unauthorized"));
            return;
        }
        String username = jwtUtil.extractUsername(token);
        User currentUser = userRepository.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("User not found"));

        Expense expense = expenseRepository.findById(expenseId).orElse(null);
        if (expense == null) {
            responseStore.setResponse(404, Map.of("message", "Expense not found"));
            return;
        }
        if (!expense.getUser().getId().equals(currentUser.getId())) {
            responseStore.setResponse(403, Map.of("message", "Access denied"));
            return;
        }
        attachmentRepository.findByIdAndExpense(attachmentId, expense).ifPresentOrElse(
                attachment -> {
                    attachmentRepository.delete(attachment);
                    responseStore.setResponse(204);
                },
                () -> responseStore.setResponse(404, Map.of("message", "Attachment not found")));
    }
}
