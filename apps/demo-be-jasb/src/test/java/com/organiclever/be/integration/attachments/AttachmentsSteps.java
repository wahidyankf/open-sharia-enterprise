package com.organiclever.be.integration.attachments;

import com.jayway.jsonpath.JsonPath;
import com.organiclever.be.integration.ResponseStore;
import com.organiclever.be.integration.steps.TokenStore;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

@Scope("cucumber-glue")
public class AttachmentsSteps {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ResponseStore responseStore;

    @Autowired
    private TokenStore tokenStore;

    @Given("^alice has created an entry with body (.*)$")
    public void aliceHasCreatedAnEntryWithBody(final String body) throws Exception {
        String token = tokenStore.getToken();
        if (token == null) {
            throw new IllegalStateException("Token not stored");
        }
        MvcResult result = mockMvc.perform(
                post("/api/v1/expenses")
                        .header("Authorization", "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
        tokenStore.setExpenseId(UUID.fromString(id));
    }

    @When("^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceUploadsFileToExpense(final String filename, final String contentType)
            throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        byte[] fileContent = ("dummy content for " + filename).getBytes();
        MockMultipartFile file = new MockMultipartFile("file", filename, contentType, fileContent);
        MvcResult result = mockMvc.perform(
                multipart("/api/v1/expenses/" + expenseId + "/attachments")
                        .file(file)
                        .header("Authorization", "Bearer " + token))
                .andReturn();
        responseStore.setResult(result);
        if (result.getResponse().getStatus() == 201) {
            String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
            tokenStore.setAttachmentId(UUID.fromString(id));
        }
    }

    @Given("alice has uploaded file {string} with content type {string} to the entry")
    public void aliceHasUploadedFileToEntry(final String filename, final String contentType)
            throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        byte[] fileContent = ("dummy content for " + filename).getBytes();
        MockMultipartFile file = new MockMultipartFile("file", filename, contentType, fileContent);
        MvcResult result = mockMvc.perform(
                multipart("/api/v1/expenses/" + expenseId + "/attachments")
                        .file(file)
                        .header("Authorization", "Bearer " + token))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
        tokenStore.setAttachmentId(UUID.fromString(id));
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceSendsGetAttachments() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/expenses/" + expenseId + "/attachments")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{attachmentId\\}$")
    public void aliceDeletesAttachment() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        UUID attachmentId = tokenStore.getAttachmentId();
        if (token == null || expenseId == null || attachmentId == null) {
            throw new IllegalStateException("Token, expense ID, or attachment ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        delete("/api/v1/expenses/" + expenseId + "/attachments/" + attachmentId)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice uploads an oversized file to POST /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceUploadsOversizedFile() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        // Create a file larger than 10MB
        byte[] largeContent = new byte[11 * 1024 * 1024]; // 11 MB
        java.util.Arrays.fill(largeContent, (byte) 'x');
        MockMultipartFile file = new MockMultipartFile("file", "large.jpg", "image/jpeg", largeContent);
        responseStore.setResult(
                mockMvc.perform(
                        multipart("/api/v1/expenses/" + expenseId + "/attachments")
                                .file(file)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @Then("the response body should contain {int} items in the {string} array")
    public void theResponseBodyShouldContainItemsInArray(final int count, final String field)
            throws Exception {
        MockMvcResultMatchers.jsonPath("$." + field).isArray()
                .match(responseStore.getResult());
        // Check the size
        String body = responseStore.getResult().getResponse().getContentAsString();
        java.util.List<?> items = JsonPath.read(body, "$." + field);
        assertThat(items).hasSize(count);
    }

    @Then("the response body should contain an attachment with {string} equal to {string}")
    public void theResponseBodyShouldContainAttachmentWithFieldEqual(
            final String field, final String value) throws Exception {
        String body = responseStore.getResult().getResponse().getContentAsString();
        java.util.List<String> values = JsonPath.read(body, "$.attachments[*]." + field);
        assertThat(values).contains(value);
    }

    @Then("the response body should contain an error message about file size")
    public void theResponseBodyShouldContainErrorMessageAboutFileSize() throws Exception {
        final String body = responseStore.getResult().getResponse().getContentAsString();
        assertThat(body).containsIgnoringCase("size");
    }

    @Given("^bob has created an entry with body (.*)$")
    public void bobHasCreatedAnEntryWithBody(final String body) throws Exception {
        // Bob registers and logs in
        // Register bob if not registered
        mockMvc.perform(
                post("/api/v1/auth/register")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"username\":\"bob\",\"email\":\"bob@example.com\",\"password\":\"Str0ng#Pass2\"}"))
                .andReturn(); // May fail if already registered, that's ok
        // Login bob
        MvcResult loginResult = mockMvc.perform(
                post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"username\":\"bob\",\"password\":\"Str0ng#Pass2\"}"))
                .andReturn();
        String bobToken = JsonPath.read(loginResult.getResponse().getContentAsString(), "$.access_token");
        // Create expense as bob
        MvcResult result = mockMvc.perform(
                post("/api/v1/expenses")
                        .header("Authorization", "Bearer " + bobToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(MockMvcResultMatchers.status().isCreated())
                .andReturn();
        String id = JsonPath.read(result.getResponse().getContentAsString(), "$.id");
        tokenStore.setBobExpenseId(UUID.fromString(id));
    }

    @When("^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    public void aliceUploadsFileToBobsExpense(final String filename, final String contentType)
            throws Exception {
        String token = tokenStore.getToken();
        UUID bobExpenseId = tokenStore.getBobExpenseId();
        if (token == null || bobExpenseId == null) {
            throw new IllegalStateException("Token or bob expense ID not stored");
        }
        byte[] fileContent = ("content for " + filename).getBytes();
        MockMultipartFile file = new MockMultipartFile("file", filename, contentType, fileContent);
        responseStore.setResult(
                mockMvc.perform(
                        multipart("/api/v1/expenses/" + bobExpenseId + "/attachments")
                                .file(file)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    public void aliceSendsGetAttachmentsOnBobsExpense() throws Exception {
        String token = tokenStore.getToken();
        UUID bobExpenseId = tokenStore.getBobExpenseId();
        if (token == null || bobExpenseId == null) {
            throw new IllegalStateException("Token or bob expense ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        get("/api/v1/expenses/" + bobExpenseId + "/attachments")
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$")
    public void aliceDeletesAttachmentOnBobsExpense() throws Exception {
        String token = tokenStore.getToken();
        UUID bobExpenseId = tokenStore.getBobExpenseId();
        UUID attachmentId = tokenStore.getAttachmentId();
        if (token == null || bobExpenseId == null || attachmentId == null) {
            throw new IllegalStateException("Token, bob expense ID, or attachment ID not stored");
        }
        responseStore.setResult(
                mockMvc.perform(
                        delete("/api/v1/expenses/" + bobExpenseId + "/attachments/" + attachmentId)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{randomAttachmentId\\}$")
    public void aliceDeletesNonExistentAttachment() throws Exception {
        String token = tokenStore.getToken();
        UUID expenseId = tokenStore.getExpenseId();
        if (token == null || expenseId == null) {
            throw new IllegalStateException("Token or expense ID not stored");
        }
        UUID randomId = UUID.randomUUID();
        responseStore.setResult(
                mockMvc.perform(
                        delete("/api/v1/expenses/" + expenseId + "/attachments/" + randomId)
                                .header("Authorization", "Bearer " + token))
                        .andReturn());
    }

    // Note: "the response body should contain a validation error for {string}"
    // is provided by AuthSteps (in integration.steps package)
}
