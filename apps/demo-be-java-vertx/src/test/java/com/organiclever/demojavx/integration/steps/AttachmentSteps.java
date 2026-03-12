package com.organiclever.demojavx.integration.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.multipart.MultipartForm;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Assertions;

public class AttachmentSteps {

    private static final int MAX_FILE_SIZE = 10 * 1024 * 1024; // 10MB

    private final ScenarioState state;

    public AttachmentSteps(ScenarioState state) {
        this.state = state;
    }

    @When("^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceUploadsFileToExpense(String filename, String contentType) throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getExpenseId();
        Assertions.assertNotNull(expenseId, "Expense ID must be set");

        byte[] content = createSampleContent(filename);
        MultipartForm form = MultipartForm.create()
                .binaryFileUpload("file", filename, Buffer.buffer(content), contentType);

        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/expenses/" + expenseId + "/attachments")
                .bearerTokenAuthentication(token)
                .sendMultipartForm(form)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
        if (response.statusCode() == 201) {
            state.setAttachmentId(response.bodyAsJsonObject().getString("id"));
        }
    }

    @When("^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    public void aliceUploadsFileToBobsExpense(String filename, String contentType) throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getBobExpenseId();
        Assertions.assertNotNull(expenseId, "Bob's expense ID must be set");

        byte[] content = createSampleContent(filename);
        MultipartForm form = MultipartForm.create()
                .binaryFileUpload("file", filename, Buffer.buffer(content), contentType);

        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/expenses/" + expenseId + "/attachments")
                .bearerTokenAuthentication(token)
                .sendMultipartForm(form)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice uploads an oversized file to POST /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceUploadsOversizedFile() throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getExpenseId();
        Assertions.assertNotNull(expenseId, "Expense ID must be set");

        byte[] oversizedContent = new byte[MAX_FILE_SIZE + 1024];
        MultipartForm form = MultipartForm.create()
                .binaryFileUpload("file", "big.jpg", Buffer.buffer(oversizedContent), "image/jpeg");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/expenses/" + expenseId + "/attachments")
                .bearerTokenAuthentication(token)
                .sendMultipartForm(form)
                .toCompletionStage()
                .toCompletableFuture()
                .get(10, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends GET /api/v1/expenses/\\{expenseId\\}/attachments$")
    public void aliceSendsGetAttachments() throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getExpenseId();
        Assertions.assertNotNull(expenseId, "Expense ID must be set");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/expenses/" + expenseId + "/attachments")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$")
    public void aliceSendsGetBobsAttachments() throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getBobExpenseId();
        Assertions.assertNotNull(expenseId, "Bob's expense ID must be set");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/expenses/" + expenseId + "/attachments")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{attachmentId\\}$")
    public void aliceSendsDeleteAttachment() throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getExpenseId();
        String attachmentId = state.getAttachmentId();
        Assertions.assertNotNull(expenseId, "Expense ID must be set");
        Assertions.assertNotNull(attachmentId, "Attachment ID must be set");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .delete("/api/v1/expenses/" + expenseId + "/attachments/" + attachmentId)
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$")
    public void aliceSendsDeleteBobsAttachment() throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getBobExpenseId();
        String attachmentId = state.getAttachmentId();
        Assertions.assertNotNull(expenseId, "Bob's expense ID must be set");
        Assertions.assertNotNull(attachmentId, "Attachment ID must be set");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .delete("/api/v1/expenses/" + expenseId + "/attachments/" + attachmentId)
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{randomAttachmentId\\}$")
    public void aliceSendsDeleteNonExistentAttachment() throws Exception {
        String token = state.getAccessToken();
        String expenseId = state.getExpenseId();
        Assertions.assertNotNull(expenseId, "Expense ID must be set");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .delete("/api/v1/expenses/" + expenseId + "/attachments/99999")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Given("alice has uploaded file {string} with content type {string} to the entry")
    public void aliceHasUploadedFileToEntry(String filename, String contentType) throws Exception {
        aliceUploadsFileToExpense(filename, contentType);
        HttpResponse<Buffer> resp = state.getLastResponse();
        if (resp != null && resp.statusCode() == 201) {
            state.setAttachmentId(resp.bodyAsJsonObject().getString("id"));
        }
    }

    @Given("^bob has created an entry with body \\{ \"amount\": \"([^\"]*)\", \"currency\": \"([^\"]*)\", \"category\": \"([^\"]*)\", \"description\": \"([^\"]*)\", \"date\": \"([^\"]*)\", \"type\": \"([^\"]*)\" \\}$")
    public void bobHasCreatedEntry(String amount, String currency, String category,
            String description, String date, String type) throws Exception {
        String bobToken = state.getBobAccessToken();
        Assertions.assertNotNull(bobToken, "Bob's access token must be set");
        JsonObject body = new JsonObject()
                .put("amount", amount)
                .put("currency", currency)
                .put("category", category)
                .put("description", description)
                .put("date", date)
                .put("type", type);
        HttpResponse<Buffer> resp = AppFactory.getClient()
                .post("/api/v1/expenses")
                .bearerTokenAuthentication(bobToken)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        Assertions.assertEquals(201, resp.statusCode(),
                "Expected 201 creating bob's entry but got " + resp.statusCode());
        state.setBobExpenseId(resp.bodyAsJsonObject().getString("id"));
    }

    @Then("the response body should contain {int} items in the {string} array")
    public void responseBodyContainsItemsInArray(int count, String field) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body);
        JsonArray arr = body.getJsonArray(field);
        Assertions.assertNotNull(arr, "Expected '" + field + "' array in response");
        Assertions.assertEquals(count, arr.size(),
                "Expected " + count + " items in '" + field + "' but got " + arr.size());
    }

    @Then("the response body should contain an attachment with {string} equal to {string}")
    public void responseBodyContainsAttachmentWithField(String field, String value) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body);
        JsonArray attachments = body.getJsonArray("attachments");
        Assertions.assertNotNull(attachments, "Expected 'attachments' array");
        boolean found = false;
        for (int i = 0; i < attachments.size(); i++) {
            JsonObject att = attachments.getJsonObject(i);
            if (value.equals(att.getString(field))) {
                found = true;
                break;
            }
        }
        Assertions.assertTrue(found,
                "Expected attachment with '" + field + "' = '" + value + "'");
    }

    private byte[] createSampleContent(String filename) {
        if (filename.endsWith(".jpg") || filename.endsWith(".jpeg")) {
            return new byte[]{(byte) 0xFF, (byte) 0xD8, (byte) 0xFF, (byte) 0xE0};
        } else if (filename.endsWith(".pdf")) {
            return "%PDF-1.4 sample".getBytes();
        } else {
            return "sample content".getBytes();
        }
    }
}
