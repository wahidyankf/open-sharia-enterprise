import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";
import { MAX_ATTACHMENT_SIZE } from "@/lib/types";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/expenses/attachments.feature"),
);

async function createExpense(ctx: TestContext, username: string, body: Record<string, unknown>): Promise<string> {
  const resp = await ctx.client.dispatch("POST", "/api/v1/expenses", body, getAuth(ctx, username));
  if (resp.status !== 201) throw new Error(`Failed: ${JSON.stringify(resp.body)}`);
  return (resp.body as { id: string }).id;
}

async function uploadFile(
  ctx: TestContext,
  username: string,
  expenseId: string,
  filename: string,
  contentType: string,
  data?: Buffer,
): Promise<{ status: number; body: unknown }> {
  const fileData = data ?? Buffer.from("fake file content for testing");
  const resp = await ctx.client.dispatch(
    "POST",
    `/api/v1/expenses/${expenseId}/attachments`,
    null,
    getAuth(ctx, username),
    { filename, contentType, size: fileData.length, data: fileData },
  );
  return resp;
}

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given, And }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });

    And('a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    And('"alice" has logged in and stored the access token', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
    });

    And(
      'alice has created an entry with body { "amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.context.expenseId = await createExpense(ctx, "alice", {
          amount: "10.50",
          currency: "USD",
          category: "food",
          description: "Lunch",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );
  });

  Scenario("Upload JPEG image returns 201 with attachment metadata", ({ When, Then, And }) => {
    When(
      'alice uploads file "receipt.jpg" with content type "image/jpeg" to POST /api/v1/expenses/{expenseId}/attachments',
      async () => {
        ctx.response = await uploadFile(ctx, "alice", ctx.context.expenseId as string, "receipt.jpg", "image/jpeg");
        if (ctx.response.status === 201) {
          ctx.context.attachmentId = (ctx.response.body as { id: string }).id;
        }
      },
    );

    Then("the response status code should be 201", () => {
      expect(ctx.response!.status).toBe(201);
    });

    And('the response body should contain a non-null "id" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).id).toBeDefined();
    });

    And('the response body should contain "filename" equal to "receipt.jpg"', () => {
      expect((ctx.response!.body as Record<string, unknown>).filename).toBe("receipt.jpg");
    });

    And('the response body should contain "contentType" equal to "image/jpeg"', () => {
      expect((ctx.response!.body as Record<string, unknown>).contentType).toBe("image/jpeg");
    });

    And('the response body should contain a non-null "url" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).url).toBeDefined();
    });
  });

  Scenario("Upload PDF document returns 201 with attachment metadata", ({ When, Then, And }) => {
    When(
      'alice uploads file "invoice.pdf" with content type "application/pdf" to POST /api/v1/expenses/{expenseId}/attachments',
      async () => {
        ctx.response = await uploadFile(
          ctx,
          "alice",
          ctx.context.expenseId as string,
          "invoice.pdf",
          "application/pdf",
        );
      },
    );

    Then("the response status code should be 201", () => {
      expect(ctx.response!.status).toBe(201);
    });

    And('the response body should contain a non-null "id" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).id).toBeDefined();
    });

    And('the response body should contain "filename" equal to "invoice.pdf"', () => {
      expect((ctx.response!.body as Record<string, unknown>).filename).toBe("invoice.pdf");
    });

    And('the response body should contain "contentType" equal to "application/pdf"', () => {
      expect((ctx.response!.body as Record<string, unknown>).contentType).toBe("application/pdf");
    });

    And('the response body should contain a non-null "url" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).url).toBeDefined();
    });
  });

  Scenario("List attachments for an entry returns all uploaded files with metadata", ({ Given, And, When, Then }) => {
    Given('alice has uploaded file "receipt.jpg" with content type "image/jpeg" to the entry', async () => {
      const resp = await uploadFile(ctx, "alice", ctx.context.expenseId as string, "receipt.jpg", "image/jpeg");
      ctx.context.attachmentId = (resp.body as { id: string }).id;
    });

    And('alice has uploaded file "invoice.pdf" with content type "application/pdf" to the entry', async () => {
      await uploadFile(ctx, "alice", ctx.context.expenseId as string, "invoice.pdf", "application/pdf");
    });

    When("alice sends GET /api/v1/expenses/{expenseId}/attachments", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        `/api/v1/expenses/${ctx.context.expenseId}/attachments`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain 2 items in the "attachments" array', () => {
      const body = ctx.response!.body as { attachments: unknown[] };
      expect(body.attachments).toHaveLength(2);
    });

    And('the response body should contain an attachment with "filename" equal to "receipt.jpg"', () => {
      const body = ctx.response!.body as { attachments: { filename: string }[] };
      expect(body.attachments.some((a) => a.filename === "receipt.jpg")).toBe(true);
    });

    And('the response body should contain an attachment with "filename" equal to "invoice.pdf"', () => {
      const body = ctx.response!.body as { attachments: { filename: string }[] };
      expect(body.attachments.some((a) => a.filename === "invoice.pdf")).toBe(true);
    });
  });

  Scenario("Delete attachment returns 204", ({ Given, When, Then }) => {
    Given('alice has uploaded file "receipt.jpg" with content type "image/jpeg" to the entry', async () => {
      const resp = await uploadFile(ctx, "alice", ctx.context.expenseId as string, "receipt.jpg", "image/jpeg");
      ctx.context.attachmentId = (resp.body as { id: string }).id;
    });

    When("alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{attachmentId}", async () => {
      ctx.response = await ctx.client.dispatch(
        "DELETE",
        `/api/v1/expenses/${ctx.context.expenseId}/attachments/${ctx.context.attachmentId}`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 204", () => {
      expect(ctx.response!.status).toBe(204);
    });
  });

  Scenario("Upload unsupported file type returns 415", ({ When, Then, And }) => {
    When(
      'alice uploads file "malware.exe" with content type "application/octet-stream" to POST /api/v1/expenses/{expenseId}/attachments',
      async () => {
        ctx.response = await uploadFile(
          ctx,
          "alice",
          ctx.context.expenseId as string,
          "malware.exe",
          "application/octet-stream",
        );
      },
    );

    Then("the response status code should be 415", () => {
      expect(ctx.response!.status).toBe(415);
    });

    And('the response body should contain a validation error for "file"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("file");
    });
  });

  Scenario("Upload file exceeding the size limit returns 413", ({ When, Then, And }) => {
    When("alice uploads an oversized file to POST /api/v1/expenses/{expenseId}/attachments", async () => {
      const oversized = Buffer.alloc(MAX_ATTACHMENT_SIZE + 1, "x");
      ctx.response = await uploadFile(
        ctx,
        "alice",
        ctx.context.expenseId as string,
        "big.jpg",
        "image/jpeg",
        oversized,
      );
    });

    Then("the response status code should be 413", () => {
      expect(ctx.response!.status).toBe(413);
    });

    And("the response body should contain an error message about file size", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("size");
    });
  });

  Scenario("Upload attachment to another user's entry returns 403", ({ Given, And, When, Then }) => {
    Given('a user "bob" is registered with email "bob@example.com" and password "Str0ng#Pass2"', async () => {
      await registerUser(ctx, "bob", "bob@example.com", "Str0ng#Pass2");
      await loginUser(ctx, "bob", "Str0ng#Pass2");
    });

    And(
      'bob has created an entry with body { "amount": "25.00", "currency": "USD", "category": "transport", "description": "Taxi", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.context.bobExpenseId = await createExpense(ctx, "bob", {
          amount: "25.00",
          currency: "USD",
          category: "transport",
          description: "Taxi",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    When(
      'alice uploads file "receipt.jpg" with content type "image/jpeg" to POST /api/v1/expenses/{bobExpenseId}/attachments',
      async () => {
        ctx.response = await uploadFile(ctx, "alice", ctx.context.bobExpenseId as string, "receipt.jpg", "image/jpeg");
      },
    );

    Then("the response status code should be 403", () => {
      expect(ctx.response!.status).toBe(403);
    });
  });

  Scenario("List attachments on another user's entry returns 403", ({ Given, And, When, Then }) => {
    Given('a user "bob" is registered with email "bob@example.com" and password "Str0ng#Pass2"', async () => {
      await registerUser(ctx, "bob", "bob@example.com", "Str0ng#Pass2");
      await loginUser(ctx, "bob", "Str0ng#Pass2");
    });

    And(
      'bob has created an entry with body { "amount": "25.00", "currency": "USD", "category": "transport", "description": "Taxi", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.context.bobExpenseId = await createExpense(ctx, "bob", {
          amount: "25.00",
          currency: "USD",
          category: "transport",
          description: "Taxi",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/expenses/{bobExpenseId}/attachments", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        `/api/v1/expenses/${ctx.context.bobExpenseId}/attachments`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 403", () => {
      expect(ctx.response!.status).toBe(403);
    });
  });

  Scenario("Delete attachment on another user's entry returns 403", ({ Given, And, When, Then }) => {
    Given('a user "bob" is registered with email "bob@example.com" and password "Str0ng#Pass2"', async () => {
      await registerUser(ctx, "bob", "bob@example.com", "Str0ng#Pass2");
      await loginUser(ctx, "bob", "Str0ng#Pass2");
    });

    And(
      'bob has created an entry with body { "amount": "25.00", "currency": "USD", "category": "transport", "description": "Taxi", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.context.bobExpenseId = await createExpense(ctx, "bob", {
          amount: "25.00",
          currency: "USD",
          category: "transport",
          description: "Taxi",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    And('alice has uploaded file "receipt.jpg" with content type "image/jpeg" to the entry', async () => {
      // Upload to alice's own expense, not bob's
      const resp = await uploadFile(ctx, "alice", ctx.context.expenseId as string, "receipt.jpg", "image/jpeg");
      ctx.context.attachmentId = (resp.body as { id: string }).id;
    });

    When("alice sends DELETE /api/v1/expenses/{bobExpenseId}/attachments/{attachmentId}", async () => {
      ctx.response = await ctx.client.dispatch(
        "DELETE",
        `/api/v1/expenses/${ctx.context.bobExpenseId}/attachments/${ctx.context.attachmentId}`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 403", () => {
      expect(ctx.response!.status).toBe(403);
    });
  });

  Scenario("Delete non-existent attachment returns 404", ({ Given, When, Then }) => {
    Given('alice has uploaded file "receipt.jpg" with content type "image/jpeg" to the entry', async () => {
      const resp = await uploadFile(ctx, "alice", ctx.context.expenseId as string, "receipt.jpg", "image/jpeg");
      ctx.context.attachmentId = (resp.body as { id: string }).id;
    });

    When("alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{randomAttachmentId}", async () => {
      ctx.response = await ctx.client.dispatch(
        "DELETE",
        `/api/v1/expenses/${ctx.context.expenseId}/attachments/${crypto.randomUUID()}`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 404", () => {
      expect(ctx.response!.status).toBe(404);
    });
  });
});
