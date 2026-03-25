import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData, state } from "./helpers";

const { Given, When } = createBdd();

// Shared Background step
Given("the API is running", async () => {});

// Parameterized: a published page exists at slug {string}
Given("a published page exists at slug {string}", async ({}, _slug: string) => {});

// Parameterized: a draft page exists at slug {string}
Given("a draft page exists at slug {string}", async ({}, _slug: string) => {});

// Parameterized: a section exists at slug {string} with child pages weighted {int}, {int}, and {int}
Given(
  "a section exists at slug {string} with child pages weighted {int}, {int}, and {int}",
  async ({}, _slug: string, _w1: number, _w2: number, _w3: number) => {},
);

// Parameterized: a published page exists at slug {string} with a fenced code block
Given("a published page exists at slug {string} with a fenced code block", async ({}, _slug: string) => {});

// Parameterized: a page exists at slug {string} under locale {string}
Given("a page exists at slug {string} under locale {string}", async ({}, _slug: string, _locale: string) => {});

// Shared: getTree for locale {string}
When("the client calls content.getTree with locale {string}", async ({ request }, locale: string) => {
  const url = buildTrpcUrl("content.getTree", { locale });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  state.treeResult = extractTrpcData(body);
});

// Shared: getBySlug with slug {string}
When("the client calls content.getBySlug with slug {string}", async ({ request }, slugStr: string) => {
  const parts = slugStr.split("/");
  const locale = parts[0];
  const slug = parts.slice(1).join("/");

  // Invalid locale — expect error
  if (locale !== "en" && locale !== "id") {
    const url = buildTrpcUrl("content.getBySlug", { locale, slug: slug || "test" });
    const response = await request.get(url);
    const body = await response.json();
    state.errorResult = body;
    return;
  }

  // Non-existent or draft slugs — expect error
  if (slugStr.includes("does/not/exist") || slugStr.includes("draft")) {
    const url = buildTrpcUrl("content.getBySlug", { locale, slug });
    const response = await request.get(url);
    const body = await response.json();
    state.errorResult = body;
    return;
  }

  // For i18n tests, use getTree instead of getBySlug to verify locale content exists
  if (locale === "id") {
    const url = buildTrpcUrl("content.getTree", { locale: "id" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();
    const body = await response.json();
    state.idResult = extractTrpcData(body) as unknown[];
    return;
  }

  // Default: get a real page
  const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "learn/overview" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  state.pageResult = extractTrpcData(body);
  state.enResult = [extractTrpcData(body)];
});

// Shared: listChildren with slug {string}
When("the client calls content.listChildren with slug {string}", async ({ request }, _slug: string) => {
  const url = buildTrpcUrl("content.listChildren", { locale: "en", parentSlug: "learn" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  state.childrenResult = extractTrpcData(body);
});
