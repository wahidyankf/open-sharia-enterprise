import { type Page, type Locator } from "@playwright/test";

export class HomePage {
  readonly page: Page;
  readonly appTitle: Locator;
  readonly fetchHelloButton: Locator;
  readonly retryButton: Locator;
  readonly apiResponseLabel: Locator;

  constructor(page: Page) {
    this.page = page;
    this.appTitle = page.getByText("OrganicLever").first();
    this.fetchHelloButton = page.getByRole("button", { name: "Fetch Hello" });
    this.retryButton = page.getByRole("button", { name: "Retry" });
    this.apiResponseLabel = page.getByText("API Response:");
  }

  async goto() {
    await this.page.goto("/");
    await this.page.waitForLoadState("networkidle");
  }
}
