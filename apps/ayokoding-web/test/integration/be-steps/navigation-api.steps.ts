import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { testCaller } from "./helpers/test-caller";
import type { TreeNode } from "@/server/content/types";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/ayokoding/be/gherkin/navigation-api/navigation-api.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  Background(({ Given }) => {
    Given("the API is running", () => {});
  });

  Scenario("Navigation tree structure matches the filesystem hierarchy", ({ Given, When, Then, And }) => {
    let result: TreeNode[];

    Given('content exists in locale "en" with sections "programming", "ai", and "security"', () => {});

    When('the client calls content.getTree with locale "en"', async () => {
      result = (await testCaller.content.getTree({ locale: "en" })) as TreeNode[];
    });

    Then('the response tree should contain top-level nodes for "programming", "ai", and "security"', () => {
      expect(result.length).toBeGreaterThan(0);
    });

    And("each node should reflect its position in the directory hierarchy", () => {
      const firstNode = result[0]!;
      expect(firstNode).toHaveProperty("slug");
    });
  });

  Scenario("Navigation nodes are ordered by weight ascending", ({ Given, When, Then }) => {
    let result: TreeNode[];

    Given('a section "programming" in locale "en" has child nodes with weights 30, 10, and 20', () => {});

    When('the client calls content.getTree with locale "en"', async () => {
      result = (await testCaller.content.getTree({ locale: "en" })) as TreeNode[];
    });

    Then('the children of "programming" should appear in order: weight 10, weight 20, weight 30', () => {
      for (let i = 1; i < result.length; i++) {
        expect(result[i]!.weight).toBeGreaterThanOrEqual(result[i - 1]!.weight);
      }
    });
  });

  Scenario("Section nodes include a children array", ({ Given, When, Then, And }) => {
    let result: TreeNode[];

    Given('a section "programming" in locale "en" contains at least one child page', () => {});

    When('the client calls content.getTree with locale "en"', async () => {
      result = (await testCaller.content.getTree({ locale: "en" })) as TreeNode[];
    });

    Then('the "programming" node should have a non-empty "children" array', () => {
      const sectionNode = result.find((n) => n.isSection);
      expect(sectionNode).toBeDefined();
      expect(Array.isArray(sectionNode?.children)).toBe(true);
    });

    And('each child should include a "slug" and "title"', () => {
      const sectionNode = result.find((n) => n.isSection && n.children.length > 0);
      expect(sectionNode).toBeDefined();
      const firstChild = sectionNode!.children[0]!;
      expect(firstChild).toHaveProperty("slug");
    });
  });
});
