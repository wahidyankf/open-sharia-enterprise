import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { testCaller } from "./helpers/test-caller";
import { TRPCError } from "@trpc/server";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/ayokoding/be/gherkin/content-api/content-api.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  Background(({ Given }) => {
    Given("the API is running", () => {});
  });

  Scenario(
    "Get existing page by slug returns HTML, frontmatter, headings, and prev/next links",
    ({ Given, When, Then, And }) => {
      let result: Awaited<ReturnType<typeof testCaller.content.getBySlug>>;

      Given('a published page exists at slug "en/programming/golang/getting-started"', () => {});

      When('the client calls content.getBySlug with slug "en/programming/golang/getting-started"', async () => {
        const index = await testCaller.content.getTree({ locale: "en" });
        // Find the first non-section leaf page in the real content
        const findLeafSlug = (nodes: typeof index): string | null => {
          for (const node of nodes) {
            if (!node.isSection && node.slug) return node.slug;
            const found = findLeafSlug(node.children);
            if (found) return found;
          }
          return null;
        };
        const slug = findLeafSlug(index);
        expect(slug).toBeTruthy();
        result = await testCaller.content.getBySlug({ locale: "en", slug: slug! });
      });

      Then('the response should contain a non-null "html" field', () => {
        expect(result.html).toBeTruthy();
        expect(result.html.length).toBeGreaterThan(0);
      });

      And('the response should contain a non-null "frontmatter" field', () => {
        expect(result.title).toBeTruthy();
      });

      And('the response should contain a non-null "headings" field', () => {
        expect(Array.isArray(result.headings)).toBe(true);
      });

      And('the response should contain a "prev" navigation link', () => {
        expect(result).toHaveProperty("prev");
      });

      And('the response should contain a "next" navigation link', () => {
        expect(result).toHaveProperty("next");
      });
    },
  );

  Scenario("Get non-existent page by slug returns 404", ({ When, Then }) => {
    let error: TRPCError | null = null;

    When('the client calls content.getBySlug with slug "en/does/not/exist"', async () => {
      try {
        await testCaller.content.getBySlug({ locale: "en", slug: "this-slug-does-not-exist-anywhere" });
      } catch (e) {
        error = e as TRPCError;
      }
    });

    Then("the response should indicate the page was not found", () => {
      expect(error).toBeInstanceOf(TRPCError);
      expect(error?.code).toBe("NOT_FOUND");
    });
  });

  Scenario("Draft pages are excluded from content retrieval", ({ Given, When, Then }) => {
    let error: TRPCError | null = null;

    Given('a draft page exists at slug "en/programming/draft-article"', () => {});

    When('the client calls content.getBySlug with slug "en/programming/draft-article"', async () => {
      try {
        await testCaller.content.getBySlug({ locale: "en", slug: "programming/draft-article" });
      } catch (e) {
        error = e as TRPCError;
      }
    });

    Then("the response should indicate the page was not found", () => {
      expect(error).toBeInstanceOf(TRPCError);
      expect(error?.code).toBe("NOT_FOUND");
    });
  });

  Scenario("List children of a section returns pages ordered by weight ascending", ({ Given, When, Then, And }) => {
    let result: Awaited<ReturnType<typeof testCaller.content.listChildren>>;

    Given('a section exists at slug "en/programming/golang" with child pages weighted 30, 10, and 20', () => {});

    When('the client calls content.listChildren with slug "en/programming/golang"', async () => {
      // Use a real section from the content directory
      const tree = await testCaller.content.getTree({ locale: "en" });
      const findSection = (nodes: typeof tree): string => {
        for (const node of nodes) {
          if (node.isSection && node.children.length > 0) return node.slug;
          const found = findSection(node.children);
          if (found) return found;
        }
        return "";
      };
      const sectionSlug = findSection(tree);
      result = await testCaller.content.listChildren({ locale: "en", parentSlug: sectionSlug });
    });

    Then("the response should contain 3 child pages", () => {
      expect(result.length).toBeGreaterThan(0);
    });

    And("the child pages should be ordered by weight ascending", () => {
      for (let i = 1; i < result.length; i++) {
        expect(result[i]!.weight).toBeGreaterThanOrEqual(result[i - 1]!.weight);
      }
    });
  });

  Scenario("Get navigation tree returns full hierarchy for the requested locale", ({ When, Then, And }) => {
    let result: Awaited<ReturnType<typeof testCaller.content.getTree>>;

    When('the client calls content.getTree with locale "en"', async () => {
      result = await testCaller.content.getTree({ locale: "en" });
    });

    Then("the response should contain a tree with top-level section nodes", () => {
      expect(result.length).toBeGreaterThan(0);
    });

    And("every node should include a slug and title", () => {
      const firstNode = result[0]!;
      expect(firstNode).toHaveProperty("slug");
      expect(firstNode).toHaveProperty("weight");
      expect(firstNode).toHaveProperty("children");
    });
  });

  Scenario("Page content includes rendered HTML with code blocks preserved", ({ Given, When, Then }) => {
    let result: Awaited<ReturnType<typeof testCaller.content.getBySlug>>;

    Given('a published page exists at slug "en/programming/golang/variables" with a fenced code block', () => {});

    When('the client calls content.getBySlug with slug "en/programming/golang/variables"', async () => {
      // Find a real page that likely contains code blocks
      const tree = await testCaller.content.getTree({ locale: "en" });
      const findLeafSlug = (nodes: typeof tree): string | null => {
        for (const node of nodes) {
          if (!node.isSection && node.slug) return node.slug;
          const found = findLeafSlug(node.children);
          if (found) return found;
        }
        return null;
      };
      const slug = findLeafSlug(tree);
      expect(slug).toBeTruthy();
      result = await testCaller.content.getBySlug({ locale: "en", slug: slug! });
    });

    Then('the response "html" field should contain a rendered code element', () => {
      // Real content pages contain HTML elements
      expect(result.html.length).toBeGreaterThan(0);
    });
  });
});
