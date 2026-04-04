import { describe, it, expect } from "vitest";
import { isValidLocale, SUPPORTED_LOCALES, DEFAULT_LOCALE, LOCALE_LABELS } from "@/lib/i18n/config";
import { t } from "@/lib/i18n/translations";

describe("i18n config", () => {
  it("supports en and id locales", () => {
    expect(SUPPORTED_LOCALES).toContain("en");
    expect(SUPPORTED_LOCALES).toContain("id");
  });

  it("defaults to en", () => {
    expect(DEFAULT_LOCALE).toBe("en");
  });

  it("validates known locales", () => {
    expect(isValidLocale("en")).toBe(true);
    expect(isValidLocale("id")).toBe(true);
  });

  it("rejects unknown locales", () => {
    expect(isValidLocale("fr")).toBe(false);
    expect(isValidLocale("xyz")).toBe(false);
    expect(isValidLocale("")).toBe(false);
  });

  it("has labels for all locales", () => {
    expect(LOCALE_LABELS.en).toBe("English");
    expect(LOCALE_LABELS.id).toBe("Bahasa Indonesia");
  });
});

describe("translations", () => {
  it("returns English translations", () => {
    expect(t("en", "readMore")).toBe("Read More");
    expect(t("en", "lastUpdated")).toBe("Last updated");
    expect(t("en", "search")).toBe("Search...");
    expect(t("en", "onThisPage")).toBe("On this page");
    expect(t("en", "openSourceProject")).toBe("Source-Available Project");
  });

  it("returns Indonesian translations", () => {
    expect(t("id", "readMore")).toBe("Baca Selengkapnya");
    expect(t("id", "lastUpdated")).toBe("Terakhir diperbarui");
    expect(t("id", "search")).toBe("Cari...");
    expect(t("id", "openSourceProject")).toBe("Proyek Source-Available");
  });

  it("returns key when translation not found", () => {
    expect(t("en", "nonExistentKey")).toBe("nonExistentKey");
  });
});
