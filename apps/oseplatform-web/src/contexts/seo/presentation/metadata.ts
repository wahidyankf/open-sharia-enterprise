import type { Metadata } from "next";

export const defaultMetadata: Metadata = {
  title: {
    default: "OSE Platform",
    template: "%s | OSE Platform",
  },
  description:
    "Open-source platform for Sharia-compliant enterprise solutions. Starting with Indonesian regulations, expanding to ERP, fintech, and global markets.",
  metadataBase: new URL("https://oseplatform.com"),
};
