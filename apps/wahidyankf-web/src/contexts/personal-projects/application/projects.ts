import { filterItems } from "@/contexts/search/application/search";

export type Project = {
  title: string;
  description: string;
  details: string[];
  links: {
    [key: string]: string;
  };
};

export const projects: Project[] = [
  {
    title: "Open Sharia Enterprise (OSE)",
    description:
      "An open-source enterprise platform for Sharia-compliant business systems. Currently in Phase 1 (OrganicLever — Productivity Tracker). Polyglot Nx monorepo spanning 30+ projects across Go, TypeScript, Java, Kotlin, Python, Rust, F#, C#, Elixir, Clojure, and Dart.",
    details: [
      "Enforces quality through Gherkin-driven spec coverage and three-level testing (unit, integration, E2E)",
      "AI-augmented development with 50+ specialized agents",
      "Key components: OrganicLever (Next.js 16 + F#/Giraffe backend), AyoKoding (free educational platform), OSE Platform",
      "MIT licensed — fully open-source across all apps and libs",
    ],
    links: {
      repository: "https://github.com/wahidyankf/ose-public",
      website: "https://oseplatform.com/",
    },
  },
  {
    title: "AyoKoding",
    description:
      "A free educational platform for software engineering, featuring a blog and YouTube channel. Created to learn in public and give back to the community.",
    details: [
      "Comprehensive learning resources for software engineering",
      "Public learning platform to share knowledge",
      "Includes a YouTube channel for video content",
    ],
    links: {
      repository: "https://github.com/organiclever/ayokoding",
      website: "https://ayokoding.com/",
      YouTube: "https://www.youtube.com/@AyoKoding",
    },
  },
  {
    title: "Organic Lever",
    description: "A web application focused on team and personal productivity (in progress).",
    details: [
      "Aims to improve team collaboration",
      "Enhances personal productivity",
      "Web-based application for easy access",
    ],
    links: {
      website: "http://organiclever.com/",
    },
  },
  {
    title: "The Organic",
    description: "A repository to showcase open source projects and toy-projects.",
    details: [
      "Collection of various open source contributions",
      "Includes experimental and learning projects",
      "Demonstrates diverse coding skills and interests",
    ],
    links: {
      repository: "https://github.com/organiclever/the-organic",
    },
  },
];

export type ProjectFilter = string;

export function filterProjects(projects: Project[], filter: ProjectFilter): Project[] {
  return filterItems(projects, filter, ["title", "description", "details"]);
}
