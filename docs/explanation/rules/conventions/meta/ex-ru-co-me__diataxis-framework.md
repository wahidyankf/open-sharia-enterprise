---
title: "Diátaxis Framework"
description: Understanding the Diátaxis documentation framework used in open-sharia-enterprise
category: explanation
subcategory: conventions
tags:
  - diataxis
  - documentation-framework
  - organization
  - conventions
created: 2025-11-22
updated: 2025-12-05
---

# Diátaxis Framework

The open-sharia-enterprise project uses the [Diátaxis framework](https://diataxis.fr/) to organize all documentation. This document explains what Diátaxis is, why we use it, and how it's implemented in our project.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Progressive Disclosure](../../principles/content/ex-ru-pr-co__progressive-disclosure.md)**: Diátaxis separates learning-oriented content (tutorials) from problem-solving (how-to) and reference material. Beginners start with tutorials, experienced users jump to how-to guides or reference. Complexity is layered, not overwhelming.

- **[Simplicity Over Complexity](../../principles/general/ex-ru-pr-ge__simplicity-over-complexity.md)**: Four clear categories (Tutorials, How-To, Reference, Explanation) instead of complex, nested documentation hierarchies. Each category serves a single, well-defined purpose.

## Purpose

This convention establishes the Diátaxis framework as the organizational structure for all documentation in the repository. It provides a systematic approach to categorizing content into four distinct types (Tutorials, How-To, Reference, Explanation), ensuring documentation serves different user needs effectively. This framework guides where new content belongs and maintains clear boundaries between documentation types.

## Scope

### What This Convention Covers

- **Documentation categorization** - The four Diátaxis categories (Tutorials, How-To, Reference, Explanation)
- **Category characteristics** - Purpose, audience, and appropriate content for each category
- **Category boundaries** - What belongs in each category vs. what doesn't
- **Navigation and discovery** - How categories help users find information
- **Content creation guidance** - When to create content in each category

### What This Convention Does NOT Cover

- **How to write content within categories** - Covered in category-specific conventions (e.g., [Tutorial Naming Convention](../tutorial/ex-ru-co-tu__naming.md), [README Quality Convention](../content/ex-ru-co-co__readme-quality.md))
- **File naming within categories** - Covered in [File Naming Convention](./ex-ru-co-me__file-naming.md)
- **Hugo site structure** - Covered in Hugo content conventions
- **Content quality standards** - Covered in [Content Quality Principles](../content/ex-ru-co-co__quality.md)

## 📋 What is Diátaxis?

Diátaxis is a systematic approach to technical documentation authoring that divides documentation into four distinct categories based on user needs and context:

```
                    Practical Steps          Understanding
                    ─────────────────────────────────────
Learning-oriented   │ TUTORIALS          │ EXPLANATION  │
                    │                    │              │
Problem-oriented    │ HOW-TO GUIDES      │ REFERENCE    │
                    ─────────────────────────────────────
                    Action-oriented       Information-oriented
```

Each category serves a different purpose and addresses different user needs.

## 📚 The Four Categories

### Tutorials (Learning-Oriented)

**Purpose**: Teach newcomers through hands-on experience

**When to use**: When you want to help someone learn a skill or concept through practice

**Characteristics**:

- Learning by doing
- Step-by-step instructions
- Concrete outcomes
- Minimal explanation (save for "Explanation" category)
- Assumes no prior knowledge
- Encouraging tone

**Example use cases**:

- "Initial Setup for the Project"
- "Your First API Call"
- "Setting Up the Development Environment"

**In our project**:

- Location: `docs/tutorials/`
- Prefix: `tu__`
- Examples: `tu__getting-started.md`, `tu__first-deployment.md`

### How-To Guides (Problem-Oriented)

**Purpose**: Solve specific problems and accomplish specific tasks

**When to use**: When users know what they want to do but need guidance on how

**Characteristics**:

- Goal-oriented
- Assumes familiarity with the project
- Focused on outcomes
- Multiple approaches when applicable
- Practical and direct
- Troubleshooting included

**Example use cases**:

- "How to Configure Sharia Compliance Rules"
- "How to Deploy to Production"
- "How to Add a New Payment Provider"

**In our project**:

- Location: `docs/how-to/`
- Prefix: `hoto__`
- Examples: `hoto__configure-api.md`, `hoto__deploy-docker.md`

### Reference (Information-Oriented)

**Purpose**: Provide accurate, comprehensive technical information

**When to use**: When users need to look up specific details, parameters, or specifications

**Characteristics**:

- Austere and neutral tone
- Organized for lookup
- Comprehensive coverage
- Accurate and up-to-date
- Structure over explanation
- Examples for clarity

**Example use cases**:

- "API Endpoint Reference"
- "Configuration Options Reference"
- "Database Schema Reference"

**In our project**:

- Location: `docs/reference/`
- Prefix: `re__`
- Examples: `re__api-reference.md`, `re__configuration-reference.md`

### Explanation (Understanding-Oriented)

**Purpose**: Deepen understanding of concepts, design decisions, and "why"

**When to use**: When users need to understand context, reasoning, or broader perspective

**Characteristics**:

- Conceptual and theoretical
- Background and context
- Design decisions and trade-offs
- Multiple perspectives
- Connections between concepts
- No instructions (save for other categories)

**Example use cases**:

- "Why We Use Conventional Commits"
- "Sharia Compliance Architecture"
- "Understanding Our Authentication Model"

**In our project**:

- Location: `docs/explanation/`
- Prefix: `ex__`
- Examples: `ex__architecture.md`, `ex-co__file-naming-convention.md`

## 🎯 Why We Use Diátaxis

### Benefits for Documentation Writers

1. **Clear Categorization** - Know exactly where new documentation belongs
2. **Consistent Structure** - Follow established patterns for each category
3. **Reduced Duplication** - Separate concerns prevent overlap
4. **Easier Maintenance** - Changes are localized to specific categories

### Benefits for Documentation Users

1. **Find What They Need** - Categories match user intent
2. **Right Level of Detail** - Each category serves its purpose
3. **Progressive Learning** - Clear path from beginner to expert
4. **Efficient Lookup** - Reference material is separate from tutorials

### Benefits for the Project

1. **Scalability** - Framework grows with the project
2. **Quality** - Clear standards improve documentation quality
3. **Completeness** - Framework reveals gaps in coverage
4. **Onboarding** - New contributors understand documentation structure

## 🏗️ How Diátaxis is Implemented

### Directory Structure

```
docs/
├── tutorials/                                # tu__ prefix - Learning-oriented
│   ├── README.md                            # Category index
│   └── ...
├── how-to/                                   # hoto__ prefix - Problem-oriented
│   ├── README.md                            # Category index
│   └── ...
├── reference/                                # re__ prefix - Information-oriented
│   ├── README.md                            # Category index
│   └── ...
└── explanation/                              # ex__ prefix - Understanding-oriented
    ├── README.md                             # Category index
    └── conventions/                          # ex-co__ prefix
        ├── README.md                         # Subcategory index
        ├── ex-co__file-naming-convention.md
        ├── ex-co__linking-convention.md
        └── ex-co__diataxis-framework.md (this file)
```

**Note on Directory Naming:**

The directory names follow semantic conventions:

- `tutorials/` is **plural** because tutorials are discrete, countable documents
- `how-to/` is the **category name** (singular) matching "How-to Guides" from Diátaxis
- `reference/` is a **mass noun** (like "reference library") representing reference material as a whole
- `explanation/` is a **mass noun** representing explanatory content as a collective

This is intentional and follows standard documentation naming conventions. See the [File Naming Convention](./ex-ru-co-me__file-naming.md) for more details.

### File Naming Integration

Each category has a unique prefix that encodes the Diátaxis category:

- `tu__` = Tutorials
- `hoto__` = How-To
- `re__` = Reference
- `ex__` = Explanation

For nested directories, add 2-letter abbreviations:

- `ex-co__` = explanation/conventions

See [File Naming Convention](./ex-ru-co-me__file-naming.md) for details.

### Frontmatter Standard

All documentation files include the category in frontmatter:

```yaml
---
title: "Document Title"
description: Brief description
category: tutorial # or how-to, reference, explanation
tags:
  - relevant-tags
created: YYYY-MM-DD
updated: YYYY-MM-DD
---
```

## 🔍 Choosing the Right Category

When creating new documentation, ask:

1. **Is the user learning a new skill?** → Tutorial
2. **Does the user have a specific problem to solve?** → How-To
3. **Does the user need to look up specific information?** → Reference
4. **Does the user need to understand concepts or "why"?** → Explanation

### Decision Tree

```
Start here
    │
    ├─ Teaching someone to DO something?
    │   │
    │   ├─ Complete beginner? → Tutorial
    │   └─ Has experience? → How-To
    │
    └─ Teaching someone to UNDERSTAND something?
        │
        ├─ Need specific facts/data? → Reference
        └─ Need context/reasoning? → Explanation
```

## Common Mistakes to Avoid

### ❌ Mixing Categories

**Don't**:

- Put explanations in tutorials (breaks flow)
- Put step-by-step instructions in reference (wrong format)
- Put troubleshooting in explanations (not actionable)

**Do**:

- Link between categories when needed
- Keep each document focused on its category
- Cross-reference related content

### ❌ Wrong Category Choice

**Tutorial misuse**:

- ❌ "Understanding Authentication Concepts" → Should be Explanation
- ✅ "Building Your First Authenticated Endpoint" → Correct Tutorial

**How-To misuse**:

- ❌ "Learning the API Basics" → Should be Tutorial
- ✅ "How to Add Rate Limiting" → Correct How-To

**Reference misuse**:

- ❌ "Why We Chose PostgreSQL" → Should be Explanation
- ✅ "PostgreSQL Configuration Options" → Correct Reference

**Explanation misuse**:

- ❌ "Steps to Deploy" → Should be How-To
- ✅ "Understanding Our Deployment Architecture" → Correct Explanation

## 🧪 Examples from Our Project

### Tutorial Example: Initial Setup

**Location**: `docs/tutorials/tu__initial-setup.md`

**Structure**:

1. Introduction - What you'll build
2. Prerequisites - What you need installed
3. Step 1: Clone the repository
4. Step 2: Install dependencies
5. Step 3: Run the application
6. Verification - Confirm it works
7. Next steps - Where to go from here

### How-To Example: Configure API

**Location**: `docs/how-to/hoto__configure-api.md`

**Structure**:

1. Problem statement - What this solves
2. Prerequisites - Assumes project setup
3. Configuration steps
4. Verification
5. Troubleshooting common issues
6. Related guides

### Reference Example: Monorepo Structure

**Location**: `docs/reference/re__monorepo-structure.md`

**Structure**:

1. Overview
2. Directory structure
3. For each component:
   - Purpose and responsibility
   - Naming conventions
   - Dependencies
   - Import patterns
   - Example usage
4. Configuration reference

### Explanation Example: This Document

**Location**: `docs/explanation/rules/conventions/meta/ex-ru-co-me__diataxis-framework.md`

**Structure**:

1. What is Diátaxis?
2. The four categories explained
3. Why we use it
4. How it's implemented
5. Decision guidance
6. Common mistakes

## 🔗 Related Documentation

- [Conventions Index](./README.md) - Overview of all documentation conventions
- [File Naming Convention](./ex-ru-co-me__file-naming.md) - How to name files with category prefixes
- [Linking Convention](../formatting/ex-ru-co-fo__linking.md) - How to link between documents
- [OSS Documentation Convention](../content/ex-ru-co-co__oss-documentation.md) - Repository-level documentation (README, CONTRIBUTING, ADRs) - complements Diátaxis internal docs structure

## 🌐 External Resources

- [Official Diátaxis Documentation](https://diataxis.fr/)
- [Diátaxis in Practice](https://diataxis.fr/application/)

---

**Last Updated**: 2025-12-05
