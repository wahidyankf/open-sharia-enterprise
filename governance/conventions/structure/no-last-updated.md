---
title: "No Last Updated Convention"
description: Superseded by the No Manual Date Metadata Convention. Non-website markdown files must not contain Last Updated footer blocks or updated frontmatter fields.
category: explanation
subcategory: conventions
tags:
  - conventions
  - frontmatter
  - maintenance
  - git
created: 2026-04-25
---

# No Last Updated Convention

> **Superseded**: This convention has been absorbed into the
> [No Manual Date Metadata Convention](./no-date-metadata.md), which covers
> all three forms of manual date metadata: `updated:` frontmatter fields,
> `**Last Updated**` footer blocks, and inline body date annotations.
> Refer to that document for the authoritative rules and enforcement guidance.

## Principles Implemented/Respected

This convention implements the following core principles (see the authoritative
[No Manual Date Metadata Convention](./no-date-metadata.md) for full details):

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: Removing manual date tracking eliminates a maintenance burden that grows with every file edit.
- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**: Git provides automatic, authoritative change tracking. Manual date fields duplicate this information poorly.
- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: By explicitly banning all forms of manual date metadata from non-website files, this convention makes the rule unambiguous.
