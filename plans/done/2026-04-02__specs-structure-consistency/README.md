# Specs Structure Consistency

## Status

**Done** | Created: 2026-04-02 | Completed: 2026-04-02

## Problem

After consolidating CLI specs under domain directories, an audit of the full `specs/` tree revealed
three inconsistencies in how Gherkin feature files are organized. The established convention
(derived from the most common pattern across BE specs, a-demo FE, and organiclever FE) is:

```text
specs/{scope}/{name}/{layer}/gherkin/{domain}/{feature}.feature
```

Three areas deviate from this pattern:

1. **FE gherkin specs** (ayokoding, oseplatform) -- flat files instead of domain subdirectories
2. **Go library specs** (golang-commons, hugo-commons) -- missing `gherkin/` wrapper directory
3. **ts-ui library specs** -- flat files under `gherkin/` instead of component subdirectories

## Goals

- Standardize all Gherkin feature file paths to follow the established subdirectory convention
- Update all references (test files, project.json, README files) to match new paths
- Maintain passing CI across all affected projects after each phase

## Scope

### In Scope

- Moving 20 feature files into proper subdirectory structure
- Updating hardcoded path references in test step files
- Updating project.json inputs (where paths are not already glob-based)
- Updating spec README files to reflect new structure

### Out of Scope

- CLI specs (rhino, ayokoding-cli, oseplatform-cli) -- kept flat per blast-radius decision
- a-demo and organiclever specs -- already consistent
- Any behavioral changes to feature files themselves

## Git Workflow

**Trunk Based Development** -- all work on `main` branch. Each phase produces an independent commit.

## Plan Documents

- [Requirements](./requirements.md) -- detailed requirements and acceptance criteria
- [Technical Documentation](./tech-docs.md) -- architecture, blast radius, file mappings
- [Delivery Plan](./delivery.md) -- phased implementation checklist

## References

- [Plans Organization Convention](../../../governance/conventions/structure/plans.md)
- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
