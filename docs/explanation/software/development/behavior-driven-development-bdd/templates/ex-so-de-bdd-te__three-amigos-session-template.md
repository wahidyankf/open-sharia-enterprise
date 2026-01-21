# Three Amigos Session Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Use Case**: Structure collaborative requirement discovery
- **Complexity**: Beginner

## Core Principles

Three Amigos sessions align with software engineering principles:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Collaborative discovery makes implicit assumptions explicit through concrete examples.

## Template Structure

```markdown
## Three Amigos Session: [Feature/Story Name]

### Participants

- **Business**: [Product Owner / Business Analyst name]
- **Development**: [Developer name]
- **Testing**: [QA Engineer / Tester name]
- **Optional**: [Domain Expert name]

### Agenda (25 minutes)

1. **Business explains feature** (5 min)
2. **Team asks questions** (10 min)
3. **Write examples collaboratively** (8 min)
4. **Decide if story ready** (2 min)

### Feature Description

[Brief description of what feature does and why it matters]

### Discussion Notes

**Business Context**:

- [Key business requirements]

**Technical Questions**:

- [Developer questions and answers]

**Test Scenarios Identified**:

- [Tester identified edge cases]

### Examples Discovered

[List concrete examples that became scenarios]

### Decision

**Ready for Sprint?** [Yes / No / Needs Research]

### Next Steps

- [Actions before story can be developed]
```

## Islamic Finance Example: Tax Mixed Assets

```markdown
## Three Amigos Session: Tax on Mixed Asset Portfolio

### Participants

- **Business**: Ahmed (Product Owner)
- **Development**: Fatima (Backend Developer)
- **Testing**: Omar (QA Engineer)
- **Domain Expert**: Sheikh Dr. Khalid (Compliance Advisor)

### Agenda

Completed in 28 minutes (slightly over due to Compliance complexity)

### Feature Description

Users with diversified wealth (gold, silver, cash, stocks, real estate) need Tax calculation across all assets with proper exemptions and aggregation.

### Discussion Notes

**Business Context**:

- 60% of users have mixed assets, not single asset type
- Current system only handles gold/silver individually
- Need: aggregate taxable wealth, apply 2.5% once

**Compliance Advisor Input**:

- Personal residence exempt (not taxable)
- Tools of trade exempt (work equipment, professional car)
- Investment real estate IS taxable
- Stocks taxable at current market value

**Technical Questions**:
Q: How to handle currency conversion?
A: Use daily exchange rates, convert all to user's base currency

**Test Scenarios**:

- Mixed portfolio with exempt assets
- All assets below individual threshold but above combined
- Real-time stock prices vs end-of-day

### Examples Discovered

1. User owns: 50g gold (below threshold) + 400g silver (below threshold) + 5K cash
   → Combined value above threshold → Tax DUE
2. User owns: personal home (exempt) + investment property (taxable)
3. User owns: work laptop (exempt) + investment stocks (taxable)

### Decision

**Ready for Sprint?** Yes (with caveat: start with static prices, real-time in Sprint 2)

### Next Steps

- Developer writes scenarios from examples
- QA prepares test data for mixed portfolios
- Ready for Sprint 15 (starting 2026-01-22)
```

Use Three Amigos sessions to align team understanding before implementation.
