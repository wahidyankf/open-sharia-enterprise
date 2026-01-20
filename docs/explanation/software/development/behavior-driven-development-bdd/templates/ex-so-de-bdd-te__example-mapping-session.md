# Example Mapping Session Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Related Templates**:
  - [Three Amigos Session](./ex-so-de-bdd-te__three-amigos-session-template.md)
  - [User Story with Acceptance Criteria](./ex-so-de-bdd-te__user-story-with-acceptance-criteria.md)
  - [Scenario Template](./ex-so-de-bdd-te__scenario-template.md)
- **Use Case**: Facilitate collaborative discovery workshops to understand requirements
- **Template Size**: ~7 KB
- **Complexity**: Beginner to Intermediate

## Overview

Example Mapping is a structured workshop technique for discovering requirements through concrete examples. Using color-coded index cards (yellow=story, blue=rule, green=example, red=question), teams collaborate for 25 minutes to explore user stories before sprint planning.

This template guides facilitation of Example Mapping sessions, helping teams decide if stories are ready for development, need more research, or are too large.

## Template Structure

```markdown
## Example Mapping Session: [Story Name]

### Session Details

- **Date**: [Date]
- **Duration**: 25 minutes (strict time-box)
- **Facilitator**: [Name]
- **Participants**:
  - Product Owner / Business Analyst: [Name]
  - Developer: [Name]
  - Tester / QA: [Name]
  - Domain Expert (optional): [Name]

### Story Card (Yellow)

[Write user story on yellow card]

**As a** [user role]  
**I want** [capability]  
**So that** [business value]

### Rules (Blue Cards)

[Write each business rule on separate blue card]

1. [Business rule 1]
2. [Business rule 2]
3. [Business rule 3]

### Examples (Green Cards)

[Write each concrete example on separate green card, grouped under relevant rule]

**Rule 1 Examples**:

- Example 1: [Concrete scenario with specific inputs and expected outcomes]
- Example 2: [Another scenario for same rule]

**Rule 2 Examples**:

- Example 3: [Scenario demonstrating second rule]

### Questions (Red Cards)

[Write each uncertainty or unknown on separate red card]

1. [Question 1 - what don't we know?]
2. [Question 2 - what needs clarification?]
3. [Question 3 - what requires research?]

### Decision

**Outcome**: [Ready / Needs Research / Too Large]

**Rationale**:

- [Explanation of decision based on number of questions, examples, complexity]

**Next Steps**:

- [Actions required before story is ready for sprint]
```

## Islamic Finance Example: Sukuk Compliance Validation

```markdown
## Example Mapping Session: Sukuk Compliance Compliance Validation

### Session Details

- **Date**: 2026-01-20
- **Duration**: 25 minutes
- **Facilitator**: Tech Lead (Sarah)
- **Participants**:
  - Product Owner: Ahmed (Islamic Finance Platform)
  - Compliance Advisor: Sheikh Dr. Khalid Al-Najjar (Fiqh expert)
  - Developer: Fatima (Backend)
  - Tester: Omar (QA Engineer)

### Story Card (Yellow)

**As a** Compliance compliance officer  
**I want** to validate Sukuk (Islamic bond) structure for Compliance compliance  
**So that** investors can confidently invest in Islamic-compliant securities

### Rules (Blue Cards)

**Rule 1**: Sukuk must be backed by tangible assets (not debt)

**Rule 2**: Underlying assets must be 100% Compliance-compliant (no interest-bearing instruments, no Forbidden businesses)

**Rule 3**: Sukuk holders must have proportionate ownership of underlying assets

**Rule 4**: Profit distribution must be based on asset performance, not guaranteed fixed returns

**Rule 5**: Sukuk structure must be approved by Compliance board before issuance

**Rule 6**: Sukuk must comply with AAOIFI standards (FAS 33, SS 17)

### Examples (Green Cards)

**Rule 1 Examples (Asset-backed requirement)**:

- **Example 1**: Sukuk backed by 100M USD real estate portfolio → **Valid** (tangible assets)
- **Example 2**: Sukuk backed by conventional bonds with interest → **Invalid** (debt-based, not asset-backed)
- **Example 3**: Sukuk backed by equipment lease contracts → **Valid** (tangible assets with ownership)

**Rule 2 Examples (100% Compliance-compliant assets)**:

- **Example 4**: Portfolio: 90M real estate + 10M conventional loans → **Invalid** (10% non-compliant)
- **Example 5**: Portfolio: 50M Permitted business + 50M Islamic finance → **Valid** (100% compliant)
- **Example 6**: Portfolio: Real estate + Brewery business → **Invalid** (alcohol Forbidden)

**Rule 3 Examples (Proportionate ownership)**:

- **Example 7**: 1000 Sukuk certificates, 100M assets, each cert = 100K ownership → **Valid**
- **Example 8**: Sukuk holders have no ownership rights, just debt claim → **Invalid** (not ownership-based)

**Rule 4 Examples (Profit distribution)**:

- **Example 9**: Profit varies based on rental income from real estate → **Valid** (asset performance)
- **Example 10**: Fixed 5% annual return regardless of asset performance → **Invalid** (resembles Interest/interest)

**Rule 5 Examples (Compliance board approval)**:

- **Example 11**: Sukuk structure reviewed and approved by certified Compliance board → **Valid**
- **Example 12**: Sukuk issued without Compliance review → **Invalid** (no compliance validation)

### Questions (Red Cards)

**Question 1**: What percentage of non-performing assets is acceptable before Sukuk becomes non-compliant? (e.g., if rental property vacancy rate exceeds 20%, does Sukuk lose compliance?)

**Question 2**: If underlying asset value decreases by 50%, how does this affect Sukuk valuation and investor rights?

**Question 3**: Which AAOIFI standards apply: FAS 33 (Investment in Sukuk) or SS 17 (Sukuk)? Or both?

**Question 4**: Can Sukuk structure differ by Madhab (Fiqh school)? Do we need to support Hanafi, Shafi'i, Maliki, Hanbali variations?

**Question 5**: What happens if one underlying asset becomes Forbidden mid-term (e.g., Permitted restaurant starts serving alcohol)? Is partial asset replacement allowed?

**Question 6**: How frequently must Compliance board re-review Sukuk (annually, quarterly, upon material changes)?

### Decision

**Outcome**: **Needs Research**

**Rationale**:

- **6 red cards (questions)**: Too many unknowns for confident implementation
- **Critical questions** require Compliance board research and AAOIFI standards review
- **11 green cards (examples)**: Good coverage of rules, but questions affect implementation approach
- **Complexity**: Multi-Madhab support question (Q4) might split this story into smaller stories

**Next Steps**:

1. **Compliance Board Consultation** (Sheikh Dr. Khalid):
   - Research Q1 (non-performing asset threshold)
   - Research Q4 (Madhab variations in Sukuk structure)
   - Research Q5 (mid-term asset replacement policy)
   - Research Q6 (re-review frequency)
   - **Timeline**: 1 week (schedule Compliance board meeting)

2. **Technical Research** (Fatima):
   - Review AAOIFI FAS 33 and SS 17 standards (Q3)
   - Document standards requirements
   - **Timeline**: 3 days

3. **Business Research** (Ahmed):
   - Consult Islamic finance experts on Q2 (asset value decrease impact)
   - Review competitor Sukuk platforms for best practices
   - **Timeline**: 3 days

4. **Follow-up Session**:
   - Schedule second Example Mapping session after research complete
   - Target: 1 week from today (2026-01-27)
   - Goal: Convert research findings into additional rules/examples, decide if ready for sprint

### Session Notes

**Compliance Advisor Insights**:

- Sheikh Dr. Khalid emphasized importance of AAOIFI compliance (Q3) - both FAS 33 and SS 17 apply
- Mentioned Hanafi Madhab allows more flexibility in asset replacement (Q5) than other schools
- Suggested starting with Shafi'i Madhab (most conservative) to ensure broad acceptance

**Developer Concerns**:

- Fatima noted Q2 (asset value decrease) has significant technical implications for real-time valuation system
- May need integration with asset pricing APIs for real-time monitoring

**Tester Observations**:

- Omar identified need for test scenarios covering asset composition changes (Q5)
- Suggested edge cases: What if 99% of assets compliant? 95%? 90%? (boundary testing)

**Estimated Story Size (preliminary)**:

- If Madhab-agnostic implementation: **13 points** (large)
- If Shafi'i-only implementation: **8 points** (medium)
- **Recommendation**: Start with Shafi'i, plan multi-Madhab support as separate story

### Card Layout (Visual Reference)
```

┌─────────────────────────────────────────────┐
│ YELLOW CARD: User Story │
│ │
│ As a Compliance compliance officer │
│ I want to validate Sukuk structure │
│ So that investors can invest confidently │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ BLUE CARD: Rule 1 │
│ │
│ Sukuk must be backed by tangible assets │
└─────────────────────────────────────────────┘
↓ has examples
┌─────────────────────────────────────────────┐
│ GREEN CARD: Example 1 │
│ │
│ Sukuk backed by 100M real estate → Valid │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ GREEN CARD: Example 2 │
│ │
│ Sukuk backed by bonds → Invalid │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ RED CARD: Question 1 │
│ │
│ What % non-performing assets acceptable? │
└─────────────────────────────────────────────┘

[Layout continues for all rules, examples, questions]

```

### Template Output: Scenarios to Write (After Research)

After research answers questions, convert examples to Gherkin scenarios:

\`\`\`gherkin
@sukuk @compliance-compliance @critical
Scenario: Approve Sukuk backed by Compliance-compliant assets
  Given Sukuk issuance of 100,000,000 USD
  And Sukuk backed by asset portfolio:
    | Asset Type       | Value (USD) | Compliance Compliant |
    | Real Estate      | 50,000,000  | Yes               |
    | Permitted Business   | 30,000,000  | Yes               |
    | Equipment Lease  | 20,000,000  | Yes               |
  And all assets verified as 100% Compliance-compliant
  And Sukuk holders have proportionate ownership (1000 certificates @ 100K each)
  When Compliance board validates Sukuk structure
  Then Sukuk should be approved as Compliance-compliant
  And approval certificate should be issued referencing AAOIFI FAS 33 & SS 17
  And investors should receive disclosure of underlying assets

@sukuk @compliance-compliance @critical
Scenario: Reject Sukuk with non-compliant assets in portfolio
  Given Sukuk issuance of 100,000,000 USD
  And Sukuk backed by asset portfolio:
    | Asset Type       | Value (USD) | Compliance Compliant |
    | Real Estate      | 90,000,000  | Yes               |
    | Conventional Loan| 10,000,000  | No                |
  When Compliance board validates Sukuk structure
  Then Sukuk should be rejected
  And rejection reason should state "Portfolio contains 10% non-compliant assets (conventional loan)"
  And recommendation should state "All underlying assets must be 100% Compliance-compliant"
\`\`\`
```

## Summary

Example Mapping transforms abstract user stories into concrete examples through structured 25-minute sessions. Use color-coded cards (yellow=story, blue=rule, green=example, red=question) to visualize requirements. Decide if story is ready (few questions), needs research (many questions), or too large (many examples/rules). This template provides facilitation structure for effective requirements discovery.
