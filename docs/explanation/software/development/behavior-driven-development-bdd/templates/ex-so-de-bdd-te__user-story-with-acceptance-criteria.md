# User Story with Acceptance Criteria Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Related Templates**:
  - [Scenario Template](./ex-so-de-bdd-te__scenario-template.md)
  - [Example Mapping Session](./ex-so-de-bdd-te__example-mapping-session.md)
  - [Three Amigos Session](./ex-so-de-bdd-te__three-amigos-session-template.md)
- **Use Case**: Link user stories to executable BDD scenarios
- **Template Size**: ~6 KB
- **Complexity**: Beginner

## Overview

This template connects user stories to BDD scenarios, transforming abstract requirements into concrete executable specifications. User stories describe the "who, what, why" of features, while BDD scenarios provide testable acceptance criteria that define "done."

## Core Principles

User stories with BDD acceptance criteria align with software engineering principles:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Acceptance criteria make "done" explicit through executable scenarios.
- **[Automation Over Manual](../../../../../../governance/principles/software-engineering/automation-over-manual.md)** - Scenarios document expected behavior before implementation.

## Template Structure

```markdown
## User Story: [Story Title]

### Story Card

**As a** [user role]  
**I want** [capability or feature]  
**So that** [business value or benefit]

### Business Value

[1-2 sentences explaining why this feature matters to users and business]

### Acceptance Criteria (BDD Scenarios)

#### Scenario 1: [Happy Path Scenario Name]

\`\`\`gherkin
@[tag] @happy-path @smoke
Scenario: [Descriptive scenario name]
Given [precondition]
And [additional context]
When [user action]
Then [expected outcome]
And [additional verification]
\`\`\`

#### Scenario 2: [Edge Case Scenario Name]

\`\`\`gherkin
@[tag] @edge-case
Scenario: [Descriptive scenario name]
Given [edge case precondition]
When [action]
Then [expected edge case behavior]
\`\`\`

#### Scenario 3: [Error Case Scenario Name]

\`\`\`gherkin
@[tag] @error-handling
Scenario: [Descriptive scenario name]
Given [error condition]
When [action triggering error]
Then [expected error handling]
And [user-friendly error message]
\`\`\`

### Definition of Done

- [ ] All acceptance criteria scenarios pass
- [ ] Code reviewed and approved
- [ ] Documentation updated
- [ ] Deployed to staging environment

### Story Points

**Estimate**: [Story points based on complexity]

### Priority

**Priority**: [High / Medium / Low]

### Dependencies

- [List any dependencies on other stories or technical work]
```

## Islamic Finance Example: Permitted Product Certification

```markdown
## User Story: Permitted Product Certification Workflow

### Story Card

**As a** certification manager  
**I want** to review and approve Permitted certification applications for food products  
**So that** customers can confidently purchase Compliance-compliant products

### Business Value

Permitted certification builds customer trust and ensures Compliance compliance for our e-commerce platform. Certified products increase conversion rates by 40% among Muslim customers, representing 25% of our user base (50,000+ active users).

### Acceptance Criteria (BDD Scenarios)

#### Scenario 1: Approve product with valid Permitted certificate

\`\`\`gherkin
@permitted @certification @happy-path @smoke @critical
Scenario: Approve product with valid Permitted certificate from recognized authority
Given product "Permitted Chicken Wings" is registered
And product has valid Permitted certificate from recognized authority "JAKIM"
And certificate number is "PERMITTED-MY-2024-1234"
And certificate expiry date is 2027-12-31
And certification manager is logged in
When manager reviews product certification
Then product should be approved
And approval status should be "Permitted Certified"
And approval date should be recorded
And certificate expiry should be tracked for renewal reminder
And product should display "Permitted Certified" badge on website
And customer should see certification details:
| Field | Value |
| Certification Body | JAKIM |
| Certificate Number | PERMITTED-MY-2024-1234 |
| Certified Date | 2024-01-15 |
| Expiry Date | 2027-12-31 |
| Permitted Status | Certified |
\`\`\`

#### Scenario 2: Reject product with expired Permitted certificate

\`\`\`gherkin
@permitted @certification @edge-case @regression
Scenario: Reject product with expired Permitted certificate
Given product "Frozen Beef" is registered
And product has Permitted certificate from "JAKIM"
And certificate expiry date is 2025-12-31 (expired)
And current date is 2026-01-20
And certification manager is logged in
When manager reviews product certification
Then product should be rejected
And rejection status should be "Certificate Expired"
And rejection reason should state "Permitted certificate expired on 2025-12-31"
And supplier should be notified to renew certificate
And product should NOT display Permitted badge on website
And product page should show "Pending Permitted Re-certification" message
\`\`\`

#### Scenario 3: Flag product with missing supplier documentation

\`\`\`gherkin
@permitted @certification @error-handling @regression
Scenario: Flag product with incomplete supplier documentation
Given product "Lamb Kebab" is registered
And product claims Permitted certification
And supplier has NOT uploaded meat supplier certificate
And supplier has NOT uploaded slaughterhouse inspection report
And certification manager is logged in
When manager reviews product certification
Then product should be flagged for review
And flag status should be "Incomplete Documentation"
And missing documents should be listed:
| Required Document | Status |
| Meat Supplier Certificate | Missing |
| Slaughterhouse Inspection | Missing |
| Permitted Certificate (Product) | Uploaded|
And supplier should be notified to submit missing documents
And product should remain in "Pending Review" status
And certification decision should be blocked until documents provided
\`\`\`

#### Scenario 4: Reject product with non-Permitted ingredients

\`\`\`gherkin
@permitted @compliance @critical @compliance-compliance
Scenario: Reject product containing Forbidden (prohibited) ingredients
Given product "Bacon-Flavored Chips" is registered
And product ingredients include "pork-derived flavoring"
And certification manager is logged in
When manager reviews product ingredients
Then product should be automatically rejected
And rejection reason should state "Contains Forbidden ingredient: pork-derived flavoring"
And product should be flagged in compliance system
And supplier should be notified of rejection
And product should NOT be listed on website
And compliance team should review supplier's other products
\`\`\`

#### Scenario 5: Track certification expiry and send renewal reminder

\`\`\`gherkin
@permitted @certification @renewal @integration
Scenario: Send renewal reminder 90 days before certificate expiry
Given product "Permitted Chicken" has valid certification
And certificate expiry date is 2026-10-20
And current date is 2026-07-22 (90 days before expiry)
When daily certification expiry check runs
Then renewal reminder should be sent to supplier email
And reminder should include:
| Field | Value |
| Product Name | Permitted Chicken |
| Certificate Number | PERMITTED-MY-2024-5678 |
| Expiry Date | 2026-10-20 |
| Days Until Expiry | 90 days |
| Action Required | Renew Permitted certification |
| Renewal Instructions | Contact JAKIM for renewal |
And product status should be updated to "Renewal Pending"
And certification manager should see renewal task in dashboard
\`\`\`

### Definition of Done

- [ ] All 5 acceptance criteria scenarios pass
- [ ] Integration with certification authority API tested
- [ ] Email notification system configured
- [ ] Permitted badge UI component implemented
- [ ] Certificate expiry tracking scheduled job deployed
- [ ] User documentation updated (certification manager guide)
- [ ] Code reviewed and approved by tech lead
- [ ] Security review completed (data privacy for certificates)
- [ ] Deployed to staging environment
- [ ] UAT approved by certification manager
- [ ] Deployed to production

### Story Points

**Estimate**: 8 points

**Breakdown**:

- Backend: 3 points (certification workflow, validation logic)
- Frontend: 2 points (review UI, badge display)
- Integration: 2 points (certification authority API, email service)
- Testing: 1 point (BDD scenarios, UAT)

### Priority

**Priority**: High

**Rationale**: Permitted certification directly impacts 25% of user base and increases conversion by 40%. Critical for market competitiveness in Muslim-majority regions.

### Dependencies

- **Technical**: Integration with JAKIM API (requires API key and sandbox access)
- **Business**: List of recognized Permitted certification authorities (requires Compliance board approval)
- **Design**: Permitted badge UI design (requires UX team mockup)
- **Related Stories**:
  - STORY-123: "User can filter products by Permitted certification"
  - STORY-456: "Supplier can upload certification documents"

### Notes

- **Madhab Consideration**: Some Fiqh schools have different Permitted standards. Current implementation follows JAKIM (Malaysia) standards, which align with Shafi'i Madhab. Future enhancement: support multiple certification bodies with different Madhab rulings.
- **Compliance**: Permitted certification must comply with local regulations in target markets (Malaysia, Indonesia, UAE, Saudi Arabia).
- **Performance**: Certificate validation should complete within 2 seconds for good UX.
```
