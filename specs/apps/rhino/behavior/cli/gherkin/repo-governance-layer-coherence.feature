@repo-governance-layer-coherence
Feature: Governance Layer Coherence Audit

  As a repository maintainer
  I want to verify that the two governance index documents agree on the layer numbering and names
  So that drift between the architecture description and the README cannot silently mislead readers

  Scenario: Both docs list identical layer numbers and names passes
    Given a repository where both governance docs list layers 0 through 5 with identical names
    When the developer runs repo-governance layer-coherence
    Then the command exits successfully
    And the layer-coherence output reports zero findings

  Scenario: Layer numbering has a gap fails
    Given a repository where the governance docs list layers 0, 1, and 3 with no layer 2
    When the developer runs repo-governance layer-coherence
    Then the command exits with a failure code
    And the layer-coherence output identifies the numbering gap

  Scenario: Two docs disagree on a layer name for the same number fails
    Given a repository where the two governance docs assign different names to the same layer number
    When the developer runs repo-governance layer-coherence
    Then the command exits with a failure code
    And the layer-coherence output identifies the layer name disagreement
