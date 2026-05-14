Feature: Gap analysis context
  As a product engineer
  I want gap analysis capabilities declared
  So that the bounded context boundary is established for future feature plans

  # Stub — detailed scenarios added in gap-analysis feature plan
  Scenario: Gap analysis context is declared
    Given the ose-app-be service is running
    When the gap-analysis bounded context is initialized
    Then the context is ready to compare regulatory and policy documents
