Feature: Internal policy context
  As a product engineer
  I want internal policy capabilities declared
  So that the bounded context boundary is established for future feature plans

  # Stub — detailed scenarios added in internal-policy feature plan
  Scenario: Internal policy context is declared
    Given the ose-app-be service is running
    When the internal-policy bounded context is initialized
    Then the context is ready to accept internal policy documents
