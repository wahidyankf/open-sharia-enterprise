Feature: Regulatory source context
  As a product engineer
  I want regulatory source capabilities declared
  So that the bounded context boundary is established for future feature plans

  # Stub — detailed scenarios added in regulatory-source feature plan
  Scenario: Regulatory source context is declared
    Given the ose-grc-be service is running
    When the regulatory-source bounded context is initialized
    Then the context is ready to accept regulatory documents
