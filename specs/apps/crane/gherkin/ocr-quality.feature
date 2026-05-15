Feature: OCR quality assessment
  As a pdf-to-md-checker agent
  I want to quantify OCR quality in image-extracted sections
  So that unreadably garbled pages are flagged for manual review

  Scenario: High error rate produces CRITICAL finding
    Given a Markdown fixture with an OCR-tagged section at 15% estimated error rate
    When I run "crane ocr quality" on the fixture
    Then a finding with criticality "CRITICAL" is returned
    And the finding includes the OCR page number

  Scenario: Clean OCR section produces no finding
    Given a Markdown fixture with an OCR-tagged section at 1% estimated error rate
    When I run "crane ocr quality" on the fixture
    Then the JSON output is an empty array

  Scenario: No OCR-tagged sections produces no finding
    Given a Markdown fixture with no OCR page tags
    When I run "crane ocr quality" on the fixture
    Then the JSON output is an empty array
