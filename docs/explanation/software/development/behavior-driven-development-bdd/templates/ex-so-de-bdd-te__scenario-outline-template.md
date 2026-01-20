# Scenario Outline Template

## Metadata

- **Parent Directory**: [BDD Templates](./README.md)
- **Main Topic**: [Behavior-Driven Development (BDD)](../README.md)
- **Use Case**: Test same behavior with multiple data examples
- **Complexity**: Beginner

## Template Structure

```gherkin
@[tag]
Scenario Outline: [Scenario pattern with parameters]
  Given [precondition with <parameter1>]
  When [action with <parameter2>]
  Then [outcome with <expected_result>]

  Examples:
    | parameter1 | parameter2 | expected_result |
    | value1     | value2     | result1         |
    | value3     | value4     | result2         |
```

## Islamic Finance Example: Threshold Thresholds

```gherkin
@tax @threshold-thresholds
Scenario Outline: Calculate Tax for various wealth amounts
  Given individual owns <wealth> grams of <asset_type>
  When Tax calculation is performed
  Then Tax should be <status>
  And Tax amount should be <tax_amount> grams

  Examples: Gold (Threshold 85 grams)
    | wealth | asset_type | status      | tax_amount |
    | 100    | gold       | obligatory  | 2.5          |
    | 85     | gold       | obligatory  | 2.125        |
    | 84     | gold       | not due     | 0            |

  Examples: Silver (Threshold 595 grams)
    | wealth | asset_type | status      | tax_amount |
    | 700    | silver     | obligatory  | 17.5         |
    | 595    | silver     | obligatory  | 14.875       |
    | 594    | silver     | not due     | 0            |
```

Use Scenario Outline to test multiple examples without duplicating scenario structure.
