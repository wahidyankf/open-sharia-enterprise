# crane-cli Gherkin Feature Files

Behavioral specifications for all crane-cli commands. Each feature file maps to one command
domain. Step definitions live in `apps/crane-cli/tests/unit/steps/`.

## Feature File Inventory

| File                          | Commands Covered                                                    | Step File           |
| ----------------------------- | ------------------------------------------------------------------- | ------------------- |
| `pdf-commands.feature`        | `crane pdf info`, `crane pdf type`, `crane pdf extract`             | `pdf_steps.py`      |
| `text-check.feature`          | `crane text check`, `crane text search`                             | `text_steps.py`     |
| `heading-check.feature`       | `crane heading infer`, `crane heading check`                        | `heading_steps.py`  |
| `nesting-check.feature`       | `crane nesting infer`, `crane nesting check`                        | `nesting_steps.py`  |
| `table-check.feature`         | `crane table detect`, `crane table check`                           | `table_steps.py`    |
| `figure-check.feature`        | `crane figure detect`, `crane figure check`                         | `figure_steps.py`   |
| `mermaid-validate.feature`    | `crane mermaid validate`                                            | `mermaid_steps.py`  |
| `ocr-quality.feature`         | `crane ocr quality`, `crane ocr extract`                            | `ocr_steps.py`      |
| `report-management.feature`   | `crane report init`, `crane report finalize`                        | `report_steps.py`   |
| `skiplist-management.feature` | `crane skiplist add`, `crane skiplist check`, `crane skiplist list` | `skiplist_steps.py` |

## Scenario Counts (target)

| Feature             | Scenarios |
| ------------------- | --------- |
| pdf-commands        | 4         |
| text-check          | 5         |
| heading-check       | 4         |
| nesting-check       | 3         |
| table-check         | 4         |
| figure-check        | 3         |
| mermaid-validate    | 5         |
| ocr-quality         | 4         |
| report-management   | 4         |
| skiplist-management | 5         |
| **Total**           | **41**    |
