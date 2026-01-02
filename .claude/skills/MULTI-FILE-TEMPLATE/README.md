# Multi-File Skill Template

Use this template structure when creating a Skill that needs more than 1,000 lines of content, or when organizing knowledge into distinct categories improves clarity.

## Structure

```
skill-name/
├── SKILL.md       # Core concepts and quick reference (start here)
├── reference.md   # Detailed specifications and comprehensive tables
└── examples.md    # Working code samples and templates
```

## When to Use Multi-File Structure

**Use multi-file when**:

- Total content exceeds ~1,000 lines
- Knowledge naturally splits into conceptual vs. reference vs. examples
- Detailed tables/matrices needed (put in reference.md)
- Multiple working code examples needed (put in examples.md)
- Progressive disclosure benefits users (quick concepts → deep reference → practical examples)

**Use single-file when**:

- Total content < 1,000 lines
- Knowledge is cohesive and doesn't naturally split
- Primarily conceptual (minimal code examples or tables)
- Quick reference is the primary use case

## File Purposes

### SKILL.md (Required)

**Purpose**: Entry point with frontmatter, core concepts, quick reference

**Content**:

- Frontmatter (name, description, allowed-tools, model)
- Overview and purpose
- Quick reference table
- Core concepts (brief, link to reference.md for details)
- Best practices
- Common mistakes (link to examples.md for corrections)
- File organization guide
- References to conventions
- Related Skills

**Length**: Aim for 200-500 lines

### reference.md (Optional)

**Purpose**: Comprehensive specifications and detailed tables

**Content**:

- Detailed specifications with all edge cases
- Comprehensive matrices and tables
- Deep-dive explanations
- Validation criteria checklists
- Decision trees
- Complete mapping to convention sections

**Length**: Can be 500-1,500+ lines

### examples.md (Optional)

**Purpose**: Working code samples and templates

**Content**:

- Basic examples (fundamental patterns)
- Advanced examples (complex integrations)
- Anti-pattern corrections (wrong vs. right)
- Quick-start templates
- Domain-specific patterns
- Comparison examples (A vs B approaches)
- Testing examples

**Length**: Can be 500-1,000+ lines

## Usage Instructions

1. **Copy this template directory**:

   ```bash
   cp -r .claude/skills/MULTI-FILE-TEMPLATE .claude/skills/your-skill-name
   ```

2. **Update SKILL.md**:
   - Set frontmatter (name, description, etc.)
   - Write core concepts
   - Add quick reference table
   - Link to reference.md and examples.md appropriately

3. **Populate reference.md**:
   - Add detailed specifications
   - Create comprehensive tables
   - Write deep-dive explanations

4. **Populate examples.md**:
   - Add working code examples
   - Show anti-pattern corrections
   - Provide quick-start templates

5. **Ensure cross-linking**:
   - SKILL.md links to reference.md for details
   - SKILL.md links to examples.md for code samples
   - reference.md and examples.md have navigation back to SKILL.md

## Examples of Multi-File Skills

See existing Skills for reference:

- `color-accessibility-diagrams` (SKILL.md + examples.md)
- `repository-architecture` (SKILL.md + reference.md)

---

**Note**: Not all Skills need all three files. Start with SKILL.md (always required) and add reference.md or examples.md only when beneficial for organization and clarity.
