# Phase 2 Rollout Order

Based on pilot learnings and baseline audit (6,000-8,000 lines duplication potential), prioritize families with highest duplication.

## Rollout Batches

### Batch 1: Remaining docs agents (5 agents) - MEDIUM Priority

**Agents**:

1. docs\_\_tutorial-maker
2. docs\_\_tutorial-checker
3. docs\_\_tutorial-fixer
4. docs\_\_file-manager
5. docs\_\_link-general-checker

**Rationale**: Complete docs family (pilot validated approach)

**Expected Reduction**: 20-40% average (similar patterns to pilot)

---

### Batch 2: readme family (3 agents) - HIGH Priority

**Agents**:

1. readme\_\_maker
2. readme\_\_checker
3. readme\_\_fixer

**Rationale**: Similar to docs family, maker-checker-fixer trio

**Expected Reduction**: 30-50% average (checker/fixer should reduce significantly)

---

### Batch 3: plan family (5 agents) - HIGH Priority

**Agents**:

1. plan\_\_maker
2. plan\_\_checker
3. plan\_\_executor
4. plan\_\_execution-checker
5. plan\_\_fixer

**Rationale**: Larger family, includes execution checker

**Expected Reduction**: 25-45% average

---

### Batch 4: ayokoding-web family - Part 1: By-Example (6 agents) - CRITICAL Priority

**Agents**:

1. apps**ayokoding-web**by-example-maker
2. apps**ayokoding-web**by-example-checker
3. apps**ayokoding-web**by-example-fixer
4. apps**ayokoding-web**general-maker
5. apps**ayokoding-web**general-checker
6. apps**ayokoding-web**general-fixer

**Rationale**: Highest duplication per baseline audit (~800 lines in by-example-maker alone)

**Expected Reduction**: 40-60% average (annotation standards, Hugo weight system, criticality systems)

---

### Batch 5: ayokoding-web family - Part 2: Validators (6 agents) - CRITICAL Priority

**Agents**:

1. apps**ayokoding-web**facts-checker
2. apps**ayokoding-web**facts-fixer
3. apps**ayokoding-web**link-checker
4. apps**ayokoding-web**link-fixer
5. apps**ayokoding-web**structure-checker
6. apps**ayokoding-web**structure-fixer

**Rationale**: Checker/fixer agents with criticality systems

**Expected Reduction**: 35-55% average

---

### Batch 6: ayokoding-web family - Part 3: Makers & Operations (4 agents) - MEDIUM Priority

**Agents**:

1. apps**ayokoding-web**structure-maker
2. apps**ayokoding-web**navigation-maker
3. apps**ayokoding-web**title-maker
4. apps**ayokoding-web**deployer

**Rationale**: Makers and operations (less duplication than checkers/fixers)

**Expected Reduction**: 20-35% average

---

### Batch 7: ose-platform-web family (4 agents) - MEDIUM Priority

**Agents**:

1. apps**ose-platform-web**content-maker
2. apps**ose-platform-web**content-checker
3. apps**ose-platform-web**content-fixer
4. apps**ose-platform-web**deployer

**Rationale**: Similar to docs family but PaperMod theme

**Expected Reduction**: 25-45% average

---

### Batch 8: workflow family (3 agents) - MEDIUM Priority

**Agents**:

1. wow\_\_workflow-maker
2. wow\_\_workflow-checker
3. wow\_\_workflow-fixer

**Rationale**: Maker-checker-fixer trio, workflow-specific

**Expected Reduction**: 25-40% average

---

### Batch 9: wow-rules family (3 agents) - CRITICAL Priority

**Agents**:

1. wow\_\_rules-maker
2. wow\_\_rules-checker
3. wow\_\_rules-fixer

**Rationale**: Core quality enforcement agents, high importance

**Expected Reduction**: 30-50% average (criticality systems, validation patterns)

---

### Batch 10: Infrastructure agents (3 agents) - LOW Priority

**Agents**:

1. swe**hugo**developer
2. social**linkedin**post-maker
3. agent\_\_maker

**Rationale**: Specialized agents, likely minimal duplication

**Expected Reduction**: 10-30% average

---

## Execution Order

1. Batch 1 (docs remaining) - Complete docs family
2. Batch 4 (ayokoding by-example) - Highest duplication
3. Batch 5 (ayokoding validators) - High duplication
4. Batch 9 (wow-rules) - Critical importance
5. Batch 2 (readme) - Standard family
6. Batch 3 (plan) - Standard family
7. Batch 6 (ayokoding makers/ops) - Complete ayokoding family
8. Batch 7 (ose-platform-web) - Standard family
9. Batch 8 (workflow) - Standard family
10. Batch 10 (infrastructure) - Final batch

**Rationale**: Prioritize CRITICAL (highest duplication/importance) → HIGH → MEDIUM → LOW
