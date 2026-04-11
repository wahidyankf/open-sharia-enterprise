---
name: swe-programming-clojure
description: Clojure coding standards from authoritative docs/explanation/software-engineering/programming-languages/clojure/ documentation
---

# Clojure Coding Standards

## Purpose

Progressive disclosure of Clojure coding standards for agents writing Clojure code.

**Usage**: Auto-loaded for agents when writing Clojure code. Provides quick reference to idioms, best practices, and antipatterns.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/clojure/README.md](../../../docs/explanation/software-engineering/programming-languages/clojure/README.md)

## Prerequisite Knowledge

**IMPORTANT**: This skill provides **OSE Platform-specific style guides**, not educational tutorials.

Complete the AyoKoding Clojure learning path first:

1. **[Clojure Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/clojure/)** - 0-95% language coverage
2. **[Clojure By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/clojure/by-example/)** - 75+ annotated examples

**See**: [Programming Language Documentation Separation](../../../governance/conventions/structure/programming-language-docs-separation.md)

## Quick Standards Reference

### Naming Conventions

**Functions/Variables**: kebab-case - `calculate-zakat`, `total-amount`, `validate-contract`

**Predicates**: end with `?` - `valid-nisab?`, `above-threshold?`

**Side-effecting functions**: end with `!` - `save-transaction!`, `send-notification!`

**Namespaces**: reverse-domain + feature - `com.oseplatform.zakat.calculator`

**Namespace aliases**: standard abbreviations - `(require [clojure.string :as str])`

### Core Idioms

```clojure
;; CORRECT: Pure function for Zakat calculation
(defn calculate-zakat
  "Calculate Zakat amount. Returns 2.5% if wealth >= nisab, else 0."
  [wealth nisab]
  (if (>= wealth nisab)
    (* wealth 0.025M)
    0M))

;; CORRECT: Threading macro for readability
(defn process-contracts [contracts nisab]
  (->> contracts
       (filter #(>= (:wealth %) nisab))
       (map #(assoc % :zakat-amount (calculate-zakat (:wealth %) nisab)))
       (remove nil?)))

;; CORRECT: Destructuring in function args
(defn format-payment [{:keys [amount currency date]}]
  (str amount " " currency " on " date))
```

### Namespaced Keywords for Domain

```clojure
;; CORRECT: Namespaced keywords for domain concepts
{:zakat/wealth 10000M
 :zakat/nisab 5000M
 :zakat/amount 250M
 :contract/id "murabaha-001"
 :contract/type :murabaha
 :contract/status :active}

;; WRONG: Unnamespaced keywords for domain data
{:wealth 10000M  ; ambiguous in a larger system
 :id "001"}
```

### Error Handling

```clojure
;; CORRECT: ex-info for structured errors
(defn validate-wealth [wealth]
  (when (neg? wealth)
    (throw (ex-info "Invalid wealth amount"
                    {:type :validation-error
                     :field :wealth
                     :value wealth
                     :message "Wealth cannot be negative"}))))

;; CORRECT: Catch specific ex-info
(try
  (validate-wealth -100M)
  (catch clojure.lang.ExceptionInfo e
    (let [data (ex-data e)]
      (log/error "Validation failed" data))))
```

### Concurrency with Atoms

```clojure
;; CORRECT: Atom for uncoordinated state
(def zakat-cache (atom {}))

;; CORRECT: swap! for atomic update (pure function)
(defn cache-calculation! [wealth result]
  (swap! zakat-cache assoc wealth result))

;; CORRECT: Refs for coordinated STM
(def total-zakat (ref 0M))
(def transaction-log (ref []))

(defn record-zakat! [amount]
  (dosync
    (alter total-zakat + amount)
    (alter transaction-log conj {:amount amount :time (java.time.Instant/now)})))
```

### Transducers for Efficiency

```clojure
;; CORRECT: Transducer pipeline (lazy, composable)
(def eligible-contracts-xf
  (comp
    (filter #(>= (:wealth %) nisab-threshold))
    (map #(assoc % :zakat (* (:wealth %) 0.025M)))
    (take 100)))

;; Apply to any collection
(into [] eligible-contracts-xf all-contracts)
(transduce eligible-contracts-xf + all-contracts)
```

### Testing

```clojure
;; CORRECT: clojure.test with descriptive names
(ns com.oseplatform.zakat.calculator-test
  (:require [clojure.test :refer [deftest testing is are]]
            [com.oseplatform.zakat.calculator :refer [calculate-zakat]]))

(deftest calculate-zakat-test
  (testing "wealth above nisab"
    (is (= 250M (calculate-zakat 10000M 5000M))))

  (testing "wealth below nisab"
    (is (= 0M (calculate-zakat 1000M 5000M))))

  (testing "wealth equal to nisab"
    (is (= 125M (calculate-zakat 5000M 5000M)))))
```

## Comprehensive Documentation

**Authoritative Index**: [docs/explanation/software-engineering/programming-languages/clojure/README.md](../../../docs/explanation/software-engineering/programming-languages/clojure/README.md)

### Mandatory Standards

1. **[Coding Standards](../../../docs/explanation/software-engineering/programming-languages/clojure/coding-standards.md)**
2. **[Testing Standards](../../../docs/explanation/software-engineering/programming-languages/clojure/testing-standards.md)**
3. **[Code Quality Standards](../../../docs/explanation/software-engineering/programming-languages/clojure/code-quality-standards.md)**
4. **[Build Configuration](../../../docs/explanation/software-engineering/programming-languages/clojure/build-configuration.md)**

### Context-Specific Standards

1. **[Error Handling](../../../docs/explanation/software-engineering/programming-languages/clojure/error-handling-standards.md)**
2. **[Concurrency](../../../docs/explanation/software-engineering/programming-languages/clojure/concurrency-standards.md)**
3. **[Functional Programming](../../../docs/explanation/software-engineering/programming-languages/clojure/functional-programming-standards.md)**
4. **[Performance](../../../docs/explanation/software-engineering/programming-languages/clojure/performance-standards.md)**
5. **[Security](../../../docs/explanation/software-engineering/programming-languages/clojure/security-standards.md)**
6. **[API Standards](../../../docs/explanation/software-engineering/programming-languages/clojure/api-standards.md)**
7. **[DDD Standards](../../../docs/explanation/software-engineering/programming-languages/clojure/ddd-standards.md)**
8. **[Java Interop](../../../docs/explanation/software-engineering/programming-languages/clojure/interop-standards.md)**

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Clojure README](../../../docs/explanation/software-engineering/programming-languages/clojure/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
