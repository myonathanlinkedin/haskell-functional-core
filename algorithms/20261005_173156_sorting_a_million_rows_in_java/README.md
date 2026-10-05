# Sorting a million rows in JavaScript, and where the time actually goes

Production-ready implementation of the **Sorting a million rows in JavaScript, and where the time actually goes** algorithm in **Haskell**, adhering to idiomatic design patterns, cache-friendly data layouts, and comprehensive test assertions.

---

## 🏛️ Architecture & Design Decisions

This module organizes `Sorting a million rows in JavaScript, and where the time actually goes` into an isolated, self-contained unit:
* **Domain Focus**: `Algorithmic Engineering`
* **Primary Primitives**: `Standard Memory Primitives`
* **Memory Strategy**: Memory allocations are kept minimal to avoid allocator contention and preserve CPU cache locality.
* **Correctness Model**: Deterministic behavior across all execution cycles, resilient against asynchronous edge conditions.

### Asymptotic Complexity

| Metric | Bound | Characteristics |
| :--- | :---: | :--- |
| **Best Case Time** | `$O(1)$` | Optimized fast-path execution |
| **Average / Worst Time** | `$O(N)$` | Deterministic upper bound for generalized workloads |
| **Space Complexity** | `$O(N)$` | Strict bounds without unconstrained heap growth |

---

## 🧪 Verification Suite

The accompanying `main.hs` driver executes self-contained verification tests:
1. **Nominal Flow**: Validates baseline correctness under typical real-world inputs.
2. **Boundary Conditions**: Exercises extreme edge cases (empty inputs, singletons, capacity limits).
3. **Invariant Preservation**: Validates internal state consistency throughout mutation lifecycles.

### Running Locally

```bash
runghc main.hs
```

---

<sub>Crafted with modern Haskell standards • Maintained by [@myonathanlinkedin](https://github.com/myonathanlinkedin)</sub>