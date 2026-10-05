# Single-Sample Prophet Inequalities: A Combinatorial to Single-Item Reduction

A clean, dependency-free **Haskell** implementation of **Single-Sample Prophet Inequalities: A Combinatorial to Single-Item Reduction**, focused on predictable latency, strict memory layout, and deterministic execution.

---

## 🏛️ Architecture & Design Decisions

This module organizes `Single-Sample Prophet Inequalities: A Combinatorial to Single-Item Reduction` into an isolated, self-contained unit:
* **Domain Focus**: `Algorithmic Engineering`
* **Primary Primitives**: `Standard Memory Primitives`
* **Memory Strategy**: Contiguous memory layouts are favored over scattered heap allocations for optimal traversal speed.
* **Correctness Model**: State consistency is verified after every mutation through formal invariant validation.

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

*Authored & verified by [@myonathanlinkedin](https://github.com/myonathanlinkedin) • Systems Engineering Portfolio*