# Least Recently Used (LRU) Cache with Doubly Linked List

Self-contained **Least Recently Used (LRU) Cache with Doubly Linked List** algorithmic primitive written in idiomatic **Haskell**. Built from scratch using standard library constructs with zero external dependencies.

---

## 🏛️ Architecture & Design Decisions

This module organizes `Least Recently Used (LRU) Cache with Doubly Linked List` into an isolated, self-contained unit:
* **Domain Focus**: `Low-Latency Systems & Memory Layout`
* **Primary Primitives**: `Contiguous Memory Buffer & Ring Pointers`
* **Memory Strategy**: Memory allocations are kept minimal to maintain clear data locality and predictable memory bounds.
* **Correctness Model**: State transitions follow clear ordering guarantees with explicit validation at each phase.

### Asymptotic Complexity

| Metric | Bound | Characteristics |
| :--- | :---: | :--- |
| **Best Case Time** | `O(1)` | Optimized fast-path execution |
| **Average / Worst Time** | `O(1)` | Deterministic upper bound for generalized workloads |
| **Space Complexity** | `O(N) bounded` | Strict bounds without unconstrained heap growth |

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

<sub>Standard Haskell reference implementation • Maintained by [@myonathanlinkedin](https://github.com/myonathanlinkedin)</sub>
