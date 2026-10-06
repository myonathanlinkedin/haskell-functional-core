# Suffix Automaton for Linear-Time Substring Indexing in Haskell

A clean, dependency-free **Haskell** reference implementation of **Suffix Automaton for Linear-Time Substring Indexing**, focused on core algorithmic mechanics, clear memory layout, and test verification.

## Implementation Details

* **Category**: `Algorithmic Engineering`
* **Data Structure Foundation**: `Standard Memory Primitives`
* **Allocation Pattern**: Zero external heap dependencies; designed as a pure in-memory algorithmic component.
* **Invariant Integrity**: State consistency is verified after mutations through assertion test coverage.

## Performance Characteristics

* **Time**: `O(N)` average, with `O(1)` best-case response under ideal conditions.
* **Space**: `O(N)` memory usage.

## Test Harness

To compile and execute the test assertions for this module:

```bash
runghc main.hs
```

---

<sub>Standard Haskell reference implementation • Maintained by [@myonathanlinkedin](https://github.com/myonathanlinkedin)</sub>
