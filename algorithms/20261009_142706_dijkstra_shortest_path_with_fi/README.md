# Dijkstra Shortest Path with Fibonacci Heap Priority Queue in Haskell

An in-memory reference implementation of **Dijkstra Shortest Path with Fibonacci Heap Priority Queue** in **Haskell**, adhering to standard library idioms, clean data structures, and assertion test suites.

## Implementation Details

* **Category**: `Graph Topology & Traversal`
* **Data Structure Foundation**: `Adjacency List & Priority Heap`
* **Allocation Pattern**: Memory allocations are kept minimal to maintain clear data locality and predictable memory bounds.
* **Invariant Integrity**: Execution behavior is validated against nominal workflows and boundary edge cases.

## Performance Characteristics

* **Time**: `O((V + E) log V)` average, with `O(V + E)` best-case response under ideal conditions.
* **Space**: `O(V + E)` memory usage.

## Test Harness

To compile and execute the test assertions for this module:

```bash
runghc main.hs
```

---

*Part of the Polyglot Systems Lab • Maintained by [@myonathanlinkedin](https://github.com/myonathanlinkedin)*