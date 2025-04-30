# MetaPipeline

**MetaPipeline** is a live Haskell pipeline system that combines:
- **Runtime Meta-Programming** using the `hint` interpreter.
- **Compile-Time Meta-Programming** using `TemplateHaskell`.

### Features:
- Build dynamic pipelines live
- Save, load, edit pipelines
- Runtime dynamic evaluation
- Compile-Time generated functions
- Fully functional, colorful CLI
- Built for learning and elegant coding

---
### Concepts:

| Technique | Usage |
|:----------|:------|
| Runtime Meta-Programming | User-written Haskell snippets compiled and run live |
| Compile-Time Meta-Programming | Auto-generated reusable functions at compile-time |
| Higher Order Functions | Function composition, pipelines |

---

### How to Run:
```bash
stack build
stack run