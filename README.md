# 🧠 Leveraging Meta-Programming in Functional Programming

Welcome to our course project for *Programming Languages* at IIT Ropar!  
This repository demonstrates how **meta-programming** can be applied effectively in a **functional programming** language — specifically **Haskell**. We explore both compile-time and runtime meta-programming constructs using first-class functions, closures, and dynamic pipelines.

---

## 📚 Table of Contents

- [About the Project](#about-the-project)
- [Key Concepts](#key-concepts)
- [Repository Structure](#repository-structure)
- [Getting Started](#getting-started)
- [Usage](#usage)
- [Demo](#demo)
- [Project Report](#project-report)
- [Team Members](#team-members)
- [License](#license)
- [Acknowledgements](#acknowledgements)

---

## 📌 About the Project

The project titled **"Leveraging Meta-Programming in Functional Programming"** focuses on building a **dynamic pipeline system** in Haskell. Users can construct a sequence of transformations at runtime and apply them to input data.

We leverage:
- Higher-order functions
- Pure functional design
- Runtime configuration of function pipelines
- Planned support for ASCII visualization and persistent configuration

This project reflects how powerful functional programming can be when extended with meta-programming concepts typically associated with languages like Lisp, Python, or C++.

---

## 🧠 Key Concepts

| Concept | Description |
|--------|-------------|
| **First-Class Functions** | Functions are treated as values, passed and returned like any other data. |
| **Closures** | Functions can capture variables from their enclosing scope. |
| **Function Composition** | Ability to compose multiple transformations cleanly and modularly. |
| **Meta-Programming** | Writing programs that generate, modify, or orchestrate other programs. |
| **Dynamic Pipelines** | Construct transformation pipelines interactively at runtime. |

---

## 📁 Repository Structure
Programming_Languages_Project/
├── Inventory/                  # Main code for the pipeline system
│   ├── Main.hs                # Entry point
│   ├── Transformations.hs     # Custom string transformation functions
│   └── Utils.hs               # Helper functions for pipeline processing
├── docs/                      # LaTeX report and documentation
│   └── project_report.tex
├── demo/                      # Example inputs and output logs
├── LICENSE
└── README.md                  # Project readme (this file)

---

## ⚙️ Getting Started

### 1. Prerequisites

Ensure you have Haskell installed:

```bash
sudo apt install ghc cabal-install
Or install via https://www.haskell.org/
```

### 2. Clone the Repository

```bash
git clone https://github.com/Tanish-pat/Programming_Languages_Project.git
cd Programming_Languages_Project/Inventory
```

### 3. Compile & Run

```bash
ghc Main.hs -o pipeline
./pipeline
```

## 📖 Project Report

The full report detailing our motivation, design, theory, and implementation is available in the docs/ folder.

Topics covered:
	•	What is meta-programming?
	•	Haskell vs. other languages for meta-programming
	•	Design of the pipeline engine
	•	Code walk-through and evaluation
	•	Limitations and future improvements


## 🧑‍💻 Team Members
	•	Rutul Patel
	•	Hemang Seth
	•	Tanish Pathania

 ## 🔮 Future Work
	•	Add persistent configuration with SQLite
	•	Enable CLI flag parsing and JSON-based pipelines
	•	Animate pipeline flow using ASCII graphics
	•	Add support for custom user-defined functions at runtime

 ## 🙌 Acknowledgements
 	•	Open-source Haskell community
	•	Our course instructor and TAs for their guidance
