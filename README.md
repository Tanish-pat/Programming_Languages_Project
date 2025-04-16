
---

## **Steps To run**
- go to root directory
- run `make`
- run `make all`
- run `./backend`
- cd into `generated`
- run `make`
- run `make all`
- run `./test`

### **1. Compile-Time Meta-Programming with Template Haskell (TH)**

We use Template Haskell to generate Haskell code at **compile-time**. This is a form of **compile-time meta-programming**, where we define specific patterns or templates for our code, and the compiler generates the corresponding Haskell code for us.

In our project:
- The `generateModel` and `generateAllModels` functions in `src/ModelGen.hs` are Template Haskell functions. They automatically generate Haskell data types based on the model specifications defined in `src/ModelRegistry.hs`.
- The `$(generateAllModels models)` quasi-quote in `src/Main.hs` is executed at **compile-time**. It takes the `models` data from `src/ModelRegistry.hs` (which specifies various models like `User`, `Product`, etc.) and generates the Haskell code for each model type. This means we don't have to manually define the data types and fields for every model—everything is automatically generated based on the `models` data structure.

This is **compile-time meta-programming** because the code that represents our models (`User`, `Product`, etc.) is generated during compilation, before our program even runs.

### **2. Run-Time Meta-Programming**

Run-time meta-programming typically involves reflecting on or modifying code during execution, such as dynamically loading modules, generating objects, or altering behavior based on input or the environment.

In our case, we are working with a **type registry system** and using **existential types** and **runtime type information** (via `Typeable`) to store and manipulate objects of various models in the `Registry.hs` module. The `AnyModel` type uses **existential quantification** to allow us to store values of any type that is an instance of `Show` and `Typeable`.

In particular:
- The `insertRecord` function in `Registry.hs` inserts records dynamically into the registry (which could happen at runtime), and we can store values of various types (like `User`, `Product`, etc.) within the `AnyModel` type.
- `getTypedRecords` and other functions rely on **runtime type reflection** (via `Typeable`) to extract and process records dynamically at runtime.

This approach allows us to handle objects of different types (i.e., different models) in a generic way, and the actual operations on these objects (e.g., insertion into a map) occur at **runtime**. This is an example of **run-time meta-programming** in a **type-safe** manner, thanks to Haskell's powerful type system.

### **Summary of Meta-Programming Usage**

- **Compile-Time Meta-Programming (via Template Haskell)**: We use Template Haskell to automatically generate Haskell code for each model based on a specification at compile-time. This eliminates the need for us to manually define each model and its fields. The generated code is included in the project before the program even runs.

- **Run-Time Meta-Programming (via Existential Types and Typeable)**: We use existential types and `Typeable` to dynamically store and retrieve objects of various types in a registry at runtime. This allows our program to work with a flexible set of models (without knowing the exact types ahead of time), which is a form of run-time meta-programming that leverages runtime type information.

### Further Improvements:
If we’re looking to expand our use of metaprogramming, here are a few ideas:
- **Dynamic Model Creation at Runtime**: We could extend our program to allow for **runtime generation** of models based on user input or data from external sources (e.g., databases, configuration files).
- **Code Generation Based on Runtime Information**: We could introduce more advanced **runtime code generation** by combining Haskell's **Template Haskell** capabilities with some runtime logic. This could involve generating functions or even entire modules on-the-fly, based on runtime conditions.

In conclusion, we are using both **compile-time** and **run-time meta-programming** in our project. Template Haskell handles **compile-time** code generation, while existential types and `Typeable` provide **run-time** flexibility.

---
