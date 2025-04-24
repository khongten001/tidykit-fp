# TidyKit FAQ

## Contents
- [TidyKit FAQ](#tidykit-faq)
  - [Contents](#contents)
  - [Why does TidyKit use ARC (Automatic Reference Counting) and interfaces?](#why-does-tidykit-use-arc-automatic-reference-counting-and-interfaces)
  - [Performance: Automatic vs Manual Memory Management](#performance-automatic-vs-manual-memory-management)
    - [For Experienced: "Why use automatic when manual is more performant?"](#for-experienced-why-use-automatic-when-manual-is-more-performant)
    - [For New Users: "Why does TidyKit require no manual freeing of objects?"](#for-new-users-why-does-tidykit-require-no-manual-freeing-of-objects)
  - [What is a Factory Method? How is it different from interfaces or static functions?](#what-is-a-factory-method-how-is-it-different-from-interfaces-or-static-functions)
  - [Why can't TidyKit.Math.Matrices use Advanced Records?](#why-cant-tidykitmathmatrices-use-advanced-records)
  - [Summary](#summary)

## Why does TidyKit use ARC (Automatic Reference Counting) and interfaces?

TidyKit uses interfaces with ARC for complex types (like matrices, JSON, and logging) to provide:

- **Automatic memory management:** Objects are automatically freed when no longer referenced, reducing memory leaks and double-frees.
- **Safety:** No need to remember to call `Free` or handle exceptions for cleanup—ARC handles it for you.
- **Modern Pascal best practices:** Many modern Pascal libraries (including FPC's own JSON, XML, and database layers) use interfaces for memory safety and composability.
- **Cleaner code:** You can return objects from functions, store them in variables, and pass them around without worrying about manual cleanup.
- **Chaining and composition:** Fluent APIs (e.g., `A.Multiply(B).Transpose`) are easier and safer with ARC.

**Example:**
```pascal
var
  A, B, C: IMatrix;
begin
  A := TMatrixKit.CreateFromArray([[1.0, 2.0], [3.0, 4.0]]);
  B := A.Transpose;
  C := A.Multiply(B);
  // No need to call Free—memory is managed automatically
end;
```

**What if I want manual memory management?**
TidyKit is designed for safety and ease of use. Manual memory management is error-prone and not recommended for most users. If you need explicit control, you can use the underlying classes directly, but this is advanced usage and not the default.

## Performance: Automatic vs Manual Memory Management

### For Experienced: "Why use automatic when manual is more performant?"

This is a valid concern. Here's why TidyKit made this trade-off:

- **Correctness over raw performance:** Memory leaks and access violations are much harder to debug than slight performance hits.
- **Real bottlenecks:** In most real-world applications, algorithm efficiency and I/O are bigger bottlenecks than ARC overhead.
- **Cost vs. Benefit:** The performance cost is minimal for most use cases, while the safety benefit is substantial.
- **Measured impact:** The overhead of reference counting varies by operation - from negligible for most use cases to more noticeable in tight loops with many temporary objects. This trade-off is generally acceptable for the safety benefits provided.
- **Selective use:** TidyKit only uses ARC for complex objects where memory management errors are most likely to occur.

If maximum performance is critical for your specific use case, you can:

1. Use the static utility functions where possible (`TidyKit.FS`, `TidyKit.Strings`, etc.)
2. Minimize temporary object creation in tight loops
3. Pre-allocate objects outside loops
4. For advanced cases, access the underlying implementation classes directly (not recommended)

### For New Users: "Why does TidyKit require no manual freeing of objects?"

TidyKit uses two approaches to memory management:

1. **Interface-based ARC:** When you see an `I`-prefixed type (e.g., `IMatrix`, `IJSONObject`), you're using automatic reference counting. The object will be automatically freed when your variable goes out of scope or is assigned a different value.

2. **Static utilities:** Functions like `TFileKit.ReadFile` don't return objects that need manual management.

**Benefits for beginners:**

- No need to remember to call `Free`
- No chance of "double-free" errors
- No memory leaks from forgotten `Free` calls
- No try-finally blocks needed for cleanup
- Safe to use in complex code paths

```pascal
// Safe, even if exceptions occur
procedure ProcessData;
var
  Data: IJSONObject;
begin
  Data := TJSON.Parse(SomeInput);
  // Use Data...
  // No Free needed - cleanup happens automatically
end;
```

## What is a Factory Method? How is it different from interfaces or static functions?

A **Factory Method** is a design pattern where a static function (usually on a class) creates and returns an instance of an object, often as an interface. This pattern is used in TidyKit.JSON and TidyKit.Logger.

- **Factory Method:**
  - Static function (e.g., `TJSON.Obj`, `TLogger.CreateContext`)
  - Returns an interface (e.g., `IJSONObject`, `ILogContext`)
  - Hides the concrete implementation from the user
  - Ensures memory is managed automatically via ARC

**Example (TidyKit.JSON):**

```pascal
var
  Person: IJSONObject;
begin
  Person := TJSON.Obj; // Factory method creates the object
  Person.Add('name', 'John Smith');
end;
```

**Example (TidyKit.Logger):**

```pascal
var
  Log: ILogContext;
begin
  Log := TLogger.CreateContext('MyApp'); // Factory method
  Log.Info('Hello!');
end;
```

- **Interfaces:**
  - Define a contract for what methods/properties an object must have
  - Enable ARC when used as variable types
  - Used for memory safety and abstraction

- **Static Functions:**
  - Functions that belong to a class but do not require an instance
  - Used for utility operations (e.g., `TFileKit.ReadFile`)
  - Do not manage or return objects/interfaces

- **Advanced Records:**
  - Value semantics (copied when assigned)
  - Can implement interfaces
  - No inheritance support
  - Used for HTTP requests in TidyKit

**Summary Table:**

| Pattern           | Example Usage                | Memory Management | Purpose                        |
|-------------------|-----------------------------|-------------------|--------------------------------|
| Static Function   | `TFileKit.ReadFile(...)`     | N/A               | Utility, no object returned    |
| Factory Method    | `TJSON.Obj`                 | ARC (interface)   | Create and return an object    |
| Interface         | `IMatrix`, `IJSONObject`    | ARC               | Abstraction, memory safety     |
| Advanced Record   | `Http.Get`                  | Value semantics   | Stack-based operations         |

## Why can't TidyKit.Math.Matrices use Advanced Records?

The matrix library uses interface-based design instead of advanced records for several important reasons:

1. **Complex inheritance hierarchy**: The implementation includes multiple matrix types like `TMatrixKit` and `TMatrixKitSparse` that inherit from each other. Records don't support inheritance.

2. **Polymorphism requirements**: The code makes heavy use of runtime polymorphism through the `IMatrix` interface. While records can implement interfaces, they don't have the same level of polymorphic capabilities as classes.

3. **Extensible design**: The library is designed to allow for different matrix implementations (dense, sparse, etc.) that can be interchanged through the `IMatrix` interface.

4. **Size and memory concerns**: Matrices can be very large structures, and advanced records use value semantics, which could lead to expensive copies when passing matrices as parameters or returning them from functions. Using reference semantics avoids unnecessary copying.

5. **Complex decompositions**: Matrix operations like LU, QR, SVD, Cholesky decompositions return composite structures with multiple matrices. The interface-based approach makes managing these relationships cleaner.

6. **Memory management**: The matrices often contain large dynamically allocated arrays. The interface reference counting provides automatic cleanup of these resources when they're no longer needed.

7. **Virtual methods**: Some operations can be optimized differently for sparse vs. dense matrices, requiring virtual method dispatch that's more natural with class implementations.

Advanced records would work well for smaller mathematical structures (like vectors, points, complex numbers) where the value semantics make sense and copying is not expensive. But for large, dynamically-sized matrices with complex operations, the class-interface approach provides better memory efficiency and flexibility.

## Summary

- TidyKit uses ARC and interfaces for safety, modernity, and ease of use.
- Factory methods are static functions that create and return interface-based objects, combining the best of both worlds.
- For simple utilities, static functions are used.
- For complex data, interfaces and factories provide safety and flexibility.
- Advanced records are used where value semantics make sense (like HTTP requests).
- Matrices use class/interface design due to their complexity and performance requirements.

If you have more questions, please open an issue or discussion on GitHub!