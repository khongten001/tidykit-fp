# TidyKit FAQ: Why ARC/Interfaces? What is a Factory Method?

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

---

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

**Summary Table:**
| Pattern           | Example Usage                | Memory Management | Purpose                        |
|-------------------|-----------------------------|-------------------|--------------------------------|
| Static Function   | `TFileKit.ReadFile(...)`     | N/A               | Utility, no object returned    |
| Factory Method    | `TJSON.Obj`                 | ARC (interface)   | Create and return an object    |
| Interface         | `IMatrix`, `IJSONObject`    | ARC               | Abstraction, memory safety     |

---

## In summary
- TidyKit uses ARC and interfaces for safety, modernity, and ease of use.
- Factory methods are static functions that create and return interface-based objects, combining the best of both worlds.
- For simple utilities, static functions are used.
- For complex data, interfaces and factories provide safety and flexibility.

If you have more questions, please open an issue or discussion on GitHub!