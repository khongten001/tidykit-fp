# Frequently Asked Questions (FAQ)

**Q1: What is the overall design philosophy of TidyKit's API?**

A1: TidyKit aims for a balance between ease of use for simple tasks and flexibility/testability for more complex components. Currently, it uses a mix of patterns:
    *   **Static Class Methods:** For stateless utility functions (like string manipulation in `TStringKit` or date formatting in `TDateTimeKit`). These are simple to call directly without creating objects.
    *   **Factory/Interface:** For components that manage state, interact with external systems (like files or networks), or benefit significantly from mocking in tests (like `TJSONFactory`/`IJSONObject`). This pattern uses interfaces for automatic memory management and allows for dependency injection and easier testing.

**Q2: Is the API stable? I see different patterns in different modules.**

A2: The API is currently undergoing simplification, planned for completion in **version 0.2.0**. You are correct that there's a mix of patterns right now. The goal for v0.2.0 is to solidify the structure described in Q1:
    *   **Factory/Interface:** Will be consistently used for `FS`, `JSON`, `Logger`, `Request`, and `Matrices`. This provides better testability and flexibility for these stateful or complex components.
    *   **Static Methods:** Will be consistently used for stateless utilities like `DateTime`, `Strings`, `Archive`, `Crypto.*`, and `Math.*` (excluding `Matrices`).

We recommend checking the `README.md`'s "Architectural Patterns" and "Roadmap" sections for the latest status on the effort of simplifying the API.

**Q3: Why use Factory/Interface planned for `TidyKit.FS` and `TidyKit.Request`?**

A3: File system (`FS`) and network (`Request`) operations interact with external systems (disk, OS, network). Using an interface (`IFileKit`, `IRequestClient`) allows developers to easily *mock* these interactions during unit testing. For example, you can create a fake `IFileKit` that simulates file operations in memory without actually touching the disk, making tests faster and more reliable. This testability is a major advantage over using only static methods for these kinds of operations.

**Q4: Why keep static methods for `TidyKit.Strings`, `TidyKit.DateTime`, etc.?**

A4: These modules primarily contain stateless helper functions. For instance, `TStringKit.Trim(' text ')` doesn't depend on any external state or previous operations. Using static methods here is simpler, more direct, and avoids the overhead of object creation and interface calls for basic utility tasks.

**Q5: What changes are planned for `TidyKit.Matrices` in v0.2.0?**

A5: `TidyKit.Matrices` currently uses a class/interface pattern. For consistency with `TidyKit.JSON` and to potentially improve testability and flexibility (e.g., allowing different matrix implementations in the future), it's planned to transition to a Factory/Interface pattern (`TMatrixFactory`, `IMatrix`) in version 0.2.0.

**Q6: What's the focus after simplifying the API in v0.2.0?**

A6: Version 0.3.0 will focus on practical application, including adding more real-world examples and tutorials to help users effectively leverage the simplified API in their projects. Performance tuning and further refinements based on user feedback are also planned. Additionally, we aim to enhance documentation and provide more comprehensive guides for new users to facilitate easier onboarding.
