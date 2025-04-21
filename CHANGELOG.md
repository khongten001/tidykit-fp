# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Planned for v0.2.0 - Simpler API Structure
- **Goal:** Introduce a simpler, more consistent API structure.
- **`TidyKit.FS`:** Will be using the Factory/Interface pattern (`TFSFactory`, `IFileKit`).
- **`TidyKit.DateTime`, `Strings`, `Math.*` (excluding Matrices), `Crypto.*`, `Archive`:** Will remain as Static Class Methods (`TStringKit`, `TDateTimeKit`, etc.) for stateless utilities.
- **`TidyKit.Matrices`:** Planned transition from the current Class/Interface (`TMatrix`) to a Factory/Interface pattern (`TMatrixFactory`, `IMatrix`) for consistency and improved testability.
- **`TidyKit.Request`:** Planned implementation using a Factory/Interface pattern (`TRequestFactory`, `IRequestClient`) for enhanced testability (mocking network requests).
- **`TidyKit.JSON`, `TidyKit.Logger`:** Existing Factory/Interface patterns will be reviewed and potentially refined for consistency.
- Documentation and examples will be updated to reflect these changes.

### Planned for v0.3.0 - Examples & Refinements
- Focus on adding more comprehensive real-world examples and tutorials.
- Performance analysis and optimization efforts.
- Refinement of error handling and diagnostic messages.
- Increased unit test coverage.

## [0.1.5] - 2025-04-21

### Added
- Added Ubuntu 24.04.02 compatibility for TidKit.DateTime and TidyKit.FS modules
- Added automatic test environment detection for TidyKit.Request
- Added HTTP fallback mechanism for testing HTTPS endpoints when OpenSSL is unavailable
- Added detailed OpenSSL installation instructions for Linux distributions
- Added cross-platform SSL/TLS initialization support for HTTP requests

### Fixed
- Fixed file timestamp handling issues on Unix systems
- Fixed path normalization for cross-platform compatibility
- Resolved file path length detection issues on Linux
- Corrected directory sorting behavior on Unix filesystems
- Fixed OpenSSL initialization and error handling on Linux systems
- Fixed HTTP request error handling to work consistently across platforms
- Improved TryGet and TryPost error handling for SSL failures

### Changed
- Reorganized platform-specific code for better readability
- Improved test organization with clearer platform-specific sections 
- Enhanced comments throughout platform-specific code sections
- Refactored TidyKit.Request.pas to use platform-specific implementations of SSL initialization
- Updated documentation to include Linux OpenSSL dependencies for HTTPS support

### Removed

...

## [0.1.0] - 2025-03-13

### Added
- Comprehensive Math modules:
  - Statistical calculations (`TStatsKit` class)
  - Financial mathematics (`TFinanceKit` class)
  - Matrix operations with decompositions (`TMatrixKit` class)
  - Trigonometric functions (`TTrigKit` class)
- JSON operations with interface-based memory management
- Logging system with multiple output destinations
- Cryptography enhancements:
  - SHA3 implementation
  - SHA2 family (SHA-256, SHA-512, SHA-512/256)
  - AES-256 encryption with CBC and CTR modes
- Archive operations (ZIP/TAR)
- HTTP client with request/response handling
- String representations for all matrix decompositions
- Initial release of TidyKit
- FileSystem operations (`TFileKit`)
- String operations (`TStringKit`)
- DateTime operations (`TDateTimeKit`)
- Core functionality
- Cross-platform support (Windows tested)
- Core mathematical types and operations
- Base file system operations
- String manipulation capabilities
- Basic error handling

### Improved
- Comprehensive documentation:
  - Created dedicated documentation files for each math module
  - Added detailed examples for all operations
  - Improved API references with mathematical explanations
  - Added cheat sheet for quick reference
- Memory-safe interface design for matrices
- Professional README with badges and detailed feature list
- Code organization and naming consistency

### Fixed
- String representation format for matrix decompositions
- Precision handling in financial calculations
- Memory leaks in matrix operations
- Error handling in statistical functions

### Known Issues
- Limited timezone support on Unix-like systems
- Untested on macOS and FreeBSD platforms