# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

...

## Release [0.1.7] - 2025-04-30

### Changed

- Moved all TidyKit.Math.* modules (Statistics, Matrices, Trigonometry, Finance) to a separate library to keep TidyKit-fp lean for application development work.
- Updated documentation to reflect the restructuring of modules.
- Removed TidyKit.Core.pas unit as it was no longer needed after the restructuring.


### Added

- New clear focus on application development utilities without scientific computing components.
- Improved installation instructions.

### Fixes

- Various minor bugfixes and performance improvements.

## Release [0.1.6] - 2025-04-24

### Added

- More examples to showcase the usage of TidyKit.FS module.

### Fixes

- Various bugfixes and improvements to the TidyKit.FS module.
- Bugfix TidyKit.Logger.pas unit

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