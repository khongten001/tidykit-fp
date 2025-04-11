# Changelog

All notable changes to TidyKit will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Added Ubuntu 24.04.02 compatibility for TidKit.DateTime and TidyKit.FS modules

### Fixed
- Fixed file timestamp handling issues on Unix systems
- Fixed path normalization for cross-platform compatibility
- Resolved file path length detection issues on Linux
- Corrected directory sorting behavior on Unix filesystems

### Changed
- Reorganized platform-specific code for better readability
- Improved test organization with clearer platform-specific sections 
- Enhanced comments throughout platform-specific code sections

### Removed

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
- Untested on Linux, macOS, and FreeBSD platforms 