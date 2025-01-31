# 🧰 TidyKit

TidyKit is a Free Pascal library that helps you tackle common tasks faster, with clean, type-safe code.

## 🎯 Why TidyKit?

- **Simplified APIs**: Common operations in just one line of code
- **Type Safety**: Catch errors at compile time, not runtime

> [!WARNING]
> This library is currently in early development stage. The API is not stable and may undergo breaking changes between versions. 
> 
> Use with caution in production environments.

## ✨ Key Features

- 🎯 **FPC 3.2.2 Compatible**: No inline var, anonymous functions, or lambda
- 🌐 **Cross-Platform**: Works on Windows, Linux, macOS, and FreeBSD
- 🧱 **Static Functions**: No instance management or memory leaks
- 🛡️ **Memory Safe**: Proper resource management
- ⚡ **High Performance**: Optimized for both 32-bit and 64-bit systems
- 📦 **Zero Dependencies**: Just FPC standard library

## 🚀 Quick Start

1. Clone and add to your project:

```bash
git clone https://github.com/ikelaiah/tidykit-fp
```

2. Add the following to your project's `uses` clause:

```pascal
program MyProject;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  TidyKit; // Add this line

begin
  // Your code here
end.
```

## 📚 Available Modules

- 📝 **String Operations**: Pattern matching, transformations, and text manipulation
- 📅 **DateTime Operations**: Parsing, formatting, arithmetic, and timezone handling
- 🗂️ **FileSystem Operations**: File/directory operations, path handling, archives (basic zip and tar support)
- 🔒 **Cryptographic Operations**: Hashing, encoding, and basic encryption

## 💡 Example Usage

```pascal
// String operations
WriteLn(TStringKit.PadCenter('title', 20, '*'));  // *******title********

// DateTime operations
WriteLn(TDateTimeKit.GetAsString(TDateTimeKit.GetNow, 'yyyy-mm-dd'));  // 2024-03-20

// File operations
TFileKit.WriteFile('output.txt', 'Hello World');
Files := TFileKit.ListFiles('.', '*.txt', True);  // Recursive search for .txt files

// Crypto operations
WriteLn(TCryptoKit.SHA256Hash('Hello World'));  // 64-char hex string
```


## 📖 Documentation

See our [cheat-sheet.md](docs/cheat-sheet.md) for a comprehensive reference of all features.

### Technical Notes

- [SHA512_Implementation](docs/SHA512_Implementation.md)
- [SHA512_256_Implementation](docs/SHA512_256_Implementation.md)
- [Tar_tutorial_using_libtar](docs/Tar_tutorial_using_libtar.md)
- [Zip_tutorial_using_zipper](docs/Zip_tutorial_using_zipper.md)

## 🧪 Platform Testing Status

- ✅ Windows (32/64-bit): Fully tested
- ⚠️ Linux, macOS, FreeBSD: Needs testing (contributions welcome!)

## 🤝 Contributing

Contributions are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## 📝 License

MIT License - see [LICENSE.md](LICENSE.md)

## 📞 Contact

Project Link: https://github.com/ikelaiah/tidykit-fp