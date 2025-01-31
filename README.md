# 🧰 TidyKit

TidyKit is a Free Pascal library that helps you tackle common tasks faster, with clean, type-safe code.

## 🎯 Why TidyKit?

- **Simplified APIs**: Common operations in just one line of code
- **Type Safety**: Catch errors at compile time, not runtime
- **Flexible Usage**: Use the entire toolkit or just the modules you need

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
- 🔄 **Modular Design**: Use only what you need

## 📚 Available Modules

- 🔒 **Cryptographic Operations** (TidyKit.Crypto)
  - Hash functions (MD5, SHA1, SHA2 family)
  - Encryption (Blowfish, XOR)
  - Base64 encoding/decoding

- 🗂️ **FileSystem Operations** (TidyKit.FS)
  - File searching and filtering
  - Path manipulation
  - File attributes handling
  - Basic zip and tar support

- 📝 **String Operations** (TidyKit.Strings)
  - Pattern matching
  - String transformations
  - Text processing

- 📅 **DateTime Operations** (TidyKit.DateTime)
  - Date arithmetic
  - Time zone handling
  - Duration calculations

## 🚀 Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/ikelaiah/tidykit-fp
   ```

2. Add the `src` directory to your project's search path.

## 📖 Usage

TidyKit offers two flexible ways to use its functionality:

### 1. Complete Package

If you want to use multiple features from TidyKit, simply use the main unit:

```pascal
program MyProject;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  TidyKit;  // Include everything

var
  Hash: string;
  Files: TSearchResults;
begin
  // Use any TidyKit functionality
  Hash := TCryptoKit.SHA256Hash('test');
  Files := TFileKit.FindFiles('*.pas');
end.
```

### 2. Individual Modules

For better control over dependencies and potentially smaller binary size, you can use only the specific modules you need:

```pascal
program MyProject;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  TidyKit.Crypto,  // Only if you need cryptographic functions
  TidyKit.FS;      // Only if you need filesystem operations

var
  Hash: string;
  Files: TSearchResults;
begin
  Hash := TCryptoKit.SHA256Hash('test');
  Files := TFileKit.FindFiles('*.pas');
end.
```

Available modules:
- `TidyKit.Crypto` - Cryptographic operations
- `TidyKit.FS` - File system operations
- `TidyKit.Strings` - String manipulation
- `TidyKit.DateTime` - Date and time utilities
- `TidyKit.Core` - Core functionality

## 💡 Example Usage

### Cryptography
```pascal
uses
  TidyKit.Crypto;

var
  EncryptedText: string;
begin
  // Hash
  WriteLn(TCryptoKit.SHA256Hash('test'));
  
  // Encryption
  EncryptedText := TCryptoKit.BlowfishCrypt('Secret text', 'key', bmEncrypt);
  WriteLn(TCryptoKit.BlowfishCrypt(EncryptedText, 'key', bmDecrypt));
end;
```

### File Operations
```pascal
uses
  TidyKit.FS;

var
  Files: TSearchResults;
begin
  Files := TFileKit.FindFiles('*.txt', True);  // True for recursive search
  try
    for var File in Files do
      WriteLn(File.Name);
  finally
    Files.Free;
  end;
end;
```

### Date/Time Operations
```pascal
uses
  TidyKit.DateTime;

var
  NextWeek: TDateTime;
begin
  NextWeek := TDateTimeKit.AddInterval(Now, 1, duWeek);
  WriteLn(DateTimeToStr(NextWeek));
end;
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