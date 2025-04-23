# FileBackup

The FileBackup example demonstrates how to use the `TidyKit.FS` module to create a file backup utility. It shows how to:

- Create and use the FileKit interface
- Search for files matching specific patterns
- Copy files preserving directory structure
- Work with file metadata
- Handle paths and directories
- Create log files

### Usage

```
FileBackup [options]

Options:
  -s, --source=DIR    Source directory (required)
  -d, --dest=DIR      Destination directory (required)
  -p, --pattern=GLOB  File pattern to include (can be used multiple times)
  -e, --exclude=GLOB  File pattern to exclude (can be used multiple times)
  -l, --log=FILE      Log file path
  -r, --recursive     Recursively process subdirectories
  -v, --verbose       Display detailed information
  --simulate          Simulate backup without copying files
  -h, --help          Display this help message
```

### Example

```
FileBackup --source=c:\documents --dest=d:\backup --pattern=*.docx --pattern=*.xlsx --recursive
```

This creates a timestamped backup of all .docx and .xlsx files from c:\documents to d:\backup, 
preserving the directory structure.
