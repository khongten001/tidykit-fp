unit TidyKit.Archive;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, zipper, libtar, zstream, TidyKit.Core, TidyKit.FS;

type
  { TArchiveCompressionLevel defines the compression level for ZIP archives }
  TArchiveCompressionLevel = (
    clNone,      // Store files without compression
    clFastest,   // Fastest compression
    clDefault,   // Default compression (balance between speed and size)
    clMaximum    // Maximum compression
  );

  { EArchiveError is raised when there is an error during archive operations }
  EArchiveError = class(Exception);

  { TArchiveKit provides methods for compressing and decompressing files. }
  TArchiveKit = class
  public
    { Creates a zip archive from a file or directory.
      
      Parameters:
        ASourcePath - The path to the file or directory to compress.
        ADestPath - The path where the zip file should be created.
        Recursive - If True and ASourcePath is a directory, includes subdirectories.
        Level - Compression level to use (default is clDefault). }
    class procedure CreateZip(const ASourcePath, ADestPath: string; 
      const Recursive: Boolean = True;
      const Level: TArchiveCompressionLevel = clDefault); static;

    { Extracts a zip archive to a directory.
      
      Parameters:
        ASourcePath - The path to the zip file.
        ADestPath - The directory where files should be extracted.
        Recursive - If True, preserves directory structure from the archive. }
    class procedure ExtractZip(const ASourcePath, ADestPath: string; 
      const Recursive: Boolean = True); static;

    { Creates a tar archive from a file or directory.
      
      Parameters:
        ASourcePath - The path to the file or directory to compress.
        ADestPath - The path where the tar file should be created.
        Recursive - If True and ASourcePath is a directory, includes subdirectories. }
    class procedure CreateTar(const ASourcePath, ADestPath: string; 
      const Recursive: Boolean = True); static;

    { Extracts a tar archive to a directory.
      
      Parameters:
        ASourcePath - The path to the tar file.
        ADestPath - The directory where files should be extracted.
        Recursive - If True, preserves directory structure from the archive. }
    class procedure ExtractTar(const ASourcePath, ADestPath: string; 
      const Recursive: Boolean = True); static;
  private
    class procedure AddDirectoryToZip(Zipper: TZipper; const Directory, BasePath: string; 
      const ZipLevel: TCompressionLevel; const Recursive: Boolean); static;
  end;

implementation

class procedure TArchiveKit.AddDirectoryToZip(Zipper: TZipper; const Directory, BasePath: string;
  const ZipLevel: TCompressionLevel; const Recursive: Boolean);
var
  Files: TFilePathArray;
  Dirs: TFilePathArray;
  RelativePath: string;
  I: Integer;
  Entry: TZipFileEntry;
begin
  // Add files in this directory
  Files := TFileKit.ListFiles(Directory, '*', False);
  for I := 0 to High(Files) do
  begin
    RelativePath := ExtractRelativePath(BasePath, Files[I]);
    Entry := Zipper.Entries.AddFileEntry(Files[I], RelativePath);
    Entry.CompressionLevel := ZipLevel;
  end;

  // Add empty directories (they won't be created otherwise)
  if Directory <> BasePath then
  begin
    RelativePath := IncludeTrailingPathDelimiter(ExtractRelativePath(BasePath, Directory));
    Entry := Zipper.Entries.AddFileEntry('', RelativePath);
    Entry.CompressionLevel := ZipLevel;
  end;

  // Recursively add subdirectories if requested
  if Recursive then
  begin
    Dirs := TFileKit.ListDirectories(Directory, '*', False);
    for I := 0 to High(Dirs) do
      AddDirectoryToZip(Zipper, Dirs[I], BasePath, ZipLevel, True);
  end;
end;

class procedure TArchiveKit.CreateZip(const ASourcePath, ADestPath: string; 
  const Recursive: Boolean = True;
  const Level: TArchiveCompressionLevel = clDefault);
var
  Zipper: TZipper;
  Entry: TZipFileEntry;
  ZipLevel: TCompressionLevel;
begin
  WriteLn('CreateZip called with Source: ', ASourcePath, ', Dest: ', ADestPath);

  if not TFileKit.Exists(ASourcePath) then
  begin
    WriteLn('Source path does not exist: ', ASourcePath);
    raise EFileNotFoundException.Create('Source path does not exist: ' + ASourcePath);
  end;

  // Ensure the destination directory exists
  TFileKit.CreateDirectory(ExtractFilePath(ADestPath));

  // Create the zipper
  Zipper := TZipper.Create;
  try
    // Set the output file
    Zipper.FileName := ADestPath;
    WriteLn('Zipper created for: ', ADestPath);

    // Map compression level
    case Level of
      clNone: ZipLevel := zstream.clnone;
      clFastest: ZipLevel := zstream.clfastest;
      clDefault: ZipLevel := zstream.cldefault;
      clMaximum: ZipLevel := zstream.clmax;
    end;
    WriteLn('Compression level set to: ', IntToStr(Ord(ZipLevel)));

    if TFileKit.DirectoryExists(ASourcePath) then
    begin
      WriteLn('Adding directory contents to ZIP');
      AddDirectoryToZip(Zipper, ASourcePath, ASourcePath, ZipLevel, Recursive);
    end
    else
    begin
      WriteLn('Adding single file to ZIP: ', ASourcePath);
      Entry := Zipper.Entries.AddFileEntry(ASourcePath, ExtractFileName(ASourcePath));
      if Entry = nil then
        raise EArchiveError.Create('Failed to add file to ZIP: ' + ASourcePath);
      Entry.CompressionLevel := ZipLevel;
    end;

    // Create the zip file
    if Zipper.Entries.Count = 0 then
      raise EArchiveError.Create('No files added to ZIP archive');

    try
      Zipper.ZipAllFiles;
      WriteLn('ZIP file created: ', ADestPath);

      // Verify the ZIP was created
      if not TFileKit.Exists(ADestPath) then
        raise EArchiveError.Create('Failed to create ZIP file: ' + ADestPath);
    except
      on E: Exception do
      begin
        WriteLn('Error creating ZIP file: ', E.Message);
        raise EArchiveError.Create('Error creating ZIP file: ' + E.Message);
      end;
    end;
  finally
    Zipper.Free;
  end;
end;

class procedure TArchiveKit.ExtractZip(const ASourcePath, ADestPath: string; const Recursive: Boolean = True);
var
  Unzipper: TUnZipper;
begin
  if not TFileKit.Exists(ASourcePath) then
    raise EFileNotFoundException.Create('ZIP file does not exist: ' + ASourcePath);

  TFileKit.CreateDirectory(ADestPath);

  Unzipper := TUnZipper.Create;
  try
    Unzipper.FileName := ASourcePath;
    Unzipper.OutputPath := ADestPath;
    
    try
      // Extract all files
      Unzipper.UnZipAllFiles;
      
      // Verify at least the destination directory exists
      if not TFileKit.DirectoryExists(ADestPath) then
        raise EArchiveError.Create('Failed to extract ZIP file: ' + ASourcePath);
    except
      on E: Exception do
        raise EArchiveError.Create('Error extracting ZIP file: ' + E.Message);
    end;
  finally
    Unzipper.Free;
  end;
end;

class procedure TArchiveKit.CreateTar(const ASourcePath, ADestPath: string; const Recursive: Boolean = True);
var
  Writer: TTarWriter;
  Files: TFilePathArray;
  Dirs: TFilePathArray;
  DirTime: TDateTime;
  I: Integer;
begin
  WriteLn('CreateTar called with Source: ', ASourcePath, ', Dest: ', ADestPath);

  if not TFileKit.Exists(ASourcePath) then
  begin
    WriteLn('Source path does not exist: ', ASourcePath);
    raise EFileNotFoundException.Create('Source path does not exist: ' + ASourcePath);
  end;

  // Ensure the destination directory exists
  TFileKit.CreateDirectory(ExtractFilePath(ADestPath));

  Writer := TTarWriter.Create(ADestPath);
  try
    if TFileKit.DirectoryExists(ASourcePath) then
    begin
      WriteLn('Adding directory contents to TAR');
      // Add directories first (with timestamps)
      if Recursive then
      begin
        Dirs := TFileKit.ListDirectories(ASourcePath, '*', True);
        for I := 0 to High(Dirs) do
        begin
          DirTime := TFileKit.GetLastWriteTime(Dirs[I]);
          Writer.AddDir(ExtractRelativePath(ASourcePath, Dirs[I]), DirTime);
          WriteLn('Added directory to TAR: ', Dirs[I]);
        end;
      end;

      // Add files
      Files := TFileKit.ListFiles(ASourcePath, '*', Recursive);
      for I := 0 to High(Files) do
      begin
        Writer.AddFile(Files[I], ExtractRelativePath(ASourcePath, Files[I]));
        WriteLn('Added file to TAR: ', Files[I]);
      end;
    end
    else
    begin
      Writer.AddFile(ASourcePath, ExtractFileName(ASourcePath));
      WriteLn('Added single file to TAR: ', ASourcePath);
    end;
  finally
    Writer.Free;  // TTarWriter writes the file when freed
    WriteLn('TAR file created: ', ADestPath);
  end;
end;

class procedure TArchiveKit.ExtractTar(const ASourcePath, ADestPath: string; const Recursive: Boolean = True);
var
  Archive: TTarArchive;
  DirRec: TTarDirRec;
  ExtractPath: string;
begin
  if not TFileKit.Exists(ASourcePath) then
    Exit;

  TFileKit.CreateDirectory(ADestPath);

  Archive := TTarArchive.Create(ASourcePath);
  try
    while Archive.FindNext(DirRec) do
    begin
      if not Recursive and (Pos(DirectorySeparator, DirRec.Name) > 0) then
        Continue;

      ExtractPath := TFileKit.CombinePaths(ADestPath, DirRec.Name);

      // Create directory for file or if it's a directory entry
      if DirRec.FileType = TFileType('5') then
        TFileKit.CreateDirectory(ExtractPath)
      else
      begin
        TFileKit.CreateDirectory(ExtractFilePath(ExtractPath));
        if DirRec.FileType = TFileType('0') then  // Regular file
          Archive.ReadFile(ExtractPath);
      end;
    end;
  finally
    Archive.Free;
  end;
end;

end. 
