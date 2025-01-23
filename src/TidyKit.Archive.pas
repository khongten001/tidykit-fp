unit TidyKit.Archive;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, zipper, libtar, TidyKit.Core, TidyKit.FS;

type
  { TCompressionLevel defines the compression level for ZIP archives }
  TCompressionLevel = (
    clNone,      // Store files without compression
    clFastest,   // Fastest compression
    clDefault,   // Default compression (balance between speed and size)
    clMaximum    // Maximum compression
  );

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
      const Level: TCompressionLevel = clDefault); static;

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
  end;

implementation

class procedure TArchiveKit.CreateZip(const ASourcePath, ADestPath: string; 
  const Recursive: Boolean = True;
  const Level: TCompressionLevel = clDefault);
var
  Zipper: TZipper;
  FileList: TStringList;
  Files: TFilePathArray;
  BasePath, RelativePath: string;
  I: Integer;
begin
  if not TFileKit.Exists(ASourcePath) then
    Exit;

  Zipper := TZipper.Create;
  FileList := TStringList.Create;
  try
    Zipper.FileName := ADestPath;
    
    // Set compression level
    case Level of
      clNone: Zipper.CompressionLevel := 0;
      clFastest: Zipper.CompressionLevel := 1;
      clDefault: Zipper.CompressionLevel := 6;
      clMaximum: Zipper.CompressionLevel := 9;
    end;

    BasePath := ExcludeTrailingPathDelimiter(ExtractFilePath(ASourcePath));

    if TFileKit.DirectoryExists(ASourcePath) then
    begin
      Files := TFileKit.ListFiles(ASourcePath, '*', Recursive);
      for I := 0 to High(Files) do
      begin
        RelativePath := ExtractRelativePath(BasePath, Files[I]);
        FileList.Add(Files[I] + '=' + RelativePath);
      end;
    end
    else
      FileList.Add(ASourcePath + '=' + ExtractFileName(ASourcePath));

    Zipper.ZipFiles(FileList);
  finally
    FileList.Free;
    Zipper.Free;
  end;
end;

class procedure TArchiveKit.ExtractZip(const ASourcePath, ADestPath: string; const Recursive: Boolean = True);
var
  Unzipper: TUnZipper;
begin
  if not TFileKit.Exists(ASourcePath) then
    Exit;

  TFileKit.CreateDirectory(ADestPath);

  Unzipper := TUnZipper.Create;
  try
    Unzipper.FileName := ASourcePath;
    Unzipper.OutputPath := ADestPath;
    Unzipper.Examine;
    Unzipper.UnZipAllFiles;
  finally
    Unzipper.Free;
  end;
end;

class procedure TArchiveKit.CreateTar(const ASourcePath, ADestPath: string; const Recursive: Boolean = True);
var
  Writer: TTarWriter;
  Files: TFilePathArray;
  BasePath: string;
  I: Integer;
begin
  if not TFileKit.Exists(ASourcePath) then
    Exit;

  Writer := TTarWriter.Create(ADestPath);
  try
    BasePath := ExcludeTrailingPathDelimiter(ExtractFilePath(ASourcePath));

    if TFileKit.DirectoryExists(ASourcePath) then
    begin
      Files := TFileKit.ListFiles(ASourcePath, '*', Recursive);
      for I := 0 to High(Files) do
        Writer.AddFile(Files[I], ExtractRelativePath(BasePath, Files[I]));
    end
    else
      Writer.AddFile(ASourcePath, ExtractFileName(ASourcePath));
  finally
    Writer.Free;
  end;
end;

class procedure TArchiveKit.ExtractTar(const ASourcePath, ADestPath: string; const Recursive: Boolean = True);
var
  Archive: TTarArchive;
begin
  if not TFileKit.Exists(ASourcePath) then
    Exit;

  TFileKit.CreateDirectory(ADestPath);

  Archive := TTarArchive.Create(ASourcePath);
  try
    Archive.Reset;
    Archive.ExtractTo(ADestPath);
  finally
    Archive.Free;
  end;
end;

end. 