unit TidyKit.FS;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, TidyKit.Core;

type
  { Platform-independent file attributes }
  TFileAttributes = record
    ReadOnly: Boolean;
    Hidden: Boolean;
    System: Boolean;
    Directory: Boolean;
    Archive: Boolean;
    SymLink: Boolean;
    Owner: string;
    Group: string;
    Permissions: string;  // Unix-style permissions string
  end;

  { Search result record }
  TSearchResult = record
    FileName: string;
    FullPath: string;
    Size: Int64;
    LastModified: TDateTime;
    IsDirectory: Boolean;
    Attributes: TFileAttributes;
  end;
  
  TSearchResults = array of TSearchResult;

  { Interface for file operations }
  IFileKit = interface(IChainable)
    ['{B1C2D3E4-5678-9ABC-DEF0-123456789ABC}']
    function GetContent: string;
    function GetPath: string;
    
    { Basic operations }
    function ReadFile(const APath: string): IFileKit;
    function WriteFile(const APath: string): IFileKit;
    function AppendFile(const APath: string): IFileKit;
    function DeleteFile: IFileKit;
    function CopyTo(const ADestPath: string): IFileKit;
    function MoveTo(const ADestPath: string): IFileKit;
    
    { Content operations }
    function SetContent(const AContent: string): IFileKit;
    function AppendText(const AText: string): IFileKit;
    function PrependText(const AText: string): IFileKit;
    function ReplaceText(const OldText, NewText: string): IFileKit;
    
    { Directory operations }
    function CreateDirectory: IFileKit;
    function DeleteDirectory(const Recursive: Boolean = True): IFileKit;
    function EnsureDirectory: IFileKit;
    
    { Path operations }
    function ChangeExtension(const NewExt: string): IFileKit;
    function GetFileName: string;
    function GetFileNameWithoutExt: string;
    function GetDirectory: string;
    function GetExtension: string;
    
    { File information }
    function Exists: Boolean;
    function DirectoryExists: Boolean;
    function Size: Int64;
    function CreationTime: TDateTime;
    function LastAccessTime: TDateTime;
    function LastWriteTime: TDateTime;
    
    { Search operations }
    function SearchFiles(const APattern: string; const Recursive: Boolean = True): TSearchResults;
    function SearchFilesIn(const ADirectory: string; const APattern: string; const Recursive: Boolean = True): TSearchResults;
    function FindNewestFile(const APattern: string; const Recursive: Boolean = True): string;
    function FindOldestFile(const APattern: string; const Recursive: Boolean = True): string;
    function FindLargestFile(const APattern: string; const Recursive: Boolean = True): string;
    function FindSmallestFile(const APattern: string; const Recursive: Boolean = True): string;
    
    { Properties }
    property Content: string read GetContent;
    property Path: string read GetPath;
  end;

  { Implementation of file operations }
  TFileKit = class(TKitBase, IFileKit)
  private
    FContent: string;
    FPath: string;
    function GetContent: string;
    function GetPath: string;
    procedure LoadContent;
    procedure SaveContent;
    function CreateSearchResult(const APath: string): TSearchResult;
  public
    constructor Create;
    
    { Interface implementations }
    function ReadFile(const APath: string): IFileKit;
    function WriteFile(const APath: string): IFileKit;
    function AppendFile(const APath: string): IFileKit;
    function DeleteFile: IFileKit;
    function CopyTo(const ADestPath: string): IFileKit;
    function MoveTo(const ADestPath: string): IFileKit;
    
    function SetContent(const AContent: string): IFileKit;
    function AppendText(const AText: string): IFileKit;
    function PrependText(const AText: string): IFileKit;
    function ReplaceText(const OldText, NewText: string): IFileKit;
    
    function CreateDirectory: IFileKit;
    function DeleteDirectory(const Recursive: Boolean = True): IFileKit;
    function EnsureDirectory: IFileKit;
    
    function ChangeExtension(const NewExt: string): IFileKit;
    function GetFileName: string;
    function GetFileNameWithoutExt: string;
    function GetDirectory: string;
    function GetExtension: string;
    
    function Exists: Boolean;
    function DirectoryExists: Boolean;
    function Size: Int64;
    function CreationTime: TDateTime;
    function LastAccessTime: TDateTime;
    function LastWriteTime: TDateTime;
    
    { Search operations }
    function SearchFiles(const APattern: string; const Recursive: Boolean = True): TSearchResults;
    function SearchFilesIn(const ADirectory: string; const APattern: string; const Recursive: Boolean = True): TSearchResults;
    function FindNewestFile(const APattern: string; const Recursive: Boolean = True): string;
    function FindOldestFile(const APattern: string; const Recursive: Boolean = True): string;
    function FindLargestFile(const APattern: string; const Recursive: Boolean = True): string;
    function FindSmallestFile(const APattern: string; const Recursive: Boolean = True): string;
  end;

implementation

{ Platform-specific helper functions }

function GetFileAttributes(const APath: string): TFileAttributes;
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
{$ENDIF}
{$IFDEF WINDOWS}
var
  Attrs: DWord;
{$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);
  
  {$IFDEF UNIX}
  if FpStat(APath, Info) = 0 then
  begin
    Result.ReadOnly := (Info.Mode and S_IWUSR) = 0;
    Result.Directory := S_ISDIR(Info.Mode);
    Result.SymLink := S_ISLNK(Info.Mode);
    
    // Convert mode to string (e.g., 'rwxr-xr-x')
    Result.Permissions := '';
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IRUSR) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IWUSR) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IXUSR) <> 0, 'x', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IRGRP) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IWGRP) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IXGRP) <> 0, 'x', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IROTH) <> 0, 'r', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IWOTH) <> 0, 'w', '-');
    Result.Permissions := Result.Permissions + IfThen((Info.Mode and S_IXOTH) <> 0, 'x', '-');
  end;
  {$ENDIF}
  
  {$IFDEF WINDOWS}
  Attrs := Windows.GetFileAttributes(PChar(APath));
  if Attrs <> $FFFFFFFF then
  begin
    Result.ReadOnly := (Attrs and FILE_ATTRIBUTE_READONLY) <> 0;
    Result.Hidden := (Attrs and FILE_ATTRIBUTE_HIDDEN) <> 0;
    Result.System := (Attrs and FILE_ATTRIBUTE_SYSTEM) <> 0;
    Result.Directory := (Attrs and FILE_ATTRIBUTE_DIRECTORY) <> 0;
    Result.Archive := (Attrs and FILE_ATTRIBUTE_ARCHIVE) <> 0;
    Result.SymLink := (Attrs and FILE_ATTRIBUTE_REPARSE_POINT) <> 0;
  end;
  {$ENDIF}
end;

function NormalizePath(const APath: string): string;
begin
  Result := APath;
  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ELSE}
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
  Result := ExpandFileName(Result);
end;

{ TFileKit }

constructor TFileKit.Create;
begin
  inherited Create;
  FContent := '';
  FPath := '';
end;

function TFileKit.GetContent: string;
begin
  Result := FContent;
end;

function TFileKit.GetPath: string;
begin
  Result := FPath;
end;

procedure TFileKit.LoadContent;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  if FileExists(FPath) then
  begin
    FileStream := TFileStream.Create(FPath, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(FileStream, FileStream.Size);
        FContent := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  end
  else
    FContent := '';
end;

procedure TFileKit.SaveContent;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  ForceDirectories(ExtractFilePath(FPath));
  FileStream := TFileStream.Create(FPath, fmCreate);
  try
    StringStream := TStringStream.Create(FContent);
    try
      FileStream.CopyFrom(StringStream, StringStream.Size);
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TFileKit.ReadFile(const APath: string): IFileKit;
begin
  FPath := NormalizePath(APath);
  LoadContent;
  Result := Self;
end;

function TFileKit.WriteFile(const APath: string): IFileKit;
begin
  FPath := NormalizePath(APath);
  SaveContent;
  Result := Self;
end;

function TFileKit.AppendFile(const APath: string): IFileKit;
begin
  FPath := ExpandFileName(APath);
  if FileExists(FPath) then
    FContent := TFile.ReadAllText(FPath) + FContent;
  SaveContent;
  Result := Self;
end;

function TFileKit.DeleteFile: IFileKit;
begin
  if FileExists(FPath) then
    SysUtils.DeleteFile(FPath);
  Result := Self;
end;

function TFileKit.CopyTo(const ADestPath: string): IFileKit;
var
  SourceStream, DestStream: TFileStream;
begin
  if FileExists(FPath) then
  begin
    ForceDirectories(ExtractFilePath(ADestPath));
    SourceStream := TFileStream.Create(FPath, fmOpenRead or fmShareDenyWrite);
    try
      DestStream := TFileStream.Create(ADestPath, fmCreate);
      try
        DestStream.CopyFrom(SourceStream, SourceStream.Size);
        FPath := ADestPath;
      finally
        DestStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  end;
  Result := Self;
end;

function TFileKit.MoveTo(const ADestPath: string): IFileKit;
begin
  if FileExists(FPath) then
  begin
    ForceDirectories(ExtractFilePath(ADestPath));
    if RenameFile(FPath, ADestPath) then
      FPath := ADestPath
    else
    begin
      CopyTo(ADestPath);
      DeleteFile;
    end;
  end;
  Result := Self;
end;

function TFileKit.SetContent(const AContent: string): IFileKit;
begin
  FContent := AContent;
  Result := Self;
end;

function TFileKit.AppendText(const AText: string): IFileKit;
begin
  FContent := FContent + AText;
  Result := Self;
end;

function TFileKit.PrependText(const AText: string): IFileKit;
begin
  FContent := AText + FContent;
  Result := Self;
end;

function TFileKit.ReplaceText(const OldText, NewText: string): IFileKit;
begin
  FContent := StringReplace(FContent, OldText, NewText, [rfReplaceAll]);
  Result := Self;
end;

function TFileKit.CreateDirectory: IFileKit;
begin
  if not DirectoryExists(FPath) then
    ForceDirectories(FPath);
  Result := Self;
end;

function TFileKit.DeleteDirectory(const Recursive: Boolean): IFileKit;
begin
  if DirectoryExists(FPath) then
  begin
    if Recursive then
      DeleteDirectory(FPath, False)
    else
      RemoveDir(FPath);
  end;
  Result := Self;
end;

function TFileKit.EnsureDirectory: IFileKit;
begin
  ForceDirectories(ExtractFilePath(FPath));
  Result := Self;
end;

function TFileKit.ChangeExtension(const NewExt: string): IFileKit;
begin
  FPath := ChangeFileExt(FPath, NewExt);
  Result := Self;
end;

function TFileKit.GetFileName: string;
begin
  Result := ExtractFileName(FPath);
end;

function TFileKit.GetFileNameWithoutExt: string;
begin
  Result := ExtractFileNameWithoutExt(FPath);
end;

function TFileKit.GetDirectory: string;
begin
  Result := ExtractFilePath(FPath);
end;

function TFileKit.GetExtension: string;
begin
  Result := ExtractFileExt(FPath);
end;

function TFileKit.Exists: Boolean;
begin
  Result := FileExists(FPath);
end;

function TFileKit.DirectoryExists: Boolean;
begin
  Result := SysUtils.DirectoryExists(FPath);
end;

function TFileKit.Size: Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(FPath, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

function TFileKit.CreationTime: TDateTime;
begin
  Result := FileDateToDateTime(FileAge(FPath));
end;

function TFileKit.LastAccessTime: TDateTime;
begin
  Result := FileDateToDateTime(FileAge(FPath));
end;

function TFileKit.LastWriteTime: TDateTime;
begin
  Result := FileDateToDateTime(FileAge(FPath));
end;

function TFileKit.CreateSearchResult(const APath: string): TSearchResult;
var
  SearchRec: TSearchRec;
begin
  Result.FullPath := NormalizePath(APath);
  Result.FileName := ExtractFileName(APath);
  Result.IsDirectory := DirectoryExists(APath);
  Result.Attributes := GetFileAttributes(APath);
  
  if FindFirst(APath, faAnyFile, SearchRec) = 0 then
  begin
    try
      Result.Size := SearchRec.Size;
      Result.LastModified := FileDateToDateTime(SearchRec.Time);
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function TFileKit.SearchFiles(const APattern: string; const Recursive: Boolean): TSearchResults;
begin
  Result := SearchFilesIn(GetCurrentDir, APattern, Recursive);
end;

function TFileKit.SearchFilesIn(const ADirectory: string; const APattern: string; const Recursive: Boolean): TSearchResults;
var
  SearchRec: TSearchRec;
  FullPath: string;
  SubDirs: TStringList;
  I: Integer;
begin
  SetLength(Result, 0);
  SubDirs := TStringList.Create;
  try
    // First, search for files matching the pattern
    if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + APattern, faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            FullPath := IncludeTrailingPathDelimiter(ADirectory) + SearchRec.Name;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := CreateSearchResult(FullPath);
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;

    // If recursive, collect subdirectories and search them
    if Recursive then
    begin
      if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + '*', faDirectory, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
               ((SearchRec.Attr and faDirectory) = faDirectory) then
              SubDirs.Add(IncludeTrailingPathDelimiter(ADirectory) + SearchRec.Name);
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;

      // Search in subdirectories
      for I := 0 to SubDirs.Count - 1 do
      begin
        Result := Concat(Result, SearchFilesIn(SubDirs[I], APattern, True));
      end;
    end;
  finally
    SubDirs.Free;
  end;
end;

function TFileKit.FindNewestFile(const APattern: string; const Recursive: Boolean): string;
var
  Files: TSearchResults;
  I: Integer;
  NewestTime: TDateTime;
begin
  Result := '';
  NewestTime := 0;
  Files := SearchFiles(APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if (Result = '') or (Files[I].LastModified > NewestTime) then
    begin
      Result := Files[I].FullPath;
      NewestTime := Files[I].LastModified;
    end;
  end;
end;

function TFileKit.FindOldestFile(const APattern: string; const Recursive: Boolean): string;
var
  Files: TSearchResults;
  I: Integer;
  OldestTime: TDateTime;
begin
  Result := '';
  OldestTime := Now;
  Files := SearchFiles(APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if (Result = '') or (Files[I].LastModified < OldestTime) then
    begin
      Result := Files[I].FullPath;
      OldestTime := Files[I].LastModified;
    end;
  end;
end;

function TFileKit.FindLargestFile(const APattern: string; const Recursive: Boolean): string;
var
  Files: TSearchResults;
  I: Integer;
  LargestSize: Int64;
begin
  Result := '';
  LargestSize := 0;
  Files := SearchFiles(APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if (Result = '') or (Files[I].Size > LargestSize) then
    begin
      Result := Files[I].FullPath;
      LargestSize := Files[I].Size;
    end;
  end;
end;

function TFileKit.FindSmallestFile(const APattern: string; const Recursive: Boolean): string;
var
  Files: TSearchResults;
  I: Integer;
  SmallestSize: Int64;
begin
  Result := '';
  SmallestSize := High(Int64);
  Files := SearchFiles(APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if (Result = '') or (Files[I].Size < SmallestSize) then
    begin
      Result := Files[I].FullPath;
      SmallestSize := Files[I].Size;
    end;
  end;
end;

end. 