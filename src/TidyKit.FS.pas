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
  Classes, SysUtils, DateUtils;

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

  { File operations }
  TFileKit = class
  private
    class function CreateSearchResult(const APath: string): TSearchResult; static;
  public
    { Basic operations }
    class function ReadFile(const APath: string): string; static;
    class procedure WriteFile(const APath: string; const AContent: string); static;
    class procedure AppendFile(const APath: string; const AContent: string); static;
    class procedure DeleteFile(const APath: string); static;
    class procedure CopyFile(const ASourcePath, ADestPath: string); static;
    class procedure MoveFile(const ASourcePath, ADestPath: string); static;
    
    { Content operations }
    class procedure AppendText(const APath, AText: string); static;
    class procedure PrependText(const APath, AText: string); static;
    class procedure ReplaceText(const APath, OldText, NewText: string); static;
    
    { Directory operations }
    class procedure CreateDirectory(const APath: string); static;
    class procedure DeleteDirectory(const APath: string; const Recursive: Boolean = True); static;
    class procedure EnsureDirectory(const APath: string); static;
    
    { Path operations }
    class function ChangeExtension(const APath, NewExt: string): string; static;
    class function GetFileName(const APath: string): string; static;
    class function GetFileNameWithoutExt(const APath: string): string; static;
    class function GetDirectory(const APath: string): string; static;
    class function GetExtension(const APath: string): string; static;
    
    { File information }
    class function Exists(const APath: string): Boolean; static;
    class function DirectoryExists(const APath: string): Boolean; static;
    class function GetSize(const APath: string): Int64; static;
    class function GetCreationTime(const APath: string): TDateTime; static;
    class function GetLastAccessTime(const APath: string): TDateTime; static;
    class function GetLastWriteTime(const APath: string): TDateTime; static;
    class function GetAttributes(const APath: string): TFileAttributes; static;
    
    { Search operations }
    class function SearchFiles(const APath, APattern: string; const Recursive: Boolean = True): TSearchResults; static;
    class function SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean): TSearchResults; static;
    class function FindNewestFile(const APath, APattern: string; const Recursive: Boolean = True): string; static;
    class function FindOldestFile(const APath, APattern: string; const Recursive: Boolean = True): string; static;
    class function FindLargestFile(const APath, APattern: string; const Recursive: Boolean = True): string; static;
    class function FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = True): string; static;
    class function IsDirectory(const APath: string): Boolean; static;
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
  Result := ExpandFileName(Result); // Ensure full path is returned
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  FillChar(LocalFileTime, SizeOf(LocalFileTime), 0);
  FillChar(SystemTime, SizeOf(SystemTime), 0);
  if FileTimeToLocalFileTime(FileTime, LocalFileTime) and
     FileTimeToSystemTime(LocalFileTime, SystemTime) then
  begin
    Result := EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay) +
              EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);
  end;
end;

{ Helper functions }

function LoadFromFile(const APath: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := '';
  if FileExists(APath) then
  begin
    FileStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(FileStream, FileStream.Size);
        Result := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;
end;

procedure SaveToFile(const APath: string; const AContent: string);
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  ForceDirectories(ExtractFilePath(APath));
  FileStream := TFileStream.Create(APath, fmCreate);
  try
    StringStream := TStringStream.Create(AContent);
    try
      FileStream.CopyFrom(StringStream, StringStream.Size);
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

{ TFileKit }

class function TFileKit.CreateSearchResult(const APath: string): TSearchResult;
var
  SearchRec: TSearchRec;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.FullPath := NormalizePath(APath);
  Result.FileName := ExtractFileName(APath);
  Result.IsDirectory := IsDirectory(APath);
  Result.Attributes := GetFileAttributes(APath);
  // Use FileAge for last modified:
  if FileExists(APath) or Result.IsDirectory then
    Result.LastModified := FileDateToDateTime(FileAge(APath));
  // Set file size:
  if FindFirst(APath, faAnyFile, SearchRec) = 0 then
  begin
      Result.Size := SearchRec.Size;
      FindClose(SearchRec);
  end;
end;

class function TFileKit.ReadFile(const APath: string): string;
begin
  Result := LoadFromFile(APath);
end;

class procedure TFileKit.WriteFile(const APath: string; const AContent: string);
begin
  if APath <> '' then
  begin
    ForceDirectories(ExtractFilePath(APath));
    SaveToFile(APath, AContent);
  end;
end;

class procedure TFileKit.AppendFile(const APath: string; const AContent: string);
var
  ExistingContent: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      ExistingContent := LoadFromFile(APath);
      SaveToFile(APath, ExistingContent + AContent);
    end
    else
      SaveToFile(APath, AContent);
  end;
end;

class procedure TFileKit.DeleteFile(const APath: string);
begin
  if FileExists(APath) then
    SysUtils.DeleteFile(APath);
end;

class procedure TFileKit.CopyFile(const ASourcePath, ADestPath: string);
var
  SourceStream, DestStream: TFileStream;
begin
  if FileExists(ASourcePath) then
  begin
    ForceDirectories(ExtractFilePath(ADestPath));
    SourceStream := TFileStream.Create(ASourcePath, fmOpenRead or fmShareDenyWrite);
    try
      DestStream := TFileStream.Create(ADestPath, fmCreate);
      try
        DestStream.CopyFrom(SourceStream, SourceStream.Size);
      finally
        DestStream.Free;
      end;
    finally
      SourceStream.Free;
    end;
  end;
end;

class procedure TFileKit.MoveFile(const ASourcePath, ADestPath: string);
var
  DestDir: string;
begin
  if FileExists(ASourcePath) then
  begin
    DestDir := ExtractFilePath(ADestPath);
    if DestDir <> '' then
      ForceDirectories(DestDir);
      
    if not RenameFile(ASourcePath, ADestPath) then
    begin
      CopyFile(ASourcePath, ADestPath);
      if FileExists(ADestPath) then
        SysUtils.DeleteFile(ASourcePath);
    end;
  end;
end;

class procedure TFileKit.AppendText(const APath, AText: string);
var
  Content: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      Content := LoadFromFile(APath);
      SaveToFile(APath, Content + AText);
    end
    else
      SaveToFile(APath, AText);
  end;
end;

class procedure TFileKit.PrependText(const APath, AText: string);
var
  Content: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      Content := LoadFromFile(APath);
      SaveToFile(APath, AText + Content);
    end
    else
      SaveToFile(APath, AText);
  end;
end;

class procedure TFileKit.ReplaceText(const APath, OldText, NewText: string);
var
  Content: string;
begin
  if APath <> '' then
  begin
    if FileExists(APath) then
    begin
      Content := LoadFromFile(APath);
      Content := StringReplace(Content, OldText, NewText, [rfReplaceAll]);
      SaveToFile(APath, Content);
    end;
  end;
end;

class procedure TFileKit.CreateDirectory(const APath: string);
begin
  if not SysUtils.DirectoryExists(APath) then
    ForceDirectories(APath);
end;

class procedure TFileKit.DeleteDirectory(const APath: string; const Recursive: Boolean = True);
var
  SearchRec: TSearchRec;
  FullPath: string;
begin
  if IsDirectory(APath) then
  begin
    if Recursive then
    begin
      if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            begin
              FullPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
              if (SearchRec.Attr and faDirectory) <> 0 then
                DeleteDirectory(FullPath, True)
              else
                SysUtils.DeleteFile(FullPath);
            end;
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
      end;
    end;
    RemoveDir(APath);
  end;
end;

class procedure TFileKit.EnsureDirectory(const APath: string);
begin
  ForceDirectories(ExtractFilePath(APath));
end;

class function TFileKit.ChangeExtension(const APath, NewExt: string): string;
begin
  Result := ChangeFileExt(APath, NewExt);
end;

class function TFileKit.GetFileName(const APath: string): string;
begin
  Result := ExtractFileName(APath);
end;

class function TFileKit.GetFileNameWithoutExt(const APath: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(APath), '');
end;

class function TFileKit.GetDirectory(const APath: string): string;
begin
  if IsDirectory(APath) then
    Result := ExtractFileName(ExcludeTrailingPathDelimiter(APath))
  else if APath <> '' then
    Result := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(APath))))
  else
    Result := ExtractFileName(ExcludeTrailingPathDelimiter(GetCurrentDir));
end;

class function TFileKit.GetExtension(const APath: string): string;
begin
  Result := ExtractFileExt(APath);
end;

class function TFileKit.Exists(const APath: string): Boolean;
begin
  Result := FileExists(APath);
end;

class function TFileKit.DirectoryExists(const APath: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(APath);
end;

class function TFileKit.GetSize(const APath: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(APath, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

class function TFileKit.GetCreationTime(const APath: string): TDateTime;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
  FillChar(FindData, SizeOf(FindData), 0);
  Handle := FindFirstFile(PChar(APath), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FileTimeToDateTime(FindData.ftCreationTime);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := FileDateToDateTime(FileAge(APath));
  {$ENDIF}
end;

class function TFileKit.GetLastAccessTime(const APath: string): TDateTime;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
  FillChar(FindData, SizeOf(FindData), 0);
  Handle := FindFirstFile(PChar(APath), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FileTimeToDateTime(FindData.ftLastAccessTime);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := FileDateToDateTime(FileAge(APath));
  {$ENDIF}
end;

class function TFileKit.GetLastWriteTime(const APath: string): TDateTime;
{$IFDEF WINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindData;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
  FillChar(FindData, SizeOf(FindData), 0);
  Handle := FindFirstFile(PChar(APath), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FileTimeToDateTime(FindData.ftLastWriteTime);
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := FileDateToDateTime(FileAge(APath));
  {$ENDIF}
end;

class function TFileKit.GetAttributes(const APath: string): TFileAttributes;
begin
  Result := GetFileAttributes(APath);
end;

class function TFileKit.SearchFiles(const APath, APattern: string; const Recursive: Boolean = True): TSearchResults;
var
  SearchDir: string;
begin
  SetLength(Result, 0);
  
  if IsDirectory(APath) then
    SearchDir := APath
  else if APath <> '' then
    SearchDir := ExtractFilePath(ExpandFileName(APath))
  else
    SearchDir := GetCurrentDir;
    
  Result := SearchFilesIn(SearchDir, APattern, Recursive);
end;

class function TFileKit.SearchFilesIn(const ADirectory, APattern: string; const Recursive: Boolean): TSearchResults;
  function DoSearch(const RawDir, Pat: string; Rec: Boolean; Visited: TStrings): TSearchResults;
  var
    NDir: string;
    SearchRec: TSearchRec;
    SubDirs: TStringList;
    SubResults: TSearchResults;
    FullPath: string;
    I, J, OldLen: Integer;
  begin
    SetLength(Result, 0);
    NDir := NormalizePath(RawDir);
    
    if Visited.IndexOf(NDir) >= 0 then
      Exit;
    Visited.Add(NDir);

    if not SysUtils.DirectoryExists(NDir) then
      Exit;
    
    SubDirs := TStringList.Create;
    try
      if FindFirst(IncludeTrailingPathDelimiter(NDir) + Pat, faAnyFile, SearchRec) = 0 then
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            FullPath := IncludeTrailingPathDelimiter(NDir) + SearchRec.Name;
            if (SearchRec.Attr and faDirectory) = 0 then // Only add files, not directories
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := CreateSearchResult(FullPath);
            end
            else if Rec and ((SearchRec.Attr and faSymLink) = 0) then
              SubDirs.Add(FullPath);
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;

      if Rec then
      begin
        for I := 0 to SubDirs.Count - 1 do
        begin
          SubResults := DoSearch(SubDirs[I], Pat, True, Visited);
          if Length(SubResults) > 0 then
          begin
            OldLen := Length(Result);
            SetLength(Result, OldLen + Length(SubResults));
            for J := 0 to High(SubResults) do
              Result[OldLen + J] := SubResults[J];
          end;
        end;
      end;
    finally
      SubDirs.Free;
    end;
  end;

var
  VisitedDirs: TStringList;
begin
  SetLength(Result, 0);
  VisitedDirs := TStringList.Create;
  try
    VisitedDirs.Sorted := True;
    VisitedDirs.Duplicates := dupIgnore;
    Result := DoSearch(ADirectory, APattern, Recursive, VisitedDirs);
  finally
    VisitedDirs.Free;
  end;
end;

class function TFileKit.FindNewestFile(const APath, APattern: string; const Recursive: Boolean = True): string;
var
  Files: TSearchResults;
  I: Integer;
  NewestTime: TDateTime;
begin
  Result := '';
  NewestTime := 0;
  Files := SearchFiles(APath, APattern, Recursive);
  try
    for I := 0 to High(Files) do
    begin
      if not Files[I].IsDirectory and ((Result = '') or (Files[I].LastModified > NewestTime)) then
      begin
        Result := Files[I].FileName;
        NewestTime := Files[I].LastModified;
      end;
    end;
  finally
    SetLength(Files, 0);
  end;
end;

class function TFileKit.FindOldestFile(const APath, APattern: string; const Recursive: Boolean = True): string;
var
  Files: TSearchResults;
  I: Integer;
  OldestTime: TDateTime;
begin
  Result := '';
  OldestTime := Now + 1;  // Future date to ensure any real date is older
  Files := SearchFiles(APath, APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if not Files[I].IsDirectory then
    begin
      if (Result = '') or (CompareDateTime(Files[I].LastModified, OldestTime) < 0) then
      begin
        Result := ExtractFileName(Files[I].FullPath);
        OldestTime := Files[I].LastModified;
      end;
    end;
  end;
end;

class function TFileKit.FindLargestFile(const APath, APattern: string; const Recursive: Boolean = True): string;
var
  Files: TSearchResults;
  I: Integer;
  LargestSize: Int64;
begin
  Result := '';
  LargestSize := -1;
  Files := SearchFiles(APath, APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if not Files[I].IsDirectory then
    begin
      if (Result = '') or (Files[I].Size > LargestSize) then
      begin
        Result := ExtractFileName(Files[I].FullPath);
        LargestSize := Files[I].Size;
      end;
    end;
  end;
end;

class function TFileKit.FindSmallestFile(const APath, APattern: string; const Recursive: Boolean = True): string;
var
  Files: TSearchResults;
  I: Integer;
  SmallestSize: Int64;
begin
  Result := '';
  SmallestSize := High(Int64);
  Files := SearchFiles(APath, APattern, Recursive);
  
  for I := 0 to High(Files) do
  begin
    if not Files[I].IsDirectory then
    begin
      if (Result = '') or (Files[I].Size < SmallestSize) then
      begin
        Result := ExtractFileName(Files[I].FullPath);
        SmallestSize := Files[I].Size;
      end;
    end;
  end;
end;

class function TFileKit.IsDirectory(const APath: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(APath);
end;

end. 
