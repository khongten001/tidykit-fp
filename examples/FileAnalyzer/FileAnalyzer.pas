program FileAnalyzer;

{$mode objfpc}{$H+}{$J-}

// This example demonstrates basic file operations using TidyKit.FS
// - File reading and analysis
// - Directory listing and traversal
// - File metadata extraction
// - Basic path operations

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Math,
  TidyKit.Core, TidyKit.FS;

const
  LINE_SEPARATOR = '-----------------------------------------------';

// Custom comparison function for word frequency sorting
function CompareWordCounts(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := PtrInt(List.Objects[Index2]) - PtrInt(List.Objects[Index1]);
end;

// ===== Simple Text Analyzer Functions =====

// Analyzes a text file and prints statistics
procedure AnalyzeTextFile(const FilePath: string);
var
  Content: string;
  LineCount, WordCount, CharCount: Integer;
  i: Integer;
  InWord: Boolean;
begin
  // Make sure file exists before analyzing
  if not TFileKit.Exists(FilePath) then
  begin
    WriteLn('Error: File not found: ', FilePath);
    Exit;
  end;
  
  // Check if this is a text file
  if not TFileKit.IsTextFile(FilePath) then
  begin
    WriteLn('Warning: File does not appear to be a text file: ', FilePath);
    WriteLn('File type: ', TFileKit.GetMimeType(FilePath));
    Exit;
  end;
  
  // Read the entire file content
  Content := TFileKit.ReadTextFile(FilePath);
  
  // Calculate statistics
  LineCount := 0;
  WordCount := 0;
  CharCount := Length(Content);
  InWord := False;
  
  // Count lines and words
  for i := 1 to Length(Content) do
  begin
    // Count line breaks
    if Content[i] = #10 then
      Inc(LineCount);
      
    // Count word boundaries
    if Content[i] in [' ', #9, #10, #13, '.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}'] then
    begin
      if InWord then
        InWord := False;
    end
    else // We found a character that's part of a word
    begin
      if not InWord then
      begin
        InWord := True;
        Inc(WordCount);
      end;
    end;
  end;
  
  // If file doesn't end with newline, count the last line
  if (CharCount > 0) and (Content[CharCount] <> #10) then
    Inc(LineCount);
  
  // Display the statistics
  WriteLn(LINE_SEPARATOR);
  WriteLn('File Analysis: ', ExtractFileName(FilePath));
  WriteLn(LINE_SEPARATOR);
  WriteLn('Size: ', TFileKit.GetSize(FilePath), ' bytes');
  WriteLn('Lines: ', LineCount);
  WriteLn('Words: ', WordCount);
  WriteLn('Characters: ', CharCount);
  WriteLn('Last modified: ', DateTimeToStr(TFileKit.GetLastWriteTime(FilePath)));
  WriteLn('Encoding: ', TFileKit.GetFileEncoding(FilePath));
  WriteLn(LINE_SEPARATOR);
end;

// Prints most common words in a text file
procedure ShowCommonWords(const FilePath: string; MaxWords: Integer = 10);
var
  Content, Word: string;
  WordList, UniqueWords: TStringList;
  i, j, Count: Integer;
  CurrentWord: string;
  InWord: Boolean;
begin
  if not TFileKit.Exists(FilePath) then
  begin
    WriteLn('Error: File not found: ', FilePath);
    Exit;
  end;
  
  Content := TFileKit.ReadTextFile(FilePath);
  WordList := TStringList.Create;
  UniqueWords := TStringList.Create;
  try
    // Extract words from content - no sorting during extraction
    WordList.Sorted := False;
    WordList.Duplicates := dupAccept;
    
    // Extract words from content
    CurrentWord := '';
    InWord := False;
    
    for i := 1 to Length(Content) do
    begin
      if Content[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] then
      begin
        CurrentWord := CurrentWord + LowerCase(Content[i]);
        InWord := True;
      end
      else if InWord then
      begin
        if Length(CurrentWord) > 2 then // Skip small words
          WordList.Add(CurrentWord);
        CurrentWord := '';
        InWord := False;
      end;
    end;
    
    // Add the last word if content doesn't end with delimiter
    if InWord and (Length(CurrentWord) > 2) then
      WordList.Add(CurrentWord);
    
    // Create a list of unique words with counts
    UniqueWords.Sorted := True;
    UniqueWords.Duplicates := dupIgnore;
    
    // First add all words to get unique entries
    for i := 0 to WordList.Count - 1 do
    begin
      UniqueWords.Add(WordList[i]);
    end;
    
    // Now count occurrences of each unique word
    for i := 0 to UniqueWords.Count - 1 do
    begin
      Count := 0;
      Word := UniqueWords[i];
      
      for j := 0 to WordList.Count - 1 do
        if WordList[j] = Word then
          Inc(Count);
      
      UniqueWords.Objects[i] := TObject(PtrInt(Count));
    end;
    
    // Sort by frequency (custom sort)
    UniqueWords.CustomSort(@CompareWordCounts);
    
    // Count word frequencies
    WriteLn('Most common words:');
    WriteLn(LINE_SEPARATOR);
    
    // Print the top words
    for i := 0 to Min(MaxWords - 1, UniqueWords.Count - 1) do
    begin
      WriteLn(UniqueWords[i], ' (', PtrInt(UniqueWords.Objects[i]), ' occurrences)');
    end;
    
  finally
    WordList.Free;
    UniqueWords.Free;
  end;
  
  WriteLn(LINE_SEPARATOR);
end;

// ===== Directory Analysis Functions =====

// Lists all files in a directory with details
procedure ListDirectoryContents(const DirPath: string; Recursive: Boolean = False);
var
  Files: TFilePathArray;
  Dirs: TFilePathArray;
  i: Integer;
  FileCount, DirCount: Integer;
  TotalSize: Int64;
  FilePath: string;
begin
  // Make sure directory exists
  if not TFileKit.DirectoryExists(DirPath) then
  begin
    WriteLn('Error: Directory not found: ', DirPath);
    Exit;
  end;
  
  // Get all files and directories
  Files := TFileKit.ListFiles(DirPath, '*', Recursive, fsName);
  Dirs := TFileKit.ListDirectories(DirPath, '*', Recursive, fsName);
  
  FileCount := Length(Files);
  DirCount := Length(Dirs);
  TotalSize := 0;
  
  // Print directory information
  WriteLn(LINE_SEPARATOR);
  WriteLn('Directory: ', DirPath);
  WriteLn('Recursive: ', BoolToStr(Recursive, 'Yes', 'No'));
  WriteLn(LINE_SEPARATOR);
  
  // Print directories
  WriteLn('Directories (', DirCount, '):');
  for i := 0 to High(Dirs) do
  begin
    WriteLn(' - ', ExtractFileName(ExcludeTrailingPathDelimiter(Dirs[i])));
  end;
  WriteLn;
  
  // Print files with sizes
  WriteLn('Files (', FileCount, '):');
  for i := 0 to High(Files) do
  begin
    FilePath := Files[i];
    TotalSize := TotalSize + TFileKit.GetSize(FilePath);
    WriteLn(' - ', ExtractFileName(FilePath), ' (', TFileKit.GetSize(FilePath), ' bytes)');
  end;
  
  // Print summary
  WriteLn(LINE_SEPARATOR);
  WriteLn('Total directories: ', DirCount);
  WriteLn('Total files: ', FileCount);
  WriteLn('Total size: ', TotalSize, ' bytes (', Format('%.2f', [TotalSize / 1024]), ' KB)');
  WriteLn(LINE_SEPARATOR);
end;

// Shows detailed directory statistics
procedure ShowDirectoryStatistics(const DirPath: string);
var
  DirInfo: TDirectoryInfo;
  NewestFile, OldestFile: string;
  NewestTime, OldestTime: TDateTime;
  Files: TSearchResults;
  i: Integer;
  TextFileCount, BinaryFileCount, InaccessibleCount: Integer;
  FileTypes: TStringList;
  FileExt: string;
  HasFiles: Boolean;
  IsFileAccessible: Boolean;
begin
  if not TFileKit.DirectoryExists(DirPath) then
  begin
    WriteLn('Error: Directory not found: ', DirPath);
    Exit;
  end;
  
  // Get basic directory info
  DirInfo := TFileKit.GetDirectoryInfo(DirPath);
  
  // Find oldest and newest files
  Files := TFileKit.SearchFiles(DirPath, '*', True);
  NewestTime := 0;
  OldestTime := Now + 1000; // Far in the future
  
  // Initialize counters
  TextFileCount := 0;
  BinaryFileCount := 0;
  InaccessibleCount := 0;
  HasFiles := False;
  NewestFile := '';
  OldestFile := '';
  
  // Use try-except for the whole directory analysis
  try
    FileTypes := TStringList.Create;
    try
      FileTypes.Sorted := True;
      FileTypes.Duplicates := dupIgnore;
      
      for i := 0 to High(Files) do
      begin
        try
          if not Files[i].IsDirectory then
          begin
            HasFiles := True;
            IsFileAccessible := True;
            
            // Check file type with error handling
            try
              if TFileKit.IsTextFile(Files[i].FullPath) then
                Inc(TextFileCount)
              else
                Inc(BinaryFileCount);
                
              // Track file extensions
              FileExt := LowerCase(ExtractFileExt(Files[i].FullPath));
              if FileExt <> '' then
                FileTypes.Add(FileExt);
              
              // Track newest/oldest files
              if Files[i].LastModified > NewestTime then
              begin
                NewestTime := Files[i].LastModified;
                NewestFile := Files[i].FullPath;
              end;
              
              if Files[i].LastModified < OldestTime then
              begin
                OldestTime := Files[i].LastModified;
                OldestFile := Files[i].FullPath;
              end;
            except
              on E: Exception do
              begin
                Inc(InaccessibleCount);
                WriteLn('Warning: Could not access file "', ExtractFileName(Files[i].FullPath), '": ', E.Message);
              end;
            end;
          end;
        except
          on E: Exception do
            // If we can't even check if it's a directory, count it as inaccessible
            Inc(InaccessibleCount);
        end;
      end;
      
      // Display detailed statistics
      WriteLn(LINE_SEPARATOR);
      WriteLn('Directory Statistics: ', DirPath);
      WriteLn(LINE_SEPARATOR);
      
      // Get basic statistics safely
      try
        WriteLn('File count: ', DirInfo.FileCount);
        WriteLn('Directory count: ', DirInfo.DirectoryCount);
        WriteLn('Total size: ', DirInfo.TotalSize, ' bytes (', 
                Format('%.2f', [DirInfo.TotalSize / 1024 / 1024]), ' MB)');
      except
        on E: Exception do
          WriteLn('Could not get complete directory information: ', E.Message);
      end;
      
      if HasFiles then
      begin
        WriteLn;
        WriteLn('Text files: ', TextFileCount);
        WriteLn('Binary files: ', BinaryFileCount);
        if InaccessibleCount > 0 then
          WriteLn('Inaccessible files: ', InaccessibleCount);
        
        if FileTypes.Count > 0 then
        begin
          WriteLn;
          WriteLn('File extensions found (', FileTypes.Count, '):');
          for i := 0 to FileTypes.Count - 1 do
            WriteLn(' - ', FileTypes[i]);
        end;
        
        WriteLn;
        if NewestFile <> '' then
          WriteLn('Newest file: ', ExtractFileName(NewestFile), ' (', DateTimeToStr(NewestTime), ')')
        else
          WriteLn('Newest file: None found');
          
        if OldestFile <> '' then
          WriteLn('Oldest file: ', ExtractFileName(OldestFile), ' (', DateTimeToStr(OldestTime), ')')
        else
          WriteLn('Oldest file: None found');
          
        try
          if DirInfo.LargestFile <> '' then
            WriteLn('Largest file: ', DirInfo.LargestFile)
          else
            WriteLn('Largest file: None found');
        except
          on E: Exception do
            WriteLn('Could not determine largest file: ', E.Message);
        end;
      end else
        WriteLn('No files found in this directory.');
        
      WriteLn(LINE_SEPARATOR);
      
    finally
      FileTypes.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error analyzing directory: ', E.Message);
      WriteLn(LINE_SEPARATOR);
    end;
  end;
end;

// ===== Main Program Menu =====

// Displays and processes the main menu
procedure ShowMenu;
var
  Choice: string;
  FilePath, DirPath: string;
begin
  repeat
    WriteLn;
    WriteLn('==== File Analysis Tool ====');
    WriteLn('1. Analyze Text File');
    WriteLn('2. Find Common Words in Text File');
    WriteLn('3. List Directory Contents');
    WriteLn('4. Show Directory Statistics');
    WriteLn('0. Exit');
    WriteLn('=========================');
    Write('Enter your choice: ');
    ReadLn(Choice);
    
    case Choice of
      '1': begin
        Write('Enter file path: ');
        ReadLn(FilePath);
        AnalyzeTextFile(FilePath);
      end;
      '2': begin
        Write('Enter file path: ');
        ReadLn(FilePath);
        ShowCommonWords(FilePath);
      end;
      '3': begin
        Write('Enter directory path: ');
        ReadLn(DirPath);
        Write('Include subdirectories? (y/n): ');
        ReadLn(Choice);
        ListDirectoryContents(DirPath, LowerCase(Choice) = 'y');
      end;
      '4': begin
        Write('Enter directory path: ');
        ReadLn(DirPath);
        ShowDirectoryStatistics(DirPath);
      end;
      '0': WriteLn('Goodbye!');
    else
      WriteLn('Invalid choice. Please try again.');
    end;
    
  until Choice = '0';
end;

// Main program
begin
  try
    WriteLn('File Analysis Tool - Using TidyKit.FS');
    WriteLn;
    
    // Show the main menu
    ShowMenu;
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
    end;
  end;
end.
