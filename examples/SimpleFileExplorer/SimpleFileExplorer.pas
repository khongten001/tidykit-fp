program SimpleFileExplorer;

{$mode objfpc}{$H+}{$J-}

// =================================================================
// Simple File Explorer - A TidyKit.FS example
// =================================================================
//
// This program demonstrates how to:
//   1. Navigate through directories
//   2. List files and their properties
//   3. Create, copy, and move files
//   4. Read and write text files
//
// All of these operations use the TidyKit.FS library which makes
// working with files and directories much easier!
// =================================================================

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  TidyKit.Core, TidyKit.FS;

var
  // The current directory we're exploring
  CurrentDirectory: string;
  
  // Whether the program should continue running
  Running: Boolean;

// ===============================================================
// Helper functions to make our code more organized
// ===============================================================

// Clears the screen to keep our display tidy
procedure ClearScreen;
begin
  {$IFDEF WINDOWS}
  // On Windows, we can use this command to clear the screen
  ExecuteProcess('cmd', '/c cls', []);
  {$ELSE}
  // On other systems (like Mac or Linux), we use this command
  WriteLn(#27'[2J'#27'[1;1H');
  {$ENDIF}
end;

// Shows information about a single file
procedure ShowFileInfo(const FilePath: string);
var
  FileName: string;
  FileSize: Int64;
  LastModified: TDateTime;
  IsTextFile: Boolean;
begin
  // First check if the file actually exists
  if not TFileKit.Exists(FilePath) then
  begin
    WriteLn('File does not exist: ', FilePath);
    Exit;
  end;
  
  // Get the basic file information using our FileKit helper
  FileName := TFileKit.GetFileName(FilePath);
  FileSize := TFileKit.GetSize(FilePath);
  LastModified := TFileKit.GetLastWriteTime(FilePath);
  IsTextFile := TFileKit.IsTextFile(FilePath);
  
  // Display a nice header
  WriteLn('=============================================');
  WriteLn('FILE INFORMATION');
  WriteLn('=============================================');
  
  // Show the information we collected
  WriteLn('Name: ', FileName);
  WriteLn('Path: ', FilePath);
  WriteLn('Size: ', FileSize, ' bytes');
  
  // Format the size to be more readable for larger files
  if FileSize > 1024*1024 then
    WriteLn('      (', FileSize div (1024*1024), '.', (FileSize mod (1024*1024)) div 10000, ' MB)')
  else if FileSize > 1024 then
    WriteLn('      (', FileSize div 1024, '.', (FileSize mod 1024) div 10, ' KB)');
    
  // Show the date in a readable format
  WriteLn('Modified: ', DateTimeToStr(LastModified));
  
  // Tell if it's a text or binary file
  if IsTextFile then
    WriteLn('Type: Text file')
  else
    WriteLn('Type: Binary file');
    
  // If it's a text file, show additional information
  if IsTextFile then
  begin
    WriteLn('Encoding: ', TFileKit.GetFileEncoding(FilePath));
    WriteLn('Lines: ', TFileKit.CountLines(FilePath));
  end;
  
  WriteLn('=============================================');
end;

// Lists the contents of the specified directory
procedure ListDirectory(const DirPath: string);
var
  Files: TFilePathArray;
  Directories: TFilePathArray;
  i, j: Integer;  // Added 'j' for the nested loop
  FileName: string;
  DirName: string;
  FileSize: Int64;
begin
  // First make sure the directory exists
  if not TFileKit.DirectoryExists(DirPath) then
  begin
    WriteLn('Directory does not exist: ', DirPath);
    Exit;
  end;
  
  // Set our current directory to this path
  CurrentDirectory := DirPath;
  
  // Clear the screen to make it look nice
  ClearScreen;
  
  // Show where we are
  WriteLn('=============================================');
  WriteLn('EXPLORING: ', DirPath);
  WriteLn('=============================================');
  
  // Get all directories in this location
  Directories := TFileKit.ListDirectories(DirPath, '*', False, fsName);
  
  // Show parent directory option if we're not at the root
  if TFileKit.GetParentDir(DirPath) <> DirPath then
    WriteLn('[..] Parent Directory');
  
  // List all directories first with a special [dir] prefix
  for i := 0 to High(Directories) do
  begin
    DirName := ExtractFileName(ExcludeTrailingPathDelimiter(Directories[i]));
    if (DirName <> '') and (DirName <> '.') and (DirName <> '..') then
      WriteLn('[dir] ', DirName);
  end;
  
  // Get all files in this location
  Files := TFileKit.ListFiles(DirPath, '*', False, fsName);
  
  // List all files with their sizes
  for i := 0 to High(Files) do
  begin
    FileName := TFileKit.GetFileName(Files[i]);
    FileSize := TFileKit.GetSize(Files[i]);
    
    // Show the file with its size
    Write(FileName);
    
    // Add spaces to align the sizes nicely
    for j := Length(FileName) to 30 do  // Changed from 'i' to 'j'
      Write(' ');
      
    // Show the size in a readable format
    if FileSize > 1024*1024 then
      WriteLn(FileSize div (1024*1024), '.', (FileSize mod (1024*1024)) div 10000, ' MB')
    else if FileSize > 1024 then
      WriteLn(FileSize div 1024, '.', (FileSize mod 1024) div 10, ' KB')
    else
      WriteLn(FileSize, ' bytes');
  end;
  
  WriteLn('=============================================');
  WriteLn('Commands: CD, VIEW, COPY, MOVE, NEW, DEL, INFO, EXIT');
  WriteLn('Example: CD foldername');
  WriteLn('=============================================');
end;

// View the contents of a text file
procedure ViewTextFile(const FilePath: string);
var
  Content: string;
  Lines: TStringList;
  i, LineCount: Integer;
begin
  // First check if the file exists
  if not TFileKit.Exists(FilePath) then
  begin
    WriteLn('File does not exist: ', FilePath);
    Exit;
  end;
  
  // Check if it's a text file
  if not TFileKit.IsTextFile(FilePath) then
  begin
    WriteLn('Not a text file: ', FilePath);
    WriteLn('Press Enter to continue...');
    ReadLn;
    Exit;
  end;
  
  // Read the file content
  Content := TFileKit.ReadTextFile(FilePath);
  
  // Split into lines for display
  Lines := TStringList.Create;
  try
    Lines.Text := Content;
    
    // Clear screen for better viewing
    ClearScreen;
    
    // Show a header with the file name
    WriteLn('=== FILE: ', TFileKit.GetFileName(FilePath), ' ===');
    WriteLn;
    
    // Show the content with line numbers
    LineCount := Lines.Count;
    for i := 0 to LineCount - 1 do
    begin
      // Show line number and content
      Write(Format('%4d: ', [i+1]));
      WriteLn(Lines[i]);
      
      // If we've shown 20 lines, pause for the user to read
      if (i > 0) and (i mod 20 = 0) and (i < LineCount - 1) then
      begin
        WriteLn;
        Write('--- Press Enter for more (', i+1, '/', LineCount, ') ---');
        ReadLn;
        WriteLn;
      end;
    end;
    
    WriteLn;
    WriteLn('=== End of file ===');
    WriteLn('Press Enter to continue...');
    ReadLn;
    
  finally
    Lines.Free;
  end;
end;

// Creates a new text file with the given content
procedure CreateTextFile(const FilePath, Content: string);
var
  Response: string;
begin
  // Check if the file already exists
  if TFileKit.Exists(FilePath) then
  begin
    Write('File already exists. Overwrite? (Y/N): ');
    ReadLn(Response);
    if UpperCase(Copy(Response, 1, 1)) <> 'Y' then
    begin
      WriteLn('Canceled.');
      Exit;
    end;
  end;
  
  // Write the content to the file
  TFileKit.WriteTextFile(FilePath, Content);
  WriteLn('File created successfully: ', FilePath);
end;

// Copies a file to a new location
procedure CopyFileToLocation(const SourcePath, DestPath: string);
var
  Response: string;
begin
  // Check if the source file exists
  if not TFileKit.Exists(SourcePath) then
  begin
    WriteLn('Source file does not exist: ', SourcePath);
    Exit;
  end;
  
  // Check if the destination file already exists
  if TFileKit.Exists(DestPath) then
  begin
    Write('Destination file already exists. Overwrite? (Y/N): ');
    ReadLn(Response);
    if UpperCase(Copy(Response, 1, 1)) <> 'Y' then
    begin
      WriteLn('Canceled.');
      Exit;
    end;
  end;
  
  // Copy the file
  TFileKit.CopyFile(SourcePath, DestPath);
  WriteLn('File copied successfully.');
  WriteLn('From: ', SourcePath);
  WriteLn('To: ', DestPath);
end;

// Moves a file to a new location
procedure MoveFileToLocation(const SourcePath, DestPath: string);
var
  Response: string;
begin
  // Check if the source file exists
  if not TFileKit.Exists(SourcePath) then
  begin
    WriteLn('Source file does not exist: ', SourcePath);
    Exit;
  end;
  
  // Check if the destination file already exists
  if TFileKit.Exists(DestPath) then
  begin
    Write('Destination file already exists. Overwrite? (Y/N): ');
    ReadLn(Response);
    if UpperCase(Copy(Response, 1, 1)) <> 'Y' then
    begin
      WriteLn('Canceled.');
      Exit;
    end;
  end;
  
  // Move the file
  TFileKit.MoveFile(SourcePath, DestPath);
  WriteLn('File moved successfully.');
  WriteLn('From: ', SourcePath);
  WriteLn('To: ', DestPath);
end;

// ===============================================================
// Main program logic
// ===============================================================

// Processes a user command
procedure ProcessCommand(const Command: string);
var
  Cmd, Param1, Param2: string;
  SpacePos, SecondSpacePos: Integer;
  NewContent: TStringList;
  Response: string;
begin
  // Find the first space to separate command from parameters
  SpacePos := Pos(' ', Command);
  
  if SpacePos > 0 then
  begin
    // Extract the command and parameter
    Cmd := UpperCase(Copy(Command, 1, SpacePos - 1));
    
    // Find if there's a second parameter
    SecondSpacePos := Pos(' ', Command, SpacePos + 1);
    
    if SecondSpacePos > 0 then
    begin
      // We have two parameters
      Param1 := Copy(Command, SpacePos + 1, SecondSpacePos - SpacePos - 1);
      Param2 := Copy(Command, SecondSpacePos + 1, Length(Command));
    end
    else
    begin
      // We have one parameter
      Param1 := Copy(Command, SpacePos + 1, Length(Command));
      Param2 := '';
    end;
  end
  else
  begin
    // No parameters, just the command
    Cmd := UpperCase(Command);
    Param1 := '';
    Param2 := '';
  end;
  
  // Process the command
  if Cmd = 'CD' then
  begin
    if Param1 = '..' then
    begin
      // Go to parent directory
      ListDirectory(TFileKit.GetParentDir(CurrentDirectory));
    end
    else
    begin
      // Form the full path and check if it exists
      if TFileKit.DirectoryExists(TFileKit.CombinePaths(CurrentDirectory, Param1)) then
        ListDirectory(TFileKit.CombinePaths(CurrentDirectory, Param1))
      else
        WriteLn('Directory not found: ', Param1);
    end;
  end
  else if Cmd = 'VIEW' then
  begin
    if Param1 <> '' then
      ViewTextFile(TFileKit.CombinePaths(CurrentDirectory, Param1))
    else
      WriteLn('Please specify a file to view.');
  end
  else if Cmd = 'INFO' then
  begin
    if Param1 <> '' then
    begin
      ShowFileInfo(TFileKit.CombinePaths(CurrentDirectory, Param1));
      WriteLn('Press Enter to continue...');
      ReadLn;
      ListDirectory(CurrentDirectory);
    end
    else
      WriteLn('Please specify a file for information.');
  end
  else if Cmd = 'COPY' then
  begin
    if (Param1 <> '') and (Param2 <> '') then
      CopyFileToLocation(
        TFileKit.CombinePaths(CurrentDirectory, Param1),
        TFileKit.CombinePaths(CurrentDirectory, Param2)
      )
    else
      WriteLn('Usage: COPY sourcefile destinationfile');
  end
  else if Cmd = 'MOVE' then
  begin
    if (Param1 <> '') and (Param2 <> '') then
      MoveFileToLocation(
        TFileKit.CombinePaths(CurrentDirectory, Param1),
        TFileKit.CombinePaths(CurrentDirectory, Param2)
      )
    else
      WriteLn('Usage: MOVE sourcefile destinationfile');
  end
  else if Cmd = 'NEW' then
  begin
    if Param1 <> '' then
    begin
      WriteLn('Enter text for the file (end with a line containing only "END"):');
      NewContent := TStringList.Create;
      try
        repeat
          ReadLn(Param2);
          if UpperCase(Param2) <> 'END' then
            NewContent.Add(Param2);
        until UpperCase(Param2) = 'END';
        
        CreateTextFile(TFileKit.CombinePaths(CurrentDirectory, Param1), NewContent.Text);
      finally
        NewContent.Free;
      end;
    end
    else
      WriteLn('Please specify a filename.');
  end
  else if Cmd = 'DEL' then
  begin
    if Param1 <> '' then
    begin
      if TFileKit.Exists(TFileKit.CombinePaths(CurrentDirectory, Param1)) then
      begin
        Write('Are you sure you want to delete "', Param1, '"? (Y/N): ');
        ReadLn(Response);
        if UpperCase(Copy(Response, 1, 1)) = 'Y' then
        begin
          TFileKit.DeleteFile(TFileKit.CombinePaths(CurrentDirectory, Param1));
          WriteLn('File deleted.');
        end
        else
          WriteLn('Deletion canceled.');
      end
      else
        WriteLn('File not found: ', Param1);
    end
    else
      WriteLn('Please specify a file to delete.');
  end
  else if (Cmd = 'EXIT') or (Cmd = 'QUIT') then
    Running := False
  else if Cmd = 'HELP' then
  begin
    WriteLn('Available commands:');
    WriteLn('  CD [dir]    - Change to directory or ".." for parent');
    WriteLn('  VIEW [file] - View contents of a text file');
    WriteLn('  INFO [file] - Show detailed information about a file');
    WriteLn('  COPY [src] [dest] - Copy a file');
    WriteLn('  MOVE [src] [dest] - Move a file');
    WriteLn('  NEW [file]  - Create a new text file');
    WriteLn('  DEL [file]  - Delete a file');
    WriteLn('  EXIT        - Exit the program');
    WriteLn('  HELP        - Show this help');
  end
  else
    WriteLn('Unknown command. Type HELP for available commands.');
end;

var
  InputLine: string;

// Main program entry point
begin
  try
    // Start in the current directory
    CurrentDirectory := TFileKit.GetCurrentDir;
    
    // Show welcome message
    WriteLn('===============================================');
    WriteLn('  SIMPLE FILE EXPLORER - A TidyKit.FS Example  ');
    WriteLn('===============================================');
    WriteLn('This program shows how to work with files and');
    WriteLn('directories using the TidyKit library.');
    WriteLn;
    WriteLn('Type HELP for available commands.');
    WriteLn('===============================================');
    WriteLn;
    
    // Show the initial directory
    ListDirectory(CurrentDirectory);
    
    // Main command loop
    Running := True;
    while Running do
    begin
      // Show prompt with current directory
      Write(CurrentDirectory, '> ');
      
      // Get and process the command
      ReadLn(InputLine);  // Fixed: read into a variable
      ProcessCommand(InputLine);  // Pass the variable instead
    end;
    
    // Show goodbye message
    WriteLn('Thank you for using Simple File Explorer!');
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      WriteLn('Press Enter to exit...');
      ReadLn;
    end;
  end;
end.
