program log_example;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  TidyKit.Log, TidyKit.Log.Targets;

var
  FileLog, ConsoleLog, MultiLog: ILogger;
  I: Integer;
begin
  try
    { Create a file logger with rotation }
    FileLog := FileLogger('application.log');
    FileLog.Info('File logger initialized');

    { Create a console logger with colors }
    ConsoleLog := ConsoleLogger;
    ConsoleLog.Info('Console logger initialized');

    { Create a multi-target logger }
    MultiLog := MultiLogger
      .AddTarget(TFileTarget.Create('multi.log').SetMaxSize(1024 * 1024))
      .AddTarget(TConsoleTarget.Create.EnableColors)
      .SetMinLevel(llDebug)
      .Enable;

    MultiLog.Info('Multi-target logger initialized');

    { Demonstrate different log levels }
    MultiLog.Debug('This is a debug message');
    MultiLog.Info('This is an info message');
    MultiLog.Warning('This is a warning message');
    MultiLog.Error('This is an error message');
    MultiLog.Fatal('This is a fatal message');

    { Demonstrate formatted messages }
    for I := 1 to 5 do
      MultiLog.Info('Counter value: %d', [I]);

    { Demonstrate error handling }
    try
      raise Exception.Create('Test exception');
    except
      on E: Exception do
        MultiLog.Error('Caught exception: %s', [E.Message]);
    end;

    { Loggers will be automatically freed when they go out of scope }
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end. 