program TestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes
  , consoletestrunner
  //, TestCaseDateTime
  //, TestCaseFS
  //, TestCaseString
  //, TestCaseCrypto
  //, TestCaseRequest
  //, TestCaseMath
  //, TestCaseArchive
  //, TidyKit.Crypto.AES256.Test
  //, TidyKit.JSON.Test
  , TidyKit.Logging.Test;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
