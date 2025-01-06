unit TidyKit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TidyKit.Core,
  TidyKit.FS,
  TidyKit.Strings,
  TidyKit.DateTime;

type
  { Re-export the core types }
  IChainable = TidyKit.Core.IChainable;
  ETidyKitException = TidyKit.Core.ETidyKitException;
  TKitBase = TidyKit.Core.TKitBase;

  { Re-export the filesystem types }
  IFileKit = TidyKit.FS.IFileKit;
  TFileKit = TidyKit.FS.TFileKit;

  { Re-export the string types }
  IStringKit = TidyKit.Strings.IStringKit;
  TStringKit = TidyKit.Strings.TStringKit;

  { Re-export the datetime types }
  IDateTimeKit = TidyKit.DateTime.IDateTimeKit;
  TDateTimeKit = TidyKit.DateTime.TDateTimeKit;

{ Factory functions for easier instantiation }
function Files: IFileKit;
function Strings: IStringKit;
function DateTime: IDateTimeKit;

implementation

function Files: IFileKit;
begin
  Result := TFileKit.Create;
end;

function Strings: IStringKit;
begin
  Result := TStringKit.Create;
end;

function DateTime: IDateTimeKit;
begin
  Result := TDateTimeKit.Create;
end;

end. 