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
  ETidyKitException = TidyKit.Core.ETidyKitException;
  TKitBase = TidyKit.Core.TKitBase;

  { Re-export the filesystem types }
  TFileKit = TidyKit.FS.TFileKit;

  { Re-export the string types }
  TStringKit = TidyKit.Strings.TStringKit;

  { Re-export the datetime types }
  TDateTimeKit = TidyKit.DateTime.TDateTimeKit;

implementation

end. 