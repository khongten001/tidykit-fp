unit TidyKit;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils,
  TidyKit.Core,
  TidyKit.FS,
  TidyKit.Strings,
  TidyKit.DateTime,
  TidyKit.Crypto;

type
  { Re-export the core types }
  ETidyKitException = TidyKit.Core.ETidyKitException;
  TKitBase = TidyKit.Core.TKitBase;

  { Re-export the filesystem types }
  TFileKit = TidyKit.FS.TFileKit;
  TSearchResults = TidyKit.FS.TSearchResults;
  TFileSortOrder = TidyKit.FS.TFileSortOrder;
  TFileAttributes = TidyKit.FS.TFileAttributes;
  TSearchResult = TidyKit.FS.TSearchResult;
  TFilePathArray = TidyKit.FS.TFilePathArray;

const
  { Re-export filesystem constants }
  fsNone = TidyKit.FS.fsNone;
  fsName = TidyKit.FS.fsName;
  fsNameDesc = TidyKit.FS.fsNameDesc;
  fsDate = TidyKit.FS.fsDate;
  fsDateDesc = TidyKit.FS.fsDateDesc;
  fsSize = TidyKit.FS.fsSize;
  fsSizeDesc = TidyKit.FS.fsSizeDesc;

  { Re-export datetime constants }
  duSecond = TidyKit.DateTime.duSecond;
  duMinute = TidyKit.DateTime.duMinute;
  duHour = TidyKit.DateTime.duHour;
  duDay = TidyKit.DateTime.duDay;
  duWeek = TidyKit.DateTime.duWeek;
  duMonth = TidyKit.DateTime.duMonth;
  duBiMonth = TidyKit.DateTime.duBiMonth;
  duQuarter = TidyKit.DateTime.duQuarter;
  duHalfYear = TidyKit.DateTime.duHalfYear;
  duYear = TidyKit.DateTime.duYear;

  { Re-export date span kind constants }
  dskPeriod = TidyKit.DateTime.dskPeriod;
  dskDuration = TidyKit.DateTime.dskDuration;

type
  { Re-export the string types }
  TStringKit = TidyKit.Strings.TStringKit;
  TMatchesResults = TidyKit.Strings.TMatchesResults;

  { Re-export the datetime types }
  TDateTimeKit = TidyKit.DateTime.TDateTimeKit;
  TDateSpanKind = TidyKit.DateTime.TDateSpanKind;
  TDateSpan = TidyKit.DateTime.TDateSpan;
  TInterval = TidyKit.DateTime.TInterval;
  TDateUnit = TidyKit.DateTime.TDateUnit;
  TTimeZoneInfo = TidyKit.DateTime.TTimeZoneInfo;
  ETimeZoneError = TidyKit.DateTime.ETimeZoneError;

  { Re-export the crypto types }
  TCryptoKit = TidyKit.Crypto.TCryptoKit;
  TBlowfishMode = TidyKit.Crypto.TBlowfishMode;
  TAESMode = TidyKit.Crypto.TAESMode;
  TAESContext = TidyKit.Crypto.TAESContext;
  TAESKit = TidyKit.Crypto.TAESKit;

const
  { Re-export AES mode constants }
  amECB = TidyKit.Crypto.amECB;
  amCBC = TidyKit.Crypto.amCBC;
  amCTR = TidyKit.Crypto.amCTR;

implementation

end. 
