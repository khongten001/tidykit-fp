unit TidyKit;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils,
  TidyKit.Core,
  TidyKit.FS,
  TidyKit.Strings,
  TidyKit.DateTime,
  TidyKit.Crypto,
  TidyKit.Crypto.AES256,
  TidyKit.JSON,
  TidyKit.Request,
  TidyKit.Math,
  TidyKit.Math.Matrices,
  TidyKit.Math.Finance,
  TidyKit.Math.Trigonometry,
  TidyKit.Math.Stats,
  TidyKit.Archive,
  TidyKit.Logging,
  TidyKit.Logging.Console,
  TidyKit.Logging.FileLog;

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
  TDirectoryInfo = TidyKit.FS.TDirectoryInfo;

  { Re-export the JSON types }
  TJSON = TidyKit.JSON.TJSON;
  IJSONValue = TidyKit.JSON.IJSONValue;
  IJSONObject = TidyKit.JSON.IJSONObject;
  IJSONArray = TidyKit.JSON.IJSONArray;
  EJSONException = TidyKit.JSON.EJSONException;

  { Re-export the crypto types }
  TCryptoKit = TidyKit.Crypto.TCryptoKit;
  TBlowfishMode = TidyKit.Crypto.TBlowfishMode;
  TAESKey = TidyKit.Crypto.TAESKey;
  TAESBlock = TidyKit.Crypto.TAESBlock;

  { Re-export the request types }
  TResponse = TidyKit.Request.TResponse;
  THttpRequest = TidyKit.Request.THttpRequest;
  TRequestResult = TidyKit.Request.TRequestResult;
  THttp = TidyKit.Request.THttp;

  { Re-export math types }
  TMatrix = TidyKit.Math.TMatrix;
  TMatrixKit = TidyKit.Math.Matrices.TMatrixKit;
  TFinanceKit = TidyKit.Math.Finance.TFinanceKit;
  TTrigKit = TidyKit.Math.Trigonometry.TTrigKit;
  TStatsKit = TidyKit.Math.Stats.TStatsKit;

  { Re-export archive types }
  TArchiveKit = TidyKit.Archive.TArchiveKit;

  { Re-export logging types }
  TLogger = TidyKit.Logging.TLogger;
  TLogLevel = TidyKit.Logging.TLogLevel;
  TLogRecord = TidyKit.Logging.TLogRecord;
  TLogDestination = TidyKit.Logging.TLogDestination;
  TConsoleDestination = TidyKit.Logging.Console.TConsoleDestination;
  TFileDestination = TidyKit.Logging.FileLog.TFileDestination;

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

  { Re-export crypto constants }
  bmEncrypt = TidyKit.Crypto.bmEncrypt;
  bmDecrypt = TidyKit.Crypto.bmDecrypt;

  { Re-export log level constants }
  llDebug = TidyKit.Logging.llDebug;
  llInfo = TidyKit.Logging.llInfo;
  llWarning = TidyKit.Logging.llWarning;
  llError = TidyKit.Logging.llError;
  llFatal = TidyKit.Logging.llFatal;

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

var
  { Re-export HTTP constants }
  Http: THttp;

implementation

initialization
  Http := Default(THttp);

end. 
