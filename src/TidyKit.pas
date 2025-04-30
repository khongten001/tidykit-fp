unit TidyKit;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils,
  TidyKit.FS,
  TidyKit.Strings,
  TidyKit.DateTime,
  TidyKit.Crypto,
  TidyKit.Crypto.AES256,
  TidyKit.JSON,
  TidyKit.Request,
  TidyKit.Archive,
  TidyKit.Logger;

type
  { Re-export the filesystem types }
  EFSError = TidyKit.FS.EFSError;
  TFileKit = TidyKit.FS.TFileKit;
  TSearchResults = TidyKit.FS.TSearchResults;
  TFileSortOrder = TidyKit.FS.TFileSortOrder;
  TFileAttributes = TidyKit.FS.TFileAttributes;
  TSearchResult = TidyKit.FS.TSearchResult;
  TFilePathArray = TidyKit.FS.TFilePathArray;
  TDirectoryInfo = TidyKit.FS.TDirectoryInfo;
  EFileSystemError = TidyKit.FS.EFileSystemError;

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
  ECryptoError = TidyKit.Crypto.ECryptoError;

  { Re-export the request types }
  TResponse = TidyKit.Request.TResponse;
  THttpRequest = TidyKit.Request.THttpRequest;
  TRequestResult = TidyKit.Request.TRequestResult;
  THttp = TidyKit.Request.THttp;
  ERequestError = TidyKit.Request.ERequestError;

  { Re-export archive types }
  TArchiveKit = TidyKit.Archive.TArchiveKit;
  EArchiveError = TidyKit.Archive.EArchiveError;

  { Re-export the logger types }
  TLogLevel = TidyKit.Logger.TLogLevel;
  TLogDestination = TidyKit.Logger.TLogDestination;
  TLogDestinations = TidyKit.Logger.TLogDestinations;
  TLogContext = TidyKit.Logger.TLogContext;
  ILogContext = TidyKit.Logger.ILogContext;
  TLogger = TidyKit.Logger.TLogger;
  ILogSink = TidyKit.Logger.ILogSink;
  ITimedOperation = TidyKit.Logger.ITimedOperation;
  TNameValuePair = TidyKit.Logger.TNameValuePair;
  TLoggerConfig = TidyKit.Logger.TLoggerConfig;
  ELoggerException = TidyKit.Logger.ELoggerException;

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

  { Re-export logger constants }
  ldConsole = TidyKit.Logger.ldConsole;
  ldFile = TidyKit.Logger.ldFile;
  llDebug = TidyKit.Logger.llDebug;
  llInfo = TidyKit.Logger.llInfo;
  llWarning = TidyKit.Logger.llWarning;
  llError = TidyKit.Logger.llError;
  llFatal = TidyKit.Logger.llFatal;

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

// Name-value pair functions
function NameValuePair(const AName: string; const AValue: string): TNameValuePair;
function NameValuePair(const AName: string; AValue: Integer): TNameValuePair;
function NameValuePair(const AName: string; AValue: Boolean): TNameValuePair;
function NameValuePair(const AName: string; AValue: Double): TNameValuePair;

// Logger singleton accessor
function Logger: TLogger;

implementation

function Logger: TLogger;
begin
  Result := TidyKit.Logger.Logger;
end;

function NameValuePair(const AName: string; const AValue: string): TNameValuePair;
begin
  Result := TidyKit.Logger.NameValuePair(AName, AValue);
end;

function NameValuePair(const AName: string; AValue: Integer): TNameValuePair;
begin
  Result := TidyKit.Logger.NameValuePair(AName, AValue);
end;

function NameValuePair(const AName: string; AValue: Boolean): TNameValuePair;
begin
  Result := TidyKit.Logger.NameValuePair(AName, AValue);
end;

function NameValuePair(const AName: string; AValue: Double): TNameValuePair;
begin
  Result := TidyKit.Logger.NameValuePair(AName, AValue);
end;

initialization
  Http := Default(THttp);

end.
