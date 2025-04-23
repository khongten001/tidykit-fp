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
  TidyKit.Logger;

type
  { Re-export the core types }
  ETidyKitException = TidyKit.Core.ETidyKitException;
  TKitBase = TidyKit.Core.TKitBase;

  { Re-export the filesystem types }
  IFileKit = TidyKit.FS.IFileKit; // Add IFileKit interface
  TFSFactory = TidyKit.FS.TFSFactory; // Add TFSFactory class
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

  { Re-export math types }
  TDoubleArray = TidyKit.Math.TDoubleArray;
  TIntegerArray = TidyKit.Math.TIntegerArray;
  TSingleArray = TidyKit.Math.TSingleArray;
  TExtendedArray = TidyKit.Math.TExtendedArray;
  TDoublePair = TidyKit.Math.TDoublePair;

  { Re-export the Matrix types }
  IMatrix = TidyKit.Math.Matrices.IMatrix;
  TMatrixKit = TidyKit.Math.Matrices.TMatrixKit;
  TMatrixArray = TidyKit.Math.Matrices.TMatrixArray;
  TLUDecomposition = TidyKit.Math.Matrices.TLUDecomposition;
  TQRDecomposition = TidyKit.Math.Matrices.TQRDecomposition;
  TEigenDecomposition = TidyKit.Math.Matrices.TEigenDecomposition;
  TCholeskyDecomposition = TidyKit.Math.Matrices.TCholeskyDecomposition;
  TEigenpair = TidyKit.Math.Matrices.TEigenpair;
  TSVD = TidyKit.Math.Matrices.TSVD;
  TSparseElement = TidyKit.Math.Matrices.TSparseElement;
  TMatrixKitSparse = TidyKit.Math.Matrices.TMatrixKitSparse;
  EMatrixError = TidyKit.Math.Matrices.EMatrixError;
  TIterativeMethod = TidyKit.Math.Matrices.TIterativeMethod;

  { Re-export the finance types }
  TFinanceKit = TidyKit.Math.Finance.TFinanceKit;
  EFinanceError = TidyKit.Math.Finance.EFinanceError;

  { Re-export the trigonometry types }
  TTrigKit = TidyKit.Math.Trigonometry.TTrigKit;

  { Re-export the statistics types }
  TStatsKit = TidyKit.Math.Stats.TStatsKit;
  EStatsError = TidyKit.Math.Stats.EStatsError;

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

  { Re-export matrix constants }
  imConjugateGradient = TidyKit.Math.Matrices.imConjugateGradient;
  imJacobi = TidyKit.Math.Matrices.imJacobi;
  imGaussSeidel = TidyKit.Math.Matrices.imGaussSeidel;

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

// Convenience functions for matrix operations
function CreateMatrix(Rows, Cols: Integer): IMatrix;
function CreateMatrixFromArray(const Data: TMatrixArray): IMatrix;
function IdentityMatrix(Size: Integer): IMatrix;
function ZerosMatrix(Rows, Cols: Integer): IMatrix;
function OnesMatrix(Rows, Cols: Integer): IMatrix;
function DiagonalMatrix(const Diagonal: array of Double): IMatrix;
function SolveLinearSystem(const A, B: IMatrix): IMatrix;
function HilbertMatrix(Size: Integer): IMatrix;
function ToeplitzMatrix(const FirstRow, FirstCol: TDoubleArray): IMatrix;
function VandermondeMatrix(const Vector: TDoubleArray): IMatrix;
function BandMatrix(Size, LowerBand, UpperBand: Integer): IMatrix;
function SymmetricMatrix(const Data: TMatrixArray): IMatrix;
function RandomMatrix(Rows, Cols: Integer; Min, Max: Double): IMatrix;
function SolveIterative(const A, B: IMatrix; Method: TIterativeMethod = imConjugateGradient; 
                        MaxIterations: Integer = 1000; Tolerance: Double = 1e-10): IMatrix;

// Convenience function for sparse matrices
function CreateSparseMatrix(Rows, Cols: Integer): IMatrix;

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

// Convenience functions for matrix operations
function CreateMatrix(Rows, Cols: Integer): IMatrix;
begin
  Result := TMatrixKit.Create(Rows, Cols);
end;

function CreateMatrixFromArray(const Data: TMatrixArray): IMatrix;
begin
  Result := TMatrixKit.CreateFromArray(Data);
end;

function IdentityMatrix(Size: Integer): IMatrix;
begin
  Result := TMatrixKit.Identity(Size);
end;

function ZerosMatrix(Rows, Cols: Integer): IMatrix;
begin
  Result := TMatrixKit.Zeros(Rows, Cols);
end;

function OnesMatrix(Rows, Cols: Integer): IMatrix;
begin
  Result := TMatrixKit.Ones(Rows, Cols);
end;

function DiagonalMatrix(const Diagonal: array of Double): IMatrix;
begin
  Result := TMatrixKit.CreateDiagonal(Diagonal);
end;

function SolveLinearSystem(const A, B: IMatrix): IMatrix;
begin
  Result := A.Inverse.Multiply(B);
end;

function HilbertMatrix(Size: Integer): IMatrix;
begin
  Result := TMatrixKit.CreateHilbert(Size);
end;

function ToeplitzMatrix(const FirstRow, FirstCol: TDoubleArray): IMatrix;
begin
  Result := TMatrixKit.CreateToeplitz(FirstRow, FirstCol);
end;

function VandermondeMatrix(const Vector: TDoubleArray): IMatrix;
begin
  Result := TMatrixKit.CreateVandermonde(Vector);
end;

function BandMatrix(Size, LowerBand, UpperBand: Integer): IMatrix;
begin
  Result := TMatrixKit.CreateBandMatrix(Size, LowerBand, UpperBand);
end;

function SymmetricMatrix(const Data: TMatrixArray): IMatrix;
begin
  Result := TMatrixKit.CreateSymmetric(Data);
end;

function RandomMatrix(Rows, Cols: Integer; Min, Max: Double): IMatrix;
begin
  Result := TMatrixKit.CreateRandom(Rows, Cols, Min, Max);
end;

function SolveIterative(const A, B: IMatrix; Method: TIterativeMethod = imConjugateGradient; 
                        MaxIterations: Integer = 1000; Tolerance: Double = 1e-10): IMatrix;
begin
  Result := A.SolveIterative(B, Method, MaxIterations, Tolerance);
end;

function CreateSparseMatrix(Rows, Cols: Integer): IMatrix;
begin
  Result := TMatrixKit.CreateSparse(Rows, Cols);
end;


initialization
  Http := Default(THttp);

end.
