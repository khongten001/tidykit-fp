unit TidyKit.JSON;

{$mode objfpc}{$H+} 
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, TypInfo;
 
type
  { Forward declarations }
  IJSONValue = interface;
  IJSONObject = interface;
  IJSONArray = interface;
  
  { Core JSON value interface }
  IJSONValue = interface
    ['{A0D8C764-395B-4A32-8C34-FB4957157241}']
    function GetAsString: string;
    function GetAsNumber: Double;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    function GetAsObject: IJSONObject;
    function GetAsArray: IJSONArray;
    
    function IsString: Boolean;
    function IsNumber: Boolean;
    function IsBoolean: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsNull: Boolean;
    
    function ToString(Pretty: Boolean = False): string;
    
    property AsString: string read GetAsString;
    property AsNumber: Double read GetAsNumber;
    property AsInteger: Integer read GetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsObject: IJSONObject read GetAsObject;
    property AsArray: IJSONArray read GetAsArray;
  end;

  { JSON object interface }
  IJSONObject = interface(IJSONValue)
    ['{B2C7F915-0D47-4A8E-B652-D0F9E4D23179}']
    function GetValue(const Name: string): IJSONValue;
    procedure SetValue(const Name: string; Value: IJSONValue);
    function GetCount: Integer;
    function GetNames: TStringArray;
    
    procedure Add(const Name: string; Value: IJSONValue); overload;
    procedure Add(const Name: string; const Value: string); overload;
    procedure Add(const Name: string; Value: Integer); overload;
    procedure Add(const Name: string; Value: Double); overload;
    procedure Add(const Name: string; Value: Boolean); overload;
    
    procedure Remove(const Name: string);
    function Contains(const Name: string): Boolean;
    
    property Values[const Name: string]: IJSONValue read GetValue write SetValue; default;
    property Count: Integer read GetCount;
    property Names: TStringArray read GetNames;
  end;

  { JSON array interface }
  IJSONArray = interface(IJSONValue)
    ['{C9E0D732-C2B1-4E6F-9F8A-8E8D5F54B3E7}']
    function GetItem(Index: Integer): IJSONValue;
    procedure SetItem(Index: Integer; Value: IJSONValue);
    function GetCount: Integer;
    
    procedure Add(Value: IJSONValue); overload;
    procedure Add(const Value: string); overload;
    procedure Add(Value: Integer); overload;
    procedure Add(Value: Double); overload;
    procedure Add(Value: Boolean); overload;
    
    procedure Delete(Index: Integer);
    procedure Clear;
    
    property Items[Index: Integer]: IJSONValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  { JSON exception class }
  EJSONException = class(Exception);

  { JSON token types }
  TJSONTokenType = (
    ttNone,
    ttCurlyOpen,
    ttCurlyClose,
    ttSquareOpen,
    ttSquareClose,
    ttColon,
    ttComma,
    ttString,
    ttNumber,
    ttTrue,
    ttFalse,
    ttNull
  );

  { JSON token record }
  TJSONToken = record
    TokenType: TJSONTokenType;
    Value: string;
  end;

implementation

end. 