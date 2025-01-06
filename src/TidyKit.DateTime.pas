unit TidyKit.DateTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, TidyKit.Core;

type
  { Interface for DateTime operations }
  IDateTimeKit = interface(IChainable)
    ['{A1B2C3D4-E5F6-G7H8-I9J0-K1L2M3N4O5P6}']
    function GetValue: TDateTime;
    
    { Basic operations }
    function From(const AValue: TDateTime): IDateTimeKit;
    function FromString(const AValue: string; const AFormat: string = ''): IDateTimeKit;
    function Now: IDateTimeKit;
    function Today: IDateTimeKit;
    function ToDateTime: TDateTime;
    function ToString(const AFormat: string = ''): string;
    
    { Date parts - both getters and setters }
    function Year(const AValue: Integer = -1): IDateTimeKit;
    function Month(const AValue: Integer = -1): IDateTimeKit;
    function Day(const AValue: Integer = -1): IDateTimeKit;
    function DayOfWeek: Integer;
    function DayOfYear: Integer;
    
    { Time parts - both getters and setters }
    function Hour(const AValue: Integer = -1): IDateTimeKit;
    function Minute(const AValue: Integer = -1): IDateTimeKit;
    function Second(const AValue: Integer = -1): IDateTimeKit;
    function MilliSecond(const AValue: Integer = -1): IDateTimeKit;
    
    { Modifications }
    function AddYears(const AYears: Integer): IDateTimeKit;
    function AddMonths(const AMonths: Integer): IDateTimeKit;
    function AddDays(const ADays: Integer): IDateTimeKit;
    function AddHours(const AHours: Integer): IDateTimeKit;
    function AddMinutes(const AMinutes: Integer): IDateTimeKit;
    function AddSeconds(const ASeconds: Integer): IDateTimeKit;
    
    { Truncations }
    function StartOfYear: IDateTimeKit;
    function StartOfMonth: IDateTimeKit;
    function StartOfWeek: IDateTimeKit;
    function StartOfDay: IDateTimeKit;
    function StartOfHour: IDateTimeKit;
    
    function EndOfYear: IDateTimeKit;
    function EndOfMonth: IDateTimeKit;
    function EndOfWeek: IDateTimeKit;
    function EndOfDay: IDateTimeKit;
    function EndOfHour: IDateTimeKit;
    
    { Comparisons }
    function IsBefore(const ADateTime: TDateTime): Boolean;
    function IsAfter(const ADateTime: TDateTime): Boolean;
    function IsSameDay(const ADateTime: TDateTime): Boolean;
    function IsSameMonth(const ADateTime: TDateTime): Boolean;
    function IsSameYear(const ADateTime: TDateTime): Boolean;
    
    { Properties }
    property Value: TDateTime read GetValue;
  end;

  { Implementation of DateTime operations }
  TDateTimeKit = class(TKitBase, IDateTimeKit)
  private
    FValue: TDateTime;
    function GetValue: TDateTime;
  public
    constructor Create;
    
    { Interface implementations }
    function From(const AValue: TDateTime): IDateTimeKit;
    function FromString(const AValue: string; const AFormat: string = ''): IDateTimeKit;
    function Now: IDateTimeKit;
    function Today: IDateTimeKit;
    function ToDateTime: TDateTime;
    function ToString(const AFormat: string = ''): string; override;
    
    function Year(const AValue: Integer = -1): IDateTimeKit;
    function Month(const AValue: Integer = -1): IDateTimeKit;
    function Day(const AValue: Integer = -1): IDateTimeKit;
    function DayOfWeek: Integer;
    function DayOfYear: Integer;
    
    function Hour(const AValue: Integer = -1): IDateTimeKit;
    function Minute(const AValue: Integer = -1): IDateTimeKit;
    function Second(const AValue: Integer = -1): IDateTimeKit;
    function MilliSecond(const AValue: Integer = -1): IDateTimeKit;
    
    function AddYears(const AYears: Integer): IDateTimeKit;
    function AddMonths(const AMonths: Integer): IDateTimeKit;
    function AddDays(const ADays: Integer): IDateTimeKit;
    function AddHours(const AHours: Integer): IDateTimeKit;
    function AddMinutes(const AMinutes: Integer): IDateTimeKit;
    function AddSeconds(const ASeconds: Integer): IDateTimeKit;
    
    function StartOfYear: IDateTimeKit;
    function StartOfMonth: IDateTimeKit;
    function StartOfWeek: IDateTimeKit;
    function StartOfDay: IDateTimeKit;
    function StartOfHour: IDateTimeKit;
    
    function EndOfYear: IDateTimeKit;
    function EndOfMonth: IDateTimeKit;
    function EndOfWeek: IDateTimeKit;
    function EndOfDay: IDateTimeKit;
    function EndOfHour: IDateTimeKit;
    
    function IsBefore(const ADateTime: TDateTime): Boolean;
    function IsAfter(const ADateTime: TDateTime): Boolean;
    function IsSameDay(const ADateTime: TDateTime): Boolean;
    function IsSameMonth(const ADateTime: TDateTime): Boolean;
    function IsSameYear(const ADateTime: TDateTime): Boolean;
  end;

implementation

{ TDateTimeKit }

constructor TDateTimeKit.Create;
begin
  inherited Create;
  FValue := 0;
end;

function TDateTimeKit.GetValue: TDateTime;
begin
  Result := FValue;
end;

function TDateTimeKit.From(const AValue: TDateTime): IDateTimeKit;
begin
  FValue := AValue;
  Result := Self;
end;

function TDateTimeKit.Now: IDateTimeKit;
begin
  FValue := SysUtils.Now;
  Result := Self;
end;

function TDateTimeKit.Today: IDateTimeKit;
begin
  FValue := Trunc(SysUtils.Now);
  Result := Self;
end;

function TDateTimeKit.FromString(const AValue: string; const AFormat: string): IDateTimeKit;
begin
  if AFormat = '' then
    FValue := StrToDateTime(AValue)
  else
    FValue := StrToDateTime(AValue, AFormat);
  Result := Self;
end;

function TDateTimeKit.ToDateTime: TDateTime;
begin
  Result := FValue;
end;

function TDateTimeKit.ToString(const AFormat: string): string;
begin
  if AFormat = '' then
    Result := DateTimeToStr(FValue)
  else
    Result := FormatDateTime(AFormat, FValue);
end;

function TDateTimeKit.Year(const AValue: Integer = -1): IDateTimeKit;
var
  Y, M, D: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeDate(FValue, Y, M, D);
    FValue := EncodeDate(AValue, M, D) + Frac(FValue);
    Result := Self;
  end;
end;

function TDateTimeKit.Month(const AValue: Integer = -1): IDateTimeKit;
var
  Y, M, D: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeDate(FValue, Y, M, D);
    FValue := EncodeDate(Y, AValue, D) + Frac(FValue);
    Result := Self;
  end;
end;

function TDateTimeKit.Day(const AValue: Integer = -1): IDateTimeKit;
var
  Y, M, D: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeDate(FValue, Y, M, D);
    FValue := EncodeDate(Y, M, AValue) + Frac(FValue);
    Result := Self;
  end;
end;

function TDateTimeKit.Hour(const AValue: Integer = -1): IDateTimeKit;
var
  H, M, S, MS: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeTime(FValue, H, M, S, MS);
    FValue := Trunc(FValue) + EncodeTime(AValue, M, S, MS);
    Result := Self;
  end;
end;

function TDateTimeKit.Minute(const AValue: Integer = -1): IDateTimeKit;
var
  H, M, S, MS: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeTime(FValue, H, M, S, MS);
    FValue := Trunc(FValue) + EncodeTime(H, AValue, S, MS);
    Result := Self;
  end;
end;

function TDateTimeKit.Second(const AValue: Integer = -1): IDateTimeKit;
var
  H, M, S, MS: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeTime(FValue, H, M, S, MS);
    FValue := Trunc(FValue) + EncodeTime(H, M, AValue, MS);
    Result := Self;
  end;
end;

function TDateTimeKit.MilliSecond(const AValue: Integer = -1): IDateTimeKit;
var
  H, M, S, MS: Word;
begin
  if AValue = -1 then
    Result := Self
  else
  begin
    DecodeTime(FValue, H, M, S, MS);
    FValue := Trunc(FValue) + EncodeTime(H, M, S, AValue);
    Result := Self;
  end;
end;

function TDateTimeKit.AddYears(const AYears: Integer): IDateTimeKit;
begin
  FValue := IncYear(FValue, AYears);
  Result := Self;
end;

function TDateTimeKit.AddMonths(const AMonths: Integer): IDateTimeKit;
begin
  FValue := IncMonth(FValue, AMonths);
  Result := Self;
end;

function TDateTimeKit.AddDays(const ADays: Integer): IDateTimeKit;
begin
  FValue := IncDay(FValue, ADays);
  Result := Self;
end;

function TDateTimeKit.AddHours(const AHours: Integer): IDateTimeKit;
begin
  FValue := IncHour(FValue, AHours);
  Result := Self;
end;

function TDateTimeKit.AddMinutes(const AMinutes: Integer): IDateTimeKit;
begin
  FValue := IncMinute(FValue, AMinutes);
  Result := Self;
end;

function TDateTimeKit.AddSeconds(const ASeconds: Integer): IDateTimeKit;
begin
  FValue := IncSecond(FValue, ASeconds);
  Result := Self;
end;

function TDateTimeKit.StartOfYear: IDateTimeKit;
begin
  FValue := StartOfTheYear(FValue);
  Result := Self;
end;

function TDateTimeKit.StartOfMonth: IDateTimeKit;
begin
  FValue := StartOfTheMonth(FValue);
  Result := Self;
end;

function TDateTimeKit.StartOfWeek: IDateTimeKit;
begin
  FValue := StartOfTheWeek(FValue);
  Result := Self;
end;

function TDateTimeKit.StartOfDay: IDateTimeKit;
begin
  FValue := DateOf(FValue);
  Result := Self;
end;

function TDateTimeKit.StartOfHour: IDateTimeKit;
begin
  FValue := StartOfTheHour(FValue);
  Result := Self;
end;

function TDateTimeKit.EndOfYear: IDateTimeKit;
begin
  FValue := EndOfTheYear(FValue);
  Result := Self;
end;

function TDateTimeKit.EndOfMonth: IDateTimeKit;
begin
  FValue := EndOfTheMonth(FValue);
  Result := Self;
end;

function TDateTimeKit.EndOfWeek: IDateTimeKit;
begin
  FValue := EndOfTheWeek(FValue);
  Result := Self;
end;

function TDateTimeKit.EndOfDay: IDateTimeKit;
begin
  FValue := EndOfTheDay(FValue);
  Result := Self;
end;

function TDateTimeKit.EndOfHour: IDateTimeKit;
begin
  FValue := EndOfTheHour(FValue);
  Result := Self;
end;

function TDateTimeKit.IsBefore(const ADateTime: TDateTime): Boolean;
begin
  Result := CompareDateTime(FValue, ADateTime) < 0;
end;

function TDateTimeKit.IsAfter(const ADateTime: TDateTime): Boolean;
begin
  Result := CompareDateTime(FValue, ADateTime) > 0;
end;

function TDateTimeKit.IsSameDay(const ADateTime: TDateTime): Boolean;
begin
  Result := SameDate(FValue, ADateTime);
end;

function TDateTimeKit.IsSameMonth(const ADateTime: TDateTime): Boolean;
begin
  Result := (YearOf(FValue) = YearOf(ADateTime)) and
           (MonthOf(FValue) = MonthOf(ADateTime));
end;

function TDateTimeKit.IsSameYear(const ADateTime: TDateTime): Boolean;
begin
  Result := YearOf(FValue) = YearOf(ADateTime);
end;

end. 