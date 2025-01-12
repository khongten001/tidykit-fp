unit TidyKit.DateTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  { DateTime operations }
  TDateTimeKit = class
  public
    { Basic operations }
    class function GetNow: TDateTime; static;
    class function GetToday: TDateTime; static;
    class function GetDateTime(const AValue: TDateTime): TDateTime; static;
    class function GetAsString(const AValue: TDateTime; const AFormat: string = ''): string; static;
    class function FromString(const AValue: string; const AFormat: string = ''): TDateTime; static;
    
    { Date parts - getters }
    class function GetYear(const AValue: TDateTime): Integer; static;
    class function GetMonth(const AValue: TDateTime): Integer; static;
    class function GetDay(const AValue: TDateTime): Integer; static;
    class function GetDayOfWeek(const AValue: TDateTime): Integer; static;
    class function GetDayOfYear(const AValue: TDateTime): Integer; static;
    class function GetHour(const AValue: TDateTime): Integer; static;
    class function GetMinute(const AValue: TDateTime): Integer; static;
    class function GetSecond(const AValue: TDateTime): Integer; static;
    class function GetMillisecond(const AValue: TDateTime): Integer; static;
    
    { Date parts - setters }
    class function SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime; static;
    class function SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime; static;
    class function SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime; static;
    class function SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime; static;
    class function SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime; static;
    class function SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime; static;
    class function SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime; static;
    
    { Modifications }
    class function AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime; static;
    class function AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime; static;
    class function AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime; static;
    class function AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime; static;
    class function AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime; static;
    class function AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime; static;
    
    { Truncations }
    class function StartOfYear(const AValue: TDateTime): TDateTime; static;
    class function StartOfMonth(const AValue: TDateTime): TDateTime; static;
    class function StartOfWeek(const AValue: TDateTime): TDateTime; static;
    class function StartOfDay(const AValue: TDateTime): TDateTime; static;
    class function StartOfHour(const AValue: TDateTime): TDateTime; static;
    
    class function EndOfYear(const AValue: TDateTime): TDateTime; static;
    class function EndOfMonth(const AValue: TDateTime): TDateTime; static;
    class function EndOfWeek(const AValue: TDateTime): TDateTime; static;
    class function EndOfDay(const AValue: TDateTime): TDateTime; static;
    class function EndOfHour(const AValue: TDateTime): TDateTime; static;
    
    { Comparisons }
    class function IsBefore(const AValue, ADateTime: TDateTime): Boolean; static;
    class function IsAfter(const AValue, ADateTime: TDateTime): Boolean; static;
    class function IsSameDay(const AValue, ADateTime: TDateTime): Boolean; static;
    class function IsSameMonth(const AValue, ADateTime: TDateTime): Boolean; static;
    class function IsSameYear(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Business day calculations }
    class function IsBusinessDay(const AValue: TDateTime): Boolean; static;
    class function NextBusinessDay(const AValue: TDateTime): TDateTime; static;
    class function PreviousBusinessDay(const AValue: TDateTime): TDateTime; static;
    class function AddBusinessDays(const AValue: TDateTime; const ADays: Integer): TDateTime; static;
  end;

implementation

{ TDateTimeKit }

class function TDateTimeKit.GetNow: TDateTime;
begin
  Result := SysUtils.Now;
end;

class function TDateTimeKit.GetToday: TDateTime;
begin
  Result := Trunc(SysUtils.Now);
end;

class function TDateTimeKit.GetDateTime(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
end;

class function TDateTimeKit.GetAsString(const AValue: TDateTime; const AFormat: string): string;
begin
  if AFormat = '' then
    Result := DateTimeToStr(AValue)
  else
    Result := FormatDateTime(AFormat, AValue);
end;

class function TDateTimeKit.FromString(const AValue: string; const AFormat: string): TDateTime;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  if AFormat = '' then
    Result := StrToDateTime(AValue, FormatSettings)
  else
  begin
    FormatSettings.ShortDateFormat := AFormat;
    Result := StrToDateTime(AValue, FormatSettings);
  end;
end;

class function TDateTimeKit.GetYear(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(AValue, Y, M, D);
  Result := Y;
end;

class function TDateTimeKit.GetMonth(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(AValue, Y, M, D);
  Result := M;
end;

class function TDateTimeKit.GetDay(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(AValue, Y, M, D);
  Result := D;
end;

class function TDateTimeKit.GetDayOfWeek(const AValue: TDateTime): Integer;
begin
  Result := SysUtils.DayOfWeek(AValue);
end;

class function TDateTimeKit.GetDayOfYear(const AValue: TDateTime): Integer;
begin
  Result := DateUtils.DayOfTheYear(AValue);
end;

class function TDateTimeKit.GetHour(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := H;
end;

class function TDateTimeKit.GetMinute(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := M;
end;

class function TDateTimeKit.GetSecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := S;
end;

class function TDateTimeKit.GetMillisecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := MS;
end;

class function TDateTimeKit.SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  DecodeDate(AValue, Y, M, D);
  Result := EncodeDate(AYear, M, D) + Frac(AValue);
end;

class function TDateTimeKit.SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  DecodeDate(AValue, Y, M, D);
  Result := EncodeDate(Y, AMonth, D) + Frac(AValue);
end;

class function TDateTimeKit.SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  DecodeDate(AValue, Y, M, D);
  Result := EncodeDate(Y, M, ADay) + Frac(AValue);
end;

class function TDateTimeKit.SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(AHour, M, S, MS);
end;

class function TDateTimeKit.SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, AMinute, S, MS);
end;

class function TDateTimeKit.SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, M, ASecond, MS);
end;

class function TDateTimeKit.SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, M, S, AMilliSecond);
end;

class function TDateTimeKit.AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime;
begin
  Result := IncYear(AValue, AYears);
end;

class function TDateTimeKit.AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime;
begin
  Result := IncMonth(AValue, AMonths);
end;

class function TDateTimeKit.AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
begin
  Result := IncDay(AValue, ADays);
end;

class function TDateTimeKit.AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime;
begin
  Result := IncHour(AValue, AHours);
end;

class function TDateTimeKit.AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime;
begin
  Result := IncMinute(AValue, AMinutes);
end;

class function TDateTimeKit.AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime;
begin
  Result := IncSecond(AValue, ASeconds);
end;

class function TDateTimeKit.StartOfYear(const AValue: TDateTime): TDateTime;
begin
  Result := StartOfTheYear(AValue);
end;

class function TDateTimeKit.StartOfMonth(const AValue: TDateTime): TDateTime;
begin
  Result := StartOfTheMonth(AValue);
end;

class function TDateTimeKit.StartOfWeek(const AValue: TDateTime): TDateTime;
begin
  Result := StartOfTheWeek(AValue);
end;

class function TDateTimeKit.StartOfDay(const AValue: TDateTime): TDateTime;
begin
  Result := DateOf(AValue);
end;

class function TDateTimeKit.StartOfHour(const AValue: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, 0, 0, 0);
end;

class function TDateTimeKit.EndOfYear(const AValue: TDateTime): TDateTime;
begin
  Result := EndOfTheYear(AValue);
end;

class function TDateTimeKit.EndOfMonth(const AValue: TDateTime): TDateTime;
begin
  Result := EndOfTheMonth(AValue);
end;

class function TDateTimeKit.EndOfWeek(const AValue: TDateTime): TDateTime;
begin
  Result := EndOfTheWeek(AValue);
end;

class function TDateTimeKit.EndOfDay(const AValue: TDateTime): TDateTime;
begin
  Result := EndOfTheDay(AValue);
end;

class function TDateTimeKit.EndOfHour(const AValue: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, 59, 59, 999);
end;

class function TDateTimeKit.IsBefore(const AValue, ADateTime: TDateTime): Boolean;
begin
  Result := CompareDateTime(AValue, ADateTime) < 0;
end;

class function TDateTimeKit.IsAfter(const AValue, ADateTime: TDateTime): Boolean;
begin
  Result := CompareDateTime(AValue, ADateTime) > 0;
end;

class function TDateTimeKit.IsSameDay(const AValue, ADateTime: TDateTime): Boolean;
begin
  Result := SameDate(AValue, ADateTime);
end;

class function TDateTimeKit.IsSameMonth(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  Result := (Y1 = Y2) and (M1 = M2);
end;

class function TDateTimeKit.IsSameYear(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  Result := Y1 = Y2;
end;

class function TDateTimeKit.IsBusinessDay(const AValue: TDateTime): Boolean;
var
  DayOfWeek: Integer;
begin
  DayOfWeek := GetDayOfWeek(AValue);
  Result := (DayOfWeek > 1) and (DayOfWeek < 7); // Monday to Friday
end;

class function TDateTimeKit.NextBusinessDay(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
  repeat
    Result := AddDays(Result, 1);
  until IsBusinessDay(Result);
end;

class function TDateTimeKit.PreviousBusinessDay(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
  repeat
    Result := AddDays(Result, -1);
  until IsBusinessDay(Result);
end;

class function TDateTimeKit.AddBusinessDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
var
  Step, RemainingDays: Integer;
begin
  Result := AValue;
  if ADays = 0 then
    Exit;
    
  Step := ADays div Abs(ADays); // 1 or -1
  RemainingDays := Abs(ADays);
  
  while RemainingDays > 0 do
  begin
    Result := AddDays(Result, Step);
    if IsBusinessDay(Result) then
      Dec(RemainingDays);
  end;
end;

end. 