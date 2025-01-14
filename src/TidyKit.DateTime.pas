unit TidyKit.DateTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  { TDateTimeKit provides a comprehensive set of date and time manipulation functions.
    All operations are static (class functions) for ease of use and memory safety.
    The kit follows a functional programming style where operations return new values
    rather than modifying existing ones. }
  TDateTimeKit = class
  public
    { Basic operations for getting and formatting date/time values }
    
    { Returns the current date and time }
    class function GetNow: TDateTime; static;
    
    { Returns the current date (time part is set to 00:00:00) }
    class function GetToday: TDateTime; static;
    
    { Converts a TDateTime value to another TDateTime value.
      Useful for type conversion and ensuring TDateTime type }
    class function GetDateTime(const AValue: TDateTime): TDateTime; static;
    
    { Formats a TDateTime value as a string.
      If AFormat is empty, uses default format.
      For format strings, use:
      - yyyy: 4-digit year    - yy: 2-digit year
      - mm: month            - dd: day
      - hh: hour (24h)       - nn: minute
      - ss: second           - zzz: millisecond }
    class function GetAsString(const AValue: TDateTime; const AFormat: string = ''): string; static;
    
    { Parses a string into a TDateTime value.
      If AFormat is empty, uses default format.
      Raises EConvertError if the string is not a valid date/time }
    class function FromString(const AValue: string; const AFormat: string = ''): TDateTime; static;
    
    { Date parts - getters
      These functions extract individual components from a TDateTime value }
    
    { Returns the year component (e.g., 2024) }
    class function GetYear(const AValue: TDateTime): Integer; static;
    
    { Returns the month component (1-12) }
    class function GetMonth(const AValue: TDateTime): Integer; static;
    
    { Returns the day component (1-31) }
    class function GetDay(const AValue: TDateTime): Integer; static;
    
    { Returns the day of week (1=Sunday, 2=Monday, ..., 7=Saturday) }
    class function GetDayOfWeek(const AValue: TDateTime): Integer; static;
    
    { Returns the day of year (1-366) }
    class function GetDayOfYear(const AValue: TDateTime): Integer; static;
    
    { Returns the hour component (0-23) }
    class function GetHour(const AValue: TDateTime): Integer; static;
    
    { Returns the minute component (0-59) }
    class function GetMinute(const AValue: TDateTime): Integer; static;
    
    { Returns the second component (0-59) }
    class function GetSecond(const AValue: TDateTime): Integer; static;
    
    { Returns the millisecond component (0-999) }
    class function GetMillisecond(const AValue: TDateTime): Integer; static;
    
    { Date parts - setters
      These functions return a new TDateTime with the specified component changed }
    
    { Sets the year while preserving month, day and time }
    class function SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime; static;
    
    { Sets the month (1-12) while preserving year, day and time }
    class function SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime; static;
    
    { Sets the day (1-31) while preserving year, month and time }
    class function SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime; static;
    
    { Sets the hour (0-23) while preserving date and other time parts }
    class function SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime; static;
    
    { Sets the minute (0-59) while preserving date and other time parts }
    class function SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime; static;
    
    { Sets the second (0-59) while preserving date and other time parts }
    class function SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime; static;
    
    { Sets the millisecond (0-999) while preserving date and other time parts }
    class function SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime; static;
    
    { Modifications
      These functions add or subtract time periods, returning a new TDateTime }
    
    { Adds the specified number of years (can be negative) }
    class function AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime; static;
    
    { Adds the specified number of months (can be negative) }
    class function AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime; static;
    
    { Adds the specified number of days (can be negative) }
    class function AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime; static;
    
    { Adds the specified number of hours (can be negative) }
    class function AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime; static;
    
    { Adds the specified number of minutes (can be negative) }
    class function AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime; static;
    
    { Adds the specified number of seconds (can be negative) }
    class function AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime; static;
    
    { Truncations
      These functions return a new TDateTime set to the start/end of a period }
    
    { Returns a DateTime set to the first moment of the year (Jan 1, 00:00:00.000) }
    class function StartOfYear(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the first moment of the month (1st day, 00:00:00.000) }
    class function StartOfMonth(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the first moment of the week (Sunday, 00:00:00.000) }
    class function StartOfWeek(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the first moment of the day (00:00:00.000) }
    class function StartOfDay(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the first moment of the hour (XX:00:00.000) }
    class function StartOfHour(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the last moment of the year (Dec 31, 23:59:59.999) }
    class function EndOfYear(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the last moment of the month (last day, 23:59:59.999) }
    class function EndOfMonth(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the last moment of the week (Saturday, 23:59:59.999) }
    class function EndOfWeek(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the last moment of the day (23:59:59.999) }
    class function EndOfDay(const AValue: TDateTime): TDateTime; static;
    
    { Returns a DateTime set to the last moment of the hour (XX:59:59.999) }
    class function EndOfHour(const AValue: TDateTime): TDateTime; static;
    
    { Comparisons
      These functions compare two DateTime values }
    
    { Returns True if AValue is before ADateTime }
    class function IsBefore(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Returns True if AValue is after ADateTime }
    class function IsAfter(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Returns True if both dates fall on the same day (ignoring time) }
    class function IsSameDay(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Returns True if both dates fall in the same month and year }
    class function IsSameMonth(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Returns True if both dates fall in the same year }
    class function IsSameYear(const AValue, ADateTime: TDateTime): Boolean; static;
    
    { Business day calculations
      These functions handle workday-related operations (Monday-Friday) }
    
    { Returns True if the date falls on a business day (Monday-Friday) }
    class function IsBusinessDay(const AValue: TDateTime): Boolean; static;
    
    { Returns the next business day after the given date }
    class function NextBusinessDay(const AValue: TDateTime): TDateTime; static;
    
    { Returns the previous business day before the given date }
    class function PreviousBusinessDay(const AValue: TDateTime): TDateTime; static;
    
    { Adds the specified number of business days (skipping weekends)
      ADays can be negative to subtract business days }
    class function AddBusinessDays(const AValue: TDateTime; const ADays: Integer): TDateTime; static;
  end;

implementation

{ TDateTimeKit }

class function TDateTimeKit.GetNow: TDateTime;
begin
  // Returns the current system date and time using SysUtils.Now
  Result := SysUtils.Now;
end;

class function TDateTimeKit.GetToday: TDateTime;
begin
  // Returns just the date part of the current system time
  // Time portion is set to 00:00:00 by truncating
  Result := Trunc(SysUtils.Now);
end;

class function TDateTimeKit.GetDateTime(const AValue: TDateTime): TDateTime;
begin
  // Simple pass-through function for type safety and consistency
  Result := AValue;
end;

class function TDateTimeKit.GetAsString(const AValue: TDateTime; const AFormat: string): string;
begin
  // Convert DateTime to string using either default or custom format
  if AFormat = '' then
    Result := DateTimeToStr(AValue)  // Use system default format
  else
    Result := FormatDateTime(AFormat, AValue);  // Use specified format
end;

class function TDateTimeKit.FromString(const AValue: string; const AFormat: string): TDateTime;
var
  FormatSettings: TFormatSettings;
begin
  // Get system default format settings
  FormatSettings := DefaultFormatSettings;
  
  // Parse string to DateTime using either default or custom format
  if AFormat = '' then
    Result := StrToDateTime(AValue, FormatSettings)
  else
  begin
    // Use provided format for parsing
    FormatSettings.ShortDateFormat := AFormat;
    Result := StrToDateTime(AValue, FormatSettings);
  end;
end;

class function TDateTimeKit.GetYear(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  // Extract date components and return just the year
  DecodeDate(AValue, Y, M, D);
  Result := Y;
end;

class function TDateTimeKit.GetMonth(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  // Extract date components and return just the month
  DecodeDate(AValue, Y, M, D);
  Result := M;
end;

class function TDateTimeKit.GetDay(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  // Extract date components and return just the day
  DecodeDate(AValue, Y, M, D);
  Result := D;
end;

class function TDateTimeKit.GetDayOfWeek(const AValue: TDateTime): Integer;
begin
  // Returns day of week where 1=Sunday, 2=Monday, ..., 7=Saturday
  Result := SysUtils.DayOfWeek(AValue);
end;

class function TDateTimeKit.GetDayOfYear(const AValue: TDateTime): Integer;
begin
  // Returns day of year (1-366)
  Result := DateUtils.DayOfTheYear(AValue);
end;

class function TDateTimeKit.GetHour(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the hour
  DecodeTime(AValue, H, M, S, MS);
  Result := H;
end;

class function TDateTimeKit.GetMinute(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the minute
  DecodeTime(AValue, H, M, S, MS);
  Result := M;
end;

class function TDateTimeKit.GetSecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the second
  DecodeTime(AValue, H, M, S, MS);
  Result := S;
end;

class function TDateTimeKit.GetMillisecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  // Extract time components and return just the millisecond
  DecodeTime(AValue, H, M, S, MS);
  Result := MS;
end;

class function TDateTimeKit.SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  // Extract current date components
  DecodeDate(AValue, Y, M, D);
  // Create new date with updated year, preserving time portion
  Result := EncodeDate(AYear, M, D) + Frac(AValue);
end;

class function TDateTimeKit.SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  // Extract current date components
  DecodeDate(AValue, Y, M, D);
  // Create new date with updated month, preserving time portion
  Result := EncodeDate(Y, AMonth, D) + Frac(AValue);
end;

class function TDateTimeKit.SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  // Extract current date components
  DecodeDate(AValue, Y, M, D);
  // Create new date with updated day, preserving time portion
  Result := EncodeDate(Y, M, ADay) + Frac(AValue);
end;

class function TDateTimeKit.SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated hour, preserving date portion
  Result := Trunc(AValue) + EncodeTime(AHour, M, S, MS);
end;

class function TDateTimeKit.SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated minute, preserving date portion
  Result := Trunc(AValue) + EncodeTime(H, AMinute, S, MS);
end;

class function TDateTimeKit.SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated second, preserving date portion
  Result := Trunc(AValue) + EncodeTime(H, M, ASecond, MS);
end;

class function TDateTimeKit.SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract current time components
  DecodeTime(AValue, H, M, S, MS);
  // Create new time with updated millisecond, preserving date portion
  Result := Trunc(AValue) + EncodeTime(H, M, S, AMilliSecond);
end;

class function TDateTimeKit.AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime;
begin
  // Add/subtract years using DateUtils function
  Result := IncYear(AValue, AYears);
end;

class function TDateTimeKit.AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime;
begin
  // Add/subtract months using DateUtils function
  Result := IncMonth(AValue, AMonths);
end;

class function TDateTimeKit.AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
begin
  // Add/subtract days using DateUtils function
  Result := IncDay(AValue, ADays);
end;

class function TDateTimeKit.AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime;
begin
  // Add/subtract hours using DateUtils function
  Result := IncHour(AValue, AHours);
end;

class function TDateTimeKit.AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime;
begin
  // Add/subtract minutes using DateUtils function
  Result := IncMinute(AValue, AMinutes);
end;

class function TDateTimeKit.AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime;
begin
  // Add/subtract seconds using DateUtils function
  Result := IncSecond(AValue, ASeconds);
end;

class function TDateTimeKit.StartOfYear(const AValue: TDateTime): TDateTime;
begin
  // Set date to January 1st of the current year at 00:00:00.000
  Result := StartOfTheYear(AValue);
end;

class function TDateTimeKit.StartOfMonth(const AValue: TDateTime): TDateTime;
begin
  // Set date to the 1st of the current month at 00:00:00.000
  Result := StartOfTheMonth(AValue);
end;

class function TDateTimeKit.StartOfWeek(const AValue: TDateTime): TDateTime;
begin
  // Set date to Sunday of the current week at 00:00:00.000
  Result := StartOfTheWeek(AValue);
end;

class function TDateTimeKit.StartOfDay(const AValue: TDateTime): TDateTime;
begin
  // Set time to 00:00:00.000, preserving the date
  Result := DateOf(AValue);
end;

class function TDateTimeKit.StartOfHour(const AValue: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract hour and set minutes, seconds, milliseconds to zero
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, 0, 0, 0);
end;

class function TDateTimeKit.EndOfYear(const AValue: TDateTime): TDateTime;
begin
  // Set date to December 31st of the current year at 23:59:59.999
  Result := EndOfTheYear(AValue);
end;

class function TDateTimeKit.EndOfMonth(const AValue: TDateTime): TDateTime;
begin
  // Set date to the last day of the current month at 23:59:59.999
  Result := EndOfTheMonth(AValue);
end;

class function TDateTimeKit.EndOfWeek(const AValue: TDateTime): TDateTime;
begin
  // Set date to Saturday of the current week at 23:59:59.999
  Result := EndOfTheWeek(AValue);
end;

class function TDateTimeKit.EndOfDay(const AValue: TDateTime): TDateTime;
begin
  // Set time to 23:59:59.999, preserving the date
  Result := EndOfTheDay(AValue);
end;

class function TDateTimeKit.EndOfHour(const AValue: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  // Extract hour and set minutes, seconds, milliseconds to their maximum values
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, 59, 59, 999);
end;

class function TDateTimeKit.IsBefore(const AValue, ADateTime: TDateTime): Boolean;
begin
  // Compare dates using SysUtils function, returns true if AValue < ADateTime
  Result := CompareDateTime(AValue, ADateTime) < 0;
end;

class function TDateTimeKit.IsAfter(const AValue, ADateTime: TDateTime): Boolean;
begin
  // Compare dates using SysUtils function, returns true if AValue > ADateTime
  Result := CompareDateTime(AValue, ADateTime) > 0;
end;

class function TDateTimeKit.IsSameDay(const AValue, ADateTime: TDateTime): Boolean;
begin
  // Compare dates ignoring time portion
  Result := SameDate(AValue, ADateTime);
end;

class function TDateTimeKit.IsSameMonth(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  // Extract date components from both dates
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  // Compare year and month
  Result := (Y1 = Y2) and (M1 = M2);
end;

class function TDateTimeKit.IsSameYear(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  // Extract date components from both dates
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  // Compare year only
  Result := Y1 = Y2;
end;

class function TDateTimeKit.IsBusinessDay(const AValue: TDateTime): Boolean;
var
  DayOfWeek: Integer;
begin
  // Get day of week (1=Sunday, 2=Monday, ..., 7=Saturday)
  DayOfWeek := GetDayOfWeek(AValue);
  // Check if it's Monday through Friday (2-6)
  Result := (DayOfWeek > 1) and (DayOfWeek < 7);
end;

class function TDateTimeKit.NextBusinessDay(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
  // Keep adding days until we find a business day
  repeat
    Result := AddDays(Result, 1);
  until IsBusinessDay(Result);
end;

class function TDateTimeKit.PreviousBusinessDay(const AValue: TDateTime): TDateTime;
begin
  Result := AValue;
  // Keep subtracting days until we find a business day
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
    
  // Determine direction (1 for forward, -1 for backward)
  Step := ADays div Abs(ADays);
  // Get absolute number of days to add/subtract
  RemainingDays := Abs(ADays);
  
  // Keep adding/subtracting days until we've found enough business days
  while RemainingDays > 0 do
  begin
    Result := AddDays(Result, Step);
    if IsBusinessDay(Result) then
      Dec(RemainingDays);
  end;
end;

end. 