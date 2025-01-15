unit TidyKit.DateTime;

{$mode objfpc}{$H+}{$J-}

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
  { Returns the current system date and time using SysUtils.Now
    This is the most direct way to get the current date and time.
    The returned value includes both date and time components.
    Example: 2024-03-15 14:30:45.123 }
  Result := SysUtils.Now;
end;

class function TDateTimeKit.GetToday: TDateTime;
begin
  { Returns just the date part of the current system time.
    Uses Trunc to remove the time portion, effectively setting it to midnight (00:00:00).
    Example: If current time is 2024-03-15 14:30:45.123,
    this returns 2024-03-15 00:00:00.000 }
  Result := Trunc(SysUtils.Now);
end;

class function TDateTimeKit.GetDateTime(const AValue: TDateTime): TDateTime;
begin
  { Simple pass-through function for type safety and consistency.
    Useful in generic code where you want to ensure you're working with TDateTime.
    Also serves as a validation point for TDateTime values. }
  Result := AValue;
end;

class function TDateTimeKit.GetAsString(const AValue: TDateTime; const AFormat: string): string;
begin
  { Convert DateTime to string using either default or custom format.
    If AFormat is empty, uses system default format (based on regional settings).
    For custom format, use format specifiers like:
    yyyy-mm-dd hh:nn:ss -> 2024-03-15 14:30:45
    dd/mm/yy hh:nn -> 15/03/24 14:30 }
  if AFormat = '' then
    Result := DateTimeToStr(AValue)  // Uses system default format
  else
    Result := FormatDateTime(AFormat, AValue);  // Uses specified custom format
end;

class function TDateTimeKit.FromString(const AValue: string; const AFormat: string): TDateTime;
var
  FormatSettings: TFormatSettings;
begin
  { Parse string to DateTime using system or custom format.
    FormatSettings ensures consistent parsing regardless of system locale.
    
    Examples:
    - Default format: '2024-03-15 14:30:45'
    - Custom format: '15/03/24' with AFormat = 'dd/mm/yy'
    
    Note: Will raise EConvertError if string doesn't match format }
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
  { Extract date components and return just the year.
    DecodeDate splits a TDateTime into year, month, and day.
    Example: For 2024-03-15, returns 2024 }
  DecodeDate(AValue, Y, M, D);
  Result := Y;
end;

class function TDateTimeKit.GetMonth(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  { Extract date components and return just the month (1-12).
    Example: For 2024-03-15, returns 3 (March) }
  DecodeDate(AValue, Y, M, D);
  Result := M;
end;

class function TDateTimeKit.GetDay(const AValue: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  { Extract date components and return just the day (1-31).
    Example: For 2024-03-15, returns 15 }
  DecodeDate(AValue, Y, M, D);
  Result := D;
end;

class function TDateTimeKit.GetDayOfWeek(const AValue: TDateTime): Integer;
begin
  { Returns day of week where:
    1 = Sunday
    2 = Monday
    3 = Tuesday
    4 = Wednesday
    5 = Thursday
    6 = Friday
    7 = Saturday
    
    Example: For Friday March 15, 2024, returns 6 }
  Result := SysUtils.DayOfWeek(AValue);
end;

class function TDateTimeKit.GetDayOfYear(const AValue: TDateTime): Integer;
begin
  { Returns day of year (1-366).
    Range is 1-365 for normal years, 1-366 for leap years.
    Example: March 15 is the 74th day of 2024 (a leap year) }
  Result := DateUtils.DayOfTheYear(AValue);
end;

class function TDateTimeKit.GetHour(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  { Extract time components and return just the hour (0-23).
    DecodeTime splits a TDateTime into hour, minute, second, and millisecond.
    Example: For 14:30:45.123, returns 14 }
  DecodeTime(AValue, H, M, S, MS);
  Result := H;
end;

class function TDateTimeKit.GetMinute(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  { Extract time components and return just the minute (0-59).
    Example: For 14:30:45.123, returns 30 }
  DecodeTime(AValue, H, M, S, MS);
  Result := M;
end;

class function TDateTimeKit.GetSecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  { Extract time components and return just the second (0-59).
    Example: For 14:30:45.123, returns 45 }
  DecodeTime(AValue, H, M, S, MS);
  Result := S;
end;

class function TDateTimeKit.GetMillisecond(const AValue: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  { Extract time components and return just the millisecond (0-999).
    Example: For 14:30:45.123, returns 123 }
  DecodeTime(AValue, H, M, S, MS);
  Result := MS;
end;

class function TDateTimeKit.SetYear(const AValue: TDateTime; const AYear: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  { Creates new date with updated year while preserving all other components.
    Uses Frac(AValue) to keep the time portion unchanged.
    
    Example:
    Input: 2023-03-15 14:30:45, AYear = 2024
    Output: 2024-03-15 14:30:45
    
    Note: Will raise EConvertError if resulting date is invalid
    (e.g., Feb 29 in non-leap year) }
  DecodeDate(AValue, Y, M, D);
  Result := EncodeDate(AYear, M, D) + Frac(AValue);
end;

class function TDateTimeKit.SetMonth(const AValue: TDateTime; const AMonth: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  { Creates new date with updated month while preserving all other components.
    AMonth must be between 1 and 12.
    
    Example:
    Input: 2024-03-15 14:30:45, AMonth = 4
    Output: 2024-04-15 14:30:45
    
    Note: Will raise EConvertError if resulting date is invalid
    (e.g., April 31 or invalid month number) }
  DecodeDate(AValue, Y, M, D);
  Result := EncodeDate(Y, AMonth, D) + Frac(AValue);
end;

class function TDateTimeKit.SetDay(const AValue: TDateTime; const ADay: Integer): TDateTime;
var
  Y, M, D: Word;
begin
  { Creates new date with updated day while preserving all other components.
    ADay must be valid for the given month (1-31, depending on month).
    
    Example:
    Input: 2024-03-15 14:30:45, ADay = 20
    Output: 2024-03-20 14:30:45
    
    Note: Will raise EConvertError if day is invalid for the month }
  DecodeDate(AValue, Y, M, D);
  Result := EncodeDate(Y, M, ADay) + Frac(AValue);
end;

class function TDateTimeKit.SetHour(const AValue: TDateTime; const AHour: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  { Creates new time with updated hour while preserving date and other time parts.
    AHour must be between 0 and 23.
    
    Example:
    Input: 2024-03-15 14:30:45, AHour = 16
    Output: 2024-03-15 16:30:45
    
    Note: Will raise EConvertError if hour is outside valid range }
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(AHour, M, S, MS);
end;

class function TDateTimeKit.SetMinute(const AValue: TDateTime; const AMinute: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  { Creates new time with updated minute while preserving all other components.
    AMinute must be between 0 and 59.
    
    Example:
    Input: 2024-03-15 14:30:45, AMinute = 45
    Output: 2024-03-15 14:45:45
    
    Note: Will raise EConvertError if minute is outside valid range }
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, AMinute, S, MS);
end;

class function TDateTimeKit.SetSecond(const AValue: TDateTime; const ASecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  { Creates new time with updated second while preserving all other components.
    ASecond must be between 0 and 59.
    
    Example:
    Input: 2024-03-15 14:30:45, ASecond = 30
    Output: 2024-03-15 14:30:30
    
    Note: Will raise EConvertError if second is outside valid range }
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, M, ASecond, MS);
end;

class function TDateTimeKit.SetMilliSecond(const AValue: TDateTime; const AMilliSecond: Integer): TDateTime;
var
  H, M, S, MS: Word;
begin
  { Creates new time with updated millisecond while preserving all other components.
    AMilliSecond must be between 0 and 999.
    
    Example:
    Input: 2024-03-15 14:30:45.123, AMilliSecond = 500
    Output: 2024-03-15 14:30:45.500
    
    Note: Will raise EConvertError if millisecond is outside valid range }
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, M, S, AMilliSecond);
end;

class function TDateTimeKit.AddYears(const AValue: TDateTime; const AYears: Integer): TDateTime;
begin
  { Adds or subtracts years from the date.
    Uses DateUtils.IncYear for safe year arithmetic.
    Handles negative values to subtract years.
    
    Examples:
    - AddYears(2024-03-15, 1) -> 2025-03-15
    - AddYears(2024-03-15, -2) -> 2022-03-15
    
    Note: Handles leap year edge cases automatically }
  Result := IncYear(AValue, AYears);
end;

class function TDateTimeKit.AddMonths(const AValue: TDateTime; const AMonths: Integer): TDateTime;
begin
  { Adds or subtracts months from the date.
    Uses DateUtils.IncMonth for safe month arithmetic.
    Handles negative values to subtract months.
    
    Examples:
    - AddMonths(2024-03-15, 2) -> 2024-05-15
    - AddMonths(2024-03-15, -1) -> 2024-02-15
    
    Note: Adjusts for different month lengths automatically }
  Result := IncMonth(AValue, AMonths);
end;

class function TDateTimeKit.AddDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
begin
  { Adds or subtracts days from the date.
    Uses DateUtils.IncDay for consistent day arithmetic.
    Handles negative values to subtract days.
    
    Examples:
    - AddDays(2024-03-15, 5) -> 2024-03-20
    - AddDays(2024-03-15, -3) -> 2024-03-12
    
    Note: Automatically handles month and year boundaries }
  Result := IncDay(AValue, ADays);
end;

class function TDateTimeKit.AddHours(const AValue: TDateTime; const AHours: Integer): TDateTime;
begin
  { Adds or subtracts hours from the time.
    Uses DateUtils.IncHour for consistent time arithmetic.
    Handles negative values to subtract hours.
    
    Examples:
    - AddHours(2024-03-15 14:30, 3) -> 2024-03-15 17:30
    - AddHours(2024-03-15 14:30, -2) -> 2024-03-15 12:30
    
    Note: Automatically handles day boundaries }
  Result := IncHour(AValue, AHours);
end;

class function TDateTimeKit.AddMinutes(const AValue: TDateTime; const AMinutes: Integer): TDateTime;
begin
  { Adds or subtracts minutes from the time.
    Uses DateUtils.IncMinute for consistent time arithmetic.
    Handles negative values to subtract minutes.
    
    Examples:
    - AddMinutes(2024-03-15 14:30, 45) -> 2024-03-15 15:15
    - AddMinutes(2024-03-15 14:30, -20) -> 2024-03-15 14:10
    
    Note: Automatically handles hour and day boundaries }
  Result := IncMinute(AValue, AMinutes);
end;

class function TDateTimeKit.AddSeconds(const AValue: TDateTime; const ASeconds: Integer): TDateTime;
begin
  { Adds or subtracts seconds from the time.
    Uses DateUtils.IncSecond for consistent time arithmetic.
    Handles negative values to subtract seconds.
    
    Examples:
    - AddSeconds(2024-03-15 14:30:45, 30) -> 2024-03-15 14:31:15
    - AddSeconds(2024-03-15 14:30:45, -15) -> 2024-03-15 14:30:30
    
    Note: Automatically handles minute, hour, and day boundaries }
  Result := IncSecond(AValue, ASeconds);
end;

class function TDateTimeKit.StartOfYear(const AValue: TDateTime): TDateTime;
begin
  { Sets date to January 1st of the current year at 00:00:00.000
    Uses DateUtils.StartOfTheYear for consistent behavior.
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-01-01 00:00:00.000 }
  Result := StartOfTheYear(AValue);
end;

class function TDateTimeKit.StartOfMonth(const AValue: TDateTime): TDateTime;
begin
  { Sets date to the 1st of the current month at 00:00:00.000
    Uses DateUtils.StartOfTheMonth for consistent behavior.
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-03-01 00:00:00.000 }
  Result := StartOfTheMonth(AValue);
end;

class function TDateTimeKit.StartOfWeek(const AValue: TDateTime): TDateTime;
begin
  { Sets date to Sunday of the current week at 00:00:00.000
    Uses DateUtils.StartOfTheWeek for consistent behavior.
    
    Example:
    Input: 2024-03-15 (Friday) 14:30:45.123
    Output: 2024-03-10 (Sunday) 00:00:00.000 }
  Result := StartOfTheWeek(AValue);
end;

class function TDateTimeKit.StartOfDay(const AValue: TDateTime): TDateTime;
begin
  { Sets time to 00:00:00.000, preserving the date
    Uses DateUtils.DateOf to extract just the date portion.
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-03-15 00:00:00.000 }
  Result := DateOf(AValue);
end;

class function TDateTimeKit.StartOfHour(const AValue: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  { Sets minutes, seconds, and milliseconds to zero, preserving the hour
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-03-15 14:00:00.000 }
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, 0, 0, 0);
end;

class function TDateTimeKit.EndOfYear(const AValue: TDateTime): TDateTime;
begin
  { Sets date to December 31st of the current year at 23:59:59.999
    Uses DateUtils.EndOfTheYear for consistent behavior.
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-12-31 23:59:59.999 }
  Result := EndOfTheYear(AValue);
end;

class function TDateTimeKit.EndOfMonth(const AValue: TDateTime): TDateTime;
begin
  { Sets date to the last day of the current month at 23:59:59.999
    Uses DateUtils.EndOfTheMonth for consistent behavior.
    Automatically handles different month lengths.
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-03-31 23:59:59.999 }
  Result := EndOfTheMonth(AValue);
end;

class function TDateTimeKit.EndOfWeek(const AValue: TDateTime): TDateTime;
begin
  { Sets date to Saturday of the current week at 23:59:59.999
    Uses DateUtils.EndOfTheWeek for consistent behavior.
    
    Example:
    Input: 2024-03-15 (Friday) 14:30:45.123
    Output: 2024-03-16 (Saturday) 23:59:59.999 }
  Result := EndOfTheWeek(AValue);
end;

class function TDateTimeKit.EndOfDay(const AValue: TDateTime): TDateTime;
begin
  { Sets time to 23:59:59.999, preserving the date
    Uses DateUtils.EndOfTheDay for consistent behavior.
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-03-15 23:59:59.999 }
  Result := EndOfTheDay(AValue);
end;

class function TDateTimeKit.EndOfHour(const AValue: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  { Sets minutes, seconds, and milliseconds to their maximum values,
    preserving the hour
    
    Example:
    Input: 2024-03-15 14:30:45.123
    Output: 2024-03-15 14:59:59.999 }
  DecodeTime(AValue, H, M, S, MS);
  Result := Trunc(AValue) + EncodeTime(H, 59, 59, 999);
end;

class function TDateTimeKit.IsBefore(const AValue, ADateTime: TDateTime): Boolean;
begin
  { Compares two dates using SysUtils.CompareDateTime
    Returns True if AValue is chronologically before ADateTime
    
    Example:
    IsBefore(2024-03-15, 2024-03-16) -> True
    IsBefore(2024-03-15 14:30, 2024-03-15 14:31) -> True }
  Result := CompareDateTime(AValue, ADateTime) < 0;
end;

class function TDateTimeKit.IsAfter(const AValue, ADateTime: TDateTime): Boolean;
begin
  { Compares two dates using SysUtils.CompareDateTime
    Returns True if AValue is chronologically after ADateTime
    
    Example:
    IsAfter(2024-03-16, 2024-03-15) -> True
    IsAfter(2024-03-15 14:31, 2024-03-15 14:30) -> True }
  Result := CompareDateTime(AValue, ADateTime) > 0;
end;

class function TDateTimeKit.IsSameDay(const AValue, ADateTime: TDateTime): Boolean;
begin
  { Compares two dates ignoring time portion
    Uses SysUtils.SameDate for accurate comparison
    
    Example:
    IsSameDay(2024-03-15 14:30, 2024-03-15 09:00) -> True
    IsSameDay(2024-03-15, 2024-03-16) -> False }
  Result := SameDate(AValue, ADateTime);
end;

class function TDateTimeKit.IsSameMonth(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  { Compares year and month components of two dates
    Extracts components using DecodeDate for accurate comparison
    
    Example:
    IsSameMonth(2024-03-15, 2024-03-01) -> True
    IsSameMonth(2024-03-15, 2024-04-15) -> False }
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  Result := (Y1 = Y2) and (M1 = M2);
end;

class function TDateTimeKit.IsSameYear(const AValue, ADateTime: TDateTime): Boolean;
var
  Y1, M1, D1, Y2, M2, D2: Word;
begin
  { Compares year components of two dates
    Extracts components using DecodeDate for accurate comparison
    
    Example:
    IsSameYear(2024-03-15, 2024-12-31) -> True
    IsSameYear(2024-03-15, 2025-03-15) -> False }
  DecodeDate(AValue, Y1, M1, D1);
  DecodeDate(ADateTime, Y2, M2, D2);
  Result := Y1 = Y2;
end;

class function TDateTimeKit.IsBusinessDay(const AValue: TDateTime): Boolean;
var
  DayOfWeek: Integer;
begin
  { Checks if date falls on Monday through Friday
    Uses GetDayOfWeek where:
    1=Sunday, 2=Monday, ..., 7=Saturday
    Returns True for days 2-6 (Monday-Friday)
    
    Example:
    IsBusinessDay(2024-03-15) -> True (Friday)
    IsBusinessDay(2024-03-16) -> False (Saturday) }
  DayOfWeek := GetDayOfWeek(AValue);
  Result := (DayOfWeek > 1) and (DayOfWeek < 7);
end;

class function TDateTimeKit.NextBusinessDay(const AValue: TDateTime): TDateTime;
begin
  { Finds the next business day (Monday-Friday)
    Keeps adding days until a business day is found
    
    Example:
    NextBusinessDay(2024-03-15 Friday) -> 2024-03-18 Monday
    NextBusinessDay(2024-03-16 Saturday) -> 2024-03-18 Monday }
  Result := AValue;
  repeat
    Result := AddDays(Result, 1);
  until IsBusinessDay(Result);
end;

class function TDateTimeKit.PreviousBusinessDay(const AValue: TDateTime): TDateTime;
begin
  { Finds the previous business day (Monday-Friday)
    Keeps subtracting days until a business day is found
    
    Example:
    PreviousBusinessDay(2024-03-18 Monday) -> 2024-03-15 Friday
    PreviousBusinessDay(2024-03-17 Sunday) -> 2024-03-15 Friday }
  Result := AValue;
  repeat
    Result := AddDays(Result, -1);
  until IsBusinessDay(Result);
end;

class function TDateTimeKit.AddBusinessDays(const AValue: TDateTime; const ADays: Integer): TDateTime;
var
  Step, RemainingDays: Integer;
begin
  { Adds or subtracts business days, skipping weekends
    
    Parameters:
    - AValue: Starting date
    - ADays: Number of business days to add (negative to subtract)
    
    Examples:
    - AddBusinessDays(2024-03-15 Friday, 2) -> 2024-03-19 Tuesday
    - AddBusinessDays(2024-03-15 Friday, -3) -> 2024-03-12 Tuesday
    
    Note: A business day is Monday through Friday }
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