# TidyKit.DateTime

The `TidyKit.DateTime` module provides comprehensive date and time manipulation utilities for Free Pascal applications. It offers a wide range of functionality for working with dates, times, timezones, and daylight saving time (DST).

## Features

- **Basic Date/Time Operations**: Create, manipulate, and format dates and times
- **Date Parts**: Extract and modify individual components of dates and times
- **Date Manipulations**: Add or subtract time periods from dates
- **Date Truncations**: Get the start or end of various time periods (day, month, year, etc.)
- **Date Comparisons**: Compare dates using various criteria
- **Business Day Functions**: Work with business days (excluding weekends and holidays)
- **Time Span Operations**: Create and manipulate time periods and intervals
- **Date Unit Operations**: Floor, ceiling, and round dates to various units
- **Timezone Support**: Work with different timezones and handle DST transitions
- **Enhanced DST Detection**: Accurate DST detection for multiple regions

## Timezone Operations

### GetTimeZone
```pascal
function GetTimeZone(const AValue: TDateTime): TTimeZoneInfo;
```
Returns timezone information for a given date/time. The `TTimeZoneInfo` record contains:
- `Name`: Timezone name (e.g., 'UTC', 'America/New_York')
- `Offset`: Offset from UTC in minutes
- `IsDST`: Whether daylight savings is in effect

Platform-specific behavior:
- **Windows**: Uses Windows API for accurate DST detection
  - Handles US DST rules (March to November)
  - Supports custom DST rules from Windows settings
  - Falls back to UTC if timezone information is unavailable
- **Linux**: Uses system timezone files
  - Checks `TZ` environment variable
  - Reads from `/etc/timezone` or `/etc/localtime`
  - Parses timezone files for offset and DST information
  - Falls back to UTC if timezone information is unavailable

### GetSystemTimeZone
```pascal
function GetSystemTimeZone: string;
```
Returns the name of the system's current timezone.

### GetTimeZoneNames
```pascal
function GetTimeZoneNames: TStringArray;
```
Returns an array of available timezone names. On Windows, includes both standard and DST names.

### Timezone Conversion
```pascal
function WithTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime;
function ForceTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime;
```
Convert dates between timezones:
- `WithTimeZone`: Converts to specified timezone while preserving the point in time
- `ForceTimeZone`: Forces a date to be interpreted in the specified timezone

### DST Handling
The library provides accurate DST detection:
- Windows: Uses system DST rules
- Linux: Parses timezone files for DST rules
- Handles edge cases around DST transitions
- Supports region-specific DST rules

### Error Handling
```pascal
type
  ETimeZoneError = class(Exception);
```
Timezone operations may raise `ETimeZoneError` for:
- Invalid timezone names
- Unsupported timezone operations
- Timezone file access errors

### Best Practices
1. Always check `IsDST` when working with dates near DST transitions
2. Use `WithTimeZone` for timezone conversions to preserve point in time
3. Handle `ETimeZoneError` for robust timezone operations
4. Consider platform-specific behavior when deploying

## Examples

### Basic Date/Time Operations

```pascal
var
  CurrentTime: TDateTime;
  NextWorkday: TDateTime;
begin
  // Get current time
  CurrentTime := TDateTimeKit.GetNow;
  
  // Get next business day
  NextWorkday := TDateTimeKit.NextBusinessDay(CurrentTime);
  
  // Format for display
  WriteLn(TDateTimeKit.GetAsString(NextWorkday, 'yyyy-mm-dd'));
end;
```

### Timezone and DST Operations

```pascal
var
  CurrentTime: TDateTime;
  TZInfo: TTimeZoneInfo;
  UTCTime: TDateTime;
begin
  // Get current time
  CurrentTime := TDateTimeKit.GetNow;
  
  // Get timezone information
  TZInfo := TDateTimeKit.GetTimeZone(CurrentTime);
  WriteLn('Timezone: ', TZInfo.Name);
  WriteLn('Offset: ', TZInfo.Offset, ' minutes');
  WriteLn('DST: ', BoolToStr(TZInfo.IsDST, True));
  
  // Convert to UTC
  UTCTime := TDateTimeKit.WithTimeZone(CurrentTime, 'UTC');
  WriteLn('UTC time: ', TDateTimeKit.GetAsString(UTCTime, 'yyyy-mm-dd hh:nn:ss'));
end;
```

### DST Transition Handling

```pascal
var
  DSTStart: TDateTime;
  TZInfo: TTimeZoneInfo;
begin
  // March 10, 2024 2:00 AM (DST start in US)
  DSTStart := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);
  
  // Check DST status
  TZInfo := TDateTimeKit.GetTimeZone(DSTStart);
  if TZInfo.IsDST then
    WriteLn('Time is in DST')
  else
    WriteLn('Time is not in DST');
end;
```

## API Reference

### TTimeZoneInfo

```pascal
TTimeZoneInfo = record
  Name: string;           // Timezone name (e.g., 'UTC', 'America/New_York')
  Offset: Integer;        // Offset from UTC in minutes
  IsDST: Boolean;        // Whether daylight savings is in effect
end;
```

### TDSTRule

```pascal
TDSTRule = record
  Region: string;           // Region identifier (e.g., 'US', 'EU', 'AU')
  StartMonth: Integer;      // Month when DST starts (1-12)
  StartWeek: Integer;       // Week of the month (1-5, where 5 means last)
  StartDayOfWeek: Integer;  // Day of week (1-7, where 1=Sunday)
  StartHour: Integer;       // Hour when DST starts (0-23)
  EndMonth: Integer;        // Month when DST ends (1-12)
  EndWeek: Integer;         // Week of the month (1-5, where 5 means last)
  EndDayOfWeek: Integer;    // Day of week (1-7, where 1=Sunday)
  EndHour: Integer;         // Hour when DST ends (0-23)
  Offset: Integer;          // DST offset in minutes (typically 60)
end;
```

### Key Functions

#### GetTimeZone

```pascal
class function GetTimeZone(const AValue: TDateTime): TTimeZoneInfo; static;
```

Returns timezone information for the specified date/time, including DST status.

#### WithTimeZone

```pascal
class function WithTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime; static;
```

Converts a date/time to a different timezone, handling DST transitions correctly.

#### ForceTimeZone

```pascal
class function ForceTimeZone(const AValue: TDateTime; const ATimeZone: string): TDateTime; static;
```

Forces a date/time to be interpreted in a specific timezone, without changing the actual time.

## Notes

- The DST detection system is designed to work on both Windows and Linux platforms
- On Windows, it uses the Windows API to determine DST status
- On Linux, it reads timezone files and applies region-specific DST rules
- The system supports multiple regions with different DST rules
- For unsupported platforms or when timezone information cannot be determined, it falls back to UTC 