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
- **Cross-Platform Timezone Support**: Reliable timezone handling on both Windows and Linux
- **Advanced DST Detection**: Accurate DST detection for multiple global regions

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
  - Handles US, EU, Australian, and other region DST rules
  - Supports standard Windows timezone database
  - Properly handles ambiguous times during DST transitions
  - Falls back to UTC if timezone information is unavailable
- **Linux**: Uses system timezone files
  - Checks `TZ` environment variable
  - Reads from `/etc/timezone` or `/etc/localtime`
  - Parses timezone files for offset and DST information
  - Fully supports IANA timezone database
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
The library provides accurate DST detection for multiple global regions:

- **North America (US/Canada)**: Second Sunday in March to First Sunday in November
- **Europe (EU)**: Last Sunday in March to Last Sunday in October
- **Australia (Southern States)**: First Sunday in October to First Sunday in April
- **New Zealand**: Last Sunday in September to First Sunday in April
- **Brazil**: First Sunday in November to Third Sunday in February

Each region's rules are correctly implemented for both Windows and Linux platforms, ensuring consistent behavior across operating systems. Special care is taken to handle ambiguous times during DST transitions (e.g., when the clock moves back and a time occurs twice).

### Cross-Platform Environment Variable Support
The library provides platform-independent functions for working with environment variables, which is crucial for timezone testing and configuration:

```pascal
// Get/set environment variable cross-platform
function GetEnvVar(const Name: string): string;
procedure SetEnvVar(const Name, Value: string);
```

These helper functions ensure consistent environment variable handling on both Windows and Linux, allowing you to:
- Save and restore timezone settings during testing
- Set the `TZ` environment variable for specific timezone tests
- Handle environment variables without platform-specific code

Example usage:
```pascal
var
  OriginalTZ: string;
begin
  // Save original timezone
  OriginalTZ := GetEnvVar('TZ');
  try
    // Set timezone for testing
    SetEnvVar('TZ', 'America/New_York');
    // Run timezone-sensitive operations...
  finally
    // Restore original timezone
    SetEnvVar('TZ', OriginalTZ);
  end;
end;
```

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
4. Use `GetEnvVar` and `SetEnvVar` for environment variable operations
5. For testing timezone behavior, save and restore timezone settings
6. When working with DST transition times, be aware of ambiguous times
7. For maximum compatibility, test both Windows and Linux behavior

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

### Cross-Platform DST Transition Handling

```pascal
var
  OriginalTZ: string;
  DSTDate: TDateTime;
  TZInfo: TTimeZoneInfo;
  // Define test dates for different regions
  USDate, EUDate, AUDate: TDateTime;
begin
  // Save original timezone setting
  OriginalTZ := GetEnvVar('TZ');
  
  try
    // Test Australian DST
    SetEnvVar('TZ', 'Australia/Sydney');
    
    // Create a datetime value for 2:00 AM on first Sunday in October 2024
    // (Note: EncodeDateTime just creates a TDateTime value, it doesn't handle DST)
    DSTDate := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0);
    
    // Use GetTimeZone to check DST status for this datetime
    TZInfo := TDateTimeKit.GetTimeZone(DSTDate);
    if TZInfo.IsDST then
      WriteLn('Time is in DST - Australian summer time is in effect')
    else
      WriteLn('Time is not in DST');
    
    // Create more datetime values for testing different regional DST transitions
    // US DST starts second Sunday in March
    USDate := EncodeDateTime(2024, 3, 10, 2, 0, 0, 0);
    // EU DST starts last Sunday in March
    EUDate := EncodeDateTime(2024, 3, 31, 1, 0, 0, 0);
    // AU DST starts first Sunday in October
    AUDate := EncodeDateTime(2024, 10, 6, 2, 0, 0, 0);
    
    // Check DST status for each region by using GetTimeZone
    SetEnvVar('TZ', 'America/New_York');
    WriteLn('US DST: ', BoolToStr(TDateTimeKit.GetTimeZone(USDate).IsDST, True));
    
    SetEnvVar('TZ', 'Europe/London');
    WriteLn('EU DST: ', BoolToStr(TDateTimeKit.GetTimeZone(EUDate).IsDST, True));
    
    SetEnvVar('TZ', 'Australia/Sydney');
    WriteLn('AU DST: ', BoolToStr(TDateTimeKit.GetTimeZone(AUDate).IsDST, True));
      
    // Convert to UTC and back
    WriteLn('UTC time: ', DateTimeToStr(TDateTimeKit.WithTimeZone(DSTDate, 'UTC')));
    WriteLn('Local time: ', DateTimeToStr(TDateTimeKit.WithTimeZone(
                              TDateTimeKit.WithTimeZone(DSTDate, 'UTC'), 
                              'Australia/Sydney')));
  finally
    // Restore original timezone
    SetEnvVar('TZ', OriginalTZ);
  end;
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

### Platform-Specific Helpers

```pascal
// Cross-platform environment variable helpers
function GetEnvVar(const Name: string): string;
procedure SetEnvVar(const Name, Value: string);
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

## Cross-Platform Considerations

### Windows
- Uses Windows API for timezone information
- Provides reliable DST detection for all major regions
- Handles ambiguous times during DST transitions
- Windows timezone names may differ from IANA names

### Linux
- Uses IANA timezone database
- Relies on the `TZ` environment variable and timezone files
- More standardized timezone naming conventions
- Requires proper installation of timezone data

### Testing Tips
- Always test with both Windows and Linux environments
- Use `GetEnvVar` and `SetEnvVar` for testing different timezones
- Pay special attention to dates near DST transitions
- For critical applications, test with different regional settings 