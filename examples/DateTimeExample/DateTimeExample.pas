program DateTimeExample;

{$mode objfpc}{$H+}{$J-}

{ This example demonstrates the date/time handling capabilities of TidyKit's TDateTimeKit class.
  It shows various date and time operations including:
  - Basic operations (get current time, format dates, parse dates)
  - Component access and modification (year, month, day, etc)
  - Date arithmetic (add/subtract time units)
  - Period and interval handling
  - Period boundaries (start/end of year, month, etc)
  - Timezone operations
  - Date comparisons and rounding
  - Special date operations }

uses
  SysUtils,
  TidyKit;

procedure DemonstrateBasicOperations;
var
  Now, Today: TDateTime;
  FormattedDate: string;
begin
  WriteLn('=== Basic Operations ===');
  
  // Get current date/time using TDateTimeKit helpers
  Now := TDateTimeKit.GetNow;      // Returns current date and time
  Today := TDateTimeKit.GetToday;  // Returns today's date at midnight
  
  // Format dates using custom format strings
  // Format: yyyy=year, mm=month, dd=day, hh=hour, nn=minute, ss=second
  FormattedDate := TDateTimeKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss');
  WriteLn('Current time: ', FormattedDate);
  WriteLn('Today at midnight: ', TDateTimeKit.GetAsString(Today));
  
  // Parse dates from strings using custom format patterns
  Now := TDateTimeKit.FromString('2024-03-15 14:30:00', 'yyyy-mm-dd hh:nn:ss');
  WriteLn('Parsed date: ', TDateTimeKit.GetAsString(Now));
  
  // Parse dates using common format helpers
  WriteLn('YMD: ', TDateTimeKit.GetAsString(TDateTimeKit.YMD('2024-03-15')));     // Year-Month-Day
  WriteLn('MDY: ', TDateTimeKit.GetAsString(TDateTimeKit.MDY('03-15-2024')));     // Month-Day-Year
  WriteLn('DMY: ', TDateTimeKit.GetAsString(TDateTimeKit.DMY('15-03-2024')));     // Day-Month-Year
  WriteLn('YQ: ', TDateTimeKit.GetAsString(TDateTimeKit.YQ('2024-1')));           // Year-Quarter
end;

procedure DemonstrateComponentAccess;
var
  Now: TDateTime;
begin
  WriteLn('=== Component Access ===');
  Now := TDateTimeKit.GetNow;
  
  // Extract basic date/time components
  WriteLn('Year: ', TDateTimeKit.GetYear(Now));           // 4-digit year
  WriteLn('Month: ', TDateTimeKit.GetMonth(Now));         // 1-12
  WriteLn('Day: ', TDateTimeKit.GetDay(Now));            // 1-31
  WriteLn('Hour: ', TDateTimeKit.GetHour(Now));          // 0-23
  WriteLn('Minute: ', TDateTimeKit.GetMinute(Now));      // 0-59
  WriteLn('Second: ', TDateTimeKit.GetSecond(Now));      // 0-59
  WriteLn('Millisecond: ', TDateTimeKit.GetMillisecond(Now)); // 0-999
  
  // Get additional calendar components
  WriteLn('Day of Week: ', TDateTimeKit.GetDayOfWeek(Now));  // 1=Sunday through 7=Saturday
  WriteLn('Day of Year: ', TDateTimeKit.GetDayOfYear(Now));  // 1-366
  WriteLn('Quarter: ', TDateTimeKit.GetQuarter(Now));        // 1-4
  WriteLn('Semester: ', TDateTimeKit.GetSemester(Now));      // 1-2
  
  // Get ISO calendar components (international standard)
  WriteLn('ISO Year: ', TDateTimeKit.GetISOYear(Now));       // ISO week-numbering year
  WriteLn('ISO Week: ', TDateTimeKit.GetISOWeek(Now));       // 1-53
  
  // Get epidemiological calendar components (used in healthcare)
  WriteLn('Epi Year: ', TDateTimeKit.GetEpiYear(Now));       // Epidemiological year
  WriteLn('Epi Week: ', TDateTimeKit.GetEpiWeek(Now));       // 1-53
end;

procedure DemonstrateComponentModification;
var
  Original, Modified: TDateTime;
begin
  WriteLn('=== Component Modification ===');
  Original := TDateTimeKit.GetNow;
  WriteLn('Original: ', TDateTimeKit.GetAsString(Original));
  
  // Modify individual components while preserving others
  Modified := Original;
  Modified := TDateTimeKit.SetYear(Modified, 2025);           // Set year to 2025
  Modified := TDateTimeKit.SetMonth(Modified, 6);             // Set month to June
  Modified := TDateTimeKit.SetDay(Modified, 15);             // Set day to 15th
  Modified := TDateTimeKit.SetHour(Modified, 14);            // Set hour to 14 (2 PM)
  Modified := TDateTimeKit.SetMinute(Modified, 30);          // Set minute to 30
  Modified := TDateTimeKit.SetSecond(Modified, 45);          // Set second to 45
  Modified := TDateTimeKit.SetMilliSecond(Modified, 500);    // Set millisecond to 500
  
  WriteLn('Modified: ', TDateTimeKit.GetAsString(Modified));
end;

procedure DemonstrateDateArithmetic;
var
  Now, Future: TDateTime;
begin
  WriteLn('=== Date Arithmetic ===');
  Now := TDateTimeKit.GetNow;
  
  // Demonstrate various ways to add/subtract time units
  Future := Now;
  Future := TDateTimeKit.AddYears(Future, 1);        // Add 1 year
  Future := TDateTimeKit.AddMonths(Future, -2);      // Subtract 2 months
  Future := TDateTimeKit.AddDays(Future, 7);         // Add 7 days
  Future := TDateTimeKit.AddHours(Future, 12);       // Add 12 hours
  Future := TDateTimeKit.AddMinutes(Future, 30);     // Add 30 minutes
  Future := TDateTimeKit.AddSeconds(Future, -15);    // Subtract 15 seconds
  
  WriteLn('Now: ', TDateTimeKit.GetAsString(Now));
  WriteLn('Future: ', TDateTimeKit.GetAsString(Future));
  
  // Business day calculations (skipping weekends)
  WriteLn('Next business day: ', TDateTimeKit.GetAsString(TDateTimeKit.NextBusinessDay(Now)));
  WriteLn('Previous business day: ', TDateTimeKit.GetAsString(TDateTimeKit.PreviousBusinessDay(Now)));
  WriteLn('5 business days later: ', TDateTimeKit.GetAsString(TDateTimeKit.AddBusinessDays(Now, 5)));
end;

procedure DemonstratePeriodOperations;
var
  Now: TDateTime;
  Period: TDateSpan;      // Represents a period of time (years, months, days)
  Duration: TDateSpan;    // Represents a duration in days
  Future: TDateTime;
  Span: TDateSpan;
  Seconds: Int64;
begin
  WriteLn('=== Period Operations ===');
  Now := TDateTimeKit.GetNow;
  
  // Create different types of time spans
  Period := TDateTimeKit.CreatePeriod(1, 2, 3);      // 1 year, 2 months, 3 days
  Duration := TDateTimeKit.CreateDuration(0, 0, 1);  // 1 day duration
  
  // Add a period to a date
  Future := TDateTimeKit.AddSpan(Now, Period);
  WriteLn('After adding period: ', TDateTimeKit.GetAsString(Future));
  
  // Subtract a duration from a date
  Future := TDateTimeKit.SubtractSpan(Future, Duration);
  WriteLn('After subtracting duration: ', TDateTimeKit.GetAsString(Future));
  
  // Calculate the period between two dates
  Span := TDateTimeKit.SpanBetween(Now, Future, TDateSpanKind.dskPeriod);
  WriteLn('Period between dates:');
  WriteLn('  Years: ', Span.Years);
  WriteLn('  Months: ', Span.Months);
  WriteLn('  Days: ', Span.Days);
  
  // Convert periods to/from seconds
  Seconds := TDateTimeKit.PeriodToSeconds(Period);
  WriteLn('Period in seconds: ', Seconds);
  
  // Convert seconds back to a standardized period
  Period := TDateTimeKit.SecondsToPeriod(Seconds);
  Period := TDateTimeKit.StandardizePeriod(Period);  // Normalize the period
  WriteLn('Standardized period:');
  WriteLn('  Years: ', Period.Years);
  WriteLn('  Months: ', Period.Months);
  WriteLn('  Days: ', Period.Days);
end;

procedure DemonstrateIntervalOperations;
var
  Now, Later: TDateTime;
  Interval1, Interval2: TInterval;  // Represents a time interval with start/end dates
begin
  WriteLn('=== Interval Operations ===');
  Now := TDateTimeKit.GetNow;
  Later := TDateTimeKit.AddDays(Now, 5);
  
  // Create time intervals
  Interval1 := TDateTimeKit.CreateInterval(Now, Later);                    // 5-day interval
  Interval2 := TDateTimeKit.CreateInterval(
    TDateTimeKit.AddDays(Now, 3),                                         // Starts 3 days from now
    TDateTimeKit.AddDays(Now, 8)                                          // Ends 8 days from now
  );
  
  // Perform interval checks
  WriteLn('Now is within interval1: ',
    TDateTimeKit.IsWithinInterval(Now, Interval1));                       // Check if date is in interval
  WriteLn('Intervals overlap: ',
    TDateTimeKit.IntervalsOverlap(Interval1, Interval2));                // Check if intervals overlap
  WriteLn('Intervals align: ',
    TDateTimeKit.IntervalAlign(Interval1, Interval2));                   // Check if intervals are adjacent
end;

procedure DemonstratePeriodBoundaries;
var
  Now, StartDate, EndDate: TDateTime;
begin
  WriteLn('=== Period Boundaries ===');
  Now := TDateTimeKit.GetNow;
  
  // Get start of various time periods
  StartDate := TDateTimeKit.StartOfYear(Now);        // First moment of the year
  WriteLn('Start of year: ', TDateTimeKit.GetAsString(StartDate));
  
  StartDate := TDateTimeKit.StartOfMonth(Now);       // First moment of the month
  WriteLn('Start of month: ', TDateTimeKit.GetAsString(StartDate));
  
  StartDate := TDateTimeKit.StartOfWeek(Now);        // First moment of the week (Sunday)
  WriteLn('Start of week: ', TDateTimeKit.GetAsString(StartDate));
  
  StartDate := TDateTimeKit.StartOfDay(Now);         // Midnight (00:00:00)
  WriteLn('Start of day: ', TDateTimeKit.GetAsString(StartDate));
  
  // Get end of various time periods
  EndDate := TDateTimeKit.EndOfYear(Now);           // Last moment of the year
  WriteLn('End of year: ', TDateTimeKit.GetAsString(EndDate));
  
  EndDate := TDateTimeKit.EndOfMonth(Now);          // Last moment of the month
  WriteLn('End of month: ', TDateTimeKit.GetAsString(EndDate));
  
  EndDate := TDateTimeKit.EndOfWeek(Now);           // Last moment of the week (Saturday)
  WriteLn('End of week: ', TDateTimeKit.GetAsString(EndDate));
  
  EndDate := TDateTimeKit.EndOfDay(Now);            // Last moment of day (23:59:59.999)
  WriteLn('End of day: ', TDateTimeKit.GetAsString(EndDate));
end;

procedure DemonstrateTimezoneOperations;
var
  Now: TDateTime;
  TZInfo: TTimeZoneInfo;     // Contains timezone details (name, offset, DST status)
  SystemTZ: string;
  TZNames: TStringArray;
  UTC, Local: TDateTime;
  I: Integer;
begin
  WriteLn('=== Timezone Operations ===');
  Now := TDateTimeKit.GetNow;
  
  // Get detailed timezone information
  TZInfo := TDateTimeKit.GetTimeZone(Now);
  WriteLn('Current timezone:');
  WriteLn('  Name: ', TZInfo.Name);
  WriteLn('  Offset (minutes): ', TZInfo.Offset);    // Minutes from UTC
  WriteLn('  Is DST: ', TZInfo.IsDST);              // Daylight Saving Time status
  
  // Get system timezone name
  SystemTZ := TDateTimeKit.GetSystemTimeZone;
  WriteLn('System timezone: ', SystemTZ);
  
  // List all available timezone names
  TZNames := TDateTimeKit.GetTimeZoneNames;
  WriteLn('Available timezones:');
  for I := Low(TZNames) to High(TZNames) do
    WriteLn('  ', TZNames[I]);
  
  // Convert between timezones
  UTC := TDateTimeKit.WithTimeZone(Now, 'UTC');      // Convert to UTC
  WriteLn('UTC time: ', TDateTimeKit.GetAsString(UTC));
  
  Local := TDateTimeKit.WithTimeZone(UTC, SystemTZ); // Convert back to local time
  WriteLn('Back to local: ', TDateTimeKit.GetAsString(Local));
end;

procedure DemonstrateDateComparisons;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('=== Date Comparisons ===');
  Date1 := TDateTimeKit.GetNow;
  Date2 := TDateTimeKit.AddDays(Date1, 1);  // One day later
  
  // Compare dates in various ways
  WriteLn('Date1 is before Date2: ',
    TDateTimeKit.IsBefore(Date1, Date2));            // True if Date1 < Date2
  WriteLn('Date2 is after Date1: ',
    TDateTimeKit.IsAfter(Date2, Date1));             // True if Date2 > Date1
  WriteLn('Same day: ',
    TDateTimeKit.IsSameDay(Date1, Date2));           // True if same calendar day
  WriteLn('Same month: ',
    TDateTimeKit.IsSameMonth(Date1, Date2));         // True if same calendar month
  WriteLn('Same year: ',
    TDateTimeKit.IsSameYear(Date1, Date2));          // True if same calendar year
  
  // Check time of day
  WriteLn('Date1 is AM: ', TDateTimeKit.IsAM(Date1)); // Before noon
  WriteLn('Date1 is PM: ', TDateTimeKit.IsPM(Date1)); // After noon
end;

procedure DemonstrateDateRounding;
var
  Now, Rounded: TDateTime;
begin
  WriteLn('=== Date Rounding ===');
  Now := TDateTimeKit.GetNow;
  WriteLn('Original: ', TDateTimeKit.GetAsString(Now));
  
  // Round to nearest time unit
  Rounded := TDateTimeKit.RoundDate(Now, TDateUnit.duHour);    // Round to nearest hour
  WriteLn('Rounded to hour: ', TDateTimeKit.GetAsString(Rounded));
  
  Rounded := TDateTimeKit.RoundDate(Now, TDateUnit.duDay);     // Round to nearest day
  WriteLn('Rounded to day: ', TDateTimeKit.GetAsString(Rounded));
  
  Rounded := TDateTimeKit.RoundDate(Now, TDateUnit.duMonth);   // Round to nearest month
  WriteLn('Rounded to month: ', TDateTimeKit.GetAsString(Rounded));
  
  // Floor (round down) to time unit
  Rounded := TDateTimeKit.FloorDate(Now, TDateUnit.duHour);    // Beginning of current hour
  WriteLn('Floor to hour: ', TDateTimeKit.GetAsString(Rounded));
  
  // Ceiling (round up) to time unit
  Rounded := TDateTimeKit.CeilingDate(Now, TDateUnit.duHour);  // Beginning of next hour
  WriteLn('Ceiling to hour: ', TDateTimeKit.GetAsString(Rounded));
end;

procedure DemonstrateSpecialOperations;
var
  Now, PrevMonth, NextMonth: TDateTime;
  DecimalDate: Double;
  DateFromDec: TDateTime;
begin
  WriteLn('=== Special Operations ===');
  Now := TDateTimeKit.GetNow;
  
  // Month boundary operations
  PrevMonth := TDateTimeKit.RollbackMonth(Now);      // Last day of previous month
  WriteLn('Last day of previous month: ',
    TDateTimeKit.GetAsString(PrevMonth));
  
  NextMonth := TDateTimeKit.RollForwardMonth(Now);   // First day of next month
  WriteLn('First day of next month: ',
    TDateTimeKit.GetAsString(NextMonth));
  
  // Convert between decimal years and dates
  DecimalDate := TDateTimeKit.GetDecimalDate(Now);   // Convert to decimal year (e.g., 2024.45)
  WriteLn('As decimal year: ', DecimalDate:0:4);
  
  DateFromDec := TDateTimeKit.DateDecimal(DecimalDate); // Convert back to date
  WriteLn('Back from decimal: ',
    TDateTimeKit.GetAsString(DateFromDec));
end;

begin
  try
    // Run all demonstrations
    DemonstrateBasicOperations;
    WriteLn;
    DemonstrateComponentAccess;
    WriteLn;
    DemonstrateComponentModification;
    WriteLn;
    DemonstrateDateArithmetic;
    WriteLn;
    DemonstratePeriodOperations;
    WriteLn;
    DemonstrateIntervalOperations;
    WriteLn;
    DemonstratePeriodBoundaries;
    WriteLn;
    DemonstrateTimezoneOperations;
    WriteLn;
    DemonstrateDateComparisons;
    WriteLn;
    DemonstrateDateRounding;
    WriteLn;
    DemonstrateSpecialOperations;
    
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
    end;
  end;
end. 
