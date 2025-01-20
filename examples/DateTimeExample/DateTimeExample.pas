program DateTimeExample;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  TidyKit;

procedure DemonstrateBasicOperations;
var
  Now, Today: TDateTime;
  FormattedDate: string;
begin
  WriteLn('=== Basic Operations ===');
  
  // Get current date/time
  Now := TDateTimeKit.GetNow;
  Today := TDateTimeKit.GetToday;
  
  // Format dates
  FormattedDate := TDateTimeKit.GetAsString(Now, 'yyyy-mm-dd hh:nn:ss');
  WriteLn('Current time: ', FormattedDate);
  WriteLn('Today at midnight: ', TDateTimeKit.GetAsString(Today));
  
  // Parse dates
  Now := TDateTimeKit.FromString('2024-03-15 14:30:00', 'yyyy-mm-dd hh:nn:ss');
  WriteLn('Parsed date: ', TDateTimeKit.GetAsString(Now));
  
  // Different format parsing
  WriteLn('YMD: ', TDateTimeKit.GetAsString(TDateTimeKit.YMD('2024-03-15')));
  WriteLn('MDY: ', TDateTimeKit.GetAsString(TDateTimeKit.MDY('03-15-2024')));
  WriteLn('DMY: ', TDateTimeKit.GetAsString(TDateTimeKit.DMY('15-03-2024')));
  WriteLn('YQ: ', TDateTimeKit.GetAsString(TDateTimeKit.YQ('2024-1')));
end;

procedure DemonstrateComponentAccess;
var
  Now: TDateTime;
begin
  WriteLn('=== Component Access ===');
  Now := TDateTimeKit.GetNow;
  
  // Basic components
  WriteLn('Year: ', TDateTimeKit.GetYear(Now));
  WriteLn('Month: ', TDateTimeKit.GetMonth(Now));
  WriteLn('Day: ', TDateTimeKit.GetDay(Now));
  WriteLn('Hour: ', TDateTimeKit.GetHour(Now));
  WriteLn('Minute: ', TDateTimeKit.GetMinute(Now));
  WriteLn('Second: ', TDateTimeKit.GetSecond(Now));
  WriteLn('Millisecond: ', TDateTimeKit.GetMillisecond(Now));
  
  // Additional components
  WriteLn('Day of Week: ', TDateTimeKit.GetDayOfWeek(Now));  // 1=Sunday
  WriteLn('Day of Year: ', TDateTimeKit.GetDayOfYear(Now));
  WriteLn('Quarter: ', TDateTimeKit.GetQuarter(Now));
  WriteLn('Semester: ', TDateTimeKit.GetSemester(Now));
  
  // ISO calendar
  WriteLn('ISO Year: ', TDateTimeKit.GetISOYear(Now));
  WriteLn('ISO Week: ', TDateTimeKit.GetISOWeek(Now));
  
  // Epidemiological calendar
  WriteLn('Epi Year: ', TDateTimeKit.GetEpiYear(Now));
  WriteLn('Epi Week: ', TDateTimeKit.GetEpiWeek(Now));
end;

procedure DemonstrateComponentModification;
var
  Original, Modified: TDateTime;
begin
  WriteLn('=== Component Modification ===');
  Original := TDateTimeKit.GetNow;
  WriteLn('Original: ', TDateTimeKit.GetAsString(Original));
  
  // Modify components
  Modified := Original;
  Modified := TDateTimeKit.SetYear(Modified, 2025);
  Modified := TDateTimeKit.SetMonth(Modified, 6);
  Modified := TDateTimeKit.SetDay(Modified, 15);
  Modified := TDateTimeKit.SetHour(Modified, 14);
  Modified := TDateTimeKit.SetMinute(Modified, 30);
  Modified := TDateTimeKit.SetSecond(Modified, 45);
  Modified := TDateTimeKit.SetMilliSecond(Modified, 500);
  
  WriteLn('Modified: ', TDateTimeKit.GetAsString(Modified));
end;

procedure DemonstrateDateArithmetic;
var
  Now, Future: TDateTime;
begin
  WriteLn('=== Date Arithmetic ===');
  Now := TDateTimeKit.GetNow;
  
  // Add/subtract time units
  Future := Now;
  Future := TDateTimeKit.AddYears(Future, 1);
  Future := TDateTimeKit.AddMonths(Future, -2);
  Future := TDateTimeKit.AddDays(Future, 7);
  Future := TDateTimeKit.AddHours(Future, 12);
  Future := TDateTimeKit.AddMinutes(Future, 30);
  Future := TDateTimeKit.AddSeconds(Future, -15);
  
  WriteLn('Now: ', TDateTimeKit.GetAsString(Now));
  WriteLn('Future: ', TDateTimeKit.GetAsString(Future));
  
  // Business day operations
  WriteLn('Next business day: ', TDateTimeKit.GetAsString(TDateTimeKit.NextBusinessDay(Now)));
  WriteLn('Previous business day: ', TDateTimeKit.GetAsString(TDateTimeKit.PreviousBusinessDay(Now)));
  WriteLn('5 business days later: ', TDateTimeKit.GetAsString(TDateTimeKit.AddBusinessDays(Now, 5)));
end;

procedure DemonstratePeriodOperations;
var
  Now: TDateTime;
  Period: TDateSpan;
  Duration: TDateSpan;
  Future: TDateTime;
  Span: TDateSpan;
  Seconds: Int64;
begin
  WriteLn('=== Period Operations ===');
  Now := TDateTimeKit.GetNow;
  
  // Create periods and durations
  Period := TDateTimeKit.CreatePeriod(1, 2, 3);  // 1 year, 2 months, 3 days
  Duration := TDateTimeKit.CreateDuration(0, 0, 1);  // 1 day
  
  // Add periods
  Future := TDateTimeKit.AddSpan(Now, Period);
  WriteLn('After adding period: ', TDateTimeKit.GetAsString(Future));
  
  // Subtract periods
  Future := TDateTimeKit.SubtractSpan(Future, Duration);
  WriteLn('After subtracting duration: ', TDateTimeKit.GetAsString(Future));
  
  // Calculate span between dates
  Span := TDateTimeKit.SpanBetween(Now, Future, TDateSpanKind.dskPeriod);
  WriteLn('Period between dates:');
  WriteLn('  Years: ', Span.Years);
  WriteLn('  Months: ', Span.Months);
  WriteLn('  Days: ', Span.Days);
  
  // Convert periods
  Seconds := TDateTimeKit.PeriodToSeconds(Period);
  WriteLn('Period in seconds: ', Seconds);
  
  Period := TDateTimeKit.SecondsToPeriod(Seconds);
  Period := TDateTimeKit.StandardizePeriod(Period);
  WriteLn('Standardized period:');
  WriteLn('  Years: ', Period.Years);
  WriteLn('  Months: ', Period.Months);
  WriteLn('  Days: ', Period.Days);
end;

procedure DemonstrateIntervalOperations;
var
  Now, Later: TDateTime;
  Interval1, Interval2: TInterval;
begin
  WriteLn('=== Interval Operations ===');
  Now := TDateTimeKit.GetNow;
  Later := TDateTimeKit.AddDays(Now, 5);
  
  // Create intervals
  Interval1 := TDateTimeKit.CreateInterval(Now, Later);
  Interval2 := TDateTimeKit.CreateInterval(
    TDateTimeKit.AddDays(Now, 3),
    TDateTimeKit.AddDays(Now, 8)
  );
  
  // Check interval operations
  WriteLn('Now is within interval1: ',
    TDateTimeKit.IsWithinInterval(Now, Interval1));
  WriteLn('Intervals overlap: ',
    TDateTimeKit.IntervalsOverlap(Interval1, Interval2));
  WriteLn('Intervals align: ',
    TDateTimeKit.IntervalAlign(Interval1, Interval2));
end;

procedure DemonstratePeriodBoundaries;
var
  Now, StartDate, EndDate: TDateTime;
begin
  WriteLn('=== Period Boundaries ===');
  Now := TDateTimeKit.GetNow;
  
  // Start of periods
  StartDate := TDateTimeKit.StartOfYear(Now);
  WriteLn('Start of year: ', TDateTimeKit.GetAsString(StartDate));
  
  StartDate := TDateTimeKit.StartOfMonth(Now);
  WriteLn('Start of month: ', TDateTimeKit.GetAsString(StartDate));
  
  StartDate := TDateTimeKit.StartOfWeek(Now);
  WriteLn('Start of week: ', TDateTimeKit.GetAsString(StartDate));
  
  StartDate := TDateTimeKit.StartOfDay(Now);
  WriteLn('Start of day: ', TDateTimeKit.GetAsString(StartDate));
  
  // End of periods
  EndDate := TDateTimeKit.EndOfYear(Now);
  WriteLn('End of year: ', TDateTimeKit.GetAsString(EndDate));
  
  EndDate := TDateTimeKit.EndOfMonth(Now);
  WriteLn('End of month: ', TDateTimeKit.GetAsString(EndDate));
  
  EndDate := TDateTimeKit.EndOfWeek(Now);
  WriteLn('End of week: ', TDateTimeKit.GetAsString(EndDate));
  
  EndDate := TDateTimeKit.EndOfDay(Now);
  WriteLn('End of day: ', TDateTimeKit.GetAsString(EndDate));
end;

procedure DemonstrateTimezoneOperations;
var
  Now: TDateTime;
  TZInfo: TTimeZoneInfo;
  SystemTZ: string;
  TZNames: TStringArray;
  UTC, Local: TDateTime;
  I: Integer;
begin
  WriteLn('=== Timezone Operations ===');
  Now := TDateTimeKit.GetNow;
  
  // Get timezone information
  TZInfo := TDateTimeKit.GetTimeZone(Now);
  WriteLn('Current timezone:');
  WriteLn('  Name: ', TZInfo.Name);
  WriteLn('  Offset (minutes): ', TZInfo.Offset);
  WriteLn('  Is DST: ', TZInfo.IsDST);
  
  // System timezone
  SystemTZ := TDateTimeKit.GetSystemTimeZone;
  WriteLn('System timezone: ', SystemTZ);
  
  // Available timezones
  TZNames := TDateTimeKit.GetTimeZoneNames;
  WriteLn('Available timezones:');
  for I := Low(TZNames) to High(TZNames) do
    WriteLn('  ', TZNames[I]);
  
  // Convert between timezones
  UTC := TDateTimeKit.WithTimeZone(Now, 'UTC');
  WriteLn('UTC time: ', TDateTimeKit.GetAsString(UTC));
  
  Local := TDateTimeKit.WithTimeZone(UTC, SystemTZ);
  WriteLn('Back to local: ', TDateTimeKit.GetAsString(Local));
end;

procedure DemonstrateDateComparisons;
var
  Date1, Date2: TDateTime;
begin
  WriteLn('=== Date Comparisons ===');
  Date1 := TDateTimeKit.GetNow;
  Date2 := TDateTimeKit.AddDays(Date1, 1);
  
  WriteLn('Date1 is before Date2: ',
    TDateTimeKit.IsBefore(Date1, Date2));
  WriteLn('Date2 is after Date1: ',
    TDateTimeKit.IsAfter(Date2, Date1));
  WriteLn('Same day: ',
    TDateTimeKit.IsSameDay(Date1, Date2));
  WriteLn('Same month: ',
    TDateTimeKit.IsSameMonth(Date1, Date2));
  WriteLn('Same year: ',
    TDateTimeKit.IsSameYear(Date1, Date2));
  
  WriteLn('Date1 is AM: ', TDateTimeKit.IsAM(Date1));
  WriteLn('Date1 is PM: ', TDateTimeKit.IsPM(Date1));
end;

procedure DemonstrateDateRounding;
var
  Now, Rounded: TDateTime;
begin
  WriteLn('=== Date Rounding ===');
  Now := TDateTimeKit.GetNow;
  WriteLn('Original: ', TDateTimeKit.GetAsString(Now));
  
  // Round to various units
  Rounded := TDateTimeKit.RoundDate(Now, TDateUnit.duHour);
  WriteLn('Rounded to hour: ', TDateTimeKit.GetAsString(Rounded));
  
  Rounded := TDateTimeKit.RoundDate(Now, TDateUnit.duDay);
  WriteLn('Rounded to day: ', TDateTimeKit.GetAsString(Rounded));
  
  Rounded := TDateTimeKit.RoundDate(Now, TDateUnit.duMonth);
  WriteLn('Rounded to month: ', TDateTimeKit.GetAsString(Rounded));
  
  // Floor to various units
  Rounded := TDateTimeKit.FloorDate(Now, TDateUnit.duHour);
  WriteLn('Floor to hour: ', TDateTimeKit.GetAsString(Rounded));
  
  // Ceiling to various units
  Rounded := TDateTimeKit.CeilingDate(Now, TDateUnit.duHour);
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
  
  // Month rolling
  PrevMonth := TDateTimeKit.RollbackMonth(Now);
  WriteLn('Last day of previous month: ',
    TDateTimeKit.GetAsString(PrevMonth));
  
  NextMonth := TDateTimeKit.RollForwardMonth(Now);
  WriteLn('First day of next month: ',
    TDateTimeKit.GetAsString(NextMonth));
  
  // Decimal dates
  DecimalDate := TDateTimeKit.GetDecimalDate(Now);
  WriteLn('As decimal year: ', DecimalDate:0:4);
  
  DateFromDec := TDateTimeKit.DateDecimal(DecimalDate);
  WriteLn('Back from decimal: ',
    TDateTimeKit.GetAsString(DateFromDec));
end;

begin
  try
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
