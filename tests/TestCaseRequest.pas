unit TestCaseRequest;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, TidyKit;

type
  { TRequestTests }
  TRequestTests = class(TTestCase)
  private
    FRequestKit: TRequestKit;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Basic HTTP methods
    procedure Test01_GetRequest;
    procedure Test02_PostRequest;
    procedure Test03_PutRequest;
    procedure Test04_DeleteRequest;
    
    // Request options
    procedure Test05_RequestWithHeaders;
    procedure Test06_RequestWithQueryParams;
    procedure Test07_RequestWithTimeout;
    
    // Content handling
    procedure Test08_JSONRequest;
    procedure Test09_FormDataRequest;
    procedure Test12_PostWithAuthAndJSON;
    
    // Error handling
    procedure Test10_InvalidURL;
    procedure Test11_NotFound;
    
    // Additional tests for robustness
    procedure Test13_QueryParamsAndJSONBody;
    procedure Test14_CustomHeadersAndAuth;
    procedure Test15_InvalidJSONBody;
    procedure Test16_RedirectHandling;
    procedure Test17_LargeJSONResponse;
    procedure Test18_EmptyResponse;
    procedure Test19_InvalidContentType;
    procedure Test20_ServerError;
  end;

implementation

procedure TRequestTests.SetUp;
begin
  FRequestKit := TRequestKit.Create;
end;

procedure TRequestTests.TearDown;
begin
  FRequestKit.Free;
end;

procedure TRequestTests.Test01_GetRequest;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Get('https://httpbin.org/get', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test02_PostRequest;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData, TestValue: TJSONData;
begin
  Options := Default(TRequestOptions);
  SetLength(Options.Headers, 1);
  Options.Headers[0] := 'Content-Type: application/x-www-form-urlencoded';
  Options.Data := 'test=value';
  Response := FRequestKit.Post('https://httpbin.org/post', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
    
    JsonData := Response.JSON.FindPath('form');
    AssertTrue('Form data should exist in response', JsonData <> nil);
    TestValue := JsonData.FindPath('test');
    AssertTrue('Test value should exist in form data', TestValue <> nil);
    AssertEquals('Form data should be echoed back', 'value', TestValue.AsString);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test03_PutRequest;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Options.Data := 'test=updated';
  Response := FRequestKit.Put('https://httpbin.org/put', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test04_DeleteRequest;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Delete('https://httpbin.org/delete', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test05_RequestWithHeaders;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  SetLength(Options.Headers, 2);
  Options.Headers[0] := 'X-Custom-Header: test';
  Options.Headers[1] := 'User-Agent: TidyKit-Test';
  Response := FRequestKit.Get('https://httpbin.org/headers', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    JsonData := Response.JSON.FindPath('headers');
    AssertTrue('Headers should exist in response', JsonData <> nil);
    AssertEquals('Custom header should be echoed back',
      'test', JsonData.FindPath('X-Custom-Header').AsString);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test06_RequestWithQueryParams;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  Options.ParamString := 'page=1&limit=10';
  Response := FRequestKit.Get('https://httpbin.org/get', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    JsonData := Response.JSON.FindPath('args');
    AssertTrue('Query params should exist in response', JsonData <> nil);
    AssertEquals('Page param should be present',
      '1', JsonData.FindPath('page').AsString);
    AssertEquals('Limit param should be present',
      '10', JsonData.FindPath('limit').AsString);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test07_RequestWithTimeout;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Options.Timeout := 1; // 1 millisecond
  Response := nil;
  try
    try
      Response := FRequestKit.Get('https://httpbin.org/delay/2', Options);
      Fail('Timeout should raise exception');
    except
      on E: ETidyKitException do
        AssertTrue('Timeout exception should be raised', True);
    end;
  finally
    if Assigned(Response) then
      Response.Free;
  end;
end;

procedure TRequestTests.Test08_JSONRequest;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  Options.JSON := '{"test": "value"}';
  Response := FRequestKit.Post('https://httpbin.org/post', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
    JsonData := Response.JSON.FindPath('json');
    AssertTrue('JSON data should exist in response', JsonData <> nil);
    AssertEquals('JSON should be echoed back',
      'value', JsonData.FindPath('test').AsString);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test09_FormDataRequest;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  SetLength(Options.Headers, 1);
  Options.Headers[0] := 'Content-Type: application/x-www-form-urlencoded';
  Options.Data := 'field1=value1&field2=value2';
  Response := FRequestKit.Post('https://httpbin.org/post', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    JsonData := Response.JSON.FindPath('form');
    AssertTrue('Form data should exist in response', JsonData <> nil);
    AssertEquals('Form field1 should be present',
      'value1', JsonData.FindPath('field1').AsString);
    AssertEquals('Form field2 should be present',
      'value2', JsonData.FindPath('field2').AsString);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test10_InvalidURL;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := nil;
  try
    try
      Response := FRequestKit.Get('invalid://url', Options);
      Fail('Invalid URL should raise exception');
    except
      on E: ETidyKitException do
        AssertTrue('Exception should be raised for invalid URL', True);
    end;
  finally
    if Assigned(Response) then
      Response.Free;
  end;
end;

procedure TRequestTests.Test11_NotFound;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Get('https://httpbin.org/status/404', Options);
  try
    AssertEquals('Status code should be 404', 404, Response.StatusCode);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test12_PostWithAuthAndJSON;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  
  // Set auth credentials
  Options.Auth[0] := 'username';
  Options.Auth[1] := 'password';
  
  // Set JSON body
  Options.JSON := '{"name": "John Doe", "age": 30}';
  
  Response := FRequestKit.Post('https://httpbin.org/post', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
    
    // Verify auth header was sent
    JsonData := Response.JSON.FindPath('headers');
    AssertTrue('Headers should exist in response', JsonData <> nil);
    AssertTrue('Authorization header should exist', 
      JsonData.FindPath('Authorization') <> nil);
    
    // Verify JSON body was sent correctly
    JsonData := Response.JSON.FindPath('json');
    AssertTrue('JSON data should exist in response', JsonData <> nil);
    AssertEquals('Name should match', 'John Doe', JsonData.FindPath('name').AsString);
    AssertEquals('Age should match', 30, JsonData.FindPath('age').AsInteger);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test13_QueryParamsAndJSONBody;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  Options.ParamString := 'version=1&format=json';
  Options.JSON := '{"action": "test", "data": 123}';
  
  Response := FRequestKit.Post('https://httpbin.org/post', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    
    // Verify query params
    JsonData := Response.JSON.FindPath('args');
    AssertTrue('Args should exist in response', JsonData <> nil);
    AssertEquals('Version param should match', '1', JsonData.FindPath('version').AsString);
    AssertEquals('Format param should match', 'json', JsonData.FindPath('format').AsString);
    
    // Verify JSON body
    JsonData := Response.JSON.FindPath('json');
    AssertTrue('JSON body should exist', JsonData <> nil);
    AssertEquals('Action should match', 'test', JsonData.FindPath('action').AsString);
    AssertEquals('Data should match', 123, JsonData.FindPath('data').AsInteger);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test14_CustomHeadersAndAuth;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
begin
  Options := Default(TRequestOptions);
  SetLength(Options.Headers, 2);
  Options.Headers[0] := 'X-API-Version: 2.0';
  Options.Headers[1] := 'X-Custom-Token: abc123';
  Options.Auth[0] := 'apiuser';
  Options.Auth[1] := 'apipass';
  
  Response := FRequestKit.Get('https://httpbin.org/headers', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    JsonData := Response.JSON.FindPath('headers');
    AssertTrue('Headers should exist in response', JsonData <> nil);
    
    // Verify custom headers
    AssertEquals('API version header should match',
      '2.0', JsonData.FindPath('X-Api-Version').AsString);
    AssertEquals('Custom token header should match',
      'abc123', JsonData.FindPath('X-Custom-Token').AsString);
    
    // Verify auth header exists
    AssertTrue('Authorization header should exist',
      JsonData.FindPath('Authorization') <> nil);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test15_InvalidJSONBody;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Options.JSON := '{"invalid": json}';  // Invalid JSON syntax
  Response := nil;
  
  try
    try
      Response := FRequestKit.Post('https://httpbin.org/post', Options);
      Fail('Invalid JSON should raise exception');
    except
      on E: ETidyKitException do
        AssertTrue('Exception should be raised for invalid JSON', True);
    end;
  finally
    if Assigned(Response) then
      Response.Free;
  end;
end;

procedure TRequestTests.Test16_RedirectHandling;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Get('https://httpbin.org/redirect/1', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response should be valid JSON', Response.JSON <> nil);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test17_LargeJSONResponse;
var
  Response: TResponse;
  Options: TRequestOptions;
  JsonData: TJSONData;
  I: Integer;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Get('https://httpbin.org/bytes/5000', Options);
  try
    AssertEquals('Status code should be 200', 200, Response.StatusCode);
    AssertTrue('Response content should not be empty', Response.Content.Size > 0);
    AssertTrue('Response size should be around 5000 bytes', 
      (Response.Content.Size >= 4900) and (Response.Content.Size <= 5100));
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test18_EmptyResponse;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Get('https://httpbin.org/status/204', Options);
  try
    AssertEquals('Status code should be 204', 204, Response.StatusCode);
    AssertEquals('Content should be empty', 0, Response.Content.Size);
  finally
    Response.Free;
  end;
end;

procedure TRequestTests.Test19_InvalidContentType;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  SetLength(Options.Headers, 1);
  Options.Headers[0] := 'Content-Type: invalid/content-type';
  Options.Data := 'test data';
  Response := nil;
  
  try
    try
      Response := FRequestKit.Post('https://httpbin.org/post', Options);
      // Should still work, just with unexpected content type
      AssertEquals('Status code should be 200', 200, Response.StatusCode);
    except
      on E: Exception do
        Fail('Should handle invalid content type gracefully');
    end;
  finally
    if Assigned(Response) then
      Response.Free;
  end;
end;

procedure TRequestTests.Test20_ServerError;
var
  Response: TResponse;
  Options: TRequestOptions;
begin
  Options := Default(TRequestOptions);
  Response := FRequestKit.Get('https://httpbin.org/status/500', Options);
  try
    AssertEquals('Status code should be 500', 500, Response.StatusCode);
  finally
    Response.Free;
  end;
end;

initialization
  RegisterTest(TRequestTests);
end. 
