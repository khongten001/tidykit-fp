unit TidyKit.Request.Test;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TidyKit.Request, TidyKit.JSON;

type
  { TRequestSimpleTests }
  TRequestSimpleTests = class(TTestCase)
  published
    // Basic HTTP methods
    procedure Test01_SimpleGet;
    procedure Test02_SimplePost;
    procedure Test03_SimplePut;
    procedure Test04_SimpleDelete;
    
    // Builder pattern tests
    procedure Test05_BuilderWithHeaders;
    procedure Test06_BuilderWithParams;
    procedure Test07_BuilderWithTimeout;
    procedure Test08_BuilderWithAuth;
    
    // Content handling
    procedure Test09_JSONRequest;
    procedure Test10_FormDataRequest;
    
    // Error handling
    procedure Test11_TryGetSuccess;
    procedure Test12_TryGetFailure;
  end;

implementation

procedure TRequestSimpleTests.Test01_SimpleGet;
var
  Response: TResponse;
begin
  Response := Http.Get('https://httpbin.org/get');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
end;

procedure TRequestSimpleTests.Test02_SimplePost;
var
  Response: TResponse;
  FormData: IJSONObject;
begin
  Response := Http.Post('https://httpbin.org/post', 'test=value');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  FormData := Response.JSON.AsObject['form'].AsObject;
  AssertTrue('Form data should exist in response', FormData.Contains('test'));
  AssertEquals('Form value should match', 'value', FormData['test'].AsString);
end;

procedure TRequestSimpleTests.Test03_SimplePut;
var
  Response: TResponse;
begin
  Response := Http.Put('https://httpbin.org/put', 'test=updated');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
end;

procedure TRequestSimpleTests.Test04_SimpleDelete;
var
  Response: TResponse;
begin
  Response := Http.Delete('https://httpbin.org/delete');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
end;

procedure TRequestSimpleTests.Test05_BuilderWithHeaders;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  Headers: IJSONObject;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/headers')
    .AddHeader('X-Custom-Header', 'test')
    .AddHeader('User-Agent', 'TidyKit-Test')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  Headers := Response.JSON.AsObject['headers'].AsObject;
  AssertTrue('Custom header should be echoed back',
    Headers['X-Custom-Header'].AsString = 'test');
end;

procedure TRequestSimpleTests.Test06_BuilderWithParams;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  Args: IJSONObject;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/get')
    .AddParam('page', '1')
    .AddParam('limit', '10')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  Args := Response.JSON.AsObject['args'].AsObject;
  AssertTrue('Args should exist in response', Assigned(Args));
  AssertEquals('Page param should be present', '1', Args['page'].AsString);
  AssertEquals('Limit param should be present', '10', Args['limit'].AsString);
end;

procedure TRequestSimpleTests.Test07_BuilderWithTimeout;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  
  try
    Response := Request
      .Get
      .URL('https://httpbin.org/delay/2')
      .WithTimeout(1)
      .Send;
  except
    on E: ERequestError do
      ExceptionRaised := True;
  end;
  
  AssertTrue('Timeout exception should be raised', ExceptionRaised);
end;

procedure TRequestSimpleTests.Test08_BuilderWithAuth;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  Headers: IJSONObject;
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/headers')
    .BasicAuth('username', 'password')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  Headers := Response.JSON.AsObject['headers'].AsObject;
  AssertTrue('Authorization header should exist',
    Headers.Contains('Authorization'));
end;

procedure TRequestSimpleTests.Test09_JSONRequest;
var
  Response: TResponse;
  JsonData: IJSONObject;
begin
  Response := Http.PostJSON('https://httpbin.org/post',
    '{"name": "John", "age": 30}');
    
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  JsonData := Response.JSON.AsObject['json'].AsObject;
  AssertEquals('JSON data should exist in response', 'John', JsonData['name'].AsString);
  AssertEquals('JSON data should exist in response', 30, JsonData['age'].AsInteger);
end;

procedure TRequestSimpleTests.Test10_FormDataRequest;
var
  Response: TResponse;
  Request: THttpRequest;  // Initialize is called automatically
  FormData: IJSONObject;
begin
  Response := Request
    .Post
    .URL('https://httpbin.org/post')
    .WithData('name=John&age=30')
    .Send;
    
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  
  FormData := Response.JSON.AsObject['form'].AsObject;
  AssertEquals('Form data should exist in response', 'John', FormData['name'].AsString);
  AssertEquals('Form data should exist in response', '30', FormData['age'].AsString);
end;

procedure TRequestSimpleTests.Test11_TryGetSuccess;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://httpbin.org/get');
  AssertTrue('Request should succeed', Result.Success);
  AssertEquals('Status code should be 200', 200, Result.Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Result.Response.JSON));
  AssertEquals('No error message expected', '', Result.Error);
end;

procedure TRequestSimpleTests.Test12_TryGetFailure;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://nonexistent.example.com');
  AssertFalse('Request should fail', Result.Success);
  AssertTrue('Error message should not be empty', Result.Error <> '');
end;

initialization
  RegisterTest(TRequestSimpleTests);
end. 
