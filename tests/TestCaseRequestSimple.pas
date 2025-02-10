unit TestCaseRequestSimple;

{$mode objfpc}{$H+}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson,
  TidyKit.Core, TidyKit.Request.Simple;

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
begin
  Response := Http.Post('https://httpbin.org/post', 'test=value');
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Form data should exist in response',
    Assigned(Response.JSON.FindPath('form')) and
    (Response.JSON.FindPath('form.test').AsString = 'value'));
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
  Request: TRequestBuilder;  // Initialize is called automatically
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/headers')
    .AddHeader('X-Custom-Header', 'test')
    .AddHeader('User-Agent', 'TidyKit-Test')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Custom header should be echoed back',
    Response.JSON.FindPath('headers.X-Custom-Header').AsString = 'test');
end;

procedure TRequestSimpleTests.Test06_BuilderWithParams;
var
  Response: TResponse;
  Request: TRequestBuilder;  // Initialize is called automatically
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/get')
    .AddParam('page', '1')
    .AddParam('limit', '10')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Args should exist in response', Assigned(Response.JSON.FindPath('args')));
  AssertTrue('Page param should be present',
    Response.JSON.FindPath('args.page').AsString = '1');
  AssertTrue('Limit param should be present',
    Response.JSON.FindPath('args.limit').AsString = '10');
end;

procedure TRequestSimpleTests.Test07_BuilderWithTimeout;
var
  Response: TResponse;
  Request: TRequestBuilder;  // Initialize is called automatically
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
    on E: ETidyKitException do
      ExceptionRaised := True;
  end;
  
  AssertTrue('Timeout exception should be raised', ExceptionRaised);
end;

procedure TRequestSimpleTests.Test08_BuilderWithAuth;
var
  Response: TResponse;
  Request: TRequestBuilder;  // Initialize is called automatically
begin
  Response := Request
    .Get
    .URL('https://httpbin.org/headers')
    .BasicAuth('username', 'password')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Authorization header should exist',
    Assigned(Response.JSON.FindPath('headers.Authorization')));
end;

procedure TRequestSimpleTests.Test09_JSONRequest;
var
  Response: TResponse;
begin
  Response := Http.PostJSON('https://httpbin.org/post',
    '{"name": "John", "age": 30}');
    
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('JSON data should exist in response',
    Response.JSON.FindPath('json.name').AsString = 'John');
  AssertTrue('JSON data should exist in response',
    Response.JSON.FindPath('json.age').AsInteger = 30);
end;

procedure TRequestSimpleTests.Test10_FormDataRequest;
var
  Response: TResponse;
  Request: TRequestBuilder;  // Initialize is called automatically
begin
  Response := Request
    .Post
    .URL('https://httpbin.org/post')
    .AddHeader('Content-Type', 'application/x-www-form-urlencoded')
    .WithData('field1=value1&field2=value2')
    .Send;
  
  AssertEquals('Status code should be 200', 200, Response.StatusCode);
  AssertTrue('Response should be valid JSON', Assigned(Response.JSON));
  AssertTrue('Form field1 should be present',
    Response.JSON.FindPath('form.field1').AsString = 'value1');
  AssertTrue('Form field2 should be present',
    Response.JSON.FindPath('form.field2').AsString = 'value2');
end;

procedure TRequestSimpleTests.Test11_TryGetSuccess;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('https://httpbin.org/get');
  AssertTrue('Request should succeed', Result.Success);
  AssertEquals('Status code should be 200', 200, Result.Response.StatusCode);
  AssertEquals('Error should be empty', '', Result.Error);
end;

procedure TRequestSimpleTests.Test12_TryGetFailure;
var
  Result: TRequestResult;
begin
  Result := Http.TryGet('invalid://url');
  AssertFalse('Request should fail', Result.Success);
  AssertTrue('Error should not be empty', Result.Error <> '');
end;

initialization
  RegisterTest(TRequestSimpleTests);
end. 
