unit TidyKit.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Exception class for TidyKit operations }
  ETidyKitException = class(Exception);

  { Base class for all TidyKit operations }
  TKitBase = class
  private
    FLastError: string;
  protected
    procedure SetError(const AError: string);
    function GetLastError: string;
  public
    constructor Create;
    property LastError: string read GetLastError;
  end;

implementation

{ TKitBase }

constructor TKitBase.Create;
begin
  inherited Create;
  FLastError := '';
end;

procedure TKitBase.SetError(const AError: string);
begin
  FLastError := AError;
end;

function TKitBase.GetLastError: string;
begin
  Result := FLastError;
end;

end. 