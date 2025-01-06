unit TidyKit.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Base interface for all chainable operations }
  IChainable = interface
    ['{D8F9E1A0-B2A1-4A5E-9C1F-8B5E8B9E1A0D}']
  end;

  { Exception class for TidyKit operations }
  ETidyKitException = class(Exception);

  { Base class for all TidyKit operations }
  TKitBase = class(TInterfacedObject, IChainable)
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