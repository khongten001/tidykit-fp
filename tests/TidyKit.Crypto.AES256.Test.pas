unit TidyKit.Crypto.AES256.Test;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit.Crypto.AES256;

type
  { TAESCipherTest }
  TAESCipherTest = class(TTestCase)
  published
    procedure TestCBCEncryption;
    procedure TestCBCDecryption;
    procedure TestCTREncryption;
    procedure TestCTRDecryption;
    procedure TestEmptyInput;
    procedure TestInvalidPadding;
  private
    function HexToBytes(const HexStr: string): TBytes;
    function BytesToHex(const Bytes: TBytes): string;
    function BytesToKey(const Bytes: TBytes): TAESKey;
    function BytesToBlock(const Bytes: TBytes): TAESBlock;
  end;

implementation

function TAESCipherTest.HexToBytes(const HexStr: string): TBytes;
var
  I: Integer;
begin
  if Length(HexStr) mod 2 <> 0 then
    raise Exception.Create('Invalid hex string length');
    
  SetLength(Result, Length(HexStr) div 2);
  for I := 0 to Length(Result) - 1 do
    Result[I] := StrToInt('$' + Copy(HexStr, I * 2 + 1, 2));
end;

function TAESCipherTest.BytesToHex(const Bytes: TBytes): string;
var
  I: Integer;
  Hex: string;
begin
  Result := '';
  for I := 0 to Length(Bytes) - 1 do
  begin
    Hex := IntToHex(Bytes[I], 2);
    Result := Result + Hex;
  end;
end;

function TAESCipherTest.BytesToKey(const Bytes: TBytes): TAESKey;
begin
  if Length(Bytes) <> 32 then
    raise EAESError.Create('Invalid key length');
  Move(Bytes[0], Result[0], 32);
end;

function TAESCipherTest.BytesToBlock(const Bytes: TBytes): TAESBlock;
begin
  if Length(Bytes) <> 16 then
    raise EAESError.Create('Invalid block length');
  Move(Bytes[0], Result[0], 16);
end;

procedure TAESCipherTest.TestCBCEncryption;
const
  // NIST SP 800-38A CBC-AES256 Test Vector
  Key: string = '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
  IV: string = '000102030405060708090a0b0c0d0e0f';
  Plaintext: string = '6bc1bee22e409f96e93d7e117393172a' +
                      'ae2d8a571e03ac9c9eb76fac45af8e51' +
                      '30c81c46a35ce411e5fbc1191a0a52ef' +
                      'f69f2445df4f9b17ad2b417be66c3710';
  ExpectedCiphertext: string = 'f58c4c04d6e5f1ba779eabfb5f7bfbd6' +
                              '9cfc4e967edb808d679f777bc6702c7d' +
                              '39f23369a9d9bacfa530e26304231461' +
                              'b2eb05e2c39be9fcda6c19078c6a9d1b';
var
  KeyBytes, IVBytes, PlainBytes, CipherBytes: TBytes;
begin
  KeyBytes := HexToBytes(Key);
  IVBytes := HexToBytes(IV);
  PlainBytes := HexToBytes(Plaintext);
  
  CipherBytes := TAES256.EncryptCBC(PlainBytes, BytesToKey(KeyBytes), BytesToBlock(IVBytes));
  AssertEquals('CBC encryption failed', ExpectedCiphertext.ToLower, 
               BytesToHex(CipherBytes).ToLower);
end;

procedure TAESCipherTest.TestCBCDecryption;
const
  // NIST SP 800-38A CBC-AES256 Test Vector
  Key: string = '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
  IV: string = '000102030405060708090a0b0c0d0e0f';
  Ciphertext: string = 'f58c4c04d6e5f1ba779eabfb5f7bfbd6' +
                       '9cfc4e967edb808d679f777bc6702c7d' +
                       '39f23369a9d9bacfa530e26304231461' +
                       'b2eb05e2c39be9fcda6c19078c6a9d1b';
  ExpectedPlaintext: string = '6bc1bee22e409f96e93d7e117393172a' +
                             'ae2d8a571e03ac9c9eb76fac45af8e51' +
                             '30c81c46a35ce411e5fbc1191a0a52ef' +
                             'f69f2445df4f9b17ad2b417be66c3710';
var
  KeyBytes, IVBytes, CipherBytes, PlainBytes: TBytes;
begin
  KeyBytes := HexToBytes(Key);
  IVBytes := HexToBytes(IV);
  CipherBytes := HexToBytes(Ciphertext);
  
  PlainBytes := TAES256.DecryptCBC(CipherBytes, BytesToKey(KeyBytes), BytesToBlock(IVBytes));
  AssertEquals('CBC decryption failed', ExpectedPlaintext.ToLower, 
               BytesToHex(PlainBytes).ToLower);
end;

procedure TAESCipherTest.TestCTREncryption;
const
  // NIST SP 800-38A CTR-AES256 Test Vector
  Key: string = '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
  IV: string = 'f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff';
  Plaintext: string = '6bc1bee22e409f96e93d7e117393172a' +
                      'ae2d8a571e03ac9c9eb76fac45af8e51' +
                      '30c81c46a35ce411e5fbc1191a0a52ef' +
                      'f69f2445df4f9b17ad2b417be66c3710';
  ExpectedCiphertext: string = '601ec313775789a5b7a7f504bbf3d228' +
                              'f443e3ca4d62b59aca84e990cacaf5c5' +
                              '2b0930daa23de94ce87017ba2d84988d' +
                              'dfc9c58db67aada613c2dd08457941a6';
var
  KeyBytes, IVBytes, PlainBytes, CipherBytes: TBytes;
begin
  KeyBytes := HexToBytes(Key);
  IVBytes := HexToBytes(IV);
  PlainBytes := HexToBytes(Plaintext);
  
  CipherBytes := TAES256.EncryptCTR(PlainBytes, BytesToKey(KeyBytes), BytesToBlock(IVBytes));
  AssertEquals('CTR encryption failed', ExpectedCiphertext.ToLower, 
               BytesToHex(CipherBytes).ToLower);
end;

procedure TAESCipherTest.TestCTRDecryption;
const
  // NIST SP 800-38A CTR-AES256 Test Vector
  Key: string = '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
  IV: string = 'f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff';
  Ciphertext: string = '601ec313775789a5b7a7f504bbf3d228' +
                       'f443e3ca4d62b59aca84e990cacaf5c5' +
                       '2b0930daa23de94ce87017ba2d84988d' +
                       'dfc9c58db67aada613c2dd08457941a6';
  ExpectedPlaintext: string = '6bc1bee22e409f96e93d7e117393172a' +
                             'ae2d8a571e03ac9c9eb76fac45af8e51' +
                             '30c81c46a35ce411e5fbc1191a0a52ef' +
                             'f69f2445df4f9b17ad2b417be66c3710';
var
  KeyBytes, IVBytes, CipherBytes, PlainBytes: TBytes;
begin
  KeyBytes := HexToBytes(Key);
  IVBytes := HexToBytes(IV);
  CipherBytes := HexToBytes(Ciphertext);
  
  PlainBytes := TAES256.DecryptCTR(CipherBytes, BytesToKey(KeyBytes), BytesToBlock(IVBytes));
  AssertEquals('CTR decryption failed', ExpectedPlaintext.ToLower, 
               BytesToHex(PlainBytes).ToLower);
end;

procedure TAESCipherTest.TestEmptyInput;
var
  Key: TAESKey;
  IV: TAESBlock;
begin
  FillChar(Key, SizeOf(Key), 0);
  FillChar(IV, SizeOf(IV), 0);
  
  AssertEquals('Empty input encryption failed', 0, Length(TAES256.EncryptCBC(nil, Key, IV)));
  AssertEquals('Empty input decryption failed', 0, Length(TAES256.DecryptCBC(nil, Key, IV)));
end;

procedure TAESCipherTest.TestInvalidPadding;
var
  Key: TAESKey;
  IV: TAESBlock;
  InvalidData: TBytes;
begin
  FillChar(Key, SizeOf(Key), 0);
  FillChar(IV, SizeOf(IV), 0);
  SetLength(InvalidData, 32);
  
  try
    TAES256.DecryptCBC(InvalidData, Key, IV);
    Fail('Expected EAESError for invalid padding');
  except
    on E: EAESError do
      ; // Expected exception
  end;
end;

initialization
  RegisterTest(TAESCipherTest);
end. 