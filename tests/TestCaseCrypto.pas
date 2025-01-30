unit TestCaseCrypto;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit,
  TidyKit.Crypto, TidyKit.Crypto.AES;

type
  { TTestCaseCrypto }
  TTestCaseCrypto = class(TTestCase)
  private
    FCryptoKit: TCryptoKit;
    FPlainText: string;
    FKey: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Hashing tests (01-19)
    procedure Test01_MD5Hash;
    procedure Test02_SHA1Hash;
    procedure Test04_HashWithEmptyString;
    procedure Test05_HashWithUnicodeString;

    // Base64 tests (20-39)
    procedure Test20_Base64Encode;
    procedure Test21_Base64Decode;
    procedure Test22_Base64RoundTrip;
    procedure Test23_Base64WithEmptyString;
    procedure Test24_Base64WithBinaryData;

    // XOR encryption tests (40-59)
    procedure Test40_XORCrypt;
    procedure Test41_XORCryptRoundTrip;
    procedure Test42_XORCryptWithEmptyString;
    procedure Test43_XORCryptWithEmptyKey;
    procedure Test44_XORCryptWithUnicodeString;

    // Blowfish tests (60-79)
    procedure Test60_BlowfishEncryption;
    procedure Test61_BlowfishDecryption;
    procedure Test62_BlowfishRoundTrip;
    procedure Test63_BlowfishWithEmptyString;
    procedure Test64_BlowfishWithEmptyKey;
    procedure Test65_BlowfishWithLongKey;
    procedure Test66_BlowfishWithUnicodeString;

    // AES tests (80-99)
    procedure Test80_AESECBEncryption;
    procedure Test81_AESECBDecryption;
    procedure Test82_AESECBRoundTrip;
    procedure Test83_AESCBCEncryption;
    procedure Test84_AESCBCDecryption;
    procedure Test85_AESCBCRoundTrip;
    procedure Test86_AESCTREncryption;
    procedure Test87_AESCTRDecryption;
    procedure Test88_AESCTRRoundTrip;
    procedure Test89_AESWithEmptyString;
    procedure Test90_AESWithEmptyKey;
    procedure Test91_AESWithLongKey;
    procedure Test92_AESWithUnicodeString;
    procedure Test93_AESWithKnownVector;
    procedure Test94_AESPaddingHandling;
    procedure Test95_AESIVHandling;
  end;

implementation

procedure TTestCaseCrypto.SetUp;
begin
  FCryptoKit := TCryptoKit.Create;
  FPlainText := 'Hello, TidyKit Crypto!';
  FKey := 'MySecretKey123456';
end;

procedure TTestCaseCrypto.TearDown;
begin
  FCryptoKit.Free;
end;

// Hashing tests (01-19)
procedure TTestCaseCrypto.Test01_MD5Hash;
begin
  AssertEquals('MD5 hash should match expected value',
    '9E107D9D372BB6826BD81D3542A419D6',
    UpperCase(TCryptoKit.MD5Hash('The quick brown fox jumps over the lazy dog')));
end;

procedure TTestCaseCrypto.Test02_SHA1Hash;
begin
  AssertEquals('SHA1 hash should match expected value',
    '2FD4E1C67A2D28FCED849EE1BB76E7391B93EB12',
    UpperCase(TCryptoKit.SHA1Hash('The quick brown fox jumps over the lazy dog')));
end;

procedure TTestCaseCrypto.Test04_HashWithEmptyString;
begin
  AssertEquals('MD5 of empty string',
    'D41D8CD98F00B204E9800998ECF8427E',
    UpperCase(TCryptoKit.MD5Hash('')));
  AssertEquals('SHA1 of empty string',
    'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709',
    UpperCase(TCryptoKit.SHA1Hash('')));
end;

procedure TTestCaseCrypto.Test05_HashWithUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('MD5 of Unicode string should not be empty',
    TCryptoKit.MD5Hash(UnicodeText) <> '');
  AssertTrue('SHA1 of Unicode string should not be empty',
    TCryptoKit.SHA1Hash(UnicodeText) <> '');
end;

// Base64 tests (20-39)
procedure TTestCaseCrypto.Test20_Base64Encode;
begin
  AssertEquals('Base64 encoding should match expected value',
    'SGVsbG8sIFdvcmxkIQ==',
    TCryptoKit.Base64Encode('Hello, World!'));
end;

procedure TTestCaseCrypto.Test21_Base64Decode;
begin
  AssertEquals('Base64 decoding should match expected value',
    'Hello, World!',
    TCryptoKit.Base64Decode('SGVsbG8sIFdvcmxkIQ=='));
end;

procedure TTestCaseCrypto.Test22_Base64RoundTrip;
begin
  AssertEquals('Base64 round trip should preserve data',
    FPlainText,
    TCryptoKit.Base64Decode(TCryptoKit.Base64Encode(FPlainText)));
end;

procedure TTestCaseCrypto.Test23_Base64WithEmptyString;
begin
  AssertEquals('Base64 encode empty string', '', TCryptoKit.Base64Encode(''));
  AssertEquals('Base64 decode empty string', '', TCryptoKit.Base64Decode(''));
end;

procedure TTestCaseCrypto.Test24_Base64WithBinaryData;
var
  BinaryData: array[0..3] of Byte;
  Encoded, Decoded: string;
begin
  BinaryData[0] := $00;
  BinaryData[1] := $FF;
  BinaryData[2] := $7F;
  BinaryData[3] := $80;
  
  SetString(Encoded, PChar(@BinaryData[0]), Length(BinaryData));
  Encoded := TCryptoKit.Base64Encode(Encoded);
  Decoded := TCryptoKit.Base64Decode(Encoded);
  
  AssertEquals('Binary data length after round trip',
    Length(BinaryData), Length(Decoded));
  AssertEquals('Binary data content after round trip',
    0, CompareByte(BinaryData[0], Decoded[1], Length(BinaryData)));
end;

// XOR encryption tests (40-59)
procedure TTestCaseCrypto.Test40_XORCrypt;
var
  Encrypted: string;
begin
  Encrypted := TCryptoKit.XORCrypt(FPlainText, FKey);
  AssertTrue('XOR encryption should change the data', FPlainText <> Encrypted);
end;

procedure TTestCaseCrypto.Test41_XORCryptRoundTrip;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.XORCrypt(FPlainText, FKey);
  Decrypted := TCryptoKit.XORCrypt(Encrypted, FKey);
  AssertEquals('XOR round trip should preserve data', FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test42_XORCryptWithEmptyString;
begin
  AssertEquals('XOR with empty string', '', TCryptoKit.XORCrypt('', FKey));
end;

procedure TTestCaseCrypto.Test43_XORCryptWithEmptyKey;
begin
  AssertEquals('XOR with empty key', '', TCryptoKit.XORCrypt(FPlainText, ''));
end;

procedure TTestCaseCrypto.Test44_XORCryptWithUnicodeString;
const
  UnicodeText = '你好，世界！';
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.XORCrypt(UnicodeText, FKey);
  Decrypted := TCryptoKit.XORCrypt(Encrypted, FKey);
  AssertEquals('XOR with Unicode string', UnicodeText, Decrypted);
end;

// Blowfish tests (60-79)
procedure TTestCaseCrypto.Test60_BlowfishEncryption;
var
  Encrypted: string;
begin
  Encrypted := TCryptoKit.BlowfishCrypt(FPlainText, FKey, bmEncrypt);
  AssertTrue('Blowfish encryption should change the data', FPlainText <> Encrypted);
  AssertTrue('Blowfish encryption should produce Base64 output',
    TCryptoKit.Base64Decode(Encrypted) <> '');
end;

procedure TTestCaseCrypto.Test61_BlowfishDecryption;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.BlowfishCrypt(FPlainText, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish decryption should restore original data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test62_BlowfishRoundTrip;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.BlowfishCrypt(FPlainText, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish round trip should preserve data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test63_BlowfishWithEmptyString;
begin
  AssertEquals('Blowfish with empty string',
    '', TCryptoKit.BlowfishCrypt('', FKey, bmEncrypt));
end;

procedure TTestCaseCrypto.Test64_BlowfishWithEmptyKey;
begin
  AssertEquals('Blowfish with empty key',
    '', TCryptoKit.BlowfishCrypt(FPlainText, '', bmEncrypt));
end;

procedure TTestCaseCrypto.Test65_BlowfishWithLongKey;
const
  LongKey = 'ThisIsAVeryLongKeyThatExceedsTheMaximumLengthForBlowfishKeys';
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.BlowfishCrypt(FPlainText, LongKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, LongKey, bmDecrypt);
  AssertEquals('Blowfish with long key should work',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test66_BlowfishWithUnicodeString;
const
  UnicodeText = '你好，世界！';
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.BlowfishCrypt(UnicodeText, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish with Unicode string',
    UnicodeText, Decrypted);
end;

// AES tests (80-99)
procedure TTestCaseCrypto.Test80_AESECBEncryption;
var
  Encrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amECB);
  AssertTrue('AES ECB encryption should change the data', FPlainText <> Encrypted);
  AssertTrue('AES ECB encryption should produce non-empty output', Encrypted <> '');
end;

procedure TTestCaseCrypto.Test81_AESECBDecryption;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amECB);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amECB);
  AssertEquals('AES ECB decryption should restore original data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test82_AESECBRoundTrip;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amECB);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amECB);
  AssertEquals('AES ECB round trip should preserve data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test83_AESCBCEncryption;
var
  Encrypted1, Encrypted2: string;
begin
  // Same plaintext encrypted twice should yield different results due to random IV
  Encrypted1 := TCryptoKit.AESEncrypt(FPlainText, FKey, amCBC);
  Encrypted2 := TCryptoKit.AESEncrypt(FPlainText, FKey, amCBC);
  AssertTrue('AES CBC encryption should change the data', FPlainText <> Encrypted1);
  AssertTrue('AES CBC encryption with same input should produce different output',
    Encrypted1 <> Encrypted2);
end;

procedure TTestCaseCrypto.Test84_AESCBCDecryption;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amCBC);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCBC);
  AssertEquals('AES CBC decryption should restore original data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test85_AESCBCRoundTrip;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amCBC);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCBC);
  AssertEquals('AES CBC round trip should preserve data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test86_AESCTREncryption;
var
  Encrypted1, Encrypted2: string;
begin
  // Same plaintext encrypted twice should yield different results due to random IV
  Encrypted1 := TCryptoKit.AESEncrypt(FPlainText, FKey, amCTR);
  Encrypted2 := TCryptoKit.AESEncrypt(FPlainText, FKey, amCTR);
  AssertTrue('AES CTR encryption should change the data', FPlainText <> Encrypted1);
  AssertTrue('AES CTR encryption with same input should produce different output',
    Encrypted1 <> Encrypted2);
end;

procedure TTestCaseCrypto.Test87_AESCTRDecryption;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amCTR);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCTR);
  AssertEquals('AES CTR decryption should restore original data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test88_AESCTRRoundTrip;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, FKey, amCTR);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCTR);
  AssertEquals('AES CTR round trip should preserve data',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test89_AESWithEmptyString;
var
  Encrypted, Decrypted: string;
begin
  // Test all modes with empty string
  Encrypted := TCryptoKit.AESEncrypt('', FKey, amECB);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amECB);
  AssertEquals('AES ECB with empty string', '', Decrypted);

  Encrypted := TCryptoKit.AESEncrypt('', FKey, amCBC);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCBC);
  AssertEquals('AES CBC with empty string', '', Decrypted);

  Encrypted := TCryptoKit.AESEncrypt('', FKey, amCTR);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCTR);
  AssertEquals('AES CTR with empty string', '', Decrypted);
end;

procedure TTestCaseCrypto.Test90_AESWithEmptyKey;
begin
  try
    TCryptoKit.AESEncrypt(FPlainText, '', amECB);
    Fail('AES encryption should fail with empty key');
  except
    on E: Exception do
      AssertTrue('Exception message should mention key',
        Pos('key', LowerCase(E.Message)) > 0);
  end;
end;

procedure TTestCaseCrypto.Test91_AESWithLongKey;
const
  LongKey = 'ThisIsAVeryLongKeyThatExceedsTheMaximumLengthForAESKeys';
var
  Encrypted, Decrypted: string;
begin
  // AES should use only the first 16 bytes of the key
  Encrypted := TCryptoKit.AESEncrypt(FPlainText, LongKey, amCBC);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, LongKey, amCBC);
  AssertEquals('AES with long key should work',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test92_AESWithUnicodeString;
const
  UnicodeText = '你好，世界！';
var
  Encrypted, Decrypted: string;
begin
  // Test all modes with Unicode
  Encrypted := TCryptoKit.AESEncrypt(UnicodeText, FKey, amECB);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amECB);
  AssertEquals('AES ECB with Unicode string', UnicodeText, Decrypted);

  Encrypted := TCryptoKit.AESEncrypt(UnicodeText, FKey, amCBC);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCBC);
  AssertEquals('AES CBC with Unicode string', UnicodeText, Decrypted);

  Encrypted := TCryptoKit.AESEncrypt(UnicodeText, FKey, amCTR);
  Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCTR);
  AssertEquals('AES CTR with Unicode string', UnicodeText, Decrypted);
end;

procedure TTestCaseCrypto.Test93_AESWithKnownVector;
const
  // Test vector from NIST Special Publication 800-38A
  PlainText = '6bc1bee22e409f96e93d7e117393172a';
  KeyHex = '2b7e151628aed2a6abf7158809cf4f3c';
  ExpectedCipher = '3ad77bb40d7a3660a89ecaf32466ef97';
var
  Context: TAESContext;
  State: array[0..3, 0..3] of Byte;
  I, J: Integer;
  ByteVal: Byte;
  KeyBytes: TBytes;
  HexStr: string;
begin
  WriteLn('Test93_AESWithKnownVector: Starting');
  WriteLn('NIST Test Vector:');
  WriteLn('PlainText: ', PlainText);
  WriteLn('Key: ', KeyHex);
  WriteLn('Expected: ', ExpectedCipher);
  WriteLn;

  // Convert PlainText hex string to State array in column-major order
  for J := 0 to 3 do
    for I := 0 to 3 do
    begin
      ByteVal := StrToInt('$' + Copy(PlainText, (J*4 + I)*2 + 1, 2));
      State[I, J] := ByteVal;
    end;

  // Print initial state
  WriteLn('Expected initial state:');
  for I := 0 to 3 do
  begin
    for J := 0 to 3 do
      Write(IntToHex(State[I, J], 2));
    WriteLn;
  end;
  WriteLn;

  // Convert Key hex string to byte array
  SetLength(KeyBytes, Length(KeyHex) div 2);
  for I := 0 to Length(KeyHex) div 2 - 1 do
    KeyBytes[I] := StrToInt('$' + Copy(KeyHex, I*2 + 1, 2));
  
  // Initialize context with key
  TAESKit.InitContext(Context, KeyBytes, ks128);

  // Encrypt
  TAESKit.ECBEncrypt(Context, State, AES_BLOCKLEN);

  // Convert result to hex string for comparison in column-major order
  HexStr := '';
  for J := 0 to 3 do
    for I := 0 to 3 do
      HexStr := HexStr + IntToHex(State[I, J], 2);

  // Print final result
  WriteLn('Final result:');
  WriteLn('Got:      ', LowerCase(HexStr));
  WriteLn('Expected: ', LowerCase(ExpectedCipher));
  WriteLn;

  // Compare with expected cipher text
  AssertEquals('AES ECB encryption should match NIST test vector',
    LowerCase(ExpectedCipher), LowerCase(HexStr));
  WriteLn('Test93_AESWithKnownVector: Finished');
end;

procedure TTestCaseCrypto.Test94_AESPaddingHandling;
const
  TestData: array[0..3] of string = (
    'A',                    // 1 byte - needs 15 bytes padding
    'ABCDEFGHIJKLMNO',     // 15 bytes - needs 1 byte padding
    'ABCDEFGHIJKLMNOP',    // 16 bytes - needs 16 bytes padding
    'ABCDEFGHIJKLMNOPQ'    // 17 bytes - needs 15 bytes padding
  );
var
  I: Integer;
  Encrypted, Decrypted: string;
begin
  for I := 0 to High(TestData) do
  begin
    Encrypted := TCryptoKit.AESEncrypt(TestData[I], FKey, amCBC);
    Decrypted := TCryptoKit.AESDecrypt(Encrypted, FKey, amCBC);
    AssertEquals(Format('AES padding test for length %d', [Length(TestData[I])]),
      TestData[I], Decrypted);
  end;
end;

procedure TTestCaseCrypto.Test95_AESIVHandling;
var
  Context: TAESContext;
  IV: array[0..15] of Byte;
  I: Integer;
begin
  // Test that IV is properly initialized
  TAESKit.InitContext(Context, TEncoding.UTF8.GetBytes(FKey));
  for I := 0 to 15 do
    AssertEquals(Format('IV byte %d should be zero after InitContext', [I]),
      0, Context.IV[I]);

  // Test that IV can be set
  for I := 0 to 15 do
    IV[I] := I;
  TAESKit.SetIV(Context, IV);
  for I := 0 to 15 do
    AssertEquals(Format('IV byte %d should match after SetIV', [I]),
      I, Context.IV[I]);
end;

initialization
  RegisterTest(TTestCaseCrypto);
end. 
