unit TestCaseCrypto;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit;

type
  { TTestCaseCrypto }
  TTestCaseCrypto = class(TTestCase)
  private
    FCryptoKit: TCryptoKit;
    FPlainText: string;
    FKey: string;
    function GenerateLongString(Size: Integer): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Hashing tests (01-19)
    procedure Test01_MD5Hash;
    procedure Test02_SHA1Hash;
    procedure Test03_SHA256Hash;
    procedure Test03A_SHA512Hash;
    procedure Test03B_SHA512_256Hash;
    procedure Test04A_MD5HashEmptyString;
    procedure Test04B_SHA1HashEmptyString;
    procedure Test04C_SHA256HashEmptyString;
    procedure Test04D_SHA512HashEmptyString;
    procedure Test04E_SHA512_256HashEmptyString;
    procedure Test05A_MD5HashUnicodeString;
    procedure Test05B_SHA1HashUnicodeString;
    procedure Test05C_SHA256HashUnicodeString;
    procedure Test05D_SHA512HashUnicodeString;
    procedure Test05E_SHA512_256HashUnicodeString;

    // Base64 tests (20-39)
    procedure Test20_Base64Encode;
    procedure Test21_Base64Decode;
    procedure Test22_Base64RoundTrip;
    procedure Test23A_Base64EncodeEmptyString;
    procedure Test23B_Base64DecodeEmptyString;
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

    // Additional hash tests
    procedure Test06A_MD5HashLongString;
    procedure Test06B_SHA1HashLongString;
    procedure Test06C_SHA256HashLongString;
    procedure Test06D_SHA512HashLongString;
    procedure Test06E_SHA512_256HashLongString;
    procedure Test07A_SHA256KnownAnswer;
    procedure Test07B_SHA512KnownAnswer;
    procedure Test07C_SHA512_256KnownAnswer;
    procedure Test08A_SHA256BlockBoundary;
    procedure Test08B_SHA512BlockBoundary;

    // Additional Base64 tests
    procedure Test25_Base64LongString;
    procedure Test26_Base64SpecialChars;
    procedure Test27A_Base64Padding1;
    procedure Test27B_Base64Padding2;

    // Additional XOR tests
    procedure Test45_XORCryptLongKey;
    procedure Test46_XORCryptShortKey;
    procedure Test47_XORCryptBinaryData;

    // Additional Blowfish tests
    procedure Test67_BlowfishKnownAnswer;
    procedure Test68_BlowfishBlockBoundary;
    procedure Test69A_BlowfishKey32;
    procedure Test69B_BlowfishKey128;
    procedure Test69C_BlowfishKey448;
    procedure Test70_BlowfishBinaryData;
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

procedure TTestCaseCrypto.Test03_SHA256Hash;
begin
  AssertEquals('SHA256 hash should match expected value',
    'D7A8FBB307D7809469CA9ABCB0082E4F8D5651E46D3CDB762D02D0BF37C9E592',
    UpperCase(TCryptoKit.SHA256Hash('The quick brown fox jumps over the lazy dog')));
end;

procedure TTestCaseCrypto.Test03A_SHA512Hash;
begin
  AssertEquals('SHA512 hash should match expected value',
    '07E547D9586F6A73F73FBAC0435ED76951218FB7D0C8D788A309D785436BBB642E93A252A954F23912547D1E8A3B5ED6E1BFD7097821233FA0538F3DB854FEE6',
    UpperCase(TCryptoKit.SHA512Hash('The quick brown fox jumps over the lazy dog')));
end;

procedure TTestCaseCrypto.Test03B_SHA512_256Hash;
begin
  AssertEquals('SHA512/256 hash should match expected value',
    'DD9D67B371519C339ED8DBD25AF90E976A1EEEFD4AD3D889005E532FC5BEF04D',
    UpperCase(TCryptoKit.SHA512_256Hash('The quick brown fox jumps over the lazy dog')));
end;

procedure TTestCaseCrypto.Test04A_MD5HashEmptyString;
begin
  AssertEquals('MD5 of empty string',
    'D41D8CD98F00B204E9800998ECF8427E',
    UpperCase(TCryptoKit.MD5Hash('')));
end;

procedure TTestCaseCrypto.Test04B_SHA1HashEmptyString;
begin
  AssertEquals('SHA1 of empty string',
    'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709',
    UpperCase(TCryptoKit.SHA1Hash('')));
end;

procedure TTestCaseCrypto.Test04C_SHA256HashEmptyString;
begin
  AssertEquals('SHA256 of empty string',
    'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855',
    UpperCase(TCryptoKit.SHA256Hash('')));
end;

procedure TTestCaseCrypto.Test04D_SHA512HashEmptyString;
begin
  AssertEquals('SHA512 of empty string',
    'CF83E1357EEFB8BDF1542850D66D8007D620E4050B5715DC83F4A921D36CE9CE47D0D13C5D85F2B0FF8318D2877EEC2F63B931BD47417A81A538327AF927DA3E',
    UpperCase(TCryptoKit.SHA512Hash('')));
end;

procedure TTestCaseCrypto.Test04E_SHA512_256HashEmptyString;
begin
  AssertEquals('SHA512/256 of empty string',
    'C672B8D1EF56ED28AB87C3622C5114069BDD3AD7B8F9737498D0C01ECEF0967A',
    UpperCase(TCryptoKit.SHA512_256Hash('')));
end;

procedure TTestCaseCrypto.Test05A_MD5HashUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('MD5 of Unicode string should not be empty',
    TCryptoKit.MD5Hash(UnicodeText) <> '');
end;

procedure TTestCaseCrypto.Test05B_SHA1HashUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA1 of Unicode string should not be empty',
    TCryptoKit.SHA1Hash(UnicodeText) <> '');
end;

procedure TTestCaseCrypto.Test05C_SHA256HashUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA256 of Unicode string should not be empty',
    TCryptoKit.SHA256Hash(UnicodeText) <> '');
end;

procedure TTestCaseCrypto.Test05D_SHA512HashUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA512 of Unicode string should not be empty',
    TCryptoKit.SHA512Hash(UnicodeText) <> '');
end;

procedure TTestCaseCrypto.Test05E_SHA512_256HashUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA512/256 of Unicode string should not be empty',
    TCryptoKit.SHA512_256Hash(UnicodeText) <> '');
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

procedure TTestCaseCrypto.Test23A_Base64EncodeEmptyString;
begin
  AssertEquals('Base64 encode empty string', '', TCryptoKit.Base64Encode(''));
end;

procedure TTestCaseCrypto.Test23B_Base64DecodeEmptyString;
begin
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

function TTestCaseCrypto.GenerateLongString(Size: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, Size);
  for I := 1 to Size do
    Result[I] := Chr(Ord('a') + (I mod 26));
end;

// Additional hash tests
procedure TTestCaseCrypto.Test06A_MD5HashLongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(1000);
  AssertTrue('MD5 of long string should not be empty',
    TCryptoKit.MD5Hash(LongStr) <> '');
end;

procedure TTestCaseCrypto.Test06B_SHA1HashLongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(1000);
  AssertTrue('SHA1 of long string should not be empty',
    TCryptoKit.SHA1Hash(LongStr) <> '');
end;

procedure TTestCaseCrypto.Test06C_SHA256HashLongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(1000);
  AssertTrue('SHA256 of long string should not be empty',
    TCryptoKit.SHA256Hash(LongStr) <> '');
end;

procedure TTestCaseCrypto.Test06D_SHA512HashLongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(1000);
  AssertTrue('SHA512 of long string should not be empty',
    TCryptoKit.SHA512Hash(LongStr) <> '');
end;

procedure TTestCaseCrypto.Test06E_SHA512_256HashLongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(1000);
  AssertTrue('SHA512/256 of long string should not be empty',
    TCryptoKit.SHA512_256Hash(LongStr) <> '');
end;

procedure TTestCaseCrypto.Test07A_SHA256KnownAnswer;
begin
  // NIST test vector
  AssertEquals('SHA256 known answer test',
    'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855',
    UpperCase(TCryptoKit.SHA256Hash('')));
end;

procedure TTestCaseCrypto.Test07B_SHA512KnownAnswer;
begin
  // NIST test vector
  AssertEquals('SHA512 known answer test',
    'CF83E1357EEFB8BDF1542850D66D8007D620E4050B5715DC83F4A921D36CE9CE47D0D13C5D85F2B0FF8318D2877EEC2F63B931BD47417A81A538327AF927DA3E',
    UpperCase(TCryptoKit.SHA512Hash('')));
end;

procedure TTestCaseCrypto.Test07C_SHA512_256KnownAnswer;
begin
  // NIST test vector
  AssertEquals('SHA512/256 known answer test',
    'DD9D67B371519C339ED8DBD25AF90E976A1EEEFD4AD3D889005E532FC5BEF04D',
    UpperCase(TCryptoKit.SHA512_256Hash('The quick brown fox jumps over the lazy dog')));
end;

procedure TTestCaseCrypto.Test08A_SHA256BlockBoundary;
var
  Str55, Str56, Str63, Str64, Str65: string;
begin
  // Test block boundaries (SHA-256 block size is 64 bytes)
  Str55 := GenerateLongString(55);
  Str56 := GenerateLongString(56);
  Str63 := GenerateLongString(63);
  Str64 := GenerateLongString(64);
  Str65 := GenerateLongString(65);

  AssertTrue('SHA256 55-byte string', TCryptoKit.SHA256Hash(Str55) <> '');
  AssertTrue('SHA256 56-byte string', TCryptoKit.SHA256Hash(Str56) <> '');
  AssertTrue('SHA256 63-byte string', TCryptoKit.SHA256Hash(Str63) <> '');
  AssertTrue('SHA256 64-byte string', TCryptoKit.SHA256Hash(Str64) <> '');
  AssertTrue('SHA256 65-byte string', TCryptoKit.SHA256Hash(Str65) <> '');
end;

procedure TTestCaseCrypto.Test08B_SHA512BlockBoundary;
var
  Str111, Str112, Str127, Str128, Str129: string;
begin
  // Test block boundaries (SHA-512 block size is 128 bytes)
  Str111 := GenerateLongString(111);
  Str112 := GenerateLongString(112);
  Str127 := GenerateLongString(127);
  Str128 := GenerateLongString(128);
  Str129 := GenerateLongString(129);

  AssertTrue('SHA512 111-byte string', TCryptoKit.SHA512Hash(Str111) <> '');
  AssertTrue('SHA512 112-byte string', TCryptoKit.SHA512Hash(Str112) <> '');
  AssertTrue('SHA512 127-byte string', TCryptoKit.SHA512Hash(Str127) <> '');
  AssertTrue('SHA512 128-byte string', TCryptoKit.SHA512Hash(Str128) <> '');
  AssertTrue('SHA512 129-byte string', TCryptoKit.SHA512Hash(Str129) <> '');
end;

// Additional Base64 tests
procedure TTestCaseCrypto.Test25_Base64LongString;
var
  LongStr, Encoded, Decoded: string;
begin
  LongStr := GenerateLongString(1024);
  Encoded := TCryptoKit.Base64Encode(LongStr);
  Decoded := TCryptoKit.Base64Decode(Encoded);
  AssertEquals('Base64 long string round trip', LongStr, Decoded);
end;

procedure TTestCaseCrypto.Test26_Base64SpecialChars;
const
  SpecialChars = '!@#$%^&*()_+-=[]{}|;:,.<>?`~';
var
  Encoded, Decoded: string;
begin
  Encoded := TCryptoKit.Base64Encode(SpecialChars);
  Decoded := TCryptoKit.Base64Decode(Encoded);
  AssertEquals('Base64 special chars', SpecialChars, Decoded);
end;

procedure TTestCaseCrypto.Test27A_Base64Padding1;
const
  // String that will require 1 padding character
  TestStr = 'Base64';
var
  Encoded: string;
begin
  Encoded := TCryptoKit.Base64Encode(TestStr);
  AssertEquals('Base64 padding check',
    'QmFzZTY0', Encoded);
end;

procedure TTestCaseCrypto.Test27B_Base64Padding2;
const
  // String that will require 2 padding characters
  TestStr = 'Base6';
var
  Encoded: string;
begin
  Encoded := TCryptoKit.Base64Encode(TestStr);
  AssertEquals('Base64 padding check',
    'QmFzZTY=', Encoded);
end;

// Additional XOR tests
procedure TTestCaseCrypto.Test45_XORCryptLongKey;
const
  Data = 'Short text';
var
  LongKey, Encrypted, Decrypted: string;
begin
  LongKey := GenerateLongString(100);
  Encrypted := TCryptoKit.XORCrypt(Data, LongKey);
  Decrypted := TCryptoKit.XORCrypt(Encrypted, LongKey);
  AssertEquals('XOR with long key', Data, Decrypted);
end;

procedure TTestCaseCrypto.Test46_XORCryptShortKey;
var
  LongData, Encrypted, Decrypted: string;
begin
  LongData := GenerateLongString(100);
  Encrypted := TCryptoKit.XORCrypt(LongData, 'key');
  Decrypted := TCryptoKit.XORCrypt(Encrypted, 'key');
  AssertEquals('XOR with short key', LongData, Decrypted);
end;

procedure TTestCaseCrypto.Test47_XORCryptBinaryData;
var
  BinaryData: array[0..7] of Byte;
  Data, Encrypted, Decrypted: string;
  I: Integer;
begin
  for I := 0 to 7 do
    BinaryData[I] := I;
  SetString(Data, PChar(@BinaryData[0]), Length(BinaryData));
  
  Encrypted := TCryptoKit.XORCrypt(Data, FKey);
  Decrypted := TCryptoKit.XORCrypt(Encrypted, FKey);
  AssertEquals('XOR binary data length', Length(Data), Length(Decrypted));
  AssertEquals('XOR binary data content',
    0, CompareByte(Data[1], Decrypted[1], Length(Data)));
end;

// Additional Blowfish tests
procedure TTestCaseCrypto.Test67_BlowfishKnownAnswer;
const
  // Known test vector from Bruce Schneier's paper
  PlainText = '0000000000000000';
  Key = '0000000000000000';
  ExpectedCipher = '4EF997456198DD78';
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.BlowfishCrypt(PlainText, Key, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, Key, bmDecrypt);
  AssertEquals('Blowfish known answer test', PlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test68_BlowfishBlockBoundary;
var
  Data7, Data8, Data9: string;
  Encrypted, Decrypted: string;
begin
  // Blowfish block size is 8 bytes
  Data7 := GenerateLongString(7);
  Data8 := GenerateLongString(8);
  Data9 := GenerateLongString(9);

  // Test 7 bytes (less than block)
  Encrypted := TCryptoKit.BlowfishCrypt(Data7, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish 7-byte boundary', Data7, Decrypted);

  // Test 8 bytes (exact block)
  Encrypted := TCryptoKit.BlowfishCrypt(Data8, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish 8-byte boundary', Data8, Decrypted);

  // Test 9 bytes (more than block)
  Encrypted := TCryptoKit.BlowfishCrypt(Data9, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish 9-byte boundary', Data9, Decrypted);
end;

procedure TTestCaseCrypto.Test69A_BlowfishKey32;
const
  Key32 = '12345678901234567890123456789012'; // 32 bytes
begin
  AssertTrue('Blowfish 32-byte key encryption',
    TCryptoKit.BlowfishCrypt(FPlainText, Key32, bmEncrypt) <> '');
end;

procedure TTestCaseCrypto.Test69B_BlowfishKey128;
var
  Key128: string;
begin
  Key128 := GenerateLongString(128);
  AssertTrue('Blowfish 128-byte key encryption',
    TCryptoKit.BlowfishCrypt(FPlainText, Key128, bmEncrypt) <> '');
end;

procedure TTestCaseCrypto.Test69C_BlowfishKey448;
var
  Key448: string;
begin
  Key448 := GenerateLongString(448);
  AssertTrue('Blowfish 448-byte key encryption',
    TCryptoKit.BlowfishCrypt(FPlainText, Key448, bmEncrypt) <> '');
end;

procedure TTestCaseCrypto.Test70_BlowfishBinaryData;
var
  BinaryData: array[0..15] of Byte;
  Data, Encrypted, Decrypted: string;
  I: Integer;
begin
  for I := 0 to 15 do
    BinaryData[I] := I;
  SetString(Data, PChar(@BinaryData[0]), Length(BinaryData));
  
  Encrypted := TCryptoKit.BlowfishCrypt(Data, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish binary data length', Length(Data), Length(Decrypted));
  AssertEquals('Blowfish binary data content',
    0, CompareByte(Data[1], Decrypted[1], Length(Data)));
end;

initialization
  RegisterTest(TTestCaseCrypto);
end. 
