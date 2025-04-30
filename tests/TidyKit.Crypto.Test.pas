unit TidyKit.Crypto.Test;


{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit, TidyKit.Crypto.AES256, DateUtils;

type
  { TTestCaseCrypto }
  TTestCaseCrypto = class(TTestCase)
  private
    FCryptoKit: TCryptoKit;
    FPlainText: string;
    FKey: string;
    FAESKey: TAESKey;
    FAESIV: TAESBlock;
    function GenerateLongString(Size: Integer): string;
    procedure InitAESKeyAndIV;
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

    // SHA3 tests (80-99)
    procedure Test80_SHA3_224EmptyString;
    procedure Test81_SHA3_256EmptyString;
    procedure Test82_SHA3_384EmptyString;
    procedure Test83_SHA3_512EmptyString;
    procedure Test84_SHA3_224KnownAnswer;
    procedure Test85_SHA3_256KnownAnswer;
    procedure Test86_SHA3_384KnownAnswer;
    procedure Test87_SHA3_512KnownAnswer;
    procedure Test88_SHA3_224LongString;
    procedure Test89_SHA3_256LongString;
    procedure Test90_SHA3_384LongString;
    procedure Test91_SHA3_512LongString;
    procedure Test92_SHA3_224UnicodeString;
    procedure Test93_SHA3_256UnicodeString;
    procedure Test94_SHA3_384UnicodeString;
    procedure Test95_SHA3_512UnicodeString;

    // AES-256 CBC tests (100-119)
    procedure Test100_AES256CBCEncryption;
    procedure Test101_AES256CBCDecryption;
    procedure Test102_AES256CBCRoundTrip;
    procedure Test103_AES256CBCEmptyString;
    procedure Test104_AES256CBCUnicodeString;
    procedure Test105_AES256CBCLongString;
    procedure Test106_AES256CBCBinaryData;
    procedure Test107_AES256CBCPadding_SingleByte;
    procedure Test107_AES256CBCPadding_FullBlock;
    procedure Test107_AES256CBCPadding_PartialBlock;
    procedure Test107_AES256CBCPadding_MultiBlock;
    procedure Test108_AES256CBCInvalidBase64;
    procedure Test109_AES256CBCInvalidKey;

    // AES-256 CTR tests (120-139)
    procedure Test120_AES256CTREncryption;
    procedure Test121_AES256CTRDecryption;
    procedure Test122_AES256CTRRoundTrip;
    procedure Test123_AES256CTREmptyString;
    procedure Test124_AES256CTRUnicodeString;
    procedure Test125_AES256CTRLongString;
    procedure Test126_AES256CTRBinaryData;
    procedure Test127_AES256CTRStreamOperation;
    procedure Test128_AES256CTRInvalidBase64;
    procedure Test129_AES256CTRInvalidKey;

    // Helper function tests (110-119)
    procedure Test110_GenerateRandomKey;
    procedure Test111_GenerateIV;
    procedure Test112_DeriveKeyBasic;
    procedure Test113_DeriveKeyDifferentPasswords;
    procedure Test114_DeriveKeyDifferentSalts;
    procedure Test115_DeriveKeyEmptyPassword;
    procedure Test116_DeriveKeyEmptySalt;
    procedure Test117_DeriveKeyIterations;
    procedure Test118_DeriveKeyWithEncryption;
    procedure Test119_DeriveKeyUnicode;

    procedure Test106_AES256CBCBinaryData_SmallBlock;
    procedure Test106_AES256CBCBinaryData_PartialBlock;
    procedure Test106_AES256CBCBinaryData_MultiBlock;
    procedure Test106_AES256CBCBinaryData_ZeroBytes;
  end;

implementation

procedure TTestCaseCrypto.SetUp;
begin
  FCryptoKit := TCryptoKit.Create;
  FPlainText := 'Hello, TidyKit Crypto!';
  FKey := 'MySecretKey123456';
  InitAESKeyAndIV;
end;

procedure TTestCaseCrypto.InitAESKeyAndIV;
var
  I: Integer;
begin
  // Initialize with a known pattern for testing
  for I := 0 to 31 do
    FAESKey[I] := I;
  for I := 0 to 15 do
    FAESIV[I] := I * 2;
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

// SHA3 tests (80-99)
procedure TTestCaseCrypto.Test80_SHA3_224EmptyString;
begin
  AssertEquals('SHA3-224 of empty string',
    '6B4E03423667DBB73B6E15454F0EB1ABD4597F9A1B078E3F5B5A6BC7',
    UpperCase(TCryptoKit.SHA3_224Hash('')));
end;

procedure TTestCaseCrypto.Test81_SHA3_256EmptyString;
begin
  AssertEquals('SHA3-256 of empty string',
    'A7FFC6F8BF1ED76651C14756A061D662F580FF4DE43B49FA82D80A4B80F8434A',
    UpperCase(TCryptoKit.SHA3_256Hash('')));
end;

procedure TTestCaseCrypto.Test82_SHA3_384EmptyString;
begin
  AssertEquals('SHA3-384 of empty string',
    '0C63A75B845E4F7D01107D852E4C2485C51A50AAAA94FC61995E71BBEE983A2AC3713831264ADB47FB6BD1E058D5F004',
    UpperCase(TCryptoKit.SHA3_384Hash('')));
end;

procedure TTestCaseCrypto.Test83_SHA3_512EmptyString;
begin
  AssertEquals('SHA3-512 of empty string',
    'A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26',
    UpperCase(TCryptoKit.SHA3_512Hash('')));
end;

procedure TTestCaseCrypto.Test84_SHA3_224KnownAnswer;
begin
  WriteLn('Test84_SHA3_224KnownAnswer: Starting');
  AssertEquals('SHA3-224 known answer test',
    'E642824C3F8CF24AD09234EE7D3C766FC9A3A5168D0C94AD73B46FDF',
    UpperCase(TCryptoKit.SHA3_224Hash('abc')));
  WriteLn('Test84_SHA3_224KnownAnswer: Finished');
end;

procedure TTestCaseCrypto.Test85_SHA3_256KnownAnswer;
begin
  WriteLn('Test85_SHA3_256KnownAnswer: Starting');
  AssertEquals('SHA3-256 known answer test',
    '3A985DA74FE225B2045C172D6BD390BD855F086E3E9D525B46BFE24511431532',
    UpperCase(TCryptoKit.SHA3_256Hash('abc')));
  WriteLn('Test85_SHA3_256KnownAnswer: Finished');
end;

procedure TTestCaseCrypto.Test86_SHA3_384KnownAnswer;
begin
  AssertEquals('SHA3-384 known answer test',
    'EC01498288516FC926459F58E2C6AD8DF9B473CB0FC08C2596DA7CF0E49BE4B298D88CEA927AC7F539F1EDF228376D25',
    UpperCase(TCryptoKit.SHA3_384Hash('abc')));
end;

procedure TTestCaseCrypto.Test87_SHA3_512KnownAnswer;
begin
  AssertEquals('SHA3-512 known answer test',
    'B751850B1A57168A5693CD924B6B096E08F621827444F70D884F5D0240D2712E10E116E9192AF3C91A7EC57647E3934057340B4CF408D5A56592F8274EEC53F0',
    UpperCase(TCryptoKit.SHA3_512Hash('abc')));
end;

procedure TTestCaseCrypto.Test88_SHA3_224LongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(10000);
  AssertTrue('SHA3-224 of long string should not be empty',
    TCryptoKit.SHA3_224Hash(LongStr) <> '');
  AssertEquals('SHA3-224 hash length', 56,
    Length(TCryptoKit.SHA3_224Hash(LongStr)));
end;

procedure TTestCaseCrypto.Test89_SHA3_256LongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(10000);
  AssertTrue('SHA3-256 of long string should not be empty',
    TCryptoKit.SHA3_256Hash(LongStr) <> '');
  AssertEquals('SHA3-256 hash length', 64,
    Length(TCryptoKit.SHA3_256Hash(LongStr)));
end;

procedure TTestCaseCrypto.Test90_SHA3_384LongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(10000);
  AssertTrue('SHA3-384 of long string should not be empty',
    TCryptoKit.SHA3_384Hash(LongStr) <> '');
  AssertEquals('SHA3-384 hash length', 96,
    Length(TCryptoKit.SHA3_384Hash(LongStr)));
end;

procedure TTestCaseCrypto.Test91_SHA3_512LongString;
var
  LongStr: string;
begin
  LongStr := GenerateLongString(10000);
  AssertTrue('SHA3-512 of long string should not be empty',
    TCryptoKit.SHA3_512Hash(LongStr) <> '');
  AssertEquals('SHA3-512 hash length', 128,
    Length(TCryptoKit.SHA3_512Hash(LongStr)));
end;

procedure TTestCaseCrypto.Test92_SHA3_224UnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA3-224 of Unicode string should not be empty',
    TCryptoKit.SHA3_224Hash(UnicodeText) <> '');
  AssertEquals('SHA3-224 hash length', 56,
    Length(TCryptoKit.SHA3_224Hash(UnicodeText)));
end;

procedure TTestCaseCrypto.Test93_SHA3_256UnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA3-256 of Unicode string should not be empty',
    TCryptoKit.SHA3_256Hash(UnicodeText) <> '');
  AssertEquals('SHA3-256 hash length', 64,
    Length(TCryptoKit.SHA3_256Hash(UnicodeText)));
end;

procedure TTestCaseCrypto.Test94_SHA3_384UnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA3-384 of Unicode string should not be empty',
    TCryptoKit.SHA3_384Hash(UnicodeText) <> '');
  AssertEquals('SHA3-384 hash length', 96,
    Length(TCryptoKit.SHA3_384Hash(UnicodeText)));
end;

procedure TTestCaseCrypto.Test95_SHA3_512UnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('SHA3-512 of Unicode string should not be empty',
    TCryptoKit.SHA3_512Hash(UnicodeText) <> '');
  AssertEquals('SHA3-512 hash length', 128,
    Length(TCryptoKit.SHA3_512Hash(UnicodeText)));
end;

// AES-256 CBC Tests

procedure TTestCaseCrypto.Test100_AES256CBCEncryption;
var
  Encrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCBC(FPlainText, FAESKey, FAESIV);
  AssertTrue('Encrypted text should not be empty', Length(Encrypted) > 0);
  AssertTrue('Encrypted text should be Base64 encoded', Encrypted <> FPlainText);
end;

procedure TTestCaseCrypto.Test101_AES256CBCDecryption;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCBC(FPlainText, FAESKey, FAESIV);
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted text should match original', FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test102_AES256CBCRoundTrip;
const
  TestData: array[1..3] of string = (
    'Short text',
    'Medium length text with some numbers 12345',
    'A longer text that will span multiple AES blocks and require proper padding'
  );
var
  I: Integer;
  Encrypted, Decrypted: string;
begin
  for I := Low(TestData) to High(TestData) do
  begin
    Encrypted := TCryptoKit.AES256EncryptCBC(TestData[I], FAESKey, FAESIV);
    Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
    AssertEquals(Format('Round trip failed for test case %d', [I]), 
                TestData[I], Decrypted);
  end;
end;

procedure TTestCaseCrypto.Test103_AES256CBCEmptyString;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCBC('', FAESKey, FAESIV);
  AssertEquals('Encryption of empty string should return empty string', '', Encrypted);
  
  Decrypted := TCryptoKit.AES256DecryptCBC('', FAESKey, FAESIV);
  AssertEquals('Decryption of empty string should return empty string', '', Decrypted);
end;

procedure TTestCaseCrypto.Test104_AES256CBCUnicodeString;
const
  UnicodeText = '你好，世界！';
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCBC(UnicodeText, FAESKey, FAESIV);
  AssertTrue('Encrypted Unicode text should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted Unicode text should match original', UnicodeText, Decrypted);
end;

procedure TTestCaseCrypto.Test105_AES256CBCLongString;
var
  LongText: string;
  Encrypted, Decrypted: string;
begin
  LongText := GenerateLongString(1000);
  Encrypted := TCryptoKit.AES256EncryptCBC(LongText, FAESKey, FAESIV);
  AssertTrue('Encrypted long text should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted long text should match original', LongText, Decrypted);
end;

procedure TTestCaseCrypto.Test106_AES256CBCBinaryData;
{$R-}
var
  BinaryData: string;
  I: Integer;
  Encrypted, Decrypted: string;
begin
  // Test with exactly one block (16 bytes)
  SetLength(BinaryData, 16);
  for I := 0 to 15 do
    BinaryData[I + 1] := Chr(I);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(BinaryData, FAESKey, FAESIV);
  AssertTrue('Encrypted small binary block should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Small binary block: length mismatch', Length(BinaryData), Length(Decrypted));
  AssertEquals('Small binary block: content mismatch', 0, CompareByte(BinaryData[1], Decrypted[1], Length(BinaryData)));
end;
{$R+}

procedure TTestCaseCrypto.Test106_AES256CBCBinaryData_SmallBlock;
{$R-}
var
  BinaryData: string;
  I: Integer;
  Encrypted, Decrypted: string;
begin
  // Test with exactly one block (16 bytes)
  SetLength(BinaryData, 16);
  for I := 0 to 15 do
    BinaryData[I + 1] := Chr(I);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(BinaryData, FAESKey, FAESIV);
  AssertTrue('Encrypted small binary block should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Small binary block: length mismatch', Length(BinaryData), Length(Decrypted));
  AssertEquals('Small binary block: content mismatch', 0, CompareByte(BinaryData[1], Decrypted[1], Length(BinaryData)));
end;
{$R+}

procedure TTestCaseCrypto.Test106_AES256CBCBinaryData_PartialBlock;
{$R-}
var
  BinaryData: string;
  I: Integer;
  Encrypted, Decrypted: string;
begin
  // Test with partial block (10 bytes)
  SetLength(BinaryData, 10);
  for I := 0 to 9 do
    BinaryData[I + 1] := Chr(I);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(BinaryData, FAESKey, FAESIV);
  AssertTrue('Encrypted partial binary block should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Partial binary block: length mismatch', Length(BinaryData), Length(Decrypted));
  AssertEquals('Partial binary block: content mismatch', 0, CompareByte(BinaryData[1], Decrypted[1], Length(BinaryData)));
end;
{$R+}

procedure TTestCaseCrypto.Test106_AES256CBCBinaryData_MultiBlock;
{$R-}
var
  BinaryData: string;
  I: Integer;
  Encrypted, Decrypted: string;
begin
  // Test with multiple blocks (48 bytes)
  SetLength(BinaryData, 48);
  for I := 0 to 47 do
    BinaryData[I + 1] := Chr(I mod 256);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(BinaryData, FAESKey, FAESIV);
  AssertTrue('Encrypted multi-block binary should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Multi-block binary: length mismatch', Length(BinaryData), Length(Decrypted));
  AssertEquals('Multi-block binary: content mismatch', 0, CompareByte(BinaryData[1], Decrypted[1], Length(BinaryData)));
end;
{$R+}

procedure TTestCaseCrypto.Test106_AES256CBCBinaryData_ZeroBytes;
{$R-}
var
  BinaryData: string;
  I: Integer;
  Encrypted, Decrypted: string;
begin
  // Test with data containing zero bytes (32 bytes)
  SetLength(BinaryData, 32);
  for I := 0 to 31 do
    if I mod 2 = 0 then
      BinaryData[I + 1] := #0
    else
      BinaryData[I + 1] := Chr(I);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(BinaryData, FAESKey, FAESIV);
  AssertTrue('Encrypted zero-byte binary should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Zero-byte binary: length mismatch', Length(BinaryData), Length(Decrypted));
  AssertEquals('Zero-byte binary: content mismatch', 0, CompareByte(BinaryData[1], Decrypted[1], Length(BinaryData)));
end;
{$R+}

// AES-256 CBC Padding Tests

procedure TTestCaseCrypto.Test107_AES256CBCPadding_SingleByte;
{$R-}
var
  TestStr, Encrypted, Decrypted: string;
begin
  // Test single byte (requires 15 bytes padding)
  TestStr := 'A';
  SetLength(TestStr, 1);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(TestStr, FAESKey, FAESIV);
  AssertTrue('Single byte encryption should not be empty', Length(Encrypted) > 0);
    
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Single byte padding failed', 1, Length(Decrypted));
  AssertEquals('Single byte content mismatch', TestStr, Decrypted);
end;
{$R+}

procedure TTestCaseCrypto.Test107_AES256CBCPadding_FullBlock;
{$R-}
var
  TestStr, Encrypted, Decrypted: string;
begin
  // Test full block (16 bytes - requires full block padding)
  TestStr := GenerateLongString(16);
  SetLength(TestStr, 16);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(TestStr, FAESKey, FAESIV);
  AssertTrue('Full block encryption should not be empty', Length(Encrypted) > 0);
    
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Full block padding failed', 16, Length(Decrypted));
  AssertEquals('Full block content mismatch', TestStr, Decrypted);
end;
{$R+}

procedure TTestCaseCrypto.Test107_AES256CBCPadding_PartialBlock;
{$R-}
var
  TestStr, Encrypted, Decrypted: string;
begin
  // Test partial block (7 bytes - requires 9 bytes padding)
  TestStr := GenerateLongString(7);
  SetLength(TestStr, 7);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(TestStr, FAESKey, FAESIV);
  AssertTrue('Partial block encryption should not be empty', Length(Encrypted) > 0);
    
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Partial block padding failed', 7, Length(Decrypted));
  AssertEquals('Partial block content mismatch', TestStr, Decrypted);
end;
{$R+}

procedure TTestCaseCrypto.Test107_AES256CBCPadding_MultiBlock;
{$R-}
var
  TestStr, Encrypted, Decrypted: string;
begin
  // Test multiple blocks plus partial (40 bytes - requires 8 bytes padding)
  TestStr := GenerateLongString(40);
  SetLength(TestStr, 40);
    
  Encrypted := TCryptoKit.AES256EncryptCBC(TestStr, FAESKey, FAESIV);
  AssertTrue('Multi-block encryption should not be empty', Length(Encrypted) > 0);
    
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, FAESKey, FAESIV);
  AssertEquals('Multi-block padding failed', 40, Length(Decrypted));
  AssertEquals('Multi-block content mismatch', TestStr, Decrypted);
end;
{$R+}

procedure TTestCaseCrypto.Test108_AES256CBCInvalidBase64;
begin
  try
    TCryptoKit.AES256DecryptCBC('Invalid Base64!', FAESKey, FAESIV);
    Fail('Should raise exception on invalid Base64 input');
  except
    on E: Exception do
      ; // Expected exception
  end;
end;

procedure TTestCaseCrypto.Test109_AES256CBCInvalidKey;
var
  InvalidKey: TAESKey;
  Encrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCBC(FPlainText, FAESKey, FAESIV);
  
  // Try decrypting with a different key
  FillChar(InvalidKey, SizeOf(InvalidKey), 0);
  try
    TCryptoKit.AES256DecryptCBC(Encrypted, InvalidKey, FAESIV);
    Fail('Should raise EAESError when using invalid key');
  except
    on E: EAESError do
      ; // Expected - decryption with wrong key should fail
  end;
end;

// AES-256 CTR Tests

procedure TTestCaseCrypto.Test120_AES256CTREncryption;
var
  Encrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCTR(FPlainText, FAESKey, FAESIV);
  AssertTrue('Encrypted text should not be empty', Length(Encrypted) > 0);
  AssertTrue('Encrypted text should be Base64 encoded', Encrypted <> FPlainText);
end;

procedure TTestCaseCrypto.Test121_AES256CTRDecryption;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCTR(FPlainText, FAESKey, FAESIV);
  Decrypted := TCryptoKit.AES256DecryptCTR(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted text should match original', FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test122_AES256CTRRoundTrip;
const
  TestData: array[1..3] of string = (
    'Short text',
    'Medium length text with some numbers 12345',
    'A longer text that will span multiple AES blocks but requires no padding in CTR mode'
  );
var
  I: Integer;
  Encrypted, Decrypted: string;
begin
  for I := Low(TestData) to High(TestData) do
  begin
    Encrypted := TCryptoKit.AES256EncryptCTR(TestData[I], FAESKey, FAESIV);
    Decrypted := TCryptoKit.AES256DecryptCTR(Encrypted, FAESKey, FAESIV);
    AssertEquals(Format('Round trip failed for test case %d', [I]), 
                TestData[I], Decrypted);
  end;
end;

procedure TTestCaseCrypto.Test123_AES256CTREmptyString;
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCTR('', FAESKey, FAESIV);
  AssertEquals('Encryption of empty string should return empty string', '', Encrypted);
  
  Decrypted := TCryptoKit.AES256DecryptCTR('', FAESKey, FAESIV);
  AssertEquals('Decryption of empty string should return empty string', '', Decrypted);
end;

procedure TTestCaseCrypto.Test124_AES256CTRUnicodeString;
const
  UnicodeText = '你好，世界！';
var
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCTR(UnicodeText, FAESKey, FAESIV);
  AssertTrue('Encrypted Unicode text should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCTR(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted Unicode text should match original', UnicodeText, Decrypted);
end;

procedure TTestCaseCrypto.Test125_AES256CTRLongString;
var
  LongText: string;
  Encrypted, Decrypted: string;
begin
  LongText := GenerateLongString(1000);
  Encrypted := TCryptoKit.AES256EncryptCTR(LongText, FAESKey, FAESIV);
  AssertTrue('Encrypted long text should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCTR(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted long text should match original', LongText, Decrypted);
end;

procedure TTestCaseCrypto.Test126_AES256CTRBinaryData;
var
  BinaryData: string;
  I: Integer;
  Encrypted, Decrypted: string;
begin
  SetLength(BinaryData, 256);
  for I := 0 to 255 do
    BinaryData[I + 1] := Chr(I);
    
  Encrypted := TCryptoKit.AES256EncryptCTR(BinaryData, FAESKey, FAESIV);
  AssertTrue('Encrypted binary data should not be empty', Length(Encrypted) > 0);
  
  Decrypted := TCryptoKit.AES256DecryptCTR(Encrypted, FAESKey, FAESIV);
  AssertEquals('Decrypted binary data should match original', BinaryData, Decrypted);
end;

procedure TTestCaseCrypto.Test127_AES256CTRStreamOperation;
var
  Parts: array[1..3] of string;
  Encrypted: array[1..3] of string;
  FullText, CombinedDecrypted: string;
  I: Integer;
begin
  // Split text into parts to simulate streaming
  Parts[1] := 'First part ';
  Parts[2] := 'second part ';
  Parts[3] := 'third part';
  FullText := Parts[1] + Parts[2] + Parts[3];
  
  // Encrypt each part with same key/IV
  for I := 1 to 3 do
    Encrypted[I] := TCryptoKit.AES256EncryptCTR(Parts[I], FAESKey, FAESIV);
  
  // Decrypt combined parts
  CombinedDecrypted := '';
  for I := 1 to 3 do
    CombinedDecrypted := CombinedDecrypted + 
                        TCryptoKit.AES256DecryptCTR(Encrypted[I], FAESKey, FAESIV);
  
  AssertEquals('Stream operation should preserve data', FullText, CombinedDecrypted);
end;

procedure TTestCaseCrypto.Test128_AES256CTRInvalidBase64;
begin
  try
    TCryptoKit.AES256DecryptCTR('Invalid Base64!', FAESKey, FAESIV);
    Fail('Should raise exception on invalid Base64 input');
  except
    on E: Exception do
      ; // Expected exception
  end;
end;

procedure TTestCaseCrypto.Test129_AES256CTRInvalidKey;
var
  InvalidKey: TAESKey;
  Encrypted, Decrypted: string;
begin
  Encrypted := TCryptoKit.AES256EncryptCTR(FPlainText, FAESKey, FAESIV);
  
  // Try decrypting with a different key
  FillChar(InvalidKey, SizeOf(InvalidKey), 0);
  Decrypted := TCryptoKit.AES256DecryptCTR(Encrypted, InvalidKey, FAESIV);
  AssertTrue('Decryption with wrong key should not match original', 
             FPlainText <> Decrypted);
end;

// Helper function tests (110-119)
procedure TTestCaseCrypto.Test110_GenerateRandomKey;
var
  Key1, Key2: TAESKey;
begin
  // Generate two keys and verify they are different (random)
  Key1 := TCryptoKit.GenerateRandomKey;
  Key2 := TCryptoKit.GenerateRandomKey;
  
  // Verify key length
  AssertEquals('Generated key should be 32 bytes', 32, SizeOf(Key1));
  
  // Verify keys are different (random)
  AssertTrue('Generated keys should be different',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)) = False);
  
  // Verify key is not all zeros
  FillChar(Key2, SizeOf(Key2), 0);
  AssertTrue('Generated key should not be all zeros',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)) = False);
end;

procedure TTestCaseCrypto.Test111_GenerateIV;
var
  IV1, IV2: TAESBlock;
begin
  // Generate two IVs and verify they are different (random)
  IV1 := TCryptoKit.GenerateIV;
  IV2 := TCryptoKit.GenerateIV;
  
  // Verify IV length
  AssertEquals('Generated IV should be 16 bytes', 16, SizeOf(IV1));
  
  // Verify IVs are different (random)
  AssertTrue('Generated IVs should be different',
    CompareMem(@IV1[0], @IV2[0], SizeOf(TAESBlock)) = False);
  
  // Verify IV is not all zeros
  FillChar(IV2, SizeOf(IV2), 0);
  AssertTrue('Generated IV should not be all zeros',
    CompareMem(@IV1[0], @IV2[0], SizeOf(TAESBlock)) = False);
end;

procedure TTestCaseCrypto.Test112_DeriveKeyBasic;
var
  Key1, Key2: TAESKey;
begin
  // Same password and salt should produce same key
  Key1 := TCryptoKit.DeriveKey('password123', 'salt123', 1000);
  Key2 := TCryptoKit.DeriveKey('password123', 'salt123', 1000);
  AssertTrue('Same password/salt should produce same key',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)));
end;

procedure TTestCaseCrypto.Test113_DeriveKeyDifferentPasswords;
var
  Key1, Key2: TAESKey;
begin
  // Different passwords should produce different keys
  Key1 := TCryptoKit.DeriveKey('password1', 'salt123', 1000);
  Key2 := TCryptoKit.DeriveKey('password2', 'salt123', 1000);
  AssertTrue('Different passwords should produce different keys',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)) = False);
end;

procedure TTestCaseCrypto.Test114_DeriveKeyDifferentSalts;
var
  Key1, Key2: TAESKey;
begin
  // Different salts should produce different keys
  Key1 := TCryptoKit.DeriveKey('password123', 'salt1', 1000);
  Key2 := TCryptoKit.DeriveKey('password123', 'salt2', 1000);
  AssertTrue('Different salts should produce different keys',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)) = False);
end;

procedure TTestCaseCrypto.Test115_DeriveKeyEmptyPassword;
var
  Key: TAESKey;
begin
  // Empty password should still produce a key
  Key := TCryptoKit.DeriveKey('', 'salt123', 1000);
  FillChar(FAESKey, SizeOf(FAESKey), 0);
  AssertTrue('Empty password should not produce all-zero key',
    CompareMem(@Key[0], @FAESKey[0], SizeOf(TAESKey)) = False);
end;

procedure TTestCaseCrypto.Test116_DeriveKeyEmptySalt;
var
  Key1, Key2: TAESKey;
begin
  // Empty salt should generate random salt internally
  Key1 := TCryptoKit.DeriveKey('password123', '', 1000);
  Key2 := TCryptoKit.DeriveKey('password123', '', 1000);
  AssertTrue('Empty salt should produce different keys (random salt)',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)) = False);
end;

procedure TTestCaseCrypto.Test117_DeriveKeyIterations;
{$R-} // Disable range checking for array operations
var
  Password: string;
  Salt: string;
  Key1, Key2: TAESKey;
  StartTime, EndTime: TDateTime;
  Time1, Time2: Int64;
  I: Integer;
begin
  Password := 'test_password';
  Salt := 'test_salt_123456';

  // Test that different iteration counts produce different keys
  Key1 := TCryptoKit.DeriveKey(Password, Salt, 1000);
  Key2 := TCryptoKit.DeriveKey(Password, Salt, 2000);
  
  AssertTrue('Keys with different iteration counts should be different',
    not CompareMem(@Key1[0], @Key2[0], Length(Key1)));

  // Test that more iterations take longer
  StartTime := Now;
  Key1 := TCryptoKit.DeriveKey(Password, Salt, 1000);
  EndTime := Now;
  Time1 := MilliSecondsBetween(EndTime, StartTime);

  StartTime := Now;
  Key2 := TCryptoKit.DeriveKey(Password, Salt, 10000);
  EndTime := Now;
  Time2 := MilliSecondsBetween(EndTime, StartTime);

  AssertTrue('More iterations should take longer', Time2 > Time1);
end;
{$R+} // Re-enable range checking

procedure TTestCaseCrypto.Test118_DeriveKeyWithEncryption;
var
  Key: TAESKey;
  IV: TAESBlock;
  Encrypted, Decrypted: string;
begin
  // Test derived key with actual encryption
  Key := TCryptoKit.DeriveKey('password123', 'salt123', 1000);
  IV := TCryptoKit.GenerateIV;
  
  Encrypted := TCryptoKit.AES256EncryptCBC(FPlainText, Key, IV);
  Decrypted := TCryptoKit.AES256DecryptCBC(Encrypted, Key, IV);
  
  AssertEquals('Encryption with derived key should work', FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test119_DeriveKeyUnicode;
var
  Key1, Key2: TAESKey;
const
  UnicodePassword = '你好，世界！';
begin
  // Unicode passwords should work
  Key1 := TCryptoKit.DeriveKey(UnicodePassword, 'salt123', 1000);
  Key2 := TCryptoKit.DeriveKey('Hello, World!', 'salt123', 1000);
  AssertTrue('Unicode and ASCII passwords should produce different keys',
    CompareMem(@Key1[0], @Key2[0], SizeOf(TAESKey)) = False);
end;

initialization
  RegisterTest(TTestCaseCrypto);
end. 
