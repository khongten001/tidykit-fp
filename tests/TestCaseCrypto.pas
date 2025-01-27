unit TestCaseCrypto;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TidyKit,
  TidyKit.Crypto, TidyKit.Crypto.AES;

type
  TAESKeySize = TidyKit.Crypto.AES.TAESKeySize;
  TAESMode = TidyKit.Crypto.AES.TAESMode;
  TAESBlockMode = TidyKit.Crypto.AES.TAESBlockMode;

  { TTestCaseCrypto }
  TTestCaseCrypto = class(TTestCase)
  private
    FCryptoKit: TCryptoKit;
    FPlainText: string;
    FKey: string;
    FIV: string;
    FAAD: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    function StringToBytes(const S: string): TBytes;
    function BytesToString(const B: TBytes): string;
  published
    // Hashing tests (01-19)
    procedure Test01_MD5Hash;
    procedure Test02_SHA1Hash;
    procedure Test03_SHA256Hash;
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

    // AES tests - CBC Mode (80-99)
    procedure Test80_AESCBCEncryption;
    procedure Test81_AESCBCDecryption;
    procedure Test82_AESCBCRoundTrip;
    procedure Test83_AESCBCWithEmptyString;
    procedure Test84_AESCBCWithEmptyKey;
    procedure Test85_AESCBCWithEmptyIV;
    procedure Test86_AESCBCWithUnicodeString;
    procedure Test87_AESCBCWithDifferentKeySizes;

    // AES tests - CTR Mode (100-119)
    procedure Test100_AESCTREncryption;
    procedure Test101_AESCTRDecryption;
    procedure Test102_AESCTRRoundTrip;
    procedure Test103_AESCTRWithEmptyString;
    procedure Test104_AESCTRWithEmptyKey;
    procedure Test105_AESCTRWithEmptyIV;
    procedure Test106_AESCTRWithUnicodeString;
    procedure Test107_AESCTRWithDifferentKeySizes;
    procedure Test108_AESCTRWithPartialBlock;
    procedure Test109_AESCTRWithLongData;

    // AES tests - GCM Mode (120-139)
    procedure Test120_AESGCMEncryption;
    procedure Test121_AESGCMDecryption;
    procedure Test122_AESGCMRoundTrip;
    procedure Test123_AESGCMWithEmptyString;
    procedure Test124_AESGCMWithEmptyKey;
    procedure Test125_AESGCMWithEmptyIV;
    procedure Test126_AESGCMWithEmptyAAD;
    procedure Test127_AESGCMWithUnicodeString;
    procedure Test128_AESGCMWithDifferentKeySizes;
    procedure Test129_AESGCMAuthenticationFailure;
    procedure Test130_AESGCMWithLongAAD;
    procedure Test131_AESGCMWithPartialBlock;
    procedure Test132_AESGCMWithLongData;
  end;

implementation

procedure TTestCaseCrypto.SetUp;
begin
  FCryptoKit := TCryptoKit.Create;
  FPlainText := 'Hello, TidyKit Crypto!';
  FKey := 'MySecretKey123456';
  FIV := 'InitVector123456!';
  FAAD := 'Additional authenticated data';
end;

procedure TTestCaseCrypto.TearDown;
begin
  FCryptoKit.Free;
end;

function TTestCaseCrypto.StringToBytes(const S: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    Result[I-1] := Ord(S[I]);
end;

function TTestCaseCrypto.BytesToString(const B: TBytes): string;
var
  I: Integer;
begin
  SetLength(Result, Length(B));
  for I := 0 to Length(B)-1 do
    Result[I+1] := Chr(B[I]);
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

procedure TTestCaseCrypto.Test04_HashWithEmptyString;
begin
  AssertEquals('MD5 of empty string',
    'D41D8CD98F00B204E9800998ECF8427E',
    UpperCase(TCryptoKit.MD5Hash('')));
  AssertEquals('SHA1 of empty string',
    'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709',
    UpperCase(TCryptoKit.SHA1Hash('')));
  AssertEquals('SHA256 of empty string',
    'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855',
    UpperCase(TCryptoKit.SHA256Hash('')));
end;

procedure TTestCaseCrypto.Test05_HashWithUnicodeString;
const
  UnicodeText = '你好，世界！';
begin
  AssertTrue('MD5 of Unicode string should not be empty',
    TCryptoKit.MD5Hash(UnicodeText) <> '');
  AssertTrue('SHA1 of Unicode string should not be empty',
    TCryptoKit.SHA1Hash(UnicodeText) <> '');
  AssertTrue('SHA256 of Unicode string should not be empty',
    TCryptoKit.SHA256Hash(UnicodeText) <> '');
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
var
  UnicodeText: string;
  Encrypted, Decrypted: string;
begin
  UnicodeText := '你好，世界！';
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
  AssertTrue('Blowfish encrypted data should be Base64 encoded',
    Length(Encrypted) > 0);
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
  AssertEquals('Blowfish with empty string', '',
    TCryptoKit.BlowfishCrypt('', FKey, bmEncrypt));
end;

procedure TTestCaseCrypto.Test64_BlowfishWithEmptyKey;
begin
  AssertEquals('Blowfish with empty key', '',
    TCryptoKit.BlowfishCrypt(FPlainText, '', bmEncrypt));
end;

procedure TTestCaseCrypto.Test65_BlowfishWithLongKey;
var
  LongKey: string;
  Encrypted, Decrypted: string;
begin
  LongKey := StringOfChar('A', 100);  // Key longer than 56 bytes
  Encrypted := TCryptoKit.BlowfishCrypt(FPlainText, LongKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, LongKey, bmDecrypt);
  AssertEquals('Blowfish with long key should work',
    FPlainText, Decrypted);
end;

procedure TTestCaseCrypto.Test66_BlowfishWithUnicodeString;
var
  UnicodeText: string;
  Encrypted, Decrypted: string;
begin
  UnicodeText := '你好，世界！';
  Encrypted := TCryptoKit.BlowfishCrypt(UnicodeText, FKey, bmEncrypt);
  Decrypted := TCryptoKit.BlowfishCrypt(Encrypted, FKey, bmDecrypt);
  AssertEquals('Blowfish with Unicode string',
    UnicodeText, Decrypted);
end;

// AES tests - CBC Mode (80-99)
procedure TTestCaseCrypto.Test80_AESCBCEncryption;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertTrue('AES-CBC encryption should change the data',
    CompareMem(@Encrypted[0], @FPlainText[1], Length(FPlainText)) = False);
  AssertTrue('AES-CBC encrypted data should not be empty',
    Length(Encrypted) > 0);
end;

procedure TTestCaseCrypto.Test81_AESCBCDecryption;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CBC decryption should restore original data',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test82_AESCBCRoundTrip;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CBC round trip should preserve data',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test83_AESCBCWithEmptyString;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(''), ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CBC with empty string',
    '', BytesToString(Encrypted));
end;

procedure TTestCaseCrypto.Test84_AESCBCWithEmptyKey;
begin
  try
    FCryptoKit.AESEncrypt(
      StringToBytes(FPlainText), ak256, amCBC,
      StringToBytes(''), StringToBytes(FIV)
    );
    Fail('AES-CBC should raise exception for empty key');
  except
    on E: Exception do
      AssertEquals('AES-CBC empty key error message', 'Key cannot be empty', E.Message);
  end;
end;

procedure TTestCaseCrypto.Test85_AESCBCWithEmptyIV;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCBC,
    StringToBytes(FKey), StringToBytes('')
  );
  AssertEquals('AES-CBC with empty IV',
    '', BytesToString(Encrypted));
end;

procedure TTestCaseCrypto.Test86_AESCBCWithUnicodeString;
var
  UnicodeText: string;
  Encrypted, Decrypted: TBytes;
begin
  UnicodeText := '你好，世界！';
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(UnicodeText), ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCBC,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CBC with Unicode string',
    UnicodeText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test87_AESCBCWithDifferentKeySizes;
var
  Encrypted, Decrypted: TBytes;
  KeySize: TAESKeySize;
begin
  for KeySize := ak128 to ak256 do
  begin
    Encrypted := FCryptoKit.AESEncrypt(
      StringToBytes(FPlainText), KeySize, amCBC,
      StringToBytes(FKey), StringToBytes(FIV)
    );
    Decrypted := FCryptoKit.AESDecrypt(
      Encrypted, KeySize, amCBC,
      StringToBytes(FKey), StringToBytes(FIV)
    );
    AssertEquals(Format('AES-CBC with key size %d should work', [Ord(KeySize)]),
      FPlainText, BytesToString(Decrypted));
  end;
end;

// AES tests - CTR Mode (100-119)
procedure TTestCaseCrypto.Test100_AESCTREncryption;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertTrue('AES-CTR encryption should change the data',
    CompareMem(@Encrypted[0], @FPlainText[1], Length(FPlainText)) = False);
  AssertTrue('AES-CTR encrypted data should not be empty',
    Length(Encrypted) > 0);
end;

procedure TTestCaseCrypto.Test101_AESCTRDecryption;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CTR decryption should restore original data',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test102_AESCTRRoundTrip;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CTR round trip should preserve data',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test103_AESCTRWithEmptyString;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(''), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CTR with empty string',
    '', BytesToString(Encrypted));
end;

procedure TTestCaseCrypto.Test104_AESCTRWithEmptyKey;
begin
  try
    FCryptoKit.AESEncrypt(
      StringToBytes(FPlainText), ak256, amCTR,
      StringToBytes(''), StringToBytes(FIV)
    );
    Fail('AES-CTR should raise exception for empty key');
  except
    on E: Exception do
      AssertEquals('AES-CTR empty key error message', 'Key cannot be empty', E.Message);
  end;
end;

procedure TTestCaseCrypto.Test105_AESCTRWithEmptyIV;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes('')
  );
  AssertEquals('AES-CTR with empty IV',
    '', BytesToString(Encrypted));
end;

procedure TTestCaseCrypto.Test106_AESCTRWithUnicodeString;
var
  UnicodeText: string;
  Encrypted, Decrypted: TBytes;
begin
  UnicodeText := '你好，世界！';
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(UnicodeText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CTR with Unicode string',
    UnicodeText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test107_AESCTRWithDifferentKeySizes;
var
  Encrypted, Decrypted: TBytes;
  KeySize: TAESKeySize;
begin
  for KeySize := ak128 to ak256 do
  begin
    Encrypted := FCryptoKit.AESEncrypt(
      StringToBytes(FPlainText), KeySize, amCTR,
      StringToBytes(FKey), StringToBytes(FIV)
    );
    Decrypted := FCryptoKit.AESDecrypt(
      Encrypted, KeySize, amCTR,
      StringToBytes(FKey), StringToBytes(FIV)
    );
    AssertEquals(Format('AES-CTR with key size %d should work', [Ord(KeySize)]),
      FPlainText, BytesToString(Decrypted));
  end;
end;

procedure TTestCaseCrypto.Test108_AESCTRWithPartialBlock;
const
  PartialText = 'Hello';  // Less than block size (16 bytes)
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(PartialText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CTR with partial block',
    PartialText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test109_AESCTRWithLongData;
var
  LongText: string;
  Encrypted, Decrypted: TBytes;
begin
  LongText := StringOfChar('A', 1000);  // Much larger than block size
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(LongText), ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amCTR,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-CTR with long data',
    LongText, BytesToString(Decrypted));
end;

// AES tests - GCM Mode (120-139)
procedure TTestCaseCrypto.Test120_AESGCMEncryption;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertTrue('AES-GCM encryption should change the data',
    CompareMem(@Encrypted[0], @FPlainText[1], Length(FPlainText)) = False);
  AssertTrue('AES-GCM encrypted data should not be empty',
    Length(Encrypted) > 0);
end;

procedure TTestCaseCrypto.Test121_AESGCMDecryption;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM decryption should restore original data',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test122_AESGCMRoundTrip;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM round trip should preserve data',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test123_AESGCMWithEmptyString;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(''), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM with empty string',
    '', BytesToString(Encrypted));
end;

procedure TTestCaseCrypto.Test124_AESGCMWithEmptyKey;
begin
  try
    FCryptoKit.AESEncrypt(
      StringToBytes(FPlainText), ak256, amGCM,
      StringToBytes(''), StringToBytes(FIV)
    );
    Fail('AES-GCM should raise exception for empty key');
  except
    on E: Exception do
      AssertEquals('AES-GCM empty key error message', 'Key cannot be empty', E.Message);
  end;
end;

procedure TTestCaseCrypto.Test125_AESGCMWithEmptyIV;
var
  Encrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes('')
  );
  AssertEquals('AES-GCM with empty IV',
    '', BytesToString(Encrypted));
end;

procedure TTestCaseCrypto.Test126_AESGCMWithEmptyAAD;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM should work without AAD',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test127_AESGCMWithUnicodeString;
var
  UnicodeText: string;
  Encrypted, Decrypted: TBytes;
begin
  UnicodeText := '你好，世界！';
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(UnicodeText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM with Unicode string',
    UnicodeText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test128_AESGCMWithDifferentKeySizes;
var
  Encrypted, Decrypted: TBytes;
  KeySize: TAESKeySize;
begin
  for KeySize := ak128 to ak256 do
  begin
    Encrypted := FCryptoKit.AESEncrypt(
      StringToBytes(FPlainText), KeySize, amGCM,
      StringToBytes(FKey), StringToBytes(FIV)
    );
    Decrypted := FCryptoKit.AESDecrypt(
      Encrypted, KeySize, amGCM,
      StringToBytes(FKey), StringToBytes(FIV)
    );
    AssertEquals(Format('AES-GCM with key size %d should work', [Ord(KeySize)]),
      FPlainText, BytesToString(Decrypted));
  end;
end;

procedure TTestCaseCrypto.Test129_AESGCMAuthenticationFailure;
var
  Encrypted: TBytes;
  TamperedKey: string;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  TamperedKey := FKey + 'tampered';
  
  try
    FCryptoKit.AESDecrypt(
      Encrypted, ak256, amGCM,
      StringToBytes(TamperedKey), StringToBytes(FIV)
    );
    Fail('AES-GCM should fail with incorrect key');
  except
    on E: Exception do
      ; // Expected exception
  end;
end;

procedure TTestCaseCrypto.Test130_AESGCMWithLongAAD;
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(FPlainText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM with long AAD',
    FPlainText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test131_AESGCMWithPartialBlock;
const
  PartialText = 'Hello';  // Less than block size (16 bytes)
var
  Encrypted, Decrypted: TBytes;
begin
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(PartialText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM with partial block',
    PartialText, BytesToString(Decrypted));
end;

procedure TTestCaseCrypto.Test132_AESGCMWithLongData;
var
  LongText: string;
  Encrypted, Decrypted: TBytes;
begin
  LongText := StringOfChar('A', 1000);  // Much larger than block size
  Encrypted := FCryptoKit.AESEncrypt(
    StringToBytes(LongText), ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  Decrypted := FCryptoKit.AESDecrypt(
    Encrypted, ak256, amGCM,
    StringToBytes(FKey), StringToBytes(FIV)
  );
  AssertEquals('AES-GCM with long data',
    LongText, BytesToString(Decrypted));
end;

initialization
  RegisterTest(TTestCaseCrypto);
end. 