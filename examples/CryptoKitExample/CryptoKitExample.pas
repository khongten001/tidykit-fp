program CryptoKitExample;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, TidyKit;

var
  PlainText, Key: string;
  Encoded, Decoded: string;
  Encrypted, Decrypted: string;
  MD5Result, SHA1Result, SHA256Result: string;
  BlowfishEncrypted, BlowfishDecrypted: string;
  AESKey, AESIV, AAD: string;
  // CBC mode variables
  AESEncryptedCBC, AESDecryptedCBC: string;
  // CTR mode variables
  AESEncryptedCTR, AESDecryptedCTR: string;
  // GCM mode variables
  AESEncryptedGCM, AESDecryptedGCM: string;

begin
  WriteLn('TidyKit.Crypto Example');
  WriteLn('=====================');
  WriteLn;
  
  // Base64 encoding/decoding example
  PlainText := 'Hello, TidyKit Crypto!';
  WriteLn('Original text: ', PlainText);
  
  Encoded := TCryptoKit.Base64Encode(PlainText);
  WriteLn('Base64 encoded: ', Encoded);
  
  Decoded := TCryptoKit.Base64Decode(Encoded);
  WriteLn('Base64 decoded: ', Decoded);
  WriteLn;
  
  // Hashing examples
  WriteLn('Hashing Examples:');
  WriteLn('----------------');
  MD5Result := TCryptoKit.MD5Hash(PlainText);
  WriteLn('MD5 hash: ', MD5Result);
  
  SHA1Result := TCryptoKit.SHA1Hash(PlainText);
  WriteLn('SHA1 hash: ', SHA1Result);
  
  SHA256Result := TCryptoKit.SHA256Hash(PlainText);
  WriteLn('SHA256 hash: ', SHA256Result);
  WriteLn;
  
  // XOR encryption/decryption example
  WriteLn('XOR Encryption Example:');
  WriteLn('----------------------');
  Key := 'SecretKey123';
  WriteLn('Original text: ', PlainText);
  WriteLn('Encryption key: ', Key);
  
  Encrypted := TCryptoKit.XORCrypt(PlainText, Key);
  WriteLn('Encrypted (shown as hex): ', StringToHex(Encrypted));
  
  Decrypted := TCryptoKit.XORCrypt(Encrypted, Key);
  WriteLn('Decrypted: ', Decrypted);
  WriteLn;
  
  // Blowfish encryption/decryption example
  WriteLn('Blowfish Encryption Example:');
  WriteLn('---------------------------');
  WriteLn('Original text: ', PlainText);
  WriteLn('Encryption key: ', Key);
  
  BlowfishEncrypted := TCryptoKit.BlowfishCrypt(PlainText, Key, bmEncrypt);
  WriteLn('Blowfish encrypted (Base64): ', BlowfishEncrypted);
  
  BlowfishDecrypted := TCryptoKit.BlowfishCrypt(BlowfishEncrypted, Key, bmDecrypt);
  WriteLn('Blowfish decrypted: ', BlowfishDecrypted);
  WriteLn;
  
  // AES encryption/decryption examples
  WriteLn('AES Encryption Examples:');
  WriteLn('-----------------------');
  AESKey := 'MySecretAESKey123';
  AESIV := 'InitVectorABC123!';
  AAD := 'Additional authenticated data for GCM mode';
  WriteLn('Original text: ', PlainText);
  WriteLn('AES Key: ', AESKey);
  WriteLn('AES IV/Nonce: ', AESIV);
  WriteLn;
  
  // AES-CBC Example (best for file encryption)
  WriteLn('AES-256-CBC Example (File Encryption):');
  WriteLn('------------------------------------');
  AESEncryptedCBC := TCryptoKit.AESCrypt(PlainText, AESKey, AESIV, ks256, abmCBC, amEncrypt);
  WriteLn('AES-CBC encrypted (Base64): ', AESEncryptedCBC);
  
  AESDecryptedCBC := TCryptoKit.AESCrypt(AESEncryptedCBC, AESKey, AESIV, ks256, abmCBC, amDecrypt);
  WriteLn('AES-CBC decrypted: ', AESDecryptedCBC);
  WriteLn;
  
  // AES-CTR Example (best for streaming)
  WriteLn('AES-256-CTR Example (Streaming):');
  WriteLn('------------------------------');
  AESEncryptedCTR := TCryptoKit.AESCrypt(PlainText, AESKey, AESIV, ks256, abmCTR, amEncrypt);
  WriteLn('AES-CTR encrypted (Base64): ', AESEncryptedCTR);
  
  AESDecryptedCTR := TCryptoKit.AESCrypt(AESEncryptedCTR, AESKey, AESIV, ks256, abmCTR, amDecrypt);
  WriteLn('AES-CTR decrypted: ', AESDecryptedCTR);
  WriteLn;
  
  // AES-GCM Example (best for network protocols)
  WriteLn('AES-256-GCM Example (Authenticated Encryption):');
  WriteLn('--------------------------------------------');
  WriteLn('Additional Auth Data: ', AAD);
  AESEncryptedGCM := TCryptoKit.AESCrypt(PlainText, AESKey, AESIV, ks256, abmGCM, amEncrypt, AAD);
  WriteLn('AES-GCM encrypted (Base64): ', AESEncryptedGCM);
  
  AESDecryptedGCM := TCryptoKit.AESCrypt(AESEncryptedGCM, AESKey, AESIV, ks256, abmGCM, amDecrypt, AAD);
  WriteLn('AES-GCM decrypted: ', AESDecryptedGCM);
  WriteLn;
  WriteLn('Note: GCM mode provides both confidentiality and authenticity.');
  WriteLn('If the AAD or encrypted data is tampered with, decryption will fail.');
  
  ReadLn;
end.

function StringToHex(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + IntToHex(Ord(S[I]), 2);
end; 