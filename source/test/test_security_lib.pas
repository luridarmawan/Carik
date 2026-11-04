unit test_security_lib;
{
  Test security_lib (source/common/security_lib.pas):
    SecureEquals, IsSafeIdentifier, IsSafeOutboundURL (anti SSRF),
    IsValidApiToken, IsExternalCommandAllowed, IsDebugAllowed.

  Fungsi yang membaca config diuji dengan config sementara: tiap test menulis
  file JSON di direktori temp lalu mengarahkan Config global ke file itu.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, common, security_lib;

type

  { TSecurityLibTestBase: config sementara per test }

  TSecurityLibTestBase = class(TTestCase)
  private
    FDir: string;
    FOldConfigFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure UseConfig(const AJson: string);
  end;

  { TSecureEqualsTest }

  TSecureEqualsTest = class(TTestCase)
  published
    procedure TestSame;
    procedure TestDifferent;
    procedure TestDifferentLength;
    procedure TestEmpty;
  end;

  { TSafeIdentifierTest }

  TSafeIdentifierTest = class(TTestCase)
  published
    procedure TestValid;
    procedure TestEmpty;
    procedure TestTooLong;
    procedure TestTraversal;
    procedure TestInvalidChars;
  end;

  { TSafeOutboundURLTest: tanpa allowlist }

  TSafeOutboundURLTest = class(TSecurityLibTestBase)
  protected
    procedure SetUp; override;
  published
    procedure TestPublicHostsAllowed;
    procedure TestPublicIPAllowed;
    procedure TestOnlyHttpAndHttps;
    procedure TestEmptyOrInvalid;
    procedure TestLoopbackBlocked;
    procedure TestPrivateRangesBlocked;
    procedure TestCloudMetadataBlocked;
    procedure TestInternalNamesBlocked;
    procedure TestSingleLabelBlocked;
    procedure TestNonCanonicalIPBlocked;
    procedure TestIPv6LiteralBlocked;
    procedure TestUserinfoTrickBlocked;
  end;

  { TAllowedHostsTest: dengan security/allowed_hosts }

  TAllowedHostsTest = class(TSecurityLibTestBase)
  published
    procedure TestExactHost;
    procedure TestSubdomain;
    procedure TestOtherHostRejected;
    procedure TestSuffixTrickRejected;
    procedure TestMultipleEntries;
    procedure TestCaseInsensitive;
    procedure TestPrivateStillBlocked;
  end;

  { TApiTokenTest }

  TApiTokenTest = class(TSecurityLibTestBase)
  published
    procedure TestNoTokenConfiguredRejectsAll;
    procedure TestBearerHeader;
    procedure TestBearerCaseInsensitive;
    procedure TestTokenHeader;
    procedure TestQueryToken;
    procedure TestWrongToken;
    procedure TestPrefixOfTokenRejected;
    procedure TestEmptyProvidedRejected;
    procedure TestAuthorizationWithoutBearerRejected;
  end;

  { TExternalCommandTest }

  TExternalCommandTest = class(TSecurityLibTestBase)
  published
    procedure TestDefaultFalse;
    procedure TestDisabledWithHosts;
    procedure TestEnabledWithoutHosts;
    procedure TestEnabledWithHosts;
  end;

  { TDebugAllowedTest }

  TDebugAllowedTest = class(TSecurityLibTestBase)
  published
    procedure TestDefaultFalse;
    procedure TestDebugFalse;
    procedure TestDebugTrue;
  end;

implementation

{ TSecurityLibTestBase }

procedure TSecurityLibTestBase.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'carik_security_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FOldConfigFile := Config.Filename;
  UseConfig('{}');
end;

procedure TSecurityLibTestBase.TearDown;
begin
  Config.Filename := FOldConfigFile;
  DeleteFile(FDir + 'config.json');
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TSecurityLibTestBase.UseConfig(const AJson: string);
var
  lst: TStringList;
  fileName: string;
begin
  fileName := FDir + 'config.json';
  lst := TStringList.Create;
  try
    lst.Text := AJson;
    lst.SaveToFile(fileName);
  finally
    lst.Free;
  end;
  // paksa reload walau nama file sama
  Config.Filename := '';
  Config.Filename := fileName;
end;

{ TSecureEqualsTest }

procedure TSecureEqualsTest.TestSame;
begin
  AssertTrue(SecureEquals('abc', 'abc'));
  AssertTrue(SecureEquals('token-123456', 'token-123456'));
end;

procedure TSecureEqualsTest.TestDifferent;
begin
  AssertFalse(SecureEquals('abc', 'abd'));
  AssertFalse(SecureEquals('abc', 'ABC'));
end;

procedure TSecureEqualsTest.TestDifferentLength;
begin
  AssertFalse(SecureEquals('abc', 'abcd'));
  AssertFalse(SecureEquals('abcd', 'abc'));
end;

procedure TSecureEqualsTest.TestEmpty;
begin
  AssertTrue(SecureEquals('', ''));
  AssertFalse(SecureEquals('', 'a'));
end;

{ TSafeIdentifierTest }

procedure TSafeIdentifierTest.TestValid;
begin
  AssertTrue(IsSafeIdentifier('carik'));
  AssertTrue(IsSafeIdentifier('bot_01-A'));
  AssertTrue(IsSafeIdentifier(StringOfChar('a', 64)));
end;

procedure TSafeIdentifierTest.TestEmpty;
begin
  AssertFalse(IsSafeIdentifier(''));
end;

procedure TSafeIdentifierTest.TestTooLong;
begin
  AssertFalse(IsSafeIdentifier(StringOfChar('a', 65)));
end;

procedure TSafeIdentifierTest.TestTraversal;
begin
  AssertFalse(IsSafeIdentifier('../x'));
  AssertFalse(IsSafeIdentifier('..'));
  AssertFalse(IsSafeIdentifier('a/b'));
  AssertFalse(IsSafeIdentifier('a\b'));
end;

procedure TSafeIdentifierTest.TestInvalidChars;
begin
  AssertFalse('spasi', IsSafeIdentifier('a b'));
  AssertFalse('titik', IsSafeIdentifier('a.json'));
  AssertFalse('titik dua', IsSafeIdentifier('c:x'));
  AssertFalse('null', IsSafeIdentifier('a'#0'b'));
end;

{ TSafeOutboundURLTest }

procedure TSafeOutboundURLTest.SetUp;
begin
  inherited SetUp;
  UseConfig('{"security":{"allowed_hosts":""}}');
end;

procedure TSafeOutboundURLTest.TestPublicHostsAllowed;
begin
  AssertTrue(IsSafeOutboundURL('https://api.carik.id/form/submit'));
  AssertTrue(IsSafeOutboundURL('http://example.com/path?a=1'));
  AssertTrue('port', IsSafeOutboundURL('https://sub.example.co.id:8443/x'));
  AssertTrue('nama mirip hex', IsSafeOutboundURL('https://cafe.bad/'));
  AssertTrue('titik di akhir', IsSafeOutboundURL('https://example.com./'));
end;

procedure TSafeOutboundURLTest.TestPublicIPAllowed;
begin
  AssertTrue(IsSafeOutboundURL('https://8.8.8.8/x'));
  AssertTrue(IsSafeOutboundURL('http://1.1.1.1/'));
end;

procedure TSafeOutboundURLTest.TestOnlyHttpAndHttps;
begin
  AssertFalse('file', IsSafeOutboundURL('file:///etc/passwd'));
  AssertFalse('gopher', IsSafeOutboundURL('gopher://example.com/'));
  AssertFalse('ftp', IsSafeOutboundURL('ftp://example.com/'));
  AssertFalse('dict', IsSafeOutboundURL('dict://example.com:11211/'));
end;

procedure TSafeOutboundURLTest.TestEmptyOrInvalid;
begin
  AssertFalse(IsSafeOutboundURL(''));
  AssertFalse(IsSafeOutboundURL('   '));
  AssertFalse(IsSafeOutboundURL('not a url'));
  AssertFalse(IsSafeOutboundURL('http://'));
end;

procedure TSafeOutboundURLTest.TestLoopbackBlocked;
begin
  AssertFalse(IsSafeOutboundURL('http://127.0.0.1/'));
  AssertFalse(IsSafeOutboundURL('http://127.10.20.30:8080/'));
  AssertFalse(IsSafeOutboundURL('http://localhost/'));
  AssertFalse(IsSafeOutboundURL('http://localhost:8080/'));
  AssertFalse(IsSafeOutboundURL('http://foo.localhost/'));
  AssertFalse(IsSafeOutboundURL('http://0.0.0.0/'));
end;

procedure TSafeOutboundURLTest.TestPrivateRangesBlocked;
begin
  AssertFalse('10/8', IsSafeOutboundURL('http://10.1.2.3/'));
  AssertFalse('172.16/12 awal', IsSafeOutboundURL('http://172.16.0.1/'));
  AssertFalse('172.16/12 akhir', IsSafeOutboundURL('http://172.31.255.255/'));
  AssertFalse('192.168/16', IsSafeOutboundURL('http://192.168.1.1/'));
  AssertFalse('CGNAT', IsSafeOutboundURL('http://100.64.0.1/'));
  AssertFalse('multicast', IsSafeOutboundURL('http://224.0.0.1/'));
  // batas luar rentang privat tetap publik
  AssertTrue('172.15', IsSafeOutboundURL('http://172.15.0.1/'));
  AssertTrue('172.32', IsSafeOutboundURL('http://172.32.0.1/'));
end;

procedure TSafeOutboundURLTest.TestCloudMetadataBlocked;
begin
  AssertFalse(IsSafeOutboundURL('http://169.254.169.254/latest/meta-data/'));
  AssertFalse(IsSafeOutboundURL('http://metadata.google.internal/'));
end;

procedure TSafeOutboundURLTest.TestInternalNamesBlocked;
begin
  AssertFalse(IsSafeOutboundURL('http://printer.local/'));
  AssertFalse(IsSafeOutboundURL('http://db.internal/'));
  AssertFalse(IsSafeOutboundURL('http://router.lan/'));
end;

procedure TSafeOutboundURLTest.TestSingleLabelBlocked;
begin
  AssertFalse(IsSafeOutboundURL('http://redis:6379/'));
  AssertFalse(IsSafeOutboundURL('http://intranet/'));
end;

procedure TSafeOutboundURLTest.TestNonCanonicalIPBlocked;
begin
  AssertFalse('desimal', IsSafeOutboundURL('http://2130706433/'));
  AssertFalse('hex pendek', IsSafeOutboundURL('http://0x7f.1/'));
  AssertFalse('oktal', IsSafeOutboundURL('http://0177.0.0.1/'));
  AssertFalse('nol di depan', IsSafeOutboundURL('http://010.0.0.1/'));
  AssertFalse('bentuk pendek', IsSafeOutboundURL('http://127.1/'));
  AssertFalse('oktet > 255', IsSafeOutboundURL('http://8.8.8.256/'));
end;

procedure TSafeOutboundURLTest.TestIPv6LiteralBlocked;
begin
  AssertFalse(IsSafeOutboundURL('http://[::1]/'));
  AssertFalse(IsSafeOutboundURL('http://[::ffff:127.0.0.1]/'));
end;

procedure TSafeOutboundURLTest.TestUserinfoTrickBlocked;
begin
  AssertFalse(IsSafeOutboundURL('http://evil.com@127.0.0.1/'));
  AssertFalse(IsSafeOutboundURL('http://user:pass@169.254.169.254/'));
end;

{ TAllowedHostsTest }

procedure TAllowedHostsTest.TestExactHost;
begin
  UseConfig('{"security":{"allowed_hosts":"api.carik.id"}}');
  AssertTrue(IsSafeOutboundURL('https://api.carik.id/x'));
end;

procedure TAllowedHostsTest.TestSubdomain;
begin
  UseConfig('{"security":{"allowed_hosts":"carik.id"}}');
  AssertTrue(IsSafeOutboundURL('https://api.carik.id/x'));
  AssertTrue(IsSafeOutboundURL('https://a.b.carik.id/x'));
end;

procedure TAllowedHostsTest.TestOtherHostRejected;
begin
  UseConfig('{"security":{"allowed_hosts":"api.carik.id"}}');
  AssertFalse(IsSafeOutboundURL('https://evil.com/x'));
  AssertFalse('host induk', IsSafeOutboundURL('https://carik.id/x'));
end;

procedure TAllowedHostsTest.TestSuffixTrickRejected;
begin
  UseConfig('{"security":{"allowed_hosts":"carik.id"}}');
  AssertFalse('akhiran tanpa titik', IsSafeOutboundURL('https://evilcarik.id/x'));
  AssertFalse('allowlist sebagai subdomain', IsSafeOutboundURL('https://carik.id.evil.com/x'));
end;

procedure TAllowedHostsTest.TestMultipleEntries;
begin
  UseConfig('{"security":{"allowed_hosts":"api.carik.id, example.com;foo.org"}}');
  AssertTrue(IsSafeOutboundURL('https://api.carik.id/'));
  AssertTrue(IsSafeOutboundURL('https://example.com/'));
  AssertTrue(IsSafeOutboundURL('https://foo.org/'));
  AssertFalse(IsSafeOutboundURL('https://bar.org/'));
end;

procedure TAllowedHostsTest.TestCaseInsensitive;
begin
  UseConfig('{"security":{"allowed_hosts":"API.Carik.ID"}}');
  AssertTrue(IsSafeOutboundURL('https://api.carik.id/'));
  AssertTrue(IsSafeOutboundURL('https://API.CARIK.ID/'));
end;

procedure TAllowedHostsTest.TestPrivateStillBlocked;
begin
  // allowlist tidak bisa membuka host lokal/privat
  UseConfig('{"security":{"allowed_hosts":"localhost,127.0.0.1,169.254.169.254"}}');
  AssertFalse(IsSafeOutboundURL('http://localhost/'));
  AssertFalse(IsSafeOutboundURL('http://127.0.0.1/'));
  AssertFalse(IsSafeOutboundURL('http://169.254.169.254/'));
end;

{ TApiTokenTest }

const
  TOKEN = 'test-token-123456';
  TOKEN_CONFIG = '{"security":{"api_token":"' + TOKEN + '"}}';

procedure TApiTokenTest.TestNoTokenConfiguredRejectsAll;
begin
  // fail-closed: tanpa security/api_token semua ditolak
  AssertFalse('kosong', IsValidApiToken('', '', ''));
  AssertFalse('bearer', IsValidApiToken('Bearer x', '', ''));
  AssertFalse('header', IsValidApiToken('', 'x', ''));
  AssertFalse('query', IsValidApiToken('', '', 'x'));
  UseConfig('{"security":{"api_token":"   "}}');
  AssertFalse('token spasi saja', IsValidApiToken('', '', ''));
end;

procedure TApiTokenTest.TestBearerHeader;
begin
  UseConfig(TOKEN_CONFIG);
  AssertTrue(IsValidApiToken('Bearer ' + TOKEN, '', ''));
  AssertTrue('spasi ekstra', IsValidApiToken('  Bearer   ' + TOKEN + '  ', '', ''));
end;

procedure TApiTokenTest.TestBearerCaseInsensitive;
begin
  UseConfig(TOKEN_CONFIG);
  AssertTrue(IsValidApiToken('bearer ' + TOKEN, '', ''));
end;

procedure TApiTokenTest.TestTokenHeader;
begin
  UseConfig(TOKEN_CONFIG);
  AssertTrue(IsValidApiToken('', TOKEN, ''));
end;

procedure TApiTokenTest.TestQueryToken;
begin
  UseConfig(TOKEN_CONFIG);
  AssertTrue(IsValidApiToken('', '', TOKEN));
end;

procedure TApiTokenTest.TestWrongToken;
begin
  UseConfig(TOKEN_CONFIG);
  AssertFalse(IsValidApiToken('Bearer salah', 'salah', 'salah'));
end;

procedure TApiTokenTest.TestPrefixOfTokenRejected;
begin
  UseConfig(TOKEN_CONFIG);
  AssertFalse('prefix', IsValidApiToken('', 'test-token', ''));
  AssertFalse('lebih panjang', IsValidApiToken('', TOKEN + 'x', ''));
end;

procedure TApiTokenTest.TestEmptyProvidedRejected;
begin
  UseConfig(TOKEN_CONFIG);
  AssertFalse(IsValidApiToken('', '', ''));
  AssertFalse(IsValidApiToken('Bearer ', '  ', ' '));
end;

procedure TApiTokenTest.TestAuthorizationWithoutBearerRejected;
begin
  UseConfig(TOKEN_CONFIG);
  AssertFalse(IsValidApiToken(TOKEN, '', ''));
  AssertFalse(IsValidApiToken('Basic ' + TOKEN, '', ''));
end;

{ TExternalCommandTest }

procedure TExternalCommandTest.TestDefaultFalse;
begin
  AssertFalse(IsExternalCommandAllowed);
end;

procedure TExternalCommandTest.TestDisabledWithHosts;
begin
  UseConfig('{"security":{"allow_external_commands":false,"allowed_hosts":"api.carik.id"}}');
  AssertFalse(IsExternalCommandAllowed);
end;

procedure TExternalCommandTest.TestEnabledWithoutHosts;
begin
  // tanpa allowlist, command eksternal tetap ditolak
  UseConfig('{"security":{"allow_external_commands":true,"allowed_hosts":""}}');
  AssertFalse(IsExternalCommandAllowed);
  UseConfig('{"security":{"allow_external_commands":true}}');
  AssertFalse(IsExternalCommandAllowed);
end;

procedure TExternalCommandTest.TestEnabledWithHosts;
begin
  UseConfig('{"security":{"allow_external_commands":true,"allowed_hosts":"api.carik.id"}}');
  AssertTrue(IsExternalCommandAllowed);
end;

{ TDebugAllowedTest }

procedure TDebugAllowedTest.TestDefaultFalse;
begin
  AssertFalse(IsDebugAllowed);
end;

procedure TDebugAllowedTest.TestDebugFalse;
begin
  UseConfig('{"systems":{"debug":false}}');
  AssertFalse(IsDebugAllowed);
end;

procedure TDebugAllowedTest.TestDebugTrue;
begin
  UseConfig('{"systems":{"debug":true}}');
  AssertTrue(IsDebugAllowed);
end;

initialization
  RegisterTest(TSecureEqualsTest);
  RegisterTest(TSafeIdentifierTest);
  RegisterTest(TSafeOutboundURLTest);
  RegisterTest(TAllowedHostsTest);
  RegisterTest(TApiTokenTest);
  RegisterTest(TExternalCommandTest);
  RegisterTest(TDebugAllowedTest);

end.
