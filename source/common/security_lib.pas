unit security_lib;
{
  Helper keamanan Carik.

  Config (config.json):
    "security": {
      "api_token": "<token panjang & acak>",
      "allowed_hosts": "api.carik.id,carik.id",
      "allow_external_commands": false
    }

  - security/api_token     : wajib untuk endpoint admin (/command, /loader).
                             Bila kosong, semua request ke endpoint tsb DITOLAK.
  - security/allowed_hosts : daftar host (dipisah koma) untuk request keluar yang
                             URL-nya berasal dari data dinamis (ExecCommand, submit form).
                             Subdomain ikut diizinkan. Bila kosong, semua host publik
                             http/https diizinkan; host lokal/privat SELALU diblokir.
  - security/allow_external_commands : (default false) izinkan command post-rich:/json-rich:
                             dari respons ExternalNLP/LLM. Hanya berlaku bila allowed_hosts
                             diisi; host tujuan wajib ada di allowlist.
  - systems/debug          : parameter _DEBUG / _FORMDEBUG hanya berlaku bila true.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, URIParser, common;

const
  SECURITY_API_TOKEN = 'security/api_token';
  SECURITY_ALLOWED_HOSTS = 'security/allowed_hosts';
  SECURITY_ALLOW_EXTERNAL_COMMANDS = 'security/allow_external_commands';
  SECURITY_DEBUG = 'systems/debug';

// perbandingan string dengan waktu konstan (terhadap isi)
function SecureEquals(const A, B: string): boolean;

// True bila salah satu token yang dikirim cocok dengan security/api_token.
// AAuthorization: nilai header Authorization ("Bearer <token>")
function IsValidApiToken(const AAuthorization, ATokenHeader, AQueryToken: string): boolean;

// True bila URL aman untuk di-request dari server (anti SSRF)
function IsSafeOutboundURL(const AURL: string): boolean;

// True bila command (post-rich:/json-rich:) dari respons ExternalNLP/LLM boleh dijalankan:
// security/allow_external_commands = true DAN security/allowed_hosts tidak kosong
// (host tujuan kemudian wajib ada di allowlist lewat IsSafeOutboundURL)
function IsExternalCommandAllowed: boolean;

// True bila parameter debug dari query string boleh berlaku
function IsDebugAllowed: boolean;

// hanya huruf, angka, '-' dan '_' (untuk nama file/ID dari input)
function IsSafeIdentifier(const AValue: string): boolean;

implementation

function SecureEquals(const A, B: string): boolean;
var
  i, diff: integer;
begin
  Result := False;
  if Length(A) <> Length(B) then
    Exit;
  diff := 0;
  for i := 1 to Length(A) do
    diff := diff or (Ord(A[i]) xor Ord(B[i]));
  Result := diff = 0;
end;

function configString(const AKey: string): string;
begin
  Result := '';
  try
    Result := Trim(string(Config.GetValue(UnicodeString(AKey), UnicodeString(''))));
  except
    Result := '';
  end;
end;

function IsValidApiToken(const AAuthorization, ATokenHeader, AQueryToken: string): boolean;
var
  expected, s: string;
begin
  Result := False;
  expected := configString(SECURITY_API_TOKEN);
  if expected = '' then
    Exit; // fail-closed: token belum dikonfigurasi

  s := Trim(AAuthorization);
  if AnsiStartsText('Bearer ', s) then
  begin
    s := Trim(Copy(s, 8, Length(s)));
    if (s <> '') and SecureEquals(s, expected) then
      Exit(True);
  end;
  s := Trim(ATokenHeader);
  if (s <> '') and SecureEquals(s, expected) then
    Exit(True);
  s := Trim(AQueryToken);
  if (s <> '') and SecureEquals(s, expected) then
    Exit(True);
end;

function isAllDigits(const AValue: string): boolean;
var
  c: char;
begin
  Result := AValue <> '';
  for c in AValue do
    if not (c in ['0'..'9']) then
      Exit(False);
end;

// host berupa IPv4 desimal a.b.c.d ? (bentuk lain: oktal/hex/desimal tunggal ditolak di pemanggil)
function parseIPv4(const AHost: string; out o1, o2, o3, o4: integer): boolean;
var
  parts: TStringArray;
  i, v: integer;
  oct: array[0..3] of integer;
begin
  Result := False;
  o1 := 0; o2 := 0; o3 := 0; o4 := 0;
  parts := AHost.Split(['.']);
  if Length(parts) <> 4 then
    Exit;
  for i := 0 to 3 do
  begin
    if (parts[i] = '') or (Length(parts[i]) > 3) then
      Exit;
    // nol di depan bisa ditafsirkan oktal oleh resolver
    if (Length(parts[i]) > 1) and (parts[i][1] = '0') then
      Exit;
    if not isAllDigits(parts[i]) then
      Exit;
    if not TryStrToInt(parts[i], v) then
      Exit;
    if (v < 0) or (v > 255) then
      Exit;
    oct[i] := v;
  end;
  o1 := oct[0]; o2 := oct[1]; o3 := oct[2]; o4 := oct[3];
  Result := True;
end;

function isPrivateIPv4(o1, o2, o3, o4: integer): boolean;
begin
  Result :=
    (o1 = 0) or                                   // 0.0.0.0/8
    (o1 = 10) or                                  // 10.0.0.0/8
    (o1 = 127) or                                 // loopback
    ((o1 = 100) and (o2 >= 64) and (o2 <= 127)) or // CGNAT 100.64.0.0/10
    ((o1 = 169) and (o2 = 254)) or                // link-local / cloud metadata
    ((o1 = 172) and (o2 >= 16) and (o2 <= 31)) or // 172.16.0.0/12
    ((o1 = 192) and (o2 = 168)) or                // 192.168.0.0/16
    ((o1 = 192) and (o2 = 0) and (o3 = 0)) or     // 192.0.0.0/24
    (o1 >= 224);                                  // multicast & reserved
end;

function isHostAllowedByList(const AHost: string): boolean;
var
  list: TStringArray;
  item: string;
begin
  Result := True;
  item := configString(SECURITY_ALLOWED_HOSTS);
  if item = '' then
    Exit; // allowlist tidak diset
  Result := False;
  list := LowerCase(item).Split([',', ';', ' ']);
  for item in list do
  begin
    if Trim(item) = '' then
      Continue;
    if (AHost = Trim(item)) or AnsiEndsStr('.' + Trim(item), AHost) then
      Exit(True);
  end;
end;

function IsSafeOutboundURL(const AURL: string): boolean;
var
  uri: TURI;
  host, scheme, lastLabel: string;
  o1, o2, o3, o4: integer;
begin
  Result := False;
  if Trim(AURL) = '' then
    Exit;
  try
    uri := ParseURI(Trim(AURL), False);
  except
    Exit;
  end;

  scheme := LowerCase(uri.Protocol);
  if (scheme <> 'http') and (scheme <> 'https') then
    Exit;

  host := LowerCase(Trim(uri.Host));
  while AnsiEndsStr('.', host) do
    host := Copy(host, 1, Length(host) - 1);
  if host = '' then
    Exit;

  // IPv6 literal & karakter aneh
  if (Pos('[', host) > 0) or (Pos(':', host) > 0) or (Pos('%', host) > 0)
    or (Pos('\', host) > 0) or (Pos('@', host) > 0) then
    Exit;

  // nama host internal
  if (host = 'localhost') or AnsiEndsStr('.localhost', host)
    or AnsiEndsStr('.local', host) or AnsiEndsStr('.internal', host)
    or AnsiEndsStr('.lan', host) then
    Exit;

  // single label (mis. "redis", "2130706433") tidak diizinkan
  if Pos('.', host) = 0 then
    Exit;

  // TLD asli tidak pernah numerik: bila label terakhir numerik/hex, host harus
  // IPv4 desimal baku & publik (tolak oktal, hex, bentuk pendek seperti 0x7f.1)
  lastLabel := Copy(host, RPos('.', host) + 1, Length(host));
  if isAllDigits(lastLabel) or AnsiStartsStr('0x', lastLabel) then
  begin
    if not parseIPv4(host, o1, o2, o3, o4) then
      Exit;
    if isPrivateIPv4(o1, o2, o3, o4) then
      Exit;
  end;

  Result := isHostAllowedByList(host);
end;

function IsExternalCommandAllowed: boolean;
var
  enabled: boolean;
begin
  Result := False;
  enabled := False;
  try
    enabled := Config.GetValue(UnicodeString(SECURITY_ALLOW_EXTERNAL_COMMANDS), False);
  except
    enabled := False;
  end;
  if not enabled then
    Exit;
  // tanpa allowlist, command eksternal tetap ditolak
  Result := configString(SECURITY_ALLOWED_HOSTS) <> '';
end;

function IsDebugAllowed: boolean;
begin
  Result := False;
  try
    Result := Config.GetValue(UnicodeString(SECURITY_DEBUG), False);
  except
    Result := False;
  end;
end;

function IsSafeIdentifier(const AValue: string): boolean;
var
  c: char;
begin
  Result := False;
  if (AValue = '') or (Length(AValue) > 64) then
    Exit;
  for c in AValue do
    if not (c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then
      Exit;
  Result := True;
end;

end.
