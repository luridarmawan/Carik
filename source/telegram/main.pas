unit main;

{$mode objfpc}{$H+}

interface

uses
  telegram_handler,
  Classes, SysUtils, fpcgi, fpjson, HTTPDefs, fastplaz_handler,
  dateutils, string_helpers, datetime_helpers;

type

  { TTelegramModule }

  TTelegramModule = class(TTelegramHandler)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function OnMessageHandler(AMessage: string; var Handled: boolean): string;
    function OnErrorHandler(AMessage: string; var Handled: boolean): string;
    function OnSpamHandler(AMessage: string; Score: integer; var Handled: boolean): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses common;

constructor TTelegramModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  OnMessage := @OnMessageHandler;
  OnError := @OnErrorHandler;
  OnSpam := @OnSpamHandler;
end;

destructor TTelegramModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TTelegramModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

function TTelegramModule.OnMessageHandler(AMessage: string;
  var Handled: boolean): string;
begin
  //Result := 'echo: ' + Carik.UserID  + '/' + AMessage;
  //Handled := true;

  Result := '';
end;

function TTelegramModule.OnErrorHandler(AMessage: string; var Handled: boolean): string;
begin
  //Result := 'your error message: ' + AMessage;
  //Handled:= True;

  Result := '';
end;

function TTelegramModule.OnSpamHandler(AMessage: string; Score: integer;
  var Handled: boolean): string;
var
  suffixMessage: string;
begin
  suffixMessage := TELEGRAM.GroupAdminList(TELEGRAM.ChatID);
  //suffixMessage := '\n@' + suffixMessage.Replace(',', ', @');

  Result := 'Pesan ini terdeteksi sebagai spamming, abaikan jika bukan.';
  if TELEGRAM.AdminListAsJson.Count > 0 then
  begin
    Result := Result + '\nSaya colek admin: ' + suffixMessage;
  end
  else
  begin
    Result := Result + '\nSaya tidak menemukan ada admin di sini.';
  end;
  Result := Result + '\n_('+Score.ToString+')_';
  Handled:= True;
end;

end.





