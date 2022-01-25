unit main;

{$mode objfpc}{$H+}

interface

uses
  facebook_handler,
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers;

type

  { TFacebookModule }

  TFacebookModule = class(TFacebookHandler)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function OnMessageHandler(AMessage: string; var Handled: boolean): string;
    function OnErrorHandler(AMessage: string; var Handled: boolean): string;
    function OnSpamHandler(AMessage: string; Score: integer;
      var Handled: boolean): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses common;

constructor TFacebookModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  OnMessage := @OnMessageHandler;
  OnError := @OnErrorHandler;
  OnSpam := @OnSpamHandler;
end;

destructor TFacebookModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TFacebookModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

function TFacebookModule.OnMessageHandler(AMessage: string;
  var Handled: boolean): string;
begin
  //Result := 'echo: ' + Carik.UserID  + '/' + AMessage;
  //Handled := true;
end;

function TFacebookModule.OnErrorHandler(AMessage: string;
  var Handled: boolean): string;
begin
  //Result := 'your error message: ' + AMessage;
  //Handled:= True;
end;

function TFacebookModule.OnSpamHandler(AMessage: string; Score: integer;
  var Handled: boolean): string;
begin

end;

end.



