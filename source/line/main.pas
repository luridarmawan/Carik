unit main;

{$mode objfpc}{$H+}

interface

uses
  line_handler,
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers;

type

  { TLineModule }

  TLineModule = class(TLineHandler)
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

constructor TLineModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  OnMessage := @OnMessageHandler;
  OnError := @OnErrorHandler;
  OnSpam := @OnSpamHandler;
end;

destructor TLineModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TLineModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

function TLineModule.OnMessageHandler(AMessage: string;
  var Handled: boolean): string;
begin
  Result := '';
  //Result := 'echo: ' + Carik.UserID  + '/' + AMessage;
  //Handled := true;
end;

function TLineModule.OnErrorHandler(AMessage: string;
  var Handled: boolean): string;
begin
  //Result := 'your error message: ' + AMessage;
  //Handled:= True;
end;

function TLineModule.OnSpamHandler(AMessage: string; Score: integer;
  var Handled: boolean): string;
begin

end;

end.



