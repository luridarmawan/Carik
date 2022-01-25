unit main;

{$mode objfpc}{$H+}

interface

uses
  direct_handler,
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler, 
    database_lib, string_helpers, dateutils, datetime_helpers;

type

  { TCarikModule }

  TCarikModule = class(TCarikHandler)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function OnMessageHandler(AMessage: string; var Handled: boolean): string;
    function OnErrorHandler(AMessage: string; var Handled: boolean): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

  end;

implementation

uses common;

constructor TCarikModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  OnMessage := @OnMessageHandler;
  OnError := @OnErrorHandler;
end;

destructor TCarikModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TCarikModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest
  );
begin
  Response.ContentType := 'application/json';
end;

function TCarikModule.OnMessageHandler(AMessage: string; var Handled: boolean
  ): string;
begin
  //Result := 'echo: ' + Carik.UserID  + '/' + AMessage;
  //Handled := true;
end;

function TCarikModule.OnErrorHandler(AMessage: string; var Handled: boolean
  ): string;
begin
  //Result := 'your error message: ' + AMessage;
  //Handled:= True;
end;




end.

