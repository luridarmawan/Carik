unit main;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule, logutil_lib, simplebot_controller, notulen_controller,
  facebookmessenger_integration,
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, database_lib;

{$include ../carik.inc}

type
  TMainModule = class(TCarikWebModule)
  private
    Facebook: TFacebookMessengerIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses json_lib, common;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  Facebook := TFacebookMessengerIntegration.Create;
  Facebook.BotName := BOTNAME_DEFAULT;
  Facebook.Token := Config[FACEBOOK_TOKEN];
end;

destructor TMainModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  //Response.ContentType := 'application/json';
end;

// GET Method Handler
procedure TMainModule.Get;
begin
  //faebook chalenge
  Response.Content := _GET['hub.challenge'];
end;

// POST Method Handler
procedure TMainModule.Post;
var
  s: string;
begin
  Facebook.RequestContent := Request.Content;
  LogUtil.Add(Request.Content, 'FB');

  Text := Facebook.Text;
  if Text = '' then
    Exit;

  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := False;
  Carik.UserID := Facebook.UserID;

  BotInit;
  Response.Content := ProcessText(Text);

  // send
  SimpleBOT.SimpleAI.ResponseText.Text :=
    StringReplace(SimpleBOT.SimpleAI.ResponseText.Text, '\n', #10, [rfReplaceAll]);
  Facebook.Send(Facebook.UserID, SimpleBOT.SimpleAI.ResponseText.Text);

end;



end.
