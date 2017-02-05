unit main;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule, logutil_lib,
  line_integration, simplebot_controller, notulen_controller,
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, database_lib;

const
  BOTNAME_DEFAULT = 'Carik';

type

  { TMainModule }

  TMainModule = class(TCarikWebModule)
  private
    LINE: TLineIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    ReplyToken, UserID: string;
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
  LINE := TLineIntegration.Create;
  LINE.BotName := BOTNAME_DEFAULT;
  LINE.Token := Config['line/default/token'];
end;

destructor TMainModule.Destroy;
begin
  Carik.Free;
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
  Response.Content := '{}';
end;

// POST Method Handler
// CURL example:
//   curl -X POST -H "Authorization: Basic dW5hbWU6cGFzc3dvcmQ=" "yourtargeturl"
procedure TMainModule.Post;
var
  s: string;
begin
  LINE.RequestContent := Request.Content;
  LogUtil.Add(Request.Content, 'LINE');

  if LINE.isMessage then
  begin
    Text := LINE.Text;
  end
  else
  begin
    if not LINE.isJoinToGroup then
      Exit;
    Text := '/invitation carikbot';
  end;

  if LINE.isGroup then
  begin
    if not LINE.isMentioned then
      Exit;
    SimpleBOT.FirstSessionResponse := True;
    SimpleBOT.SecondSessionResponse := True;
    Carik.GroupChatID := LINE.GroupID;
    Carik.GroupName := LINE.GroupName;
  end;

  if LINE.isSticker then
  begin
    s := SimpleBOT.GetResponse('LINEEmojiResponse');
    ReplyToken := LINE.ReplyToken;
    LINE.SendSticker(ReplyToken, '1', s);
    Exit;
  end;

  if Text = '' then
    Exit;
  {
  SimpleBOT.UserData['Name'] := userName;
  SimpleBOT.UserData['FullName'] := fullName;
  }
  BotInit;
  Response.Content := ProcessText(Text);

  SimpleBOT.SimpleAI.ResponseText.Text :=
    StringReplace(SimpleBOT.SimpleAI.ResponseText.Text, '\n', #10, [rfReplaceAll]);

  // reply message
  ReplyToken := LINE.ReplyToken;
  LINE.Reply(ReplyToken, SimpleBOT.SimpleAI.ResponseText.Text);

  Response.Content := 'OK';
end;



end.



