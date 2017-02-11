unit main;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule, logutil_lib, telegram_integration,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib;

{$include ../carik.inc}

type

  { TMainModule }

  TMainModule = class(TCarikWebModule)
  private
    forceRespond: boolean;
    TELEGRAM: TTelegramIntegration;

    function isMentioned(Message: string): boolean;
    function isReply: boolean;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    //UserID, ChatID, ChatType,
    MessageID: string;

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

  TELEGRAM := TTelegramIntegration.Create;
  TELEGRAM.Token := Config[TELEGRAM_TOKEN];
end;

destructor TMainModule.Destroy;
begin
  TELEGRAM.Free;
  inherited Destroy;
end;

function TMainModule.isMentioned(Message: string): boolean;
begin
  Result := False;
  if pos('@' + LowerCase(BOTNAME_DEFAULT), Text) > 0 then
    Result := True;
  if pos('Bot', Text) > 0 then    // force dectect as Bot  (____Bot)
    Result := True;
end;

function TMainModule.isReply: boolean;
var
  json: TJSONUtil;
  s: string;
begin
  Result := False;
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Request.Content);
    s := json['message/reply_to_message/from/username'];
    if pos('Bot', s) > 0 then
      Result := True;
  except
  end;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TMainModule.Get;
begin
  Response.ContentType := 'application/json';
  Response.Content := '{}';
end;

// POST Method Handler
// CURL example:
//   curl "http://local-carik.fastplaz.com/ai/" -X POST -d '{"message":{"message_id":0,"chat":{"id":0},"text":"Hi"}}'
procedure TMainModule.Post;
var
  updateID, lastUpdateID: longint;
  j: integer;
  s: string;
begin
  updateID := 0;
  forceRespond := False;
  if AppData.debug then
    LogUtil.Add(Request.Content, 'TELE');

  TELEGRAM.RequestContent := Request.Content;
  updateID := TELEGRAM.UpdateID;

  Text := TELEGRAM.Text;

  //TODO: if emoticons

  Carik.UserID := TELEGRAM.UserID;
  Carik.UserName := TELEGRAM.UserName;
  Carik.FullName := TELEGRAM.FullName;
  Carik.GroupChatID := TELEGRAM.ChatID;
  Carik.GroupName := TELEGRAM.GroupName;
  Carik.IsGroup := TELEGRAM.IsGroup;
  if Carik.IsRecording then
  begin
    Carik.RecordTelegramMessage(Request.Content);
  end;

  // check last telegram session
  lastUpdateID := s2i(_SESSION['UPDATE_ID']);
  if updateID < lastUpdateID then
  begin
    Exit;
  end;
  _SESSION['UPDATE_ID'] := updateID;

  // only for telegram group
  if TELEGRAM.IsGroup then
  begin
    if Text = '' then
      Text := TELEGRAM.ImageCaption;
    if not isReply then
    begin
      if not isMentioned(Text) then
      begin
        if TELEGRAM.IsInvitation then
        begin
          Text := '/invitation ' + TELEGRAM.InvitedUserName + ' ' +
            TELEGRAM.InvitedFullName;
          if TELEGRAM.InvitedFullName = BOTNAME_DEFAULT + 'Bot' then
            Carik.Invited;
        end
        else
        begin
          Response.Content := '{"status":"nomention"}';
          Exit;
        end;
      end;

    end;
  end;//-- if TELEGRAM.IsGroup

  // remove mention from text
  Text := LowerCase(Text);
  Text := StringReplace(Text, '@' + BOTNAME_DEFAULT + 'Bot', '',
    [rfReplaceAll, rfIgnoreCase]);
  Text := Trim(Text);
  if Text = '' then
    Exit;

  SimpleBOT.TrimMessage := True;
  // TODO: REMOVE - force
  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := True;

  SimpleBOT.UserData['Name'] := TELEGRAM.UserName;
  SimpleBOT.UserData['FullName'] := TELEGRAM.FullName;

  MessengerMode := mmTelegram;
  BotInit;
  Response.Content := ProcessText(Text);

  //TODO: rekam pembicaraan sendiri

  if TELEGRAM.IsGroup then
  begin
    if Carik.IsDisabled then
    begin
      if not forceRespond then
      begin
        Response.ContentType := 'application/json';
        Response.Content := '{"status":"silent"}';
        Exit;
      end;
    end;
  end;

  //Exec Command
  if Carik.IsCommand(SimpleBOT.SimpleAI.ResponseText.Text) then
  begin
    SimpleBOT.SimpleAI.ResponseText.Text :=
      Carik.ExecCommand(SimpleBOT.SimpleAI.ResponseText.Text);
    if SimpleBOT.SimpleAI.ResponseText.Text = '' then
      SimpleBOT.SimpleAI.ResponseText.Text :=
        SimpleBOT.GetResponse('DataTidakAdaResponse');
    //TODO: generate user data and object
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;

  MessageID := TELEGRAM.MessageID;
  if SimpleBOT.SimpleAI.Action = '' then // no mention reply, if no 'action'
    MessageID := '';
  if SimpleBOT.SimpleAI.Action = 'telegram_menu' then
    MessageID := '';

  if SimpleBOT.SimpleAI.ResponseText.Count = 0 then
  begin
    Exit;
  end;

  //SimpleBOT.SimpleAI.ResponseText.Add('satu');
  //SimpleBOT.SimpleAI.ResponseText.Add('dua');
  //SimpleBOT.SimpleAI.ResponseText.Add('tiga');

  TELEGRAM.Token := Config[TELEGRAM_TOKEN];
  TELEGRAM.SendMessage(TELEGRAM.ChatID, SimpleBOT.SimpleAI.ResponseText[0], MessageID);

  if SimpleBOT.SimpleAI.ResponseText.Count > 1 then
  begin
    for j := 1 to SimpleBOT.SimpleAI.ResponseText.Count - 1 do
    begin
      s := SimpleBOT.SimpleAI.ResponseText[j];
      if s <> '' then
      begin
        TELEGRAM.SendMessage(TELEGRAM.ChatID, s, '');
      end;
      // TODO: rekam percakapan si BOT

    end;
  end;



  Response.ContentType := 'application/json';
end;



end.
