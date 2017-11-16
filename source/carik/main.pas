unit main;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule,
  fpjson, RegExpr,
  carik_controller, simplebot_controller, logutil_lib, resiibacor_integration,
  clarifai_integration, telegram_integration, googleplacesearch_integration,
  movie_controller, currencyibacor_integration,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib;

const
  BOTNAME_DEFAULT = 'Carik';
  CLARIFAI_TOKEN = 'clarifai/token';
  TELEGRAM_TOKEN = 'telegram/token';

type

  { TMainModule }

  TMainModule = class(TCarikWebModule)
  private
    forceRespond: boolean;
    jsonData: TJSONData;

    FInvitedFirstName, FInvitedUserName: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    function OnErrorHandler(const Message: string): string;
  end;

implementation

uses json_lib, common;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;

  Carik := TCarikController.Create;
end;

destructor TMainModule.Destroy;
begin
  Carik.Free;
  inherited Destroy;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TMainModule.Get;
begin
  Response.Content := '{}';
end;

// POST Method Handler
// CURL example:
//   curl "http://local-carik.fastplaz.com/ai/" -X POST -d '{"message":{"message_id":0,"chat":{"id":0},"text":"Hi"}}'
procedure TMainModule.Post;
var
  s, text_response: string;
  chatID, chatType, _userID, fullName, userName, telegramToken: string;
  i, j: integer;
  updateID, lastUpdateID: longint;
  _regex: TRegExpr;
begin
  updateID := 0;
  forceRespond := False;

  // telegram style
  //   {"message":{"message_id":0,"text":"Hi","chat":{"id":0}}}
  try
    jsonData := GetJSON(Request.Content);
    try
      Text := jsonData.GetPath('message.text').AsString;
    except
    end;
    if Text = 'False' then
      Text := '';
    updateID := jsonData.GetPath('update_id').AsInteger;
    messageID := jsonData.GetPath('message.message_id').AsString;
    chatID := jsonData.GetPath('message.chat.id').AsString;
    chatType := jsonData.GetPath('message.chat.type').AsString;
    try
      _userID := jsonData.GetPath('message.from.id').AsString;
      userName := jsonData.GetPath('message.from.username').AsString;
      fullName := trim(jsonData.GetPath('message.from.first_name').AsString +
        ' ' + jsonData.GetPath('message.from.last_name').AsString);
      if fullName = '' then
        fullName := userName;
    except
    end;
  except
  end;

  // maybe submitted from post data
  if Text = '' then
    Text := _POST['text'];

  // CarikBOT isRecording
  Carik.UserID := _userID;
  Carik.UserName := userName;
  Carik.FullName := fullName;
  Carik.GroupChatID := chatID;
  try
    Carik.GroupName := jsonData.GetPath('message.chat.title').AsString;
  except
  end;

  // remove mention from text
  Text := LowerCase(Text);
  {
  if Pos('@' + BOTNAME_DEFAULT, Text) = 1 then
  begin
    Text := StringReplace(Text, '@' + BOTNAME_DEFAULT + 'Bot', '', [rfReplaceAll, rfIgnoreCase]);
  end;
  s := '@' + Config[_AI_CONFIG_NAME] + 'bot';
  s := LowerCase(s);
  if Pos(s, Text) = 1 then
    Text := StringReplace(Text, s, '', [rfReplaceAll, rfIgnoreCase]);
  }
  Text := StringReplace(Text, '@' + BOTNAME_DEFAULT + 'Bot', '',
    [rfReplaceAll, rfIgnoreCase]);
  Text := Trim(Text);
  if Text = '' then
    Exit;

  // Main AI BOT
  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.TrimMessage := True;

  // TODO: REMOVE - force
  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := True;

  SimpleBOT.chatID := chatID;
  if userName <> '' then
  begin
    SimpleBOT.UserData['Name'] := userName;
    SimpleBOT.UserData['FullName'] := fullName;
  end;

  BotInit;
  Response.Content := ProcessText(Text);


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

  //---
  SimpleBOT.Free;
  Response.ContentType := 'application/json';
end;



function TMainModule.OnErrorHandler(const Message: string): string;
var
  s: string;
begin
  s := Trim(Message);
  s := StringReplace(SimpleBOT.GetResponse('InginTahu', ''), '%word%',
    s, [rfReplaceAll]);
  Result := s;


  // simpan message ke DB, untuk dipelajari oleh AI

end;



end.
