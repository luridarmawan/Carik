unit main;

{$mode objfpc}{$H+}

interface

uses
  notulen_controller, simplebot_controller, logutil_lib, resiibacor_integration,
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, database_lib;

const
  BOTNAME_DEFAULT = 'bot';

type

  { TMainModule }

  TMainModule = class(TMyCustomWebModule)
  private
    jsonData: TJSONData;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function defineHandler(const IntentName: string; Params: TStrings): string;
    function resiHandler(const IntentName: string; Params: TStrings): string;

    function isTelegram: boolean;
    function isTelegramGroup: boolean;
    function isMentioned(Text: string): boolean;
    function isReply: boolean;
  public
    Carik: TNotulenController;
    SimpleBOT: TSimpleBotModule;
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

  Carik := TNotulenController.Create;
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
  Text, chatID, chatType, messageID, fullName, userName, telegramToken: string;
  i, j: integer;
  x, updateID, lastUpdateID: longint;
begin
  updateID := 0;

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
      userName := jsonData.GetPath('message.from.username').AsString;
      fullName := jsonData.GetPath('message.from.first_name').AsString +
        ' ' + jsonData.GetPath('message.from.last_name').AsString;
    except
    end;
  except
  end;

  // maybe submitted from post data
  if Text = '' then
    Text := _POST['text'];

  // CarikBOT isRecording
  Carik.UserName := userName;
  try
    Carik.GroupName := jsonData.GetPath('message.chat.title').AsString;
  except
  end;
  if isTelegram then
  begin
    if ((chatType = 'group') or (chatType = 'supergroup')) then
    begin
      if Carik.Recording then
      begin
        Carik.RecordTelegramMessage(Request.Content);
      end;
    end;
  end; // Carik - end

  if isTelegram then
  begin
    if AppData.debug then
      LogUtil.Add(Request.Content, 'input');
    // last message only
    lastUpdateID := s2i(_SESSION['UPDATE_ID']);
    if updateID < lastUpdateID then
    begin
      Exit;
    end;
    _SESSION['UPDATE_ID'] := updateID;

    //if isTelegramGroup then
    if ((chatType = 'group') or (chatType = 'supergroup')) then
    begin
      if not isReply then
      begin
        if (not isMentioned(Text)) then
        begin
          _SESSION['UPDATE_ID'] := updateID;
          Response.Content := 'nop';
          Exit;
        end;
      end;
    end;

  end;// isTelegram

  if Text = '' then
    Exit;

  // remove mention from text
  Text := LowerCase(Text);
  if Pos('@' + BOTNAME_DEFAULT, Text) = 1 then
    Text := StringReplace(Text, '@' + BOTNAME_DEFAULT, '', [rfReplaceAll, rfIgnoreCase]);
  s := '@' + Config[_AI_CONFIG_NAME] + 'bot';
  s := LowerCase( s);
  if Pos(s, Text) = 1 then
    Text := StringReplace(Text, s, '', [rfReplaceAll, rfIgnoreCase]);
  Text := Trim(Text);

  // Main AI BOT
  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.chatID := chatID;
  if userName <> '' then
  begin
    SimpleBOT.UserData['Name'] := userName;
    SimpleBOT.UserData['FullName'] := fullName;
  end;
  SimpleBOT.OnError := @OnErrorHandler;  // Your Custom Message
  SimpleBOT.Handler['define'] := @defineHandler;
  SimpleBOT.Handler['carik_start'] := @Carik.StartHandler;
  SimpleBOT.Handler['carik_stop'] := @Carik.StopHandler;
  SimpleBOT.Handler['resi_paket'] := @resiHandler;
  text_response := SimpleBOT.Exec(Text);
  Response.Content := text_response;

  // Send To Telegram
  // add paramater 'telegram=1' to your telegram url
  if isTelegram then
  begin
    telegramToken := Config[_TELEGRAM_CONFIG_TOKEN];
    if SimpleBOT.SimpleAI.Action = '' then // no mention reply, if no 'action'
      messageID := '';
    if SimpleBOT.SimpleAI.Action = 'telegram_menu' then
      messageID := '';
    for j := 0 to SimpleBOT.SimpleAI.ResponseText.Count - 1 do
    begin
      if i > 0 then
        messageID := '';
      try
        s := SimpleBOT.SimpleAI.ResponseText[j];
        if s <> '' then
        begin
          {
          if isTelegramGroup then;
            if j = 0 then
              s := fullName + ', ' + s;
          }
          SimpleBOT.TelegramSend(telegramToken, chatID, messageID, s);
        end;
        //Delay(200);
      except
      end;
    end;

    //Response.Content := '{}';
    //Exit;
  end;

  //---
  SimpleBOT.Free;
  Response.ContentType := 'application/json';
end;

function TMainModule.defineHandler(const IntentName: string; Params: TStrings): string;
var
  keyName, keyValue: string;
begin

  // global define
  keyName := Params.Values['Key'];
  if keyName <> '' then
  begin
    keyName := Params.Values['Key'];
    keyValue := Params.Values['Value'];
    Result := keyName + ' = ' + keyValue;
    Result := SimpleBOT.GetResponse('HalBaru');
    Result := StringReplace(Result, '%word%', UpperCase(keyName), [rfReplaceAll]);
  end;

  Result := SimpleBOT.StringReplacement(Result);

  // Example Set & Get temporer user data
  {
  SimpleBOT.UserData[ 'name'] := 'Luri Darmawan';
  varstring :=   SimpleBOT.UserData[ 'name'];
  }

  // Save to database
  //   keyName & keyValue
end;

function TMainModule.resiHandler(const IntentName: string; Params: TStrings
  ): string;
begin
  with TResiIbacorController.Create do
  begin
    Token := Config['ibacor/token'];
    Vendor := Params.Values['vendor_value'];
    AirwayBill := Params.Values['nomor_value'];
    Result := Find();
    Free;
  end;
  if Result = '' then
    Result := 'Maaf, gagal mencari kode pengiriman ini.';
end;

function TMainModule.isTelegram: boolean;
begin
  Result := False;
  if _GET['telegram'] = '1' then
    Result := True;
end;

function TMainModule.isTelegramGroup: boolean;
var
  json: TJSONUtil;
  chatType: string;
begin
  Result := False;
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Request.Content);
    chatType := json['message/chat/type'];
    if chatType = 'group' then
      Result := True;
    if chatType = 'supergroup' then
      Result := True;
  except
  end;
  json.Free;
end;

function TMainModule.isMentioned(Text: string): boolean;
begin
  Result := False;
  if pos('@' + BOTNAME_DEFAULT, Text) > 0 then
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


