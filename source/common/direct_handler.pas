unit direct_handler;

{$mode objfpc}{$H+}
{___$ModeSwitch UnicodeStrings}
{ $codepage cp1252}
{$codepage UTF8}
{$DEFINE INTERNAL_CARIK__}

//pertanyaan atau perintah

interface

uses
  StrUtils,
  kamuskemdikbud_integration,
  elasticsearch_integration,
  carik_webmodule,
  fpjson, RegExpr, googlegeocoding_integration,
  carik_controller, simplebot_controller, logutil_lib,
  clarifai_integration, telegram_integration, googleplacesearch_integration,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib,
  json_helpers, string_helpers, datetime_helpers, array_helpers;

const
  BOTNAME_DEFAULT = 'Carik';

type

  { TCarikHandler }

  TCarikHandler = class(TCarikWebModule)
  private
    FOnMessage: TOnMessageEvent;
    forceRespond: boolean;
    jsonData: TJSONData;

    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    procedure Put; override;
    procedure Options; override;

    function OnErrorHandler(const Message: string): string;
    Property OnMessage : TOnMessageEvent Read FOnMessage Write FOnMessage;
  end;

implementation

uses json_lib, common;

constructor TCarikHandler.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;

  Carik := TCarikController.Create;
end;

destructor TCarikHandler.Destroy;
begin
  Carik.Free;
  inherited Destroy;
end;


// Init First
procedure TCarikHandler.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

// GET Method Handler
procedure TCarikHandler.Get;
begin
  OutputJson(0, 'Invalid Method');
end;

// POST Method Handler
// CURL example:
//   curl "http://local-carik.fastplaz.com/ai/" -X POST -d '{"message":{"message_id":0,"chat":{"id":0},"text":"Hi"}}'
procedure TCarikHandler.Post;
var
  s, text_response, responseFormat: string;
  chatID, chatType, _userID, fullName, userName, groupID, groupName: string;
  i, j: integer;
  json: TJSONUtil;
  cmdAction, parameterAction, fieldAction : TStrings;
  isHandled: Boolean;
  replyText: TStringList;
begin
  Response.Content := '{}';
  forceRespond := False;
  BOLD_CODE := '**';
  ITALIC_CODE := '*';
  responseFormat := '';
  Text := '';

  // telegram style
  //   {"message":{"message_id":0,"text":"Hi","chat":{"id":0}}}
  try
    jsonData := GetJSON(Request.Content);
    try
      Text := jsonData.GetPath('message.text').AsString;
    except
    end;
    //Text := jsonData.Value['message/text'];
    if Text = 'False' then
      Text := '';

    messageID := jsonData.Value['message/message_id'];
    chatID := jsonData.Value['message/chat/id'];
    chatType := jsonData.Value['message/chat/type'];
    groupID := jsonData.Value['message/chat/group_id'];
    groupName := jsonData.Value['message/chat/group_name'];

    _userID := jsonData.Value['message/from/id'];
    userName := jsonData.Value['message/from/username'];
    if userName = '' then
      userName := trim(jsonData.Value['message/from/name']);
    fullName := trim( jsonData.Value['message/from/first_name'] + ' '
      + jsonData.Value['message/from/last_name']);
    if fullName = '' then
      fullName := trim(jsonData.Value['message/from/name']);
    if fullName = '' then
      fullName := userName;
    responseFormat := jsonData.Value['message/format'];
    DashboardDeviceID := s2i(jsonData.Value['message/chat/dashboard_device_id']);
  except
  end;

  try
    MessageType := jsonData.Value['message/chat/type'];
    replyFromMessageID := jsonData.Value['message/reply/message_id'];
    if replyFromMessageID.IsNotEmpty then
    begin
      isReplyMessage := True;
      replyFromUserID := jsonData.Value['message/reply/user_id'];
      replyFromFullName := jsonData.Value['message/reply/full_name'];
      replyFromText := jsonData.Value['message/reply/text'];

      SimpleBOT.AdditionalParameters.Values['reply_from_message_id'] := replyFromMessageID;
      SimpleBOT.AdditionalParameters.Values['reply_from_user_id'] := replyFromUserID;
      SimpleBOT.AdditionalParameters.Values['reply_from_fullname'] := replyFromFullName;
      SimpleBOT.AdditionalParameters.Values['reply_from_text'] := replyFromText;
    end;
  except
  end;

  if MessageType.IsEqualTo('image') then
  begin
    try
      FileList := TJSONArray(jsonData.GetPath('message').GetPath('files'));
    except
    end;
  end;

  // maybe submitted from post data
  //if Text = '' then
  //  Text := _POST['text'];

  ChannelId := _GET['channel'];
  if ChannelId.IsEmpty then
  begin
    ChannelId := Request.QueryString;
    //todo: remove after mobile app fixed
    {$IFDEF INTERNAL_CARIK}
    if not ChannelId.IsExists('channel=android') then Exit;
    {$ENDIF}
    ChannelId := '';///TODO:
  end;
  if ChannelId = 'm' then
    ChannelId := 'android';
  if ChannelId = 'an' then
    ChannelId := 'android';
  if ChannelId = 'and' then
    ChannelId := 'android';
  if ChannelId = 'whatsapp' then
    MessengerMode := mmWhatsapp;
  if ChannelId = 'discord' then
    MessengerMode := mmDiscord;

  // CarikBOT isRecording
  Carik.UserPrefix := '';
  Carik.UserID := chatID;
  Carik.UserID := _userID; //!
  Carik.UserName := userName;
  Carik.FullName := fullName;

  SessionController.SessionPrefix := 'carik';
  SessionController.SessionSuffix := chatID;
  SessionController.ForceUniqueID := chatID;
  SessionController.StartSession;

  SimpleBOT.SessionUserID := UniqueID;
  SimpleBOT.FirstSessionResponse := s2b(Config[CONFIG_FIRST_SESSION_RESPONSE]);

  try
    Carik.GroupName := jsonData.GetPath('message.chat.title').AsString;
  except
  end;
  Carik.UserPrefix := channelID;

  // remove mention from text
  OriginalText := Text.Trim;
  Text := Text.ToLower;
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
  //Text := StringReplace(Text, '@' + BOTNAME_DEFAULT + 'Bot', '',
  //  [rfReplaceAll, rfIgnoreCase]);

  Text := Text.Replace('@+62811857001','');  //ulil
  Text := Text.Replace('@62811857001','');  //ulil
  Text := Text.Replace('@' + BOTNAME_DEFAULT + 'Bot','');
  Text := Text.Replace('@' + BOTNAME_DEFAULT + ' Bot','');
  Text := Text.Replace('@' + BOTNAME_DEFAULT,'');
  Text := Text.Replace(BOTNAME_DEFAULT + 'Bot','');

  Text := Trim(Text);
  if Text = '' then
    Exit;

  // Main AI BOT
  //SimpleBOT := TSimpleBotModule.Create;
  //if BotName.IsNotEmpty then
  //  SimpleBOT.BotName := BotName;
  //SimpleBOT.TrimMessage := True;

  // TODO: REMOVE - force
  //SimpleBOT.FirstSessionResponse := False;
  //SimpleBOT.SecondSessionResponse := True;

  SimpleBOT.chatID := chatID;
  if userName <> '' then
  begin
    try
      SimpleBOT.UserData['Name'] := userName;
      SimpleBOT.UserData['FullName'] := fullName;
    except
    end;
  end;

  s := jsonData.Value['message/chat/is_group'];
  Carik.IsGroup := s2b(s);

  if groupID.IsEmpty then
    groupID := _GET['groupID'];
  if not groupID.IsEmpty then
  begin
    chatID := groupID;
    Carik.GroupChatID := groupID;
    SimpleBOT.AdditionalParameters.Values['group_id'] := groupID;
    SimpleBOT.AdditionalParameters.Values['GroupID'] := groupID;
    if groupName.IsNotEmpty then
    begin
      SimpleBOT.AdditionalParameters.Values['group_name'] := groupName;
      SimpleBOT.AdditionalParameters.Values['GroupName'] := groupName;
    end;
  end;
  SimpleBOT.AdditionalParameters.Values['FullName'] := fullName;
  SimpleBOT.AdditionalParameters.Values['full_name'] := fullName;
  if IsDebug then
  begin
    SimpleBOT.AdditionalParameters.Values['ClientID'] := Config[CONFIG_CLIENT_ID];
  end;


  // check if any custom handler
  if Assigned(FOnMessage) then
  begin
    isHandled := False;
    replyText := TStringList.Create;
    replyText.Text := FOnMessage(OriginalText, isHandled);
    for j:=0 to SimpleBOT.AdditionalParameters.Count-1 do
    begin
      s := SimpleBOT.AdditionalParameters.Names[j];
      SimpleBOT.SimpleAI.Parameters.Values[s] := SimpleBOT.AdditionalParameters.ValueFromIndex[j]; //manual add parameter
    end;
  end;


  if FormInputHandler() then
  begin
    isHandled := True;
  end;

  if not isHandled then
    Text := GenerateTextFromCustomActionOption(Text);
  if IsMuted then
  begin
    //LogUtil.Add(SimpleBOT.SimpleAI.ResponseJson, 'mute');
    LogChatPayload.Text:= Response.Content;
    LogChat(ChannelId, Carik.GroupChatID, Carik.GroupName, Carik.UserID, Carik.UserName, Carik.FullName, OriginalText, '', Carik.IsGroup, True);
    //OutputJson(11, 'muted: ' + MutedUntil.AsString);
    Response.Content:= SimpleBOT.SimpleAI.ResponseJson;
    Exit;
  end;

  //Text := Text.Replace(',',''); //koma
  if not isHandled then
  begin
    BotInit;
    GPTTimeout := GPT_TIMEOUT_DEFAULT;
    Response.Content := ProcessText(Text);
    SimpleBOT.SimpleAI.ResponseText.Text := Prefix + RemoveDummyImageLink(SimpleBOT.SimpleAI.ResponseText.Text).Trim;
    if responseFormat = 'text' then
    begin
      SimpleBOT.SimpleAI.ResponseText.Text := RemoveMarkDown(SimpleBOT.SimpleAI.ResponseText.Text);
    end;
    // if oTTo: telegram, instagram
    if (ChannelId = '52') or (ChannelId = '60') then
    begin
      // remove markdown
      s := (SimpleBOT.SimpleAI.ResponseText.Text);
      s := preg_replace('\*(.*?)\*', '$1', s);
      s := preg_replace('\[(.*?)\]\((.*?)\)', '$1, $2', s); // url
      SimpleBOT.SimpleAI.ResponseText.Text := s;
    end;
    // if oTTo: wa, bisnis
    if (ChannelId = '61') or (ChannelId = '62') then
    begin
      s := (SimpleBOT.SimpleAI.ResponseText.Text);
      s := preg_replace('\*\*(.*?)\*\*', '*$1*', s); // bold*
      s := preg_replace('\[(.*?)\]\((.*?)\)', '$1, $2', s); // url
      SimpleBOT.SimpleAI.ResponseText.Text := s;
    end;
  end else begin
    SimpleBOT.SimpleAI.ResponseText.Text := replyText.Text;
    replyText.Free
  end;
  Response.Content := SimpleBOT.SimpleAI.ResponseJson;

  if not SimpleBOT.IsExternal then
  begin
    if SimpleBOT.SimpleAI.IntentName <> 'Waktu' then
    begin
      //ulil
      s := 'http://global-tts.carik.id/speech/generate/' + UrlEncode(SimpleBOT.SimpleAI.ResponseText.Text);
      //s := file_get_contents( s); //ulil
    end;
  end;

  //Exec Command
  {
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
  }

  if IsCommand(SimpleBOT.SimpleAI.ResponseText.Text) then
  begin
    SimpleBOT.SimpleAI.ResponseText.Text :=
      ExecCommand(SimpleBOT.SimpleAI.ResponseText.Text);
    if SimpleBOT.SimpleAI.ResponseText.Text = '' then
      SimpleBOT.SimpleAI.ResponseText.Text :=
        SimpleBOT.GetResponse('DataTidakAdaResponse');
    //TODO: generate user data and object
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;

  {
  if not Prefix.IsEmpty then
  begin
    SimpleBOT.SimpleAI.ResponseText[0] := Prefix + SimpleBOT.SimpleAI.ResponseText[0];
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;
  }
  if Prefix.IsNotEmpty then
  begin
    if HideTextReply then
    begin
      SimpleBOT.SimpleAI.ResponseText.Text := Prefix;
      Prefix := '';
    end;
  end;

  if Suffix.IsNotEmpty then
  begin
    if GenericContent then
    begin
      if SimpleBOT.SimpleAI.ResponseText.Count = 0 then
        SimpleBOT.SimpleAI.ResponseText.Add(Suffix)
      else
      begin
        j := SimpleBOT.SimpleAI.ResponseText.Count-1;
        SimpleBOT.SimpleAI.ResponseText[j] := SimpleBOT.SimpleAI.ResponseText[j] + Suffix;
      end;
      Response.Content := SimpleBOT.SimpleAI.ResponseJson;
    end;
  end;

  // custom action: button, quickreply
  if IsCustomAction then
  begin
    SaveActionToUserData(CustomReplyType, CustomReplyData.Data);
    if not CustomActionAsText.IsEmpty then
    begin
      SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
        + '\n' + ACTION_CAPTION + '\n' + CustomActionAsText.Replace(#10,'\n');
      if CustomActionSuffix.IsNotEmpty then
        SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
          + '\n' + CustomActionSuffix.Replace(#10,'\n');
      Response.Content := SimpleBOT.SimpleAI.ResponseJson;
    end;
  end;
  Response.Content := GenerateResponseJson;

  if ActionCallback <> '' then
  begin
    parameterAction := Explode(ActionCallback, '|');
    cmdAction := Explode(parameterAction[0], '.');

    json := TJSONUtil.Create;
    json.LoadFromJsonString( Response.Content, False);
    json['response/action/callback_string'] := ActionCallback;
    json['response/action/callback_name'] := cmdAction[0];
    if cmdAction.Count > 1 then
      json['response/action/callback_method'] := cmdAction[1];
    for i := 1 to parameterAction.count - 1 do
    begin
      fieldAction := Explode(parameterAction[i], '=');
      json['response/action/parameter_' + i2s(i)] := parameterAction[i];
      try
        json['response/action/' + fieldAction[0]] := fieldAction[1];
      except
      end;
      fieldAction.Free;
    end;

    //json['processing_time'] := ProcessingTime;
    //if SimpleBOT.SimpleAI.ElapsedTime > 0 then
    //begin
    //  json['processing_time'] := SimpleBOT.SimpleAI.ElapsedTime.ToString;
    //end;
    Response.Content := json.AsJSONFormated;
  end;

  if Assigned(ElementArray) then
  begin
    json := TJSONUtil.Create;
    json.LoadFromJsonString( Response.Content, False);

    //json.ValueArray['response/action/data'] := ElementArray;
    Response.Content := json.AsJSONFormated;
    json.Free;
  end;

  //---

  text_response := SimpleBOT.SimpleAI.ResponseText.Text;
  json := TJSONUtil.Create;
  json.LoadFromJsonString( SimpleBOT.SimpleAI.ResponseJson, False);
  s := json['response/action/callback_string'];
  if s <> '' then
  begin
    //text_response := '#internalAction';
  end;
  json.Free;


  if not (_GET['_DEBUG'] = '1') then
  begin
    if channelID = 'direct' then
      channelID := DEFAULT_CHANNEL_ID;
    if channelID = 'android' then
      channelID := ANDROID_CHANNEL_ID;
    if channelID = 'i' then
      channelID := IOS_CHANNEL_ID;
    if channelID = 'ios' then
      channelID := IOS_CHANNEL_ID;
    //if channelID = 'whatsapp' then
    //  channelID := WHATSAPP_CHANNEL_ID;
    if channelID = 'instagram' then
      channelID := INSTAGRAM_CHANNEL_ID;
    if channelID = 'telegram_userbot' then
      channelID := TELEGRAM_USERBOT_CHANNEL_ID;
    Analytics(ChannelId, SimpleBOT.SimpleAI.IntentName, Text, channelID + '-' + Carik.UserID);
    if (_GET['_NOLOG'] = '') then
    begin
      LogChatPayload.Text:= Response.Content;
      LogChat(ChannelId, Carik.GroupChatID, Carik.GroupName, Carik.UserID, Carik.UserName, Carik.FullName, OriginalText, text_response, Carik.IsGroup, True);
    end;
  end;

  SimpleBOT.Free;
  Response.ContentType := 'application/json';
end;

procedure TCarikHandler.Put;
begin
  inherited Put;
end;

procedure TCarikHandler.Options;
begin
  Response.Content := '{}';
end;



function TCarikHandler.OnErrorHandler(const Message: string): string;
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
