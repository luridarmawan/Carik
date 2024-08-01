unit facebook_handler;

{$mode objfpc}{$H+}

interface

uses
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  carik_webmodule, logutil_lib, simplebot_controller, carik_controller,
  facebookmessenger_integration, witai_integration, json_lib,
  fpjson, process,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib,
  string_helpers, json_helpers;

{$include ../common/carik.inc}
_DEVELOPMENT_ = False;

type

  { TFacebookHandler }

  TFacebookHandler = class(TCarikWebModule)
  private
    FOnMessage: TOnMessageEvent;
    FOnSpam: TOnSpamEvent;
    Facebook: TFacebookMessengerIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);

    // Payload
    function payloadTestHandler(const APayload, ATitle: string): string;
    function payloadTextHandler(const APayload, ATitle: string): string;
    function isPayloadCommand(const AText: string): boolean;

    function sendButton(ACaption: string; ADataAsJson: TJSONUtil): boolean;
    function generateQuickReplay: boolean;

  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    
    Property OnMessage : TOnMessageEvent Read FOnMessage Write FOnMessage;
    Property OnSpam : TOnSpamEvent Read FOnSpam Write FOnSpam;
  end;

implementation

uses common;

constructor TFacebookHandler.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  Facebook := TFacebookMessengerIntegration.Create;
  Facebook.BotName := BotName;
  Facebook.Token := Config[FACEBOOK_TOKEN];
  if Token.IsNotEmpty then
    Facebook.Token := Token;

end;

destructor TFacebookHandler.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TFacebookHandler.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  //Response.ContentType := 'application/json';
end;

function TFacebookHandler.payloadTestHandler(const APayload, ATitle: string): string;
begin
  Result := 'echo: ' + ATitle;
end;

function TFacebookHandler.payloadTextHandler(const APayload, ATitle: string): string;
var
  payloadText: string;
  lst: TStrings;
begin
  payloadText := ATitle;
  lst := Explode(APayload, '&');
  if lst.Count > 1 then
    payloadText := lst.Values['text'];
  Result := payloadText;
end;

function TFacebookHandler.isPayloadCommand(const AText: string): boolean;
begin
  Result := AText.IsExists('|');
end;

function TFacebookHandler.sendButton(ACaption: string; ADataAsJson: TJSONUtil
  ): boolean;
var
  indexItem: integer;
  buttonList: TJSONArray;
  buttonData: TJSONArray;

  procedure _generateButtonList(AData: TJSONArray; var Output: TJSONArray);
  var
    rowIndex, itemIndex: integer;
    s, url, size, callbackData: string;
    sourceRowData: TJSONUtil;
    buttonData: TJSONObject;
    lst: TStrings;
  begin
    if AData.Count = 0 then
      Exit;
    sourceRowData := TJSONUtil.Create;
    for rowIndex := 0 to AData.Count - 1 do
    begin
      sourceRowData.LoadFromJsonString(AData.Items[rowIndex].AsJSON, False);
      for itemIndex := 0 to TJSONArray(sourceRowData.Data).Count - 1 do
      begin
        s := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('text');
        callbackData := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('callback_data');
        url := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('url');
        try
          size := 'full';
          size := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('size');
          if size.IsEmpty then size := 'full';
        except
        end;

        buttonData := TJSONObject.Create;
        buttonData.Add('title', s);

        // url
        if not url.IsEmpty then
        begin
          buttonData.Add('type', 'web_url');
          buttonData.Add('url', url);
          buttonData.Add('webview_share_button', 'hide');
          buttonData.Add('webview_height_ratio', size);
        end;

        // callback data
        if not callbackData.IsEmpty then
        begin
          lst := Explode(callbackData, '&');
          if not lst.Values['text'].IsEmpty then
          begin
            buttonData.Add('type', 'postback');
            buttonData.Add('payload', 'text='+lst.Values['text'].Trim);
          end;
        end;

        Output.Add(buttonData);
      end;

    end;
    sourceRowData.Free;
  end;

begin
  Result := False;

  if ADataAsJson.Data.Count = 0 then
    Exit;

  ACaption := ACaption.Replace('\n', #10);

  buttonList := TJSONArray.Create;
  _generateButtonList(TJSONArray(ADataAsJson.Data), buttonList);

  buttonData := TJSONArray.Create;
  for indexItem := 0 to buttonList.Count - 1 do
  begin
    buttonData.Add(buttonList.Items[indexItem]);
    //die('o:'+buttonData.AsJSON);exit;

    if ((indexItem+1)mod 3) = 0 then
    begin
      Facebook.SendButton(Facebook.UserID, ACaption, buttonData);
      buttonData.Clear;
      ACaption := '»';
    end;
  end;
  if buttonData.Count > 0 then
  begin
    Facebook.SendButton(Facebook.UserID, ACaption, buttonData);
  end;
  buttonData.Free;
  Result := True;
end;

function TFacebookHandler.generateQuickReplay: boolean;
var
  rowIndex, itemIndex: integer;
  s, title, callbackData, image: string;
  sendAsButton: boolean;
begin
  Result := False;
  for rowIndex := 0 to CustomReplyData.Data.Count-1 do
  begin
    for itemIndex:=0 to CustomReplyData.Data.Items[rowIndex].Count-1 do
    begin
      sendAsButton := True;
      title := CustomReplyData.Data.Items[rowIndex].Items[itemIndex].Value['text'];
      callbackData := CustomReplyData.Data.Items[rowIndex].Items[itemIndex].Value['callback_data'];
      image := CustomReplyData.Data.Items[rowIndex].Items[itemIndex].Value['image'];
      if image.IsEmpty then
        image := QUICK_REPLY_IMAGE_DEFAULT;

      s := CustomReplyData.Data.Items[rowIndex].Items[itemIndex].Value['request_location'];
      if s = 'True' then
      begin
        //deprecated
        //Facebook.QuickReply.AddLocation;
        //sendAsButton := False;
        callbackData := 'text=_send_location';
      end;
      s := CustomReplyData.Data.Items[rowIndex].Items[itemIndex].Value['request_contact'];
      if s = 'True' then
      begin
        Facebook.QuickReply.AddPhone;
        sendAsButton := False;
      end;

      if sendAsButton then
        Facebook.QuickReply.AddText(title,callbackData,image);
    end;

  end;
  Result := True;
end;

// GET Method Handler
procedure TFacebookHandler.Get;
begin
  //facebook chalenge
  // _GET['hub.mode'] <== subscribe
  // _GET['hub.verify_token']
  Response.Content := _GET['hub.challenge'];
  if isEmpty(Response.Content) then
    OutputJson(400, ERR_INVALID_PARAMETER);
end;

// POST Method Handler
procedure TFacebookHandler.Post;
var
  j: integer;
  s, simg, mp3File, fbVoice: string;
  url, callName, callDesc, payloadText: String;
  lst: TStrings;
  isHandled, localReplyDisable, canSendMessage, canSendButton: boolean;
  replyText: TStringList;
begin
  if _GET['_debug'] <> '1' then
    if not _DEVELOPMENT_ then CloseConnection('{}'); //ulil
  s := '';
  canSendMessage := True;
  canSendButton := False;
  MessengerMode := mmFacebook;
  ChannelId := 'facebook';
  Facebook.RequestContent := Request.Content;
  SessionController.SessionPrefix := 'facebook';
  SessionController.SessionSuffix := Facebook.UserID;
  SessionController.ForceUniqueID := Facebook.UserID;
  SessionController.StartSession;

  LogUtil.Add(Request.Content, 'FB');

  Text := Facebook.Text;
  Carik.UserPrefix := 'fb';
  Carik.UserID := Facebook.UserID;
  SimpleBOT.SessionUserID := UniqueID;
  SimpleBOT.FirstSessionResponse := s2b(Config[FACEBOOK_BOT_FIRST_SESSION_RESPONSE]);

  Facebook.GetProfile(Facebook.UserID);

  localReplyDisable := s2b(Config[FACEBOOK_BOT_REPLY_DISABLE]);
  if (localReplyDisable or replyDisable) then
  begin
    LogChat(FACEBOOK_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName,
      Facebook.UserID, Facebook.FullName, Facebook.FullName, Facebook.Text, '',
      Carik.IsGroup, True, 0, 0); //todo: Facebook.MessageID.AsInteger
    die('{"state":"silent"}');
    exit;
  end;

  // voice processing
  if Facebook.IsVoice then
  begin
    fbVoice := 'ztemp/voice/' + Facebook.MessageID + '.mp4';
    if FileExists(fbVoice) then
      DeleteFile(fbVoice);
    if not Facebook.DownloadVoiceTo(fbVoice) then
    begin
      Response.Content := '{"status":"voicefailed"}';
      Exit;
    end;

    mp3File := 'ztemp/voice/' + Facebook.MessageID + '.mp3';
    if FileExists(mp3File) then
      DeleteFile(mp3File);
    if Exec('./ffmpeg', ['-i', fbVoice, '-ac', '1', mp3File], s, swoNone) then
    begin
      with TWitAiIntegration.Create do
      begin
        Token := Config[WITAI_TOKEN];
        //ContentType := Facebook.VoiceType; use default
        Text := trim(SpeechToText(mp3File));
        if Text = '' then
        begin
          LogUtil.Add(ResultText, 'FB-voice');
        end;
        Free;
      end;
      Prefix := SimpleBOT.GetResponse('VoiceResult');
      Prefix := format(Prefix, [Text]) + '\n\n';
    end;

  end;// end - voice processing

  if Facebook.isImage then
  begin
    Text := Facebook.ImageCaption;
    if Text = '' then
      Text := CMD_FULL_IMAGE_ANALYZE;
    if Pos('translate', Text) > 0 then
    begin
      Text := CMD_IMAGE_TRANSLATION;
    end;

    ImageID := Facebook.ImageID;    //TODO: wrong ID,cek function get imageID
    ImageURL := Facebook.ImageURL;

    simg := 'FB-' + Carik.UserID + '-imgId-' + MessageID;
    s := SimpleBOT.UserData[simg];
    if s = ImageURL then
    begin
      Exit;
    end
    else
    begin
      SimpleBOT.UserData[simg] := ImageURL;
    end;

  end; //-- isImage

  // Is Location
  if Facebook.IsLocation then
  begin
    SimpleBOT.UserData['LOC_LAT'] := FloatToStr(Facebook.LocationLatitude);
    SimpleBOT.UserData['LOC_LON'] := FloatToStr(Facebook.LocationLongitude);
    SimpleBOT.UserData['LOC_NAME'] := Facebook.LocationName;
    SimpleBOT.UserData['LOC_DATE'] := DateTimeToStr(Now);

    // still on topic, find location
    if ContextFocus <> '' then
    begin
      Text := ContextFocus + ' ' + SimpleBOT.UserData['LOC_LAT'] +
        ' ' + SimpleBOT.UserData['LOC_LON'] + ' ' + SimpleBOT.UserData['CONTEXT_DETAIL'];
      LogUtil.Add(Text, 'LOKASI');
    end;

    // special for distance_fromto
    if ContextFocus = 'distance_fromto' then
    begin
      Text := 'Jarak dari ' + SimpleBOT.UserData['LOCATION_BASE'] + ' ke '
        + SimpleBOT.UserData['LOC_NAME'];
    end;

  end; //-- Is Location

  if (Facebook.isPostback and (not Facebook.isReferral)) then
  begin
    Facebook.PayloadHandler['test'] := @payloadTestHandler;
    Facebook.PayloadHandler['TEXT'] := @payloadTextHandler;
    payloadText := Facebook.PayloadHandling;
    if payloadText.IsEmpty then
    begin
      lst := Explode(Facebook.Payload, '&');
      payloadText := lst.Values['text'];
      lst.Free;
    end;

    // bypass
    payloadText := Facebook.PayloadTitle;

    if Text = '' then
    begin
      Text := SimpleBOT.GetResponse(payloadText);
      if isPayloadCommand(Text) then
      begin
        lst := Explode(Text, '|');
        case lst[0] of
          'call':
          begin
            if lst.Count > 2 then
              callName := lst[2];
            if lst.Count > 3 then
              callDesc := lst[3];
            Facebook.SendCall(Facebook.UserID, lst[1], callName, callDesc);
          end;
          'url':
          begin
            if lst.Count > 3 then
              callDesc := lst[3];
            url := lst[2];
            url := url.Replace('%id%', Facebook.UserID);
            Facebook.SendButtonURL(Facebook.UserID, lst[1], url, callDesc);
          end;
        end;
        exit;
      end
      else
      begin
        if not Text.IsEmpty then
        begin
          // direct send from payload_response
          Text := Text.Replace('\n',#13);
          SimpleBOT.SimpleAI.ResponseText.Text := TrimFacebookMessage(Text);
          Facebook.Send(Facebook.UserID, SimpleBOT.SimpleAI.ResponseText.Text);
          Exit;
        end;
        Text := payloadText;//else
      end;
    end; // if text = ''

    if AppData.debug then
    begin
      LogUtil.Add( Facebook.ResultText, 'Payload');
    end;
  end;// isPostback
  if (Facebook.isPostback and Facebook.isReferral) then //first time
  begin
    //Text := Facebook.ReferralRef;
  end;
  if Facebook.isReferral then //existing thread
  begin
    Text := Facebook.ReferralRef;
  end;

  if Facebook.isQuickReply then
  begin
    lst := Explode(Facebook.QuickReplyPayload, '&');
    Text := lst.Values['text'].Trim;
    lst.Free;
  end;

  if Text = '' then
  begin
    Response.Content := '{"status":"empty"}';
    Exit;
  end;
  OriginalText := Text;

  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := False;
  Carik.UserID := Facebook.UserID;
  Carik.UserPrefix := 'fb';

  SimpleBOT.UserData['Name'] := Facebook.FirtName;
  SimpleBOT.UserData['FullName'] := Facebook.FullName;
  SimpleBOT.AdditionalParameters.Values['UserID'] := 'fb-' + Facebook.UserID;
  if not isEmpty( Facebook.MessageID.Trim) then
    SimpleBOT.AdditionalParameters.Values['ChatID'] := 'fb-' + Facebook.MessageID;

  SimpleBOT.AdditionalParameters.Values['FullName'] := Facebook.FullName;
  SimpleBOT.AdditionalParameters.Values['full_name'] := Facebook.FullName;

  // check if any custom handler
  isHandled := False;
  if Assigned(FOnMessage) then
  begin
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

  Text := Text.Replace(',','');
  if not isHandled then
  begin
    BotInit;
    Response.Content := ProcessText(Text);//ulil
    SimpleBOT.SimpleAI.ResponseText.Text := RemoveMarkDown(RemoveDummyImageLink(SimpleBOT.SimpleAI.ResponseText.Text)).Trim;
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end else begin
    SimpleBOT.SimpleAI.ResponseText.Text := replyText.Text;
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
    replyText.Free;
  end;

  s := Facebook.Text;
  if Facebook.isPostback then
  begin
    s := 'callback|'+SimpleBOT.SimpleAI.IntentName;
  end;
  LogChat(FACEBOOK_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName,
    Carik.UserID, Facebook.FullName, Facebook.FullName, s, SimpleBOT.SimpleAI.ResponseText.Text,
    Carik.IsGroup, True);
  //if not TELEGRAM.IsGroup then
  begin
    if IsUserSuspended(FACEBOOK_CHANNEL_ID, Carik.UserID) then
    begin
      if AppData.debug then
        LogUtil.Add(Carik.UserID + ' suspended', 'USERCHECK');
      Exit;
    end;
  end;

  if IsCommand(SimpleBOT.SimpleAI.ResponseText.Text) then
  begin
    //xxx
  end;

  //Exec Command - carik base
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

  if _DEVELOPMENT_ then Exit;//ulil

  if IsCustomAction then
  begin
    //SaveActionToUserData;
    if CustomReplyType = 'button' then
    begin
      canSendButton := True;
      canSendMessage := False;
      ButtonCaption := SimpleBOT.SimpleAI.ResponseText.Text;
      if ButtonCaption.Length > 640 then
      begin
        canSendMessage := True;
        ButtonCaption := '📋 Pilihan lain';
      end;
    end;
    if CustomReplyType = 'card' then
    begin
      ElementArray := TJSONArray(CustomReplyData.Data);
      CanSendTemplateCard := true;
      SimpleBOT.SimpleAI.ResponseText.Text := '';
      ButtonCaption := SimpleBOT.SimpleAI.CustomReply['action/button_title'];
      if ButtonCaption.IsEmpty then
        ButtonCaption := 'Detail';
    end;
    if CustomReplyType = 'quickreply' then
    begin
      {
      manual test
      Facebook.QuickReply.AddText('gempa', 'info gempa', 'https://i.pinimg.com/originals/39/44/6c/39446caa52f53369b92bc97253d2b2f1.png');
      Facebook.QuickReply.AddText('satu', 'echo satu', 'https://i.pinimg.com/originals/39/44/6c/39446caa52f53369b92bc97253d2b2f1.png');
      Facebook.QuickReply.AddText('dua', 'echo dua', 'https://i.pinimg.com/originals/39/44/6c/39446caa52f53369b92bc97253d2b2f1.png');
      Facebook.SendQuickReply(Facebook.UserID, 'contoh');
      }
      generateQuickReplay;
      canSendMessage := False;
    end;
    if ((CustomReplyType = 'menu') or (CustomReplyType = 'list')) then
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
    if CustomReplyType = 'form' then
    begin
      canSendMessage := True;
    end;
  end;

  if not Prefix.IsEmpty then
  begin
    SimpleBOT.SimpleAI.ResponseText[0] := Prefix + SimpleBOT.SimpleAI.ResponseText[0];
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;
  if not Suffix.IsEmpty then
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

  if SendRichContent then
  begin
    die('...rich...');
    exit;
    //LINE.Push(line.UserID, RichContent, True);
    //LogUtil.Add( LINE.ResultText, 'FB-Rich');
  end;

  // send
  SimpleBOT.SimpleAI.ResponseText.Text :=
    StringReplace(SimpleBOT.SimpleAI.ResponseText.Text,
    '\n', #10, [rfReplaceAll]);
  SimpleBOT.SimpleAI.ResponseText.Text :=
    TrimFacebookMessage(SimpleBOT.SimpleAI.ResponseText.Text);

  if canSendMessage then
  begin
    Facebook.Send(Facebook.UserID, SimpleBOT.SimpleAI.ResponseText.Text);
    if not Facebook.IsSuccessfull then
    begin
      LogUtil.Add(Facebook.ResultText, 'FB');
    end;
  end;

  // if speaking mode
  if SpeakingMode then
  begin
    if Config[CARIK_TTS_URL] <> '' then
    begin
      s := PrepareTextToSpeech(SimpleBOT.SimpleAI.ResponseText.Text);
      if s <> '' then
      begin
        s := Config[CARIK_TTS_URL] + s;
        Facebook.SendAudio(Facebook.UserID, s);
      end;
    end;
  end;

  if SendAudio then
  begin
    Facebook.SendAudio(Facebook.UserID, FileURL);
  end;

  if SendPhoto then
  begin
    Facebook.SendImage(Facebook.UserID, FileURL);
    Facebook.Send(Facebook.UserID,ImageCaption);
  end;

  if SendQuickReplayLocation then
    Facebook.QuickReply.AddLocation;
  if Facebook.QuickReply.Count > 0 then
  begin
    Facebook.SendQuickReply(Facebook.UserID, SimpleBOT.SimpleAI.ResponseText.Text);
  end;

  if canSendButton then
  begin
    if sendButton(ButtonCaption, CustomReplyData) then
    begin
      canSendMessage := False;
    end;
  end;

  if CanSendTemplateCard then
    Facebook.SendTemplateCard(Facebook.UserID, ElementArray, ButtonCaption);

  s := jsonGetData(SimpleBOT.SimpleAI.CustomReply.Data, 'suffix');
  if s.IsNotEmpty then
    Facebook.Send(Facebook.UserID, s);


  s := Text;
  if Facebook.isPostback then
  begin
    s := 'callback|'+SimpleBOT.SimpleAI.IntentName;
  end;
  Analytics('facebook', SimpleBOT.SimpleAI.IntentName, s, 'fb-' + Carik.UserID);
end;



end.
