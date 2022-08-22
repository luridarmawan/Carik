unit line_handler;

{$mode objfpc}{$H+}

interface

uses
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets, fpopenssl,
  {$endif}
  carik_webmodule, logutil_lib, json_lib,
  line_integration, simplebot_controller, carik_controller,
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib, string_helpers, json_helpers;

{$include ../common/carik.inc}
_DEVELOPMENT_ = False;

type

  { TLineHandler }

  TLineHandler = class(TCarikWebModule)
  private
    FOnMessage: TOnMessageEvent;
    FOnSpam: TOnSpamEvent;
    forceRespond: boolean;
    LINE: TLineIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);

    function sendButton(ACaption: string; ADataAsJson: TJSONUtil; AImageDefault: string; AsQuickReply: boolean = False): boolean;
    function sendCard(ADataAsJson: TJSONUtil; AImageDefault: string): boolean;
    function sendCard(ADataAsArray: TJSONArray; AImageDefault: string): boolean;
  public
    ReplyToken: string;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    Property OnMessage : TOnMessageEvent Read FOnMessage Write FOnMessage;
    Property OnSpam : TOnSpamEvent Read FOnSpam Write FOnSpam;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common;

constructor TLineHandler.CreateNew(AOwner: TComponent; CreateMode: integer);
var
  s: string;
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;

  // is custom Bot ?
  s := Trim(_GET['botid']);
  if s <> '' then
  begin
    //Config.Filename := 'config/config-' + s + '.json';
    //SimpleBOT.StorageFileName := 'files/' + s + '-data/carik-userdata.dat';
    //SimpleBOT.LoadConfig(''); //todo: gagal load
  end;

  LINE := TLineIntegration.Create;
  LINE.BotName := BOTNAME_DEFAULT;
  LINE.Token := Config['line/default/token'];
  forceRespond := False;
end;

destructor TLineHandler.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TLineHandler.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';  //TODO: uncomment
end;

function TLineHandler.sendButton(ACaption: string; ADataAsJson: TJSONUtil;
  AImageDefault: string; AsQuickReply: boolean): boolean;
var
  i, j, itemCount, captionLength: integer;
  s, itemLabel, itemType, itemData, itemURI, itemImage, templateAsString: string;
  buttonList, itemAsArray: TJSONArray;

  procedure _generateButtonList(AData: TJSONArray; var Output: TJSONArray);
  var
    rowIndex, itemIndex: integer;
    s, url, callbackData, image: string;
    sourceRowData: TJSONUtil;
    buttonData: TJSONObject;
    lst: TStrings;
  begin
    if AData.Count = 0 then
      Exit;
    itemCount := 0;
    sourceRowData := TJSONUtil.Create;
    for rowIndex := 0 to AData.Count - 1 do
    begin
      sourceRowData.LoadFromJsonString(AData.Items[rowIndex].AsJSON, False);
      for itemIndex := 0 to TJSONArray(sourceRowData.Data).Count - 1 do
      begin
        itemCount := itemCount + 1;
        if not AsQuickReply then
          if itemCount > 4 then
            Break;
        s := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('text');
        callbackData := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('callback_data');
        url := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('url');
        image := TJSONArray(sourceRowData.Data).Items[itemIndex].ValueOfNameAsString('image');

        buttonData := TJSONObject.Create;
        buttonData.Add('label', s);

        // url
        if not url.IsEmpty then
        begin
          buttonData.Add('type', 'uri');
          buttonData.Add('uri', url);
        end;

        // callback data
        if not callbackData.IsEmpty then
        begin
          lst := Explode(callbackData, '&');
          if not lst.Values['text'].IsEmpty then
          begin
            buttonData.Add('type', 'postback');
            buttonData.Add('data', 'text='+lst.Values['text'].Trim);
            buttonData.Add('textReply', lst.Values['text'].Trim);
          end;
          lst.Free;
        end;

        if not image.IsEmpty then
        begin
          buttonData.Add('image', image);
        end;
        Output.Add(buttonData);
      end;

    end;
    sourceRowData.Free;
  end;

begin
  Response.Content:= '';
  Result := False;
  if ADataAsJson.Data.Count = 0 then
    Exit;

  ACaption := ACaption.Replace('\n', #10);

  buttonList := TJSONArray.Create;
  _generateButtonList(TJSONArray(ADataAsJson.Data), buttonList);


  if itemCount > 4 then
  begin
    // send as text message
    //ulil: .....
  end
  else
  begin

  end;//if itemCount > 4 then

  // generate quick reply
  if AsQuickReply then
  begin
    itemAsArray := TJSONArray.Create;
    for i := 0 to buttonList.Count-1 do
    begin
      itemType := buttonList.Items[i].Value['type'];
      itemLabel := buttonList.Items[i].Value['label'];
      itemData := buttonList.Items[i].Value['textReply'];
      itemImage := buttonList.Items[i].Value['image'];

      with TJSONUtil.Create do
      begin
        Value['type'] := 'action';
        if not itemImage.IsEmpty then
          Value['imageUrl'] := itemImage;
        Value['action/type'] := 'message';
        Value['action/label'] := itemLabel;
        Value['action/text'] := itemData;

        itemAsArray.Add(Data);
      end;

    end;

    with TJSONUtil.Create do
    begin
      Value['type'] := 'text';
      Value['text'] := ACaption;
      ValueArray['quickReply/items'] := itemAsArray;

      LINE.Push( LINE.UserID, AsJSON, true);

      Free;
    end;

    buttonList.Free;
    Exit;
  end;

  // generate line button template
  with TLineTemplateMessage.Create('buttons') do
  begin
    AltText := 'Pilihan';
    Title := ' ';
    Text := ACaption;
    s := UTF8ToString(ACaption);
    captionLength := s.Length;
    if captionLength > 60 then
      Text := 'Pilihan:';
    ThumbnailImageURL := AImageDefault;

    for i := 0 to buttonList.Count-1 do
    begin
      itemType := buttonList.Items[i].Value['type'];
      itemLabel := buttonList.Items[i].Value['label'];
      if itemType = 'message' then
      begin
        itemData := buttonList.Items[i].Value['data'];
        AddActionMessage(itemLabel, itemData)
      end;
      if itemType = 'postback' then
      begin
        itemData := buttonList.Items[i].Value['data'];
        AddActionPostBack(itemLabel, itemData)
      end;
      if itemType = 'uri' then
      begin
        itemURI := buttonList.Items[i].Value['uri'];
        AddActionURI(itemLabel, itemURI);
      end;
    end;
    templateAsString := AsJSON;

    //Send to User
    s := ACaption;
    s := preg_replace('\*(.*?)\*', '$1', s);
    s := preg_replace('\[(.*?)\]\((.*?)\)', '$1, $2', s); // url
    ACaption := s;
    if captionLength > 60 then
      LINE.Send( LINE.UserID, ACaption);
    LINE.Push( LINE.UserID, templateAsString, true);
    Free;
  end;

  buttonList.Free;
  Result := True;
end;

function TLineHandler.sendCard(ADataAsJson: TJSONUtil; AImageDefault: string
  ): boolean;
var
  i: integer;
  cardTitle, subTitle, actionType, actionData: string;
  cardList: TLineTemplateMessage;
  cardItem: TJSONUtil;
  actionAsArray: TJSONArray;
begin
  Result := False;
  ElementArray := TJSONArray(ADataAsJson.Data);

  cardList := TLineTemplateMessage.Create('carousel');
  cardList.AltText := 'Tanggapan Anda';
  cardList.Title := 'Perlu tanggapan Anda';
  cardList.Text := 'Silahkan Pilih:';

  Response.Content:='';
  for i := 0 to ElementArray.Count-1 do
  begin
    //if i = 5 then
    //  Break;
    cardTitle := ElementArray.Items[i].Value['title'];
    if cardTitle.Length > 40 then
      cardTitle := cardTitle.Substring(0,37)+'...';
    subTitle := ElementArray.Items[i].Value['sub_title'];
    if subTitle.Length > 60 then
      subTitle := subTitle.Substring(0,56)+'...';
    cardItem := TJSONUtil.Create;
    cardItem['type'] := 'buttons';
    cardItem['title'] := cardTitle;
    cardItem['text'] := subTitle;
    cardItem['thumbnailImageUrl'] := ElementArray.Items[i].Value['image_url'];

    actionAsArray := TJSONArray.Create;
    ButtonCaption := SimpleBOT.SimpleAI.CustomReply['action/button_title'];
    if ButtonCaption.IsEmpty then
      ButtonCaption := 'Info lanjut';

    actionType := 'uri';
    actionData := ElementArray.Items[i].Value['url'];
    if actionData.IsEmpty then
    begin
      actionType := 'postback';
      actionData := ElementArray.Items[i].Value['callback_data'];
    end;
    with TJSONUtil.Create do
    begin
      Value['type'] := actionType;
      Value['label'] := 'Detail';
      if actionType = 'uri' then
        Value['uri'] := actionData;
      if actionType = 'postback' then
      begin
        Value['data'] := actionData;
        //bValue['text'] := cardTitle;
      end;
      actionAsArray.Add(Data);
    end;
    cardItem.Data.Add('actions', actionAsArray);

    cardList.AddColumnAsJson(cardItem.AsJSON);
    cardItem.Free;
  end;
  LINE.Push( line.UserID, cardList.AsJSON, true);

  cardList.Free;
  Result := True;
end;

function TLineHandler.sendCard(ADataAsArray: TJSONArray; AImageDefault: string
  ): boolean;
var
  i: integer;
  cardTitle, subTitle, imgUrl : string;
  cardList: TLineTemplateMessage;
  cardItem: TJSONUtil;
  actionAsArray: TJSONArray;
begin
  Result := False;
  cardList := TLineTemplateMessage.Create('carousel');
  cardList.AltText := 'Tanggapan Anda';
  cardList.Title := 'Perlu tanggapan Anda';
  cardList.Text := 'Silahkan Pilih:';

  Response.Content:='';
  for i := 0 to ADataAsArray.Count-1 do
  begin
    //if i = 5 then
    //  Break;
    cardTitle := ADataAsArray.Items[i].Value['title'];
    if cardTitle.Length > 40 then
      cardTitle := cardTitle.Substring(0,37)+'...';
    subTitle := ADataAsArray.Items[i].Value['sub_title'];
    if subTitle.Length > 60 then
      subTitle := subTitle.Substring(0,56)+'...';
    cardItem := TJSONUtil.Create;
    cardItem['type'] := 'buttons';
    cardItem['title'] := cardTitle;
    cardItem['text'] := subTitle;
    imgUrl := ADataAsArray.Items[i].Value['image_url'];
    imgUrl := 'https://services.carik.id/img/u/' + UrlEncode(UrlEncode(imgUrl)); //ulil: masukkan dalam config
    cardItem['thumbnailImageUrl'] := imgUrl;

    actionAsArray := TJSONArray.Create;
    ButtonCaption := SimpleBOT.SimpleAI.CustomReply['action/button_title'];
    if ButtonCaption.IsEmpty then
      ButtonCaption := 'Info lanjut';
    with TJSONUtil.Create do
    begin
      Value['type'] := 'uri';
      Value['label'] := 'Detail';
      Value['uri'] := ADataAsArray.Items[i].Value['url'];
      actionAsArray.Add(Data);
    end;
    cardItem.Data.Add('actions', actionAsArray);

    cardList.AddColumnAsJson(cardItem.AsJSON);
    cardItem.Free;
  end;
  LINE.Push( line.UserID, cardList.AsJSON, true);

  cardList.Free;
  Result := True;

end;

// GET Method Handler
procedure TLineHandler.Get;
begin
  Response.Content := '{}';
end;

// POST Method Handler
// CURL example:
//   curl -X POST -H "Authorization: Basic dW5hbWU6cGFzc3dvcmQ=" "yourtargeturl"
procedure TLineHandler.Post;
var
  j: integer;
  template, s, lineVoice: string;
  isHandled, canSendMessage, canSendButton, canSendQuickReply, canSendCard: boolean;
  replyText: TStringList;
begin
  if not _DEVELOPMENT_ then CloseConnection('{}'); //ulil
  MessengerMode := mmLine;
  canSendMessage := True;
  canSendButton := False;
  canSendQuickReply := False;
  canSendCard := False;
  ChannelId := 'line';
  LINE.RequestContent := Request.Content;
  LogUtil.Add(Request.Content, 'LINE');

  SessionController.SessionPrefix := 'line';
  SessionController.SessionSuffix := LINE.UserID;
  SessionController.ForceUniqueID := LINE.UserID;
  SessionController.StartSession;

  LogUtil.Add(Request.Content, 'LINE');

  LINE.GetProfile(LINE.UserID);
  ReplyToken := LINE.ReplyToken;
  Carik.UserPrefix := 'ln';
  Carik.UserID := LINE.UserID;
  Carik.UserName := LINE.UserProfile['displayName'];//&pictureUrl
  SimpleBOT.SessionUserID := UniqueID;
  SimpleBOT.FirstSessionResponse := s2b(Config[LINE_BOT_FIRST_SESSION_RESPONSE]);

  if LINE.isVoice then
  begin
    lineVoice := 'ztemp/cache/' + LINE.MessageID + '.mp4';
    if FileExists(lineVoice) then
      DeleteFile(lineVoice);
    if not LINE.GetContent(LINE.MessageID, lineVoice) then
    begin
      Response.Content := '{"status":"voicefailed"}';
      Exit;
    end;

    Text := SpeechToText(lineVoice);

    if Text <> '' then
    begin
      s := SimpleBOT.GetResponse('VoiceResult');
      s := format(s, [Text]);
      s := TrimLineMessage(s);
      LINE.Reply(ReplyToken, s);
      forceRespond := True;
    end
    else
    begin
      s := SimpleBOT.GetResponse('VoiceResultNone');
      s := StringReplace(s, '%username%', '', [rfReplaceAll]);
      s := TrimLineMessage(s);
      LINE.Reply(ReplyToken, s);
      Exit;
    end;

  end;//-- isVoice


  if LINE.isPostback  then
  begin
    Text := LINE.PostbackData.Values['text'];
    if not Text.IsEmpty then
      forceRespond := True;
  end;

  if not forceRespond then
  begin
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
  end;

  // is Location ?
  if LINE.isLocation then
  begin
    SimpleBOT.UserData['LOC_LAT'] := FloatToStr(LINE.LocationLatitude);
    SimpleBOT.UserData['LOC_LON'] := FloatToStr(LINE.LocationLongitude);
    SimpleBOT.UserData['LOC_NAME'] := LINE.LocationName;
    SimpleBOT.UserData['LOC_DATE'] := DateTimeToStr(Now);

    if ObjectFocus <> '' then
    begin
      Text := ObjectFocus + ' ' + SimpleBOT.UserData['LOC_LAT'] +
        ' ' + SimpleBOT.UserData['LOC_LON'] + ' ' + SimpleBOT.UserData['OBJECT_DETAIL'];
    end;

  end;//--- is location - end

  // is image ?
  if LINE.isImage then
  begin
    s := 'ztemp/cache/' + LINE.MessageID + '.jpg';
    if FileExists(s) then
      DeleteFile(s);
    if not LINE.GetContent(LINE.MessageID, s) then
    begin
      Response.Content := '{"status":"imagefailed"}';
      Exit;
    end;
    Text := CMD_FULL_IMAGE_ANALYZE;
    ImageID := LINE.MessageID;
    ImageURL := 'https://fire.carik.id/carik/' + s; // TODO: ganti url
    LogUtil.Add(ImageURL, 'LINE');
  end;//--- is image - end

  Carik.UserPrefix := 'lm';
  Carik.UserID := LINE.UserID;
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
  begin
    Response.Content := '{"status":"empty"}';
    Exit;
  end;

  OriginalText := Text;

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
    Suffix := Suffix.Replace('\n', #10);
  end;

  if not isHandled then
    Text := GenerateTextFromCustomActionOption(Text);

  SimpleBOT.AdditionalParameters.Values['FullName'] := Carik.FullName;

  if not isHandled then
  begin
    BotInit;
    Response.Content := ProcessText(Text);
    SimpleBOT.SimpleAI.ResponseText.Text := RemoveDummyImageLink(SimpleBOT.SimpleAI.ResponseText.Text).Trim;
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end
  else
  begin
    SimpleBOT.SimpleAI.ResponseText.Text := replyText.Text;
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
    replyText.Free;
  end;
  //exit; //ulil

  SimpleBOT.SimpleAI.ResponseText.Text :=
    StringReplace(SimpleBOT.SimpleAI.ResponseText.Text, '\n', #10, [rfReplaceAll]);

  // custom action: button, quickreply
  if IsCustomAction then
  begin
    SaveActionToUserData(CustomReplyType, CustomReplyData.Data);
    ButtonCaption := SimpleBOT.SimpleAI.ResponseText.Text;
    if CustomReplyType = 'button' then
    begin
      canSendButton := True;
      canSendMessage := False;
    end;
    if CustomReplyType = 'quickreply' then
    begin
      canSendQuickReply := True;
      canSendMessage := False;
    end;
    if ((CustomReplyType = 'menu') or (CustomReplyType = 'list')) then
    begin
      if not CustomActionAsText.IsEmpty then
      begin
        SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
          + #10 + ACTION_CAPTION + #10 + CustomActionAsText;
        if CustomActionSuffix.IsNotEmpty then
          SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
            + '\n' + CustomActionSuffix.Replace(#10,'\n');
        Response.Content := SimpleBOT.SimpleAI.ResponseJson;
      end;
    end;
    if CustomReplyType = 'card' then
    begin
      canSendCard := True;
      canSendMessage := False;
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

  // reply message
  ReplyToken := LINE.ReplyToken;
  SimpleBOT.SimpleAI.ResponseText.Text :=
    TrimLineMessage(RemoveMarkDown(SimpleBOT.SimpleAI.ResponseText.Text));

  // remove markdown
  s := (SimpleBOT.SimpleAI.ResponseText.Text);
  s := preg_replace('\*(.*?)\*', '$1', s);
  s := preg_replace('\[(.*?)\]\((.*?)\)', '$1, $2', s); // url
  SimpleBOT.SimpleAI.ResponseText.Text := s;
  if canSendMessage then
  begin
    if forceRespond then
      LINE.Send(LINE.UserID, SimpleBOT.SimpleAI.ResponseText.Text)
    else
      LINE.Reply(ReplyToken, SimpleBOT.SimpleAI.ResponseText.Text);
  end;
  if SimpleBOT.Debug then
    LogUtil.Add( LINE.ResultText, 'LINE');

  if SpeakingMode then
  begin
    if Config[CARIK_TTS_URL] <> '' then
    begin
      s := PrepareTextToSpeech(SimpleBOT.SimpleAI.ResponseText.Text);

      // TODO:
      //1. generate URL text2speech, jika lebih dari 200 karakter, pake post method.
      //2. lokasi monumen, gedung, keluar errornya, tapi ada.


      // todo: prepare post url text2speech
      if s <> '' then
      begin
        LINE.Debug := True;
        LINE.SendAudio(LINE.UserID, Config[CARIK_TTS_URL] + s);
      end;
    end;
  end;


  if canSendButton then
  begin
    if sendButton(ButtonCaption, CustomReplyData, '') then
    begin
      //canSendMessage := False;
    end;
  end;

  if canSendQuickReply then
  begin
    if sendButton(ButtonCaption, CustomReplyData, '', True) then
    begin
    end;
  end;

  if canSendCard then
  begin
    if sendCard(CustomReplyData, '') then
    begin

    end;
  end;
  if CanSendTemplateCard then //todo: duplicate with canSendCard
  begin
    sendCard(ElementArray, '');
  end;

  if SendVenue then
  begin
    LINE.SendLocation(LINE.UserID, VenueName, VenueAddress, VenueLatitude,
      VenueLongitude);
  end;

  if SendAudio then
  begin
    LINE.Debug := True;
    LINE.SendAudio(LINE.UserID, FileURL);
    LogUtil.Add( LINE.ResultText, 'LINE');
  end;

  if SendPhoto then
  begin
    LINE.Debug := True;
    LogUtil.Add( 'LINE-Photo', 'LINE');
    LogUtil.Add( FileURL, 'LINE');
    LINE.SendImage(LINE.UserID, FileURL);
    LogUtil.Add( LINE.ResultText, 'LINE');
  end;

  if SendRichContent then
  begin
    LogUtil.Add( 'LINE-Rich Content', 'LINE');
    LINE.Push(line.UserID, RichContent, True);
    LogUtil.Add(LINE.ResultText, 'LINE-Rich');
  end;

  LogChat(LINE_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName,
    Carik.UserID, Carik.UserName, Carik.FullName, Text, SimpleBOT.SimpleAI.ResponseText.Text, Carik.IsGroup, True);
  Analytics('line', SimpleBOT.SimpleAI.IntentName, Text, 'ln-' + Carik.UserID);
  //Response.Content := 'OK';
end;



end.
