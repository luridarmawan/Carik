unit main;

{$mode objfpc}{$H+}

interface

uses
  botframework_integration,
  carik_webmodule, logutil_lib, telegram_integration, witai_integration,
  process, Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib;

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
  if pos('@' + LowerCase(BOTNAME_DEFAULT), LowerCase(Text)) > 0 then
    Result := True;
  if pos(' bot ', ' ' + LowerCase(Message) + ' ') > 0 then
    // force dectect as Bot  (____Bot)
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
  Response.ContentType := 'application/json';
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
  updateID, lastUpdateID: longint;
  j: integer;
  s, audioCaption, voiceFileName, mp3FileName: string;
begin
  MessengerMode := mmTelegram;
  updateID := 0;
  forceRespond := False;
  if AppData.debug then
    LogUtil.Add(Request.Content, 'TELE');

  if not (_GET['token'] = '') then
    TELEGRAM.Token := _GET['token'];

  TELEGRAM.RequestContent := Request.Content;
  TELEGRAM.Token := Config[TELEGRAM_TOKEN];
  updateID := TELEGRAM.UpdateID;
  MessageID := TELEGRAM.MessageID;

  Carik.UserPrefix := 'tl';
  Carik.UserID := TELEGRAM.UserID;
  Carik.UserName := TELEGRAM.UserName;
  Carik.FullName := TELEGRAM.FullName;
  Carik.GroupChatID := TELEGRAM.ChatID;
  Carik.GroupName := TELEGRAM.GroupName;
  Carik.IsGroup := TELEGRAM.IsGroup;
  SimpleBOT.SessionUserID := UniqueID;

  Text := TELEGRAM.Text;

  {$include before.pas}


  //todo: cara membuat
  // isImage
  if TELEGRAM.isImage(False) then
  begin
    if ((TELEGRAM.ImageCaption = '') and TELEGRAM.IsGroup) then
    begin
      Response.ContentType := 'application/json';
      Response.Content := '{"status":"no caption image"}';
      if Carik.IsRecording then
      begin
        Carik.GroupChatID := TELEGRAM.ChatID;
        Carik.GroupName := TELEGRAM.GroupName;
        Carik.RecordTelegramMessage(Request.Content);
        Response.Content := '{"status":"record image"}';
      end;
      Exit;
    end;
    Text := TELEGRAM.ImageCaption;
    if Text = '' then
      Text := CMD_FULL_IMAGE_ANALYZE;
    if Pos('translate', Text) > 0 then
    begin
      if TELEGRAM.IsGroup then
        Text := '@carikbot ' + CMD_IMAGE_TRANSLATION
      else
        Text := CMD_IMAGE_TRANSLATION;
    end;

    TELEGRAM.isImage(True);
    ImageID := TELEGRAM.ImageID;
    ImageURL := TELEGRAM.ImageURL;
  end;//-- isImage

  // Is Location
  if TELEGRAM.IsLocation then
  begin
    SimpleBOT.UserData['LOC_LAT'] := FloatToStr(TELEGRAM.LocationLatitude);
    SimpleBOT.UserData['LOC_LON'] := FloatToStr(TELEGRAM.LocationLongitude);
    SimpleBOT.UserData['LOC_NAME'] := TELEGRAM.LocationName;
    SimpleBOT.UserData['LOC_DATE'] := DateTimeToStr(Now);

    // still on topic, find location
    if ContextFocus <> '' then
    begin
      Text := ContextFocus + ' ' + SimpleBOT.UserData['LOC_LAT'] +
        ' ' + SimpleBOT.UserData['LOC_LON'] + ' ' + SimpleBOT.UserData['CONTEXT_DETAIL'];
      LogUtil.Add(Text, 'LOKASI');
    end;
  end;// Is Location

  //TODO: if emoticons
  Carik.UserPrefix := 'tl';
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
    Response.Content := '{"status":"expired"}';
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
          LogUtil.Add('invitation', '#1');
          if not Carik.isSapaMemberBaru then
          begin
            Response.Content := '{"status":"invitation"}';
            Exit;
          end;
          Text := '/invitation ' + TELEGRAM.InvitedUserName + ' ' +
            TELEGRAM.InvitedFullName;
          InvitedUserName := TELEGRAM.InvitedUserName;
          InvitedFullName := TELEGRAM.InvitedFullName;
          if TELEGRAM.InvitedUserName = BOTNAME_DEFAULT + 'Bot' then
            Carik.Invited;
          LogUtil.Add(Text, '#2');
        end
        else
        begin
          if not forceRespond then
          begin
            Response.Content := '{"status":"nomention"}';
            Exit;
          end;
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
  begin
    Response.Content := '{"status":"empty"}';
    Exit;
  end;

  SimpleBOT.TrimMessage := True;
  // TODO: REMOVE - force
  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := False;

  SimpleBOT.UserData['Name'] := TELEGRAM.UserName;
  SimpleBOT.UserData['FullName'] := TELEGRAM.FullName;

  //TODO: add to other platform
  SimpleBOT.AdditionalParameters.Values['UserID'] := 'tl-' + TELEGRAM.UserID; //TODO: tambahkan ke messenger lain
  SimpleBOT.AdditionalParameters.Values['ChatID'] := 'tl-' + TELEGRAM.ChatID; //TODO: tambahkan ke messenger lain

  BotInit;
  Response.Content := ProcessText(Text);

  //Exit;//ulil

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

  if SimpleBOT.SimpleAI.Action = '' then // no mention reply, if no 'action'
    MessageID := '';
  if SimpleBOT.SimpleAI.Action = 'telegram_menu' then
    MessageID := '';

  if SimpleBOT.SimpleAI.ResponseText.Count > 0 then
  begin

    // if speaking mode
    if SpeakingMode then
    begin
      if Config[CARIK_TTS_URL] <> '' then
      begin
        audioCaption := SimpleBOT.SimpleAI.ResponseText[0];
        audioCaption := copy(audioCaption, 0, pos('\n', audioCaption) - 1);

        s := PrepareTextToSpeech(SimpleBOT.SimpleAI.ResponseText.Text);
        if s <> '' then
          TELEGRAM.SendAudio(TELEGRAM.ChatID, Config[CARIK_TTS_URL] + s,
            audioCaption, MessageID);
      end;
    end;

    // Send Message
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

  end;// if SimpleBOT.SimpleAI.ResponseText.Count > 0

  if SendVenue then
  begin
    TELEGRAM.SendVenue(TELEGRAM.ChatID, VenueName, VenueAddress,
      VenueLatitude, VenueLongitude, '');
  end;

  if SendAudio then
  begin
    TELEGRAM.SendAudio(TELEGRAM.ChatID, FileURL, Caption, MessageID);
  end;

  if SendPhoto then
  begin
    //TELEGRAM.SendPhotoFromURL( TELEGRAM.ChatID, FileURL, Caption, MessageID);
  end;

  Analytics('telegram', SimpleBOT.SimpleAI.IntentName, Text, 'tl-' + Carik.UserID);
  Response.ContentType := 'application/json';
end;



end.
