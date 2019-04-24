unit main;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule, logutil_lib, simplebot_controller, carik_controller,
  facebookmessenger_integration, witai_integration,
  fpjson, process,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib;

{$include ../carik.inc}

type

  { TMainModule }

  TMainModule = class(TCarikWebModule)
  private
    FPrefix: string;
    Facebook: TFacebookMessengerIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);

    // Payload
    function payloadTestHandler(const APayload, ATitle: String): String;

  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  Facebook := TFacebookMessengerIntegration.Create;
  Facebook.BotName := BOTNAME_DEFAULT;
  Facebook.Token := Config[FACEBOOK_TOKEN];
  FPrefix := '';
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

function TMainModule.payloadTestHandler(const APayload, ATitle: String): String;
begin
  Result := 'echo: ' + ATitle;
end;

// GET Method Handler
procedure TMainModule.Get;
begin
  //facebook chalenge
  Response.Content := _GET['hub.challenge'];
end;

// POST Method Handler
procedure TMainModule.Post;
var
  s, simg, mp3File, fbVoice: string;
begin
  MessengerMode := mmFacebook;
  Facebook.RequestContent := Request.Content;
  LogUtil.Add(Request.Content, 'FB');

  if not (_GET['token'] = '') then
    Facebook.Token := _GET['token'];

  Text := Facebook.Text;
  Carik.UserPrefix := 'fb';
  Carik.UserID := Facebook.UserID;
  SimpleBOT.SessionUserID := UniqueID;

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
      FPrefix := SimpleBOT.GetResponse('VoiceResult');
      FPrefix := format(FPrefix, [Text]) + '\n\n';
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

    // force close connection
    Response.Content := '';
    Response.SetCustomHeader('Connection', 'close');
    Response.SetCustomHeader('Content-Length', '0');
    Response.SendResponse;

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
  end; //-- Is Location

  if Facebook.isPostback then
  begin
    Facebook.PayloadHandler['test'] := @payloadTestHandler;
    Text := Facebook.PayloadHandling;

    die( 'zzz: ' + Text);
    Text := '';
  end;

  if Text = '' then
  begin
    Response.Content := '{"status":"empty"}';
    Exit;
  end;

  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := False;
  Carik.UserID := Facebook.UserID;
  Carik.UserPrefix := 'fb';

  SimpleBOT.UserData['Name'] := Facebook.UserID;
  SimpleBOT.UserData['FullName'] := Facebook.UserID; //TODO: Get Full Name
  SimpleBOT.AdditionalParameters.Values['UserID'] := 'fb-' + Facebook.UserID;
  SimpleBOT.AdditionalParameters.Values['ChatID'] := 'fb-' + Facebook.MessageID;

  BotInit;
  Response.Content := ProcessText(Text);

  LogChat(FACEBOOK_CHANNEL_ID, Carik.GroupChatID, Carik.UserID, Carik.UserName, Facebook.Text, SimpleBOT.SimpleAI.ResponseText.Text, Carik.IsGroup, True);
  //if not TELEGRAM.IsGroup then
  begin
    if IsUserSuspended( FACEBOOK_CHANNEL_ID, Carik.UserID) then
    begin
      if AppData.debug then
         LogUtil.Add( Carik.UserID + ' suspended', 'USERCHECK');
      Exit;
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

  //Exit;//ulil

  if SendRichContent then
  begin
    die('...rich...');
    exit;
    //LINE.Push(line.UserID, RichContent, True);
    //LogUtil.Add( LINE.ResultText, 'FB-Rich');
  end;

  // send
  SimpleBOT.SimpleAI.ResponseText.Text :=
    StringReplace(FPrefix + SimpleBOT.SimpleAI.ResponseText.Text,
    '\n', #10, [rfReplaceAll]);
  SimpleBOT.SimpleAI.ResponseText.Text :=
    TrimFacebookMessage(SimpleBOT.SimpleAI.ResponseText.Text);
  Facebook.Send(Facebook.UserID, SimpleBOT.SimpleAI.ResponseText.Text);
  if not Facebook.IsSuccessfull then
  begin
    LogUtil.Add(Facebook.ResultText, 'FB');
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
        die(s);
        Facebook.SendAudio(Facebook.UserID, s);
      end;
    end;
  end;

  if SendAudio then
  begin
    Facebook.SendAudio(Facebook.UserID, FileURL);
  end;

  Analytics('facebook', SimpleBOT.SimpleAI.IntentName, Text, 'fb-' + Carik.UserID);
end;



end.
