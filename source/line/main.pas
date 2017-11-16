unit main;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule, logutil_lib,
  line_integration, simplebot_controller, carik_controller,
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib;

{$include ../carik.inc}

type

  { TMainModule }

  TMainModule = class(TCarikWebModule)
  private
    forceRespond: boolean;
    LINE: TLineIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    ReplyToken: string;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
var
  s: string;
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;

  // is custom Bot ?
  s := Trim(_GET['botid']);
  if s <> '' then
  begin
    Config.Filename := 'config/config-' + s + '.json';
    SimpleBOT.StorageFileName := 'files/' + s + '-data/carik-userdata.dat';
    //SimpleBOT.LoadConfig(''); //todo: gagal load
  end;

  LINE := TLineIntegration.Create;
  LINE.BotName := BOTNAME_DEFAULT;
  LINE.Token := Config['line/default/token'];
  forceRespond := False;
end;

destructor TMainModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  //Response.ContentType := 'application/json';  //TODO: uncomment
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
  template, s, lineVoice: string;
begin
  MessengerMode := mmLine;
  LINE.RequestContent := Request.Content;
  LogUtil.Add(Request.Content, 'LINE');

  ReplyToken := LINE.ReplyToken;
  Carik.UserPrefix := 'lm';
  Carik.UserID := LINE.UserID;
  SimpleBOT.SessionUserID := UniqueID;

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

  {
  SimpleBOT.UserData['Name'] := userName;
  SimpleBOT.UserData['FullName'] := fullName;
  }
  MessengerMode := mmLine;
  BotInit;
  Response.Content := ProcessText(Text);

  SimpleBOT.SimpleAI.ResponseText.Text :=
    StringReplace(SimpleBOT.SimpleAI.ResponseText.Text, '\n', #10, [rfReplaceAll]);

  // reply message
  ReplyToken := LINE.ReplyToken;
  SimpleBOT.SimpleAI.ResponseText.Text :=
    TrimLineMessage(SimpleBOT.SimpleAI.ResponseText.Text);
  if forceRespond then
    LINE.Send(LINE.UserID, SimpleBOT.SimpleAI.ResponseText.Text)
  else
    LINE.Reply(ReplyToken, SimpleBOT.SimpleAI.ResponseText.Text);


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

  if SendRichContent then
  begin
    LogUtil.Add( 'LINE-Rich Content', 'LINE');
    LINE.Push(line.UserID, RichContent, True);
    LogUtil.Add(LINE.ResultText, 'LINE-Rich');
  end;

  //Response.Content := 'OK';
end;



end.
