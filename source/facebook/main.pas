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
  TMainModule = class(TCarikWebModule)
  private
    FPrefix: string;
    Facebook: TFacebookMessengerIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
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
  Response.ContentType := 'application/json';
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

  Text := Facebook.Text;
  Carik.UserPrefix := 'fb';
  Carik.UserID := Facebook.UserID;
  SimpleBOT.SessionUserID := UniqueID;

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

  if Text = '' then
  begin
    Response.Content := '{"status":"empty"}';
    Exit;
  end;

  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := False;
  Carik.UserID := Facebook.UserID;
  Carik.UserPrefix := 'fb';

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

  if SendRichContent then
  begin
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

end;



end.
