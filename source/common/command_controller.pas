unit command_controller;

{$mode objfpc}{$H+}

interface

uses
  carik_webmodule, regexpr_lib,
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler, 
    database_lib, string_helpers, dateutils, datetime_helpers, json_helpers;

{$include ../common/carik.inc}

type

  { TCommandController }

  TCommandController = class(TCarikWebModule)
  private
    userIdAsArray: TStrings;
    FUserId: string;
    function validateMuteDate(ADateAsString: string): string;
    procedure prepareData;
    procedure commandUserset();

  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    procedure Options; override;
  end;

implementation

uses common;

function TCommandController.validateMuteDate(ADateAsString: string): string;
begin
  Result := '';
  if ADateAsString.IsEmpty then Exit;

  MutedUntil := MutedUntil.FromString(ADateAsString);
  if MutedUntil > Now then
    Result := ADateAsString;
end;

procedure TCommandController.prepareData;
begin
  userIdAsArray := Explode(FUserId, '-');
  if (userIdAsArray.Count = 1) then
  begin
    userIdAsArray.Free;
    OutputJson(400, ERR_INVALID_PARAMETER);
  end;

  if userIdAsArray[0] = '5' then userIdAsArray[0] := 'wa';
  if userIdAsArray[0] = 'tl' then ChannelId := 'telegram';
  if userIdAsArray[0] = 'fb' then ChannelId := 'facebook';
  if userIdAsArray[0] = 'wa' then ChannelId := 'whatsapp'; //whatsapp, public_nlp
  if ChannelId.IsEmpty then OutputJson(400, ERR_INVALID_PARAMETER);
  FUserId := userIdAsArray[1];

  if not ClientId.IsEmpty then
    SessionPrefix := ClientId + '-';

  // Set Session
  SessionController.SessionPrefix := ChannelId;
  SessionController.SessionSuffix := FUserId;
  SessionController.ForceUniqueID := FUserId;
  SessionController.StartSession;

  SimpleBOT.SessionUserID := userIdAsArray[0] + FUserId;
end;

procedure TCommandController.commandUserset();
  procedure setUpMute();
  const
    VAR_MUTE = 'duration';
  var
    s: string;
  begin
    if _POST[VAR_MUTE] = '0' then _POST[VAR_MUTE] := '';
    if not _POST[VAR_MUTE].IsEmpty then
    begin
      s := Now.IncMinute(_POST[VAR_MUTE].ToInteger).AsString;
    end;
    SimpleBOT.UserData[SessionPrefix + 'mute'] := s;
  end;
begin
  setUpMute();
end;

constructor TCommandController.CreateNew(AOwner: TComponent; CreateMode: integer
  );
begin
  inherited CreateNew(AOwner, CreateMode);
end;

destructor TCommandController.Destroy;
begin
  userIdAsArray.Free;
  inherited Destroy;
end;

// GET Method Handler
procedure TCommandController.Get;
var
  s: string;
  json: TJSONUtil;
begin
  Response.ContentType := 'application/json';

  FUserId := _GET['userId'];
  if FUserId.IsEmpty then OutputJson(400, ERR_INVALID_PARAMETER);
  prepareData;

  if Operation = OPERATION_USERSET then
  begin
    json := TJSONUtil.Create;
    json['code'] := Int64(0);
    if ClientId = '0' then
      json['data/client_id'] := ''
    else
      json['data/client_id'] := ClientId;
    json['data/channel_id'] := ChannelId;
    json['data/user_id'] := FUserId;
    json['data/mute'] := validateMuteDate(SimpleBOT.UserData[SessionPrefix + 'mute']);
    json['data/last_visit'] := SimpleBOT.UserData['NLP_VISITLAST'];

    Response.Content := json.AsJSON;
    json.Free;
    Exit;
  end;

  OutputJson(1, FAILED);


  die(FUserId + '/' + ClientId);
end;

// POST Method Handler
procedure TCommandController.Post;
var
  s: string;
begin
  Response.ContentType := 'application/json';

  FUserId := _POST['userId'];
  if FUserId.IsEmpty then OutputJson(400, ERR_INVALID_PARAMETER);
  prepareData;

  if Operation = OPERATION_USERSET then
  begin
    commandUserset();
    OutputJson(0, OK + ' ' + Now.AsString);
  end;

  OutputJson(1, FAILED);
end;

// OPTIONS Method Handler
procedure TCommandController.Options;
begin
  Response.Code := 204;
  Response.Content := '';
end;


end.

