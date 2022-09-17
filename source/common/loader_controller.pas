unit loader_controller;
{
  USAGE:
    curl http://local-bot.carik.test/ai/carik.bin/loader -X PUT
}

{$mode objfpc}{$H+}

interface

uses
  process,
  redis_controller, simplebot_controller,
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers, json_helpers;

{$include carik.inc}

type

  { TLoaderController }

  TLoaderController = class(TMyCustomController)
  private
    FBotName, FToken: string;
    FRedis: TRedisConstroller;
    SimpleBOT: TSimpleBotModule;
    function loadToRedis(AKeyName: string; AFileName: string): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Put; override;
    procedure Post; override;
    procedure Options; override;
  end;

implementation

uses common;

function TLoaderController.loadToRedis(AKeyName: string; AFileName: string
  ): string;
var
  s, loaderFileName: string;
begin
  Result := 'failed';
  loaderFileName := Config['redis/loader_path'];
  if not FileExists(loaderFileName) then
  begin
    Result := 'Loader not found';
    Exit;
  end;

  if Exec('sh', [
    loaderFileName,
    AKeyName,
    AFileName], s, swoNone) then
  begin
    Result := s;
  end;

end;

constructor TLoaderController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FRedis := TRedisConstroller.Create();
end;

destructor TLoaderController.Destroy;
begin
  FRedis.Free;
  inherited Destroy;
end;

// GET Method Handler
procedure TLoaderController.Get;
begin
  OutputJson(0, 'Invalid Method');
end;

procedure TLoaderController.Put;
var
  s, tempFileName: string;
  lst: TStrings;
  json, outputJson: TJSONUtil;
begin
  Response.Content := '';

  //TODO: check authentication

  tempFileName := GetTempDir + DirectorySeparator + 'file.nlp';
  tempFileName := tempFileName.Replace('//', '/');
  if FileExists( tempFileName) then
    DeleteFile(tempFileName);

  FBotName := _GET['name'];
  FToken := _GET['token'];
  if FBotName.IsEmpty then
    FBotName := Config[CONFIG_BOTNAME];

  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.BotName := FBotName;
  SimpleBOT.LoadNLPDataFromFile;

  lst := TStringList.Create;
  json := TJSONUtil.Create;
  outputJson := TJSONUtil.Create;

  // entities
  SimpleBOT.SimpleAI.SimpleAILib.Intent.Entities.Data.GetStrings(lst);
  json.Clear;
  json['date'] := Now.AsString;
  json['data'] := base64_encode(lst.Text.Trim);
  json.SaveToFile(tempFileName);
  s := loadToRedis( FBotName + _NLP_REDIS_ENTITIES, tempFileName);
  outputJson['responses/entities'] := s;

  // intent
  SimpleBOT.SimpleAI.SimpleAILib.Intent.Data.GetStrings(lst);
  json.Clear;
  json['date'] := Now.AsString;
  json['data'] := base64_encode(lst.Text.Trim);
  json.SaveToFile(tempFileName);
  s := loadToRedis( FBotName + _NLP_REDIS_INTENTS, tempFileName);
  outputJson['responses/intents'] := s;

  // response
  SimpleBOT.SimpleAI.ResponseData.GetStrings(lst);
  json.Clear;
  json['date'] := Now.AsString;
  json['data'] := base64_encode(lst.Text.Trim);
  json.SaveToFile(tempFileName);
  s := loadToRedis( FBotName + _NLP_REDIS_RESPONSES, tempFileName);
  outputJson['responses/responses'] := s;

  Response.Content := outputJson.AsJSONFormated;
  outputJson.Free;
  json.Free;
  lst.Free;
end;

// POST Method Handler
procedure TLoaderController.Post;
begin
  Response.Content := '';
end;

// OPTIONS Method Handler
procedure TLoaderController.Options;
begin
  Response.Code := 204;
  Response.Content := '';
end;

end.


