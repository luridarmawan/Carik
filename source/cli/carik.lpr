program carik;

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}
  cthreads,
  {$ENDIF}
  fpjson,
  common,
  carik_webmodule, simplebot_controller,
  Classes,
  SysUtils,
  CustApp,
  fastplaz_handler,
  config_lib;

type

  { TCarik }

  TCarik = class(TCustomApplication)
  private
    SimpleBOT: TSimpleBotModule;

    function getText: string;
    function RemoveMarkDown(AText: string): string;
    function OnErrorHandler(const Message: string): string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TCarik }

  function TCarik.getText: string;
  var
    i: integer;
  begin
    Result := '';
    if ParamCount = 0 then
      Exit;

    for i := 1 to ParamCount do
    begin
      Result := Result + ' ' + Params[i];
    end;
  end;

  function TCarik.RemoveMarkDown(AText: string): string;
  begin
    Result := preg_replace('\*\*(.*?)\*\*', '*$1*', AText); // bold
    Result := preg_replace('_(.*?)_', '$1', Result); // italic
    Result := preg_replace('\[(.+)\]\((.+)\)', '$2', Result); // url
    Result := preg_replace('tel:(.*?)', '$1', Result); // link tel:123456
    Result := preg_replace('```(.*?)```', '\n$1\n', Result); // skrip
    Result := Trim(Result);
  end;

  function TCarik.OnErrorHandler(const Message: string): string;
  begin
    Result := 'error nihh...';        samain dgn onerror carik_webmodule
  end;

  procedure TCarik.DoRun;
  var
    i: integer;
    text,
    ErrorMsg: string;
    json, j2: TJSONData;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    SimpleBOT.SimpleAI.AdditionalParameters.Values['OriginalText'] := getText;
    SimpleBOT.CLI := True;
    text := SimpleBOT.Exec(getText);
    if not text.IsEmpty then
    begin
      json := GetJSON(text);
      try
        text := json.GetPath('response.text').AsJSON;
        j2 := GetJSON(text);
        text := '';
        for i:=0 to j2.Count-1 do
        begin
          text := text + #13 + j2.Items[i].AsString;
        end;
      except
        text := '';
      end;
    end;
    text := RemoveMarkDown(text);
    text := text.Replace('\n', #10);
    text := trim(text);
    WriteLn(text);

    // stop program loop
    json.Free;
    Terminate;
  end;

  constructor TCarik.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;

    SimpleBOT := TSimpleBotModule.Create;
    SimpleBOT.StorageType := stFile;
    SimpleBOT.StorageFileName := 'files/carik/carik-userdata.dat';
    SimpleBOT.OnError := @OnErrorHandler;
    SimpleBOT.TrimMessage := False;
    SimpleBOT.IsStemming := False;
    SimpleBOT.StandardWordCheck := False;
  end;

  destructor TCarik.Destroy;
  begin
    SimpleBOT.Free;
    inherited Destroy;
  end;

  procedure TCarik.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExtractFileName(ExeName), ' -h');
  end;

var
  Application: TCarik;

begin
  Config := TMyConfig.Create(nil);
  Config.Filename := 'config/config.json';

  Application := TCarik.Create(nil);
  Application.Title:='Carik Console';
  Application.Run;
  Application.Free;
end.
