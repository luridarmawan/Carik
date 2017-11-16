program carik;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  carik_webmodule,
  Classes,
  SysUtils,
  CustApp,
  fastplaz_handler, config_lib;

type

  { TCarik }

  TCarik = class(TCustomApplication)
  private
    function getText: string;
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

  procedure TCarik.DoRun;
  var
    ErrorMsg: string;
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


    WriteLn('OK: ');
    //ReadLn;

    // stop program loop
    Terminate;
  end;

  constructor TCarik.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TCarik.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TCarik.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TCarik;

begin
  Config := TMyConfig.Create(nil);
  Config.Filename := 'config/config.json';

  Application := TCarik.Create(nil);
  Application.Title := 'Carik Console';
  Application.Run;
  Application.Free;
end.
