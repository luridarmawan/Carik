unit main;

{$mode objfpc}{$H+}

interface

uses
  line_integration,
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, database_lib;

type
  TMainModule = class(TMyCustomWebModule)
  private
    LINE: TLineIntegration;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    ReplyToken, UserID, Text: string;
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

  LINE := TLineIntegration.Create;
end;

destructor TMainModule.Destroy;
begin
  LINE.Free;
  inherited Destroy;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  //Response.ContentType := 'application/json';
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
  s: string;
begin
  LINE.RequestContent:= Request.Content;
  if not LINE.isMessage then
    Exit;

  ReplyToken := LINE.ReplyToken;
  Text := LINE.Text;

  LINE.Token := Config['line/default/token'];
  LINE.Reply( ReplyToken, Text) ;

  Response.Content := 'OK';
end;



end.



