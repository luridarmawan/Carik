unit main;

{$mode objfpc}{$H+}

interface

uses
  fpjson, RegExpr,
  notulen_controller, simplebot_controller, logutil_lib, resiibacor_integration,
  movie_controller, currencyibacor_integration,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, database_lib;

const
  BOTNAME_DEFAULT = 'carik'; // always lowercase

type

  { TMainModule }

  TMainModule = class(TMyCustomWebModule)
  private
    forceRespond: boolean;
    jsonData: TJSONData;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function defineHandler(const IntentName: string; Params: TStrings): string;
    function resiHandler(const IntentName: string; Params: TStrings): string;
    function voucherConvensionalHandler(const IntentName: string;
      Params: TStrings): string;
    function voucherHandler(const IntentName: string; Params: TStrings): string;
    function movieInfoHandler(const IntentName: string; Params: TStrings): string;
    function moviePlayHandler(const IntentName: string; Params: TStrings): string;
    function currencyHandler(const IntentName: string; Params: TStrings): string;
    function botEnableHandler(const IntentName: string; Params: TStrings): string;
    function botDisableHandler(const IntentName: string; Params: TStrings): string;

    function isTelegram: boolean;
    function isTelegramGroup: boolean;
    function isMentioned(Text: string): boolean;
    function isReply: boolean;
  public
    Text: string;
    Carik: TNotulenController;
    SimpleBOT: TSimpleBotModule;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    function OnErrorHandler(const Message: string): string;
  end;

implementation

uses json_lib, common;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;

  Carik := TNotulenController.Create;
end;

destructor TMainModule.Destroy;
begin
  Carik.Free;
  inherited Destroy;
end;

// Init First
procedure TMainModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
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
  s, text_response: string;
  chatID, chatType, messageID, fullName, userName, telegramToken: string;
  i, j: integer;
  x, updateID, lastUpdateID: longint;
  _regex: TRegExpr;
begin
  updateID := 0;
  forceRespond := False;

  // telegram style
  //   {"message":{"message_id":0,"text":"Hi","chat":{"id":0}}}
  try
    jsonData := GetJSON(Request.Content);
    try
      Text := jsonData.GetPath('message.text').AsString;
    except
    end;
    if Text = 'False' then
      Text := '';
    updateID := jsonData.GetPath('update_id').AsInteger;
    messageID := jsonData.GetPath('message.message_id').AsString;
    chatID := jsonData.GetPath('message.chat.id').AsString;
    chatType := jsonData.GetPath('message.chat.type').AsString;
    try
      userName := jsonData.GetPath('message.from.username').AsString;
      fullName := jsonData.GetPath('message.from.first_name').AsString +
        ' ' + jsonData.GetPath('message.from.last_name').AsString;
    except
    end;
  except
  end;

  // jika ada emoticon, hilang - force with regex
  // "\ud83d\ude2d"
  //if Text = '' then
  begin
    _regex := TRegExpr.Create;
    try
      //_regex.Expression := '(\\u(\w+))(\\u(\w+))';
      _regex.Expression := '"text":"([\\.\$\@A-Za-z0-9=_ :;\-"]+)"';
      if _regex.Exec(Request.Content) then
      begin
        Text := _regex.Match[1];
      end;
    except
    end;
    _regex.Free;
  end;

  // maybe submitted from post data
  if Text = '' then
    Text := _POST['text'];

  // CarikBOT isRecording
  Carik.UserName := userName;
  try
    Carik.GroupName := jsonData.GetPath('message.chat.title').AsString;
  except
  end;
  if isTelegram then
  begin
    if ((chatType = 'group') or (chatType = 'supergroup')) then
    begin
      Carik.IsGroup := True;
      if Carik.Recording then
      begin
        Carik.RecordTelegramMessage(Request.Content);
      end;
    end;
  end; // Carik - end

  if isTelegram then
  begin
    if AppData.debug then
      LogUtil.Add(Request.Content, 'input');
    // last message only
    lastUpdateID := s2i(_SESSION['UPDATE_ID']);
    if updateID < lastUpdateID then
    begin
      Exit;
    end;
    _SESSION['UPDATE_ID'] := updateID;

    //if isTelegramGroup then
    if ((chatType = 'group') or (chatType = 'supergroup')) then
    begin
      if not isReply then
      begin
        if (not isMentioned(Text)) then
        begin
          _SESSION['UPDATE_ID'] := updateID;
          Response.Content := 'nop';
          Exit;
        end;
      end;
    end;

  end;// isTelegram

  if Text = '' then
    Exit;

  // remove mention from text
  Text := LowerCase(Text);
  if Pos('@' + BOTNAME_DEFAULT, Text) = 1 then
    Text := StringReplace(Text, '@' + BOTNAME_DEFAULT, '', [rfReplaceAll, rfIgnoreCase]);
  s := '@' + Config[_AI_CONFIG_NAME] + 'bot';
  s := LowerCase(s);
  if Pos(s, Text) = 1 then
    Text := StringReplace(Text, s, '', [rfReplaceAll, rfIgnoreCase]);
  Text := Trim(Text);

  // Main AI BOT
  SimpleBOT := TSimpleBotModule.Create;
  if not isTelegramGroup then
  begin
    SimpleBOT.FirstSessionResponse := True;
    SimpleBOT.SecondSessionResponse := True;
  end;
  SimpleBOT.chatID := chatID;
  if userName <> '' then
  begin
    SimpleBOT.UserData['Name'] := userName;
    SimpleBOT.UserData['FullName'] := fullName;
  end;
  SimpleBOT.OnError := @OnErrorHandler;  // Your Custom Message
  SimpleBOT.Handler['define'] := @defineHandler;
  SimpleBOT.Handler['carik_start'] := @Carik.StartHandler;
  SimpleBOT.Handler['carik_stop'] := @Carik.StopHandler;
  SimpleBOT.Handler['carik_check'] := @Carik.CheckHandler;
  SimpleBOT.Handler['carik_topic'] := @Carik.TopicHandler;
  SimpleBOT.Handler['resi_paket'] := @resiHandler;
  SimpleBOT.Handler['voucher_konvensional'] := @voucherConvensionalHandler;
  SimpleBOT.Handler['voucher'] := @voucherHandler;
  SimpleBOT.Handler['movie_play'] := @moviePlayHandler;
  SimpleBOT.Handler['movie_info'] := @movieInfoHandler;
  SimpleBOT.Handler['currency'] := @currencyHandler;
  SimpleBOT.Handler['bot_enable'] := @botEnableHandler;
  SimpleBOT.Handler['bot_disable'] := @botDisableHandler;
  text_response := SimpleBOT.Exec(Text);
  Response.Content := text_response;

  //TODO
  //- rekam pembicaraan dia sendiri
  //- pilihan abaikan session

  // Carik diem ?
  if Carik.IsGroup then
  begin
    if Carik.IsDisabled then
    begin
      if not forceRespond then
      begin
        Response.Content := 'silent';
        Exit;
      end;
    end;
  end;

  // Send To Telegram
  // add paramater 'telegram=1' to your telegram url
  if isTelegram then
  begin
    telegramToken := Config[_TELEGRAM_CONFIG_TOKEN];
    if SimpleBOT.SimpleAI.Action = '' then // no mention reply, if no 'action'
      messageID := '';
    if SimpleBOT.SimpleAI.Action = 'telegram_menu' then
      messageID := '';
    for j := 0 to SimpleBOT.SimpleAI.ResponseText.Count - 1 do
    begin
      if i > 0 then
        messageID := '';
      try
        s := SimpleBOT.SimpleAI.ResponseText[j];
        if s <> '' then
        begin
          {
          if isTelegramGroup then;
            if j = 0 then
              s := fullName + ', ' + s;
          }
          SimpleBOT.TelegramSend(telegramToken, chatID, messageID, s);

          // TODO: rekam percakapan si BOT

        end;
        //Delay(200);
      except
      end;
    end;

    //Response.Content := '{}';
    //Exit;
  end;

  //---
  SimpleBOT.Free;
  Response.ContentType := 'application/json';
end;

function TMainModule.defineHandler(const IntentName: string; Params: TStrings): string;
var
  keyName, keyValue: string;
begin

  // global define
  keyName := Params.Values['Key'];
  if keyName <> '' then
  begin
    keyName := Params.Values['Key'];
    keyValue := Params.Values['Value'];
    Result := keyName + ' = ' + keyValue;
    Result := SimpleBOT.GetResponse('HalBaru');
    Result := StringReplace(Result, '%word%', UpperCase(keyName), [rfReplaceAll]);
  end;

  Result := SimpleBOT.StringReplacement(Result);

  // Example Set & Get temporer user data
  {
  SimpleBOT.UserData[ 'name'] := 'Luri Darmawan';
  varstring :=   SimpleBOT.UserData[ 'name'];
  }

  // Save to database
  //   keyName & keyValue
end;

function TMainModule.resiHandler(const IntentName: string; Params: TStrings): string;
begin
  with TResiIbacorController.Create do
  begin
    Token := Config['ibacor/token'];
    Vendor := Params.Values['vendor_value'];
    AirwayBill := Params.Values['nomor_value'];
    Result := Find();
    Free;
  end;
  if Result = '' then
    Result := 'Maaf, gagal mencari kode pengiriman ini.';
end;

function TMainModule.voucherConvensionalHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _nominal, _nomor, _pin: string;
  lst: TStrings;
begin
  Result := '';
  lst := Explode(Text, '.');
  _nominal := lst[0];
  _nomor := lst[1];
  _pin := lst[2];

  s := SimpleBOT.SimpleAI.GetResponse(IntentName + 'Response');
  s := StringReplace(s, '%nomor%', _nomor, [rfReplaceAll]);
  s := StringReplace(s, '%nominal%', _nominal, [rfReplaceAll]);
  s := StringReplace(s, '%pin%', _pin, [rfReplaceAll]);
  Result := s;
end;

function TMainModule.voucherHandler(const IntentName: string; Params: TStrings): string;
var
  s, _nominal, _nomor: string;
  _nominalFloat: double;
begin
  Result := '';
  _nomor := Params.Values['nomorponsel_value'];
  _nominal := Params.Values['nominalpulsa_value'] + Params.Values['satuan'];
  _nominal := StringHumanToNominal(_nominal);
  _nominalFloat := StringHumanToFloat(_nominal);
  ThousandSeparator := '.';
  _nominal := FormatFloat('###,##0', _nominalFloat);


  s := SimpleBOT.SimpleAI.GetResponse(IntentName + 'Response');
  s := StringReplace(s, '%nomor%', _nomor, [rfReplaceAll]);
  s := StringReplace(s, '%nominal%', _nominal, [rfReplaceAll]);
  Result := s;
end;

function TMainModule.movieInfoHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  with TMovieController.Create do
  begin
    Result := Find(Params.Values['judul_value']);
  end;
end;

function TMainModule.moviePlayHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := 'https://www.youtube.com/results?search\_query=' +
    UrlEncode(Params.Values['title_value']);
end;

function TMainModule.currencyHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  with TCurrencyIbacorIntegration.Create do
  begin
    Token := Config['ibacor/token'];
    if Token <> '' then
    begin
      Result := Converter(Params.Values['dari_value'], Params.Values['ke_value'],
        s2i(Params.Values['nominal_value']));
      if Result = '' then
        Result := 'Maaf, konversi tidak bisa dilakukan.';
    end;
    Free;
  end;
end;

function TMainModule.botEnableHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Carik.EnableBot then
    Result := SimpleBOT.GetResponse(IntentName + 'Response');
end;

function TMainModule.botDisableHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Carik.DisableBot then
  begin
    forceRespond := True;
  end;
end;

function TMainModule.isTelegram: boolean;
begin
  Result := False;
  if _GET['telegram'] = '1' then
    Result := True;
end;

function TMainModule.isTelegramGroup: boolean;
var
  json: TJSONUtil;
  chatType: string;
begin
  Result := False;
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Request.Content);
    chatType := json['message/chat/type'];
    if chatType = 'group' then
      Result := True;
    if chatType = 'supergroup' then
      Result := True;
  except
  end;
  json.Free;
end;

function TMainModule.isMentioned(Text: string): boolean;
begin
  Result := False;
  if pos('@' + BOTNAME_DEFAULT, Text) > 0 then
    Result := True;
  if pos('Bot', Text) > 0 then    // force dectect as Bot  (____Bot)
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

function TMainModule.OnErrorHandler(const Message: string): string;
var
  s: string;
begin
  s := Trim(Message);
  s := StringReplace(SimpleBOT.GetResponse('InginTahu', ''), '%word%',
    s, [rfReplaceAll]);
  Result := s;


  // simpan message ke DB, untuk dipelajari oleh AI

end;



end.
