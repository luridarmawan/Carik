unit carik_webmodule;

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  common, json_lib, fastplaz_handler, notulen_controller, simplebot_controller, logutil_lib,
  movie_controller, currencyibacor_integration, clarifai_integration,
  telegram_integration, resiibacor_integration, googleplacesearch_integration,
  Classes, SysUtils;

const
  CLARIFAI_TOKEN = 'clarifai/default/token';
  TELEGRAM_TOKEN = 'telegram/default/token';
  GOOGLE_KEY = 'google/default/key';

type

  { TCarikWebModule }

  TCarikWebModule = class(TMyCustomWebModule)
  private
    forceRespond: boolean;

    // TELEGRAM
    function isTelegramGroup: boolean;
    function getTelegramImageID: string;

    // HANDLER
    function defineHandler(const IntentName: string; Params: TStrings): string;
    function resiHandler(const IntentName: string; Params: TStrings): string;
    function voucherConvensionalHandler(const IntentName: string;
      Params: TStrings): string;
    function voucherHandler(const IntentName: string; Params: TStrings): string;
    function movieInfoHandler(const IntentName: string; Params: TStrings): string;
    function moviePlayHandler(const IntentName: string; Params: TStrings): string;
    function currencyHandler(const IntentName: string; Params: TStrings): string;
    function tebakGambarHandler(const IntentName: string; Params: TStrings): string;
    function lokasiHandler(const IntentName: string; Params: TStrings): string;

    function botEnableHandler(const IntentName: string; Params: TStrings): string;
    function botDisableHandler(const IntentName: string; Params: TStrings): string;

  public
    Text: string;
    Carik: TNotulenController;
    SimpleBOT: TSimpleBotModule;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    function ProcessText(AMessage: string): string;
    procedure BotInit;
    function OnErrorHandler(const Message: string): string;
  end;


implementation

const
  _CARIK_RESI_MSG_RESI_TIDAKDITEMUKAN = 'Maaf, gagal mencari kode pengiriman %s.';
  _CARIK_CURRENCY_MSG_FAILED = 'Maaf, konversi tidak bisa dilakukan.';

{ TCarikWebModule }

function TCarikWebModule.isTelegramGroup: boolean;
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

function TCarikWebModule.getTelegramImageID: string;
var
  _json: TJSONData;
begin
  Result := '';
  _json := GetJSON(Request.Content);
  try
    Result := _json.GetPath('message.photo[2].file_id').AsString;
  except
    try
      Result := _json.GetPath('message.photo[1].file_id').AsString;
    except
      try
        Result := _json.GetPath('message.photo[0].file_id').AsString;
      except
        on e: Exception do
        begin
        end;
      end;
    end;
  end;
  _json.Free;
end;

function TCarikWebModule.defineHandler(const IntentName: string;
  Params: TStrings): string;
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

function TCarikWebModule.resiHandler(const IntentName: string;
  Params: TStrings): string;
begin
  with TResiIbacorController.Create do
  begin
    Token := Config['ibacor/default/token'];
    if Token = '' then
      Token := Config['ibacor/token'];
    Vendor := Params.Values['vendor_value'];
    AirwayBill := Params.Values['nomor_value'];
    Result := Find();
    Free;
  end;
  if Result = '' then
    Result := Format(_CARIK_RESI_MSG_RESI_TIDAKDITEMUKAN,
      [Params.Values['nomor_value']]);
end;

function TCarikWebModule.voucherConvensionalHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _nominal, _nomor, _pin: string;
  lst: TStrings;
begin
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

function TCarikWebModule.voucherHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _nominal, _nomor: string;
  _nominalFloat: double;
begin
  _nomor := Params.Values['nomorponsel_value'];
  _nominal := Params.Values['nominalpulsa_value'] + Params.Values['satuan'];
  _nominal := StringHumanToNominal(_nominal);
  _nominalFloat := StringHumanToFloat(_nominal);
  DefaultFormatSettings.ThousandSeparator := '.';
  _nominal := FormatFloat('###,##0', _nominalFloat);

  s := SimpleBOT.SimpleAI.GetResponse(IntentName + 'Response');
  s := StringReplace(s, '%nomor%', _nomor, [rfReplaceAll]);
  s := StringReplace(s, '%nominal%', _nominal, [rfReplaceAll]);
  Result := s;
end;

function TCarikWebModule.movieInfoHandler(const IntentName: string;
  Params: TStrings): string;
begin
  with TMovieController.Create do
  begin
    Result := Find(Params.Values['judul_value']);
  end;
end;

function TCarikWebModule.moviePlayHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := 'https://www.youtube.com/results?search\_query=' +
    UrlEncode(Params.Values['title_value']);
end;

function TCarikWebModule.currencyHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  with TCurrencyIbacorIntegration.Create do
  begin
    Token := Config['ibacor/default/token'];
    if Token = '' then
      Token := Config['ibacor/token'];
    if Token <> '' then
    begin
      if Params.Values['ke_value'] = 'rupiah' then
        Params.Values['ke_value'] := 'idr';
      Result := Converter(Params.Values['dari_value'], Params.Values['ke_value'],
        s2i(Params.Values['nominal_value']));
      if Result = '' then
        Result := _CARIK_CURRENCY_MSG_FAILED;
    end;
    Free;
  end;
end;

function TCarikWebModule.tebakGambarHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _url: string;
  _img: TClarifai;
begin
  Result := '';
  if Carik.IsDisabled then
    Exit;
  if Config[CLARIFAI_TOKEN] = '' then
    Exit;
  if Config[TELEGRAM_TOKEN] = '' then
    Exit;
  if not isTelegramGroup then
    Exit;
  if Carik.IsImageRecognitionDisabled then
    Exit;
  s := getTelegramImageID;
  if s = '' then
    Exit;

  _url := '';
  with TTelegramIntegration.Create do
  begin
    Token := Config[TELEGRAM_TOKEN];
    s := GetFileURL(s);
    if s <> '' then
    begin
      _url := format(TELEGRAM_FILEURL, [Token]) + s;
    end;
  end;

  if _url = '' then
    // TODO: !!! kalau text kosong "@CarikBot" bingung dia
    Exit;

  _img := TClarifai.Create;
  _img.Token := Config[CLARIFAI_TOKEN];
  _img.ImageURL := _url;
  Result := _img.GetTagsAsString;
  _img.Free;

  s := SimpleBOT.GetResponse(IntentName + 'Response');
  Result := Format(s, [Result]);
  Carik.ImageRecognitionCounting;
end;

function TCarikWebModule.lokasiHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword: string;
begin
  _keyword := Params.Values['Lokasi_value'] + ' ' + Params.Values['keyword_value'];
  LogUtil.Add( _keyword, 'lokasi');
  with TGooglePlace.Create do
  begin
    Key := Config[GOOGLE_KEY];
    Result := SearchAsText(_keyword);
    Free;
  end;
  LogUtil.Add( Result, 'lokasi');
end;

function TCarikWebModule.botEnableHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Carik.EnableBot then
    Result := SimpleBOT.GetResponse(IntentName + 'Response');
end;

function TCarikWebModule.botDisableHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Carik.DisableBot then
  begin
    forceRespond := True;
  end;
end;

constructor TCarikWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  SimpleBOT := TSimpleBotModule.Create;
  Carik := TNotulenController.Create;
end;

destructor TCarikWebModule.Destroy;
begin
  Carik.Free;
  SimpleBOT.Free;
  inherited Destroy;
end;

function TCarikWebModule.ProcessText(AMessage: string): string;
begin
  SimpleBOT.TrimMessage := True;

  SimpleBOT.OnError := @OnErrorHandler;  // Your Custom Message
  Result := SimpleBOT.Exec(AMessage);
end;

procedure TCarikWebModule.BotInit;
begin
  SimpleBOT.Handler['define'] := @defineHandler;
  SimpleBOT.Handler['resi_paket'] := @resiHandler;
  SimpleBOT.Handler['voucher_konvensional'] := @voucherConvensionalHandler;
  SimpleBOT.Handler['voucher'] := @voucherHandler;
  SimpleBOT.Handler['movie_play'] := @moviePlayHandler;
  SimpleBOT.Handler['movie_info'] := @movieInfoHandler;
  SimpleBOT.Handler['currency'] := @currencyHandler;
  SimpleBOT.Handler['tebak_gambar'] := @tebakGambarHandler;
  SimpleBOT.Handler['lokasi'] := @lokasiHandler;
  SimpleBOT.Handler['bot_enable'] := @botEnableHandler;
  SimpleBOT.Handler['bot_disable'] := @botDisableHandler;

  {
  SimpleBOT.Handler['carik_start'] := @Carik.StartHandler;
  SimpleBOT.Handler['carik_stop'] := @Carik.StopHandler;
  SimpleBOT.Handler['carik_check'] := @Carik.CheckHandler;
  SimpleBOT.Handler['carik_topic'] := @Carik.TopicHandler;
  SimpleBOT.Handler['carik_send'] := @Carik.SendHandler;
  SimpleBOT.Handler['carik_admin_tambah'] := @carikAdminTambahHandler;
  SimpleBOT.Handler['carik_admin_hapus'] := @carikAdminHapusHandler;
  SimpleBOT.Handler['carik_group_info'] := @carikGroupInfoHandler;
  }
end;

function TCarikWebModule.OnErrorHandler(const Message: string): string;
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
