unit carik_webmodule;

{$mode objfpc}{$H+}

interface

uses
  fpjson, strutils, md5, sha1,
  common, json_lib, fastplaz_handler, carik_controller, simplebot_controller,
  logutil_lib, kawalpemilu_integration, zomato_integration, yandextranslate_integration,
  movie_controller, currencyibacor_integration, cognitiveocr_integration,
  cognitivedomainspecific_integration, cognitiveanalyze_integration,
  clarifai_integration, ibacorpolicenumber_integration, alquranindonesia_integration,
  telegram_integration, resiibacor_integration, googleplacesearch_integration,
  kamussunda_integration, bmkg_integration, openweathermap_integration,
  apixu_integration, jobplanet_integration, bca_integration, witai_integration,
  maskofajadwalshalat_integration, line_integration, ibacortrainschedule_integration,
  facebookmessenger_integration, portalpulsa_integration,
  kloudlesscalendar_integration, rss_lib, http_lib, IniFiles,
  process, dateutils, Classes, SysUtils;

{$include carik.inc}

type

  TMessengerMode = (mmNone, mmTelegram, mmLine, mmFacebook, mmSkype, mmSlack);

  { TCarikWebModule }

  TCarikWebModule = class(TMyCustomWebModule)
  private
    FCaption: string;
    FFileURL: string;
    FImageCaption: string;
    FImageID: string;
    FImagePath: string;
    FImageURL: string;
    FInvitedFullName: string;
    FInvitedUserName: string;
    FLanguage: string;
    FMessengerMode: TMessengerMode;
    forceRespond: boolean;
    FRichContent: string;
    FSendAudio: boolean;
    FSendPhoto: boolean;
    FSendRichContent: boolean;
    FSendVenue: boolean;
    FVenueAddress: string;
    FVenueLatitude: double;
    FVenueLongitude: double;
    FVenueName: string;

    // TELEGRAM
    function getIsObjectFocusExpired: boolean;
    function getisSpeakingMode: boolean;
    function getUniqueID: string;
    function isGroup: boolean;
    function isTelegramGroup: boolean;
    function getTelegramImageID: string;

    function generateCalendarID: string;
    function generateEventName(AEventName: string): string;

    // HANDLER
    function defineHandler(const IntentName: string; Params: TStrings): string;
    function botStartHandler(const IntentName: string; Params: TStrings): string;
    function resiHandler(const IntentName: string; Params: TStrings): string;
    procedure setisSpeakingMode(AValue: boolean);
    function voucherConvensionalHandler(const IntentName: string;
      Params: TStrings): string;
    function voucherHandler(const IntentName: string; Params: TStrings): string;
    function movieInfoHandler(const IntentName: string; Params: TStrings): string;
    function moviePlayHandler(const IntentName: string; Params: TStrings): string;
    function currencyHandler(const IntentName: string; Params: TStrings): string;
    function ocrCognitiveHandler(const IntentName: string; Params: TStrings): string;
    function imageTranslationHandler(const IntentName: string; Params: TStrings): string;
    function imageSpecificCognitiveHandler(const IntentName: string;
      Params: TStrings): string;

    function imageAnalyzeCognitiveHandler(const IntentName: string;
      Params: TStrings): string;
    function imageFullAnalyzeHandler(const IntentName: string;
      Params: TStrings): string;
    function tebakGambarHandler(const IntentName: string; Params: TStrings): string;
    function lokasiHandler(const IntentName: string; Params: TStrings): string;
    function lokasiDenganKoordinatHandler(const IntentName: string;
      Params: TStrings): string;
    function lokasiKulinerHandler(const IntentName: string; Params: TStrings): string;
    function lokasiKulinerDenganKoordinatHandler(const IntentName: string;
      Params: TStrings): string;
    function carikAdminTambahHandler(const IntentName: string; Params: TStrings): string;
    function carikAdminHapusHandler(const IntentName: string; Params: TStrings): string;
    function carikNewMemberCustomMessageHandler(const IntentName: string;
      Params: TStrings): string;
    function carikMemberBaruHandler(const IntentName: string; Params: TStrings): string;
    function carikMemberBaruAbaikanHandler(const IntentName: string;
      Params: TStrings): string;
    function carikMemberBaruSapaHandler(const IntentName: string;
      Params: TStrings): string;
    function quickCountHandler(const IntentName: string; Params: TStrings): string;
    function kofaJadwalImsyakHandler(const IntentName: string; Params: TStrings): string;
    function kofaJadwalSholatHandler(const IntentName: string; Params: TStrings): string;

    function alquranTerjemahanHandler(const IntentName: string;
      Params: TStrings): string;

    function kloudlessCalendarEventListHandler(const IntentName: string;
      Params: TStrings): string;
    function kloudlessCalendarEventCreateHandler(const IntentName: string;
      Params: TStrings): string;

    function bmkgSimpleInfoHandler(const IntentName: string; Params: TStrings): string;
    function openweatherInfoHandler(const IntentName: string; Params: TStrings): string;
    function apixuweatherInfoHandler(const IntentName: string; Params: TStrings): string;
    function beritaHariIniHandler(const IntentName: string; Params: TStrings): string;

    function conversionHashHandler(const IntentName: string; Params: TStrings): string;

    function jobPlanetInfoHandler(const IntentName: string; Params: TStrings): string;
    function jobPlanetReviewHandler(const IntentName: string; Params: TStrings): string;
    function jobPlanetSalaryHandler(const IntentName: string; Params: TStrings): string;
    function jobPlanetVacancyHandler(const IntentName: string; Params: TStrings): string;
    function jobPlanetInterviewHandler(const IntentName: string;
      Params: TStrings): string;

    function smartHomeGeneralHandler(const IntentName: string; Params: TStrings): string;
    function smartHomeOnHandler(const IntentName: string; Params: TStrings): string;
    function smartHomeOffHandler(const IntentName: string; Params: TStrings): string;

    function richContentHandler(const IntentName: string; Params: TStrings): string;

    function echoHandler(const IntentName: string; Params: TStrings): string;
    function texttospeechHandler(const IntentName: string; Params: TStrings): string;
    function speakingModeOnHandler(const IntentName: string; Params: TStrings): string;
    function speakingModeOffHandler(const IntentName: string; Params: TStrings): string;

    function trainScheduleHandler(const IntentName: string; Params: TStrings): string;

    function bcaTestHandler(const IntentName: string; Params: TStrings): string;

    function botEnableHandler(const IntentName: string; Params: TStrings): string;
    function botDisableHandler(const IntentName: string; Params: TStrings): string;

    function mortgageCalculator(ALoanAmount: double; ALongInstallment: double = 10;
      AInterest: double = 8): double;
    function mortgageCalculatorHandler(const IntentName: string;
      Params: TStrings): string;


    function GenerateLineCarouselFromCulinaryData(ATitle, AJson: string): string;
    function LineProperySearch(ATitle, AJson: string): string;
    function LineBerita(ATitle, AJson: string): string;
    function FacebookBerita(ATitle, AJson: string): string;
  public
    MessageID: string;
    Text: string;
    Carik: TCarikController;
    SimpleBOT: TSimpleBotModule;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    function RemoveMarkDown(AText: string): string;

    function PrepareTextToSpeech(AText: string): string;

  published
    property UniqueID: string read getUniqueID;
    property MessengerMode: TMessengerMode read FMessengerMode write FMessengerMode;
    property Language: string read FLanguage write FLanguage;
    property SendAudio: boolean read FSendAudio write FSendAudio;
    property SendPhoto: boolean read FSendPhoto write FSendPhoto;
    property SendRichContent: boolean read FSendRichContent;
    property FileURL: string read FFileURL;
    property Caption: string read FCaption;
    property RichContent: string read FRichContent;

    // OBJECT
    property isObjectFocusExpired: boolean read getIsObjectFocusExpired;
    function ObjectFocus: string;

    // Kamus
    function translateIndonesiaToSundaHandler(const IntentName: string;
      Params: TStrings): string;
    function translateSundaToIndonesiaHandler(const IntentName: string;
      Params: TStrings): string;
    function yandexTranslateDetectHandler(const IntentName: string;
      Params: TStrings): string;
    function yandexSmartTranslateDetectHandler(const IntentName: string;
      Params: TStrings): string;

    function pajakKendaraanHandler(const IntentName: string; Params: TStrings): string;
    function ProcessText(AMessage: string): string;
    procedure BotInit;
    function OnErrorHandler(const Message: string): string;

    function TrimFacebookMessage(const AMessage: string): string;
    function TrimLineMessage(const AMessage: string): string;

    property ImageID: string read FImageID write FImageID;
    property ImageURL: string read FImageURL write FImageURL;
    property ImagePath: string read FImagePath write FImagePath;
    property ImageCaption: string read FImageCaption write FImageCaption;

    property InvitedUserName: string read FInvitedUserName write FInvitedUserName;
    property InvitedFullName: string read FInvitedFullName write FInvitedFullName;

    property SendVenue: boolean read FSendVenue;
    property VenueName: string read FVenueName;
    property VenueAddress: string read FVenueAddress;
    property VenueLatitude: double read FVenueLatitude;
    property VenueLongitude: double read FVenueLongitude;

    property SpeakingMode: boolean read getisSpeakingMode write setisSpeakingMode;
  end;

  { TSmartHomeTestIntegration }

  // FOR TESTING ONLY
  TSmartHomeTestIntegration = class(TInterfacedObject)
  private
    FAccountName: string;
    FUserData: TIniFile;
    FAccountID: string;
    FStorageFileName: string;
    FStorageType: TStorageType;
    function getActive: boolean;
    procedure setActive(AValue: boolean);
    procedure setStorageType(AValue: TStorageType);
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    procedure AddDevice(ADeviceName: string);
    procedure SetDevice(ADeviceName: string; AStatus: boolean);
    function IsHaveDevice: boolean;
    function IsDeviceExist(ADeviceName: string): boolean;
    function GetDeviceAsText: string;
    function GetStatusAsText(ADeviceName: string): string;
  published
    property Active: boolean read getActive write setActive;
    property AccountID: string read FAccountID write FAccountID;
    property AccountName: string read FAccountName write FAccountName;
    property StorageType: TStorageType read FStorageType write setStorageType;
    property StorageFileName: string read FStorageFileName write FStorageFileName;
  end;


implementation

const
  _RESPONSE = 'Response';
  _CARIK_RESI_MSG_RESI_TIDAKDITEMUKAN = 'Maaf, gagal mencari kode pengiriman %s.';
  _CARIK_CURRENCY_MSG_FAILED = 'Maaf, konversi tidak bisa dilakukan.';

  _TELEGRAM_ERR_GROUP_ONLY = 'Fitur ini hanya bisa dilakukan di *Telegram Group*';

  _OBJECT_DISCUSSION_MAXTIME = 10; // in minutes
  //_ICON_WEKER = '⏰⏱';
  //_ICON_NUMBER = '0️⃣1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣8️⃣9️⃣🔟';
  _ICON_NUMBER_ARRAY: array [0..10] of string =
    ('0️⃣', '1️⃣', '2️⃣', '3️⃣', '4️⃣', '5️⃣', '6️⃣', '7️⃣', '8️⃣', '9️⃣', '🔟');

  _CARIK_SPEAKING_MODE = '_SPEAKING_MODE';

  DEFAULT_RESTAURANT_IMAGE_URL = 'https://fire.carik.id/images/restorant/default2.png';
  DEFAULT_NEWS_IMAGE_URL = 'https://fire.carik.id/images/news/news.png';
  TEXT2SPEECH_MAX_CHAR = 250;

{ TSmartHomeTestIntegration }

procedure TSmartHomeTestIntegration.setStorageType(AValue: TStorageType);
begin
  if FStorageType = AValue then
    Exit;
  FStorageType := AValue;

  if FStorageType = stFile then
  begin

  end;
end;

function TSmartHomeTestIntegration.getActive: boolean;
begin
  Result := FUserData.ReadBool('AccountList', FAccountID, False);
end;

procedure TSmartHomeTestIntegration.setActive(AValue: boolean);
begin
  FUserData.WriteBool('AccountList', FAccountID, AValue);
end;

constructor TSmartHomeTestIntegration.Create;
begin
  FAccountID := '';
  FStorageType := stFile;
  FStorageFileName := 'files/carik/carik-smarthome.dat';

  FUserData := TIniFile.Create(FStorageFileName);
end;

destructor TSmartHomeTestIntegration.Destroy;
begin
  if Assigned(FUserData) then
    FUserData.Free;
end;

function TSmartHomeTestIntegration.IsHaveDevice: boolean;
begin
  Result := False;
  if FAccountID = '' then
    Exit;

  Result := FUserData.ReadBool('AccountList', FAccountID, False);
end;

function TSmartHomeTestIntegration.IsDeviceExist(ADeviceName: string): boolean;
begin
  Result := FUserData.ReadBool(FAccountID, ADeviceName, False);
end;

function TSmartHomeTestIntegration.GetDeviceAsText: string;
var
  i: integer;
  s: string;
  lst: TStringList;
begin
  Result := '';
  lst := TStringList.Create;
  FUserData.ReadSectionRaw(FAccountID, lst);

  for i := 0 to lst.Count - 1 do
  begin
    if lst.ValueFromIndex[i] = '1' then
    begin
      s := lst.Names[i];
      Result := Result + s + ':'#10' ' + GetStatusAsText(s) + #10;
    end;
  end;
  lst.Free;
end;

function TSmartHomeTestIntegration.GetStatusAsText(ADeviceName: string): string;
var
  s: string;
  d: TDateTime;
begin
  Result := 'Mati, ';
  if FUserData.ReadString(FAccountID + '-' + ADeviceName, 'status', '0') = '1' then
    Result := 'Nyala, ';

  s := FUserData.ReadString(FAccountID + '-' + ADeviceName, 'time', '');
  d := ScanDateTime('yyyy-mm-dd HH:nn:ss', s);
  Result := Result + ' ' + FormatDateTime('dd/mm HH:nn', d);
end;

procedure TSmartHomeTestIntegration.AddDevice(ADeviceName: string);
begin
  FUserData.WriteBool(FAccountID, ADeviceName, True);
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'by_id', '');
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'by_name', '');
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'status', '1');
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'time',
    FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
end;

procedure TSmartHomeTestIntegration.SetDevice(ADeviceName: string; AStatus: boolean);
begin
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'by_id', FAccountID);
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'by_name', FAccountName);
  FUserData.WriteBool(FAccountID + '-' + ADeviceName, 'status', AStatus);
  FUserData.WriteString(FAccountID + '-' + ADeviceName, 'time',
    FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
end;

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

function TCarikWebModule.getIsObjectFocusExpired: boolean;
begin

end;

function TCarikWebModule.getisSpeakingMode: boolean;
var
  s: string;
begin
  Result := False;
  s := UniqueID + _CARIK_SPEAKING_MODE;

  if SimpleBOT.UserData[s] = '1' then
    Result := True;
end;

function TCarikWebModule.getUniqueID: string;
begin
  Result := '';
  if FMessengerMode = mmTelegram then
    Result := 'te';
  if FMessengerMode = mmFacebook then
    Result := 'fb';
  if FMessengerMode = mmLine then
    Result := 'ln';
  if FMessengerMode = mmSkype then
    Result := 'sk';
  if FMessengerMode = mmSlack then
    Result := 'sl';

  if isGroup then
    Result := Result + Carik.GroupChatID
  else
    Result := Result + Carik.UserID;

end;

function TCarikWebModule.isGroup: boolean;
begin
  Result := False;
  if isTelegramGroup then
    Result := True;
end;

procedure TCarikWebModule.setisSpeakingMode(AValue: boolean);
var
  s: string;
begin
  s := UniqueID + _CARIK_SPEAKING_MODE;

  if AValue then
    SimpleBOT.UserData[s] := '1'
  else
    SimpleBOT.UserData[s] := '';

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

function TCarikWebModule.generateCalendarID: string;
begin
  Result := Carik.UserID + '-' + Carik.UserName;
  if isTelegramGroup then
    Result := Carik.GroupChatID + '-';
  Result := Carik.UserPrefix + '|' + Result;
end;

function TCarikWebModule.generateEventName(AEventName: string): string;
begin
  Result := generateCalendarID + '|' + AEventName;
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

function TCarikWebModule.botStartHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Carik.IsGroup then
    Exit;

  SimpleBOT.Debug := True;
  SimpleBOT.UserData['id'] := Carik.UserID;
  SimpleBOT.UserData['type'] := copy(getUniqueID, 0, 2);
  SimpleBOT.UserData['datetime'] := FormatDateTime('yyyy-mm-dd HH:nn:ss', Now);
end;

function TCarikWebModule.resiHandler(const IntentName: string;
  Params: TStrings): string;
begin
  with TResiIbacorController.Create do
  begin
    Token := Config['IBACOR_TOKEN'];
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

  with TPortalPulsaIntegration.Create do
  begin
    UserID:= 'P21765';
    Key:= 'dc40e6d1c4dcd48cd254cc5cc1248ea5';
    Secret:='d10fdea4e4085145e63bf5521fe1a2314a30bd67fd047f76891799cf4401ab83';
    TransactionID:= FormatDateTime('yyyymmddHHnnss', Now) + _nomor;

    if IsiPulsa( _nomor,'5') then
    begin

    end;
    LogUtil.Add( Message, 'PULSA');
    Free;
  end;
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

  //TODO: dihapus aja


  with TPortalPulsaIntegration.Create do
  begin
    UserID:= 'P21765';
    Key:= 'dc40e6d1c4dcd48cd254cc5cc1248ea5';
    Secret:='d10fdea4e4085145e63bf5521fe1a2314a30bd67fd047f76891799cf4401ab83';
    TransactionID:= FormatDateTime('yyyymmddHHnnss', Now) + _nomor;

    if IsiPulsa( _nomor,'5') then
    begin

    end;
    LogUtil.Add( Message, 'PULSA');
    Free;
  end;


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
var
  nominal: string;
begin
  Result := '';
  nominal := Params.Values['nominal_value'];
  if nominal = '' then
    nominal := '1';
  with TCurrencyIbacorIntegration.Create do
  begin
    Debug := True;
    Token := Config[IBACOR_TOKEN];
    if Token <> '' then
    begin
      if Params.Values['ke_value'] = 'rupiah' then
        Params.Values['ke_value'] := 'idr';
      if Params.Values['ke_value'] = 'dollar' then
        Params.Values['ke_value'] := 'usd';
      if Params.Values['ke_value'] = 'dolar' then
        Params.Values['ke_value'] := 'usd';
      Result := Converter(Params.Values['dari_value'], Params.Values['ke_value'],
        s2i(nominal));
      if Result = '' then
        Result := _CARIK_CURRENCY_MSG_FAILED;
    end;
    Free;
  end;
end;

function TCarikWebModule.ocrCognitiveHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _url: string;
begin
  Result := '';
  if Config[COGNITIVE_OCR_TOKEN] = '' then
    Exit;
  if not isTelegramGroup then
  begin
    Result := _TELEGRAM_ERR_GROUP_ONLY;
    Exit;
  end;
  s := getTelegramImageID;
  if s = '' then
    Exit;

  _url := '';
  with TTelegramIntegration.Create do
  begin
    Token := Config[TELEGRAM_TOKEN];
    s := GetFilePath(s);
    if s <> '' then
    begin
      _url := format(TELEGRAM_FILEURL, [Token]) + s;
    end;
  end;

  if _url = '' then
    // TODO: !!! kalau text kosong "@CarikBot" bingung dia
    Exit;

  with TCognitiveOCR.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Result := Scan(_url);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;

  if Result <> '' then
  begin
    s := SimpleBOT.GetResponse(IntentName + _RESPONSE);
    Result := Format(s, [Result]);
  end
  else
    Result := SimpleBOT.GetResponse(IntentName + 'NoText');
end;

function TCarikWebModule.imageTranslationHandler(const IntentName: string;
  Params: TStrings): string;
var
  s: string;
begin
  Result := '';
  if FImageURL = '' then
    Exit;

  // scan image to text
  with TCognitiveOCR.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Result := Scan(FImageURL);
    Free;
  end;
  Result := Trim(Result);

  if Result = '' then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'NoText');
    Exit;
  end;

  with TYandexTranslateIntegration.Create do
  begin
    Key := Config[YANDEX_KEY];
    s := Translate('en-id', Result);
    s := Trim(s);
    Free;
  end;
  if s <> '' then
  begin
    if s <> Result then
      Result := SimpleBOT.GetResponse(IntentName + 'Response') +
        Result + #10#10'*Translation:*'#10 + s
    else
      Result := SimpleBOT.GetResponse(IntentName + 'Response') + Result;
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.imageSpecificCognitiveHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _url: string;
begin
  Result := '';
  if Config[COGNITIVE_OCR_TOKEN] = '' then
    Exit;
  if not isTelegramGroup then
  begin
    Result := _TELEGRAM_ERR_GROUP_ONLY;
    Exit;
  end;
  s := getTelegramImageID;
  if s = '' then
    Exit;

  _url := '';
  with TTelegramIntegration.Create do
  begin
    Token := Config[TELEGRAM_TOKEN];
    s := GetFilePath(s);
    if s <> '' then
    begin
      _url := format(TELEGRAM_FILEURL, [Token]) + s;
    end;
  end;

  if _url = '' then
    // TODO: !!! kalau text kosong "@CarikBot" bingung dia
    Exit;

  with TCognitiveDomainSpecific.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Model := 'celebrities';
    Result := Scan(_url);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;

  if Result <> '' then
  begin
    s := SimpleBOT.GetResponse(IntentName + _RESPONSE);
    Result := Format(s, [Result]);
  end
  else
    Result := SimpleBOT.GetResponse(IntentName + 'NotFound');
end;

function TCarikWebModule.imageAnalyzeCognitiveHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _url: string;
begin
  Result := '';
  if Config[COGNITIVE_OCR_TOKEN] = '' then
    Exit;
  if not isTelegramGroup then
  begin
    Result := _TELEGRAM_ERR_GROUP_ONLY;
    Exit;
  end;
  s := getTelegramImageID;
  if s = '' then
    Exit;

  _url := '';
  with TTelegramIntegration.Create do
  begin
    Token := Config[TELEGRAM_TOKEN];
    s := GetFilePath(s);
    if s <> '' then
    begin
      _url := format(TELEGRAM_FILEURL, [Token]) + s;
    end;
  end;

  if _url = '' then
    // TODO: !!! kalau text kosong "@CarikBot" bingung dia
    Exit;

  with TCognitiveAnalyze.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Features := 'Description,Faces,Categories,Tags,Adult';
    Details := 'celebrities';
    Result := Analyze(_url);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    if Result <> '' then
    begin
      // force translate
      //Result := StringReplace(Result, 'indoor', 'dalam ruangan', [rfReplaceAll]);

      with TYandexTranslateIntegration.Create do
      begin
        Key := Config[YANDEX_KEY];
        s := Translate('en-id', Caption);
        s := StringReplace(s, #10, '\n', [rfReplaceAll]);
        Free;
      end;
      Result := Result + s;
    end;
    Free;
  end;

  if Result <> '' then
  begin
    s := SimpleBOT.GetResponse(IntentName + _RESPONSE);
    Result := Format(s, [Result]);
  end
  else
    Result := SimpleBOT.GetResponse(IntentName + 'NotFound');

end;

function TCarikWebModule.imageFullAnalyzeHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _tags, clarifai: string;
  img: TClarifai;
begin
  Result := '';
  if FImageURL = '' then
    Exit;

  // clarifai
  img := TClarifai.Create;
  img.ClientID := Config[CLARIFAI_CLIENTID];
  img.ClientSecret := Config[CLARIFAI_CLIENTSECRET];
  img.Token := img.RequestToken;
  img.ImageURL := FImageURL;
  if FMessengerMode = mmFacebook then
    clarifai := img.GetTagsAsString(True)  // if facebook, download it first
  else
    clarifai := img.GetTagsAsString;
  img.Free;

  if clarifai <> '' then
  begin
    with TYandexTranslateIntegration.Create do
    begin
      Key := Config[YANDEX_KEY];
      clarifai := Translate('en-id', clarifai);
      Free;
    end;
    Result := Result + #10#10'*Kategori yg mirip:*'#10 + clarifai;
  end;

  // OCR
  with TCognitiveOCR.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    s := Scan(FImageURL);
    Free;
  end;
  s := Trim(s);
  if s <> '' then
    Result := Result + #10#10'*Sepertinya ada tulisan:*'#10 + s;

  // Analyze
  with TCognitiveAnalyze.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Features := 'Description,Faces,Categories,Tags,Adult';
    Details := 'celebrities';
    TagCharacter := '#';
    _tags := Analyze(FImageURL);
    _tags := StringReplace(_tags, #10, '\n', [rfReplaceAll]);
    if _tags <> '' then
    begin
      // force translate
      //s := StringReplace(s, 'indoor', 'dalam ruangan', [rfReplaceAll]);

      s := StringReplace(Caption, 'picture', 'foto/gambar', [rfReplaceAll]);
      with TYandexTranslateIntegration.Create do
      begin
        Key := Config[YANDEX_KEY];
        FLanguage := Detect(s);
        if FLanguage = 'id' then
          FLanguage := 'id-en'
        else
          FLanguage := FLanguage + '-id';
        s := Translate(FLanguage, s);
        Free;
      end;
    end;
    Free;
  end;
  Result := Result + #10#10'*Tags:*'#10 + _tags + #10 + s;

  // tokoh dikenal
  with TCognitiveDomainSpecific.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Model := 'celebrities';
    s := Scan(FImageURL);
    Free;
  end;
  if s <> '' then
    Result := Result + #10#10'*Sepertinya ada foto:*'#10 + s;

  Result := Trim(Result);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
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
  if Config[TELEGRAM_TOKEN] = '' then
    Exit;
  if not isTelegramGroup then
  begin
    Result := 'Fitur ini hanya bisa dilakukan di *Telegram Group*';
    Exit;
  end;
  if Carik.IsImageRecognitionDisabled then
    Exit;
  s := getTelegramImageID;
  if s = '' then
    Exit;

  _url := '';
  with TTelegramIntegration.Create do
  begin
    Token := Config[TELEGRAM_TOKEN];
    s := GetFilePath(s);
    if s <> '' then
    begin
      _url := format(TELEGRAM_FILEURL, [Token]) + s;
    end;
  end;

  if _url = '' then
    // TODO: !!! kalau text kosong "@CarikBot" bingung dia
    Exit;

  _img := TClarifai.Create;
  _img.ClientID := Config[CLARIFAI_CLIENTID];
  _img.ClientSecret := Config[CLARIFAI_CLIENTSECRET];
  _img.Token := _img.RequestToken;
  _img.ImageURL := _url;
  Result := _img.GetTagsAsString;
  _img.Free;

  if Result <> '' then
  begin
    s := SimpleBOT.GetResponse(IntentName + 'Response');
    Result := Format(s, [Result]);
  end
  else
    Result := SimpleBOT.GetResponse(IntentName + 'NoResponse');

  Carik.ImageRecognitionCounting;
end;

function TCarikWebModule.lokasiHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword: string;
begin
  _keyword := Params.Values['keyword_value'];
  Result := Params.Values['Lokasi_value'] + ' ' + Params.Values['keyword_value'];
  if (_keyword = 'sini') or (_keyword = 'disini') or (_keyword = 'di sini') or
    (_keyword = 'terdekat') or (_keyword = '') or (Result = 'rumah sakit') then
  begin
    SimpleBOT.UserData['OBJECT_DETAIL'] := Params.Values['Lokasi_value'];
    Result := SimpleBOT.GetResponse(IntentName + 'NoLocation');
    Result := StringReplace(Result, '%lokasi%', Params.Values['Lokasi_value'],
      [rfReplaceAll]);
    Exit;
  end;

  _keyword := trim(Params.Values['Lokasi_value'] + ' ' +
    Params.Values['keyword_value']);
  with TGooglePlaceIntegration.Create do
  begin
    Key := Config[GOOGLE_KEY];
    Result := SearchAsText(_keyword, s2f(Params.Values['lat_value']),
      s2f(Params.Values['lon_value']));

    if Count = 1 then
    begin
      FVenueName := Title;
      FVenueAddress := Address;
      FVenueLatitude := Latitude;
      FVenueLongitude := Longitude;
      FSendVenue := True;
      if MessengerMode = mmTelegram then
        Result := '';
      if MessengerMode = mmLine then
        Result := '';
    end;

    if Result = '' then
      Result := SimpleBOT.GetResponse(IntentName + 'NotFound');
    Free;
  end;
end;

// internal usage
// lokasi_sesuatu -6.228018 106.82453 terminal
function TCarikWebModule.lokasiDenganKoordinatHandler(const IntentName: string;
  Params: TStrings): string;
begin
  {
  _keyword := Params.Values['$3'];
  with TGooglePlaceIntegration.Create do
  begin
    Key := Config[GOOGLE_KEY];
    Result := SearchAsText(_keyword, s2f(Params.Values['$1']), s2f(Params.Values['$2']));
    Free;
  end;
  }

  Params.Values['lat_value'] := Params.Values['$1'];
  Params.Values['lon_value'] := Params.Values['$2'];
  Params.Values['keyword_value'] := Params.Values['$3'];
  Result := lokasiHandler(IntentName, Params);
end;

function TCarikWebModule.lokasiKulinerHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _keyword: string;
begin
  _keyword := Params.Values['keyword_value'];
  if (_keyword = 'sini') or (_keyword = 'disini') or (_keyword = 'di sini') or
    (_keyword = '') then
  begin
    SimpleBOT.UserData['OBJECT_DETAIL'] := Params.Values['Lokasi_value'];
    Result := SimpleBOT.GetResponse(IntentName + 'NoLocation');
    Result := StringReplace(Result, '%lokasi%', Params.Values['Lokasi_value'],
      [rfReplaceAll]);
    Exit;
  end;

  with TZomatoIntegration.Create do
  begin
    Key := Config[ZOMATO_KEY];
    s := SearchAsJson(_keyword, s2f(Params.Values['lat_value']),
      s2f(Params.Values['lon_value']), 5);
    if s <> '' then
      Result := ConvertJsonToTextInfo(s);
    Free;
  end;

  if s = '' then
  begin
    Result := SimpleBOT.GetResponse('NotFound');
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Exit;
  end;

  if MessengerMode = mmLine then
  begin
    s := GenerateLineCarouselFromCulinaryData(_keyword, s);
    if s <> '' then
    begin
      FSendRichContent := True;
      FRichContent := s;
      Result := SimpleBOT.GetResponse(IntentName + 'Waiting');
    end;
  end
  else
  begin
    //already in result variable
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

// internal usage
// lokasi_kuliner -6.228018 106.82453 terminal
function TCarikWebModule.lokasiKulinerDenganKoordinatHandler(
  const IntentName: string; Params: TStrings): string;
begin
  {
  with TZomatoIntegration.Create do
  begin
    Key := Config[ZOMATO_KEY];
    Result := Search(Params.Values['$3'], s2f(Params.Values['$1']),
      s2f(Params.Values['$2']));
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;
  }

  Params.Values['lat_value'] := Params.Values['$1'];
  Params.Values['lon_value'] := Params.Values['$2'];
  Params.Values['keyword_value'] := Params.Values['$3'];
  Result := lokasiKulinerHandler(IntentName, Params);
end;

function TCarikWebModule.carikAdminTambahHandler(const IntentName: string;
  Params: TStrings): string;
begin
  if not isTelegramGroup then
    Exit;
  if Carik.AdminAdd(Params.Values['username_value']) then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'Response');
    Result := StringReplace(Result, '%username_value%',
      Params.Values['username_value'], [rfReplaceAll]);
  end;
end;

function TCarikWebModule.carikAdminHapusHandler(const IntentName: string;
  Params: TStrings): string;
begin
  if not isTelegramGroup then
    Exit;
  if Carik.AdminDel(Params.Values['username_value']) then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'Response');
    Result := StringReplace(Result, '%username_value%',
      Params.Values['username_value'], [rfReplaceAll]);
  end;
end;

function TCarikWebModule.carikNewMemberCustomMessageHandler(
  const IntentName: string; Params: TStrings): string;
var
  s: string;
begin
  Result := 'Maaf, sepertinya kurang lengkap';
  s := Params.Values['reponse_value'];

  if not Carik.IsPermitted then
  begin
    Result := SimpleBOT.GetResponse('NoAccess');
    Exit;
  end;

  s := StringReplace(s, #10, '\n', [rfReplaceAll]);
  s := StringReplace(s, #13, '\n', [rfReplaceAll]);
  Carik.CustomMessage['WELCOME'] := s;
  Result := SimpleBOT.GetResponse(IntentName + 'Response') +
    SimpleBOT.StringReplacement(s);
  if s = '' then
    Result := SimpleBOT.GetResponse(IntentName + 'Remove');

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.carikMemberBaruHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := Carik.CustomMessage['WELCOME'];
  if Result = '' then
    Result := SimpleBOT.GetResponse(IntentName + 'Response');//ulil
  Result := StringReplace(Result, '%username%', FInvitedUserName, [rfReplaceAll]);
  Result := StringReplace(Result, '%fullname%', FInvitedFullName, [rfReplaceAll]);
end;

function TCarikWebModule.carikMemberBaruAbaikanHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := Carik.MemberBaruAbaikanHandler(IntentName, Params);
  if Result = 'OK' then
    SimpleBOT.UserData[_GROUP_MEMBERBARU_ABAIKAN] := '1';
end;

function TCarikWebModule.carikMemberBaruSapaHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := Carik.MemberBaruSapaHandler(IntentName, Params);
  if Result = 'OK' then
    SimpleBOT.UserData[_GROUP_MEMBERBARU_ABAIKAN] := '0';
end;

function TCarikWebModule.quickCountHandler(const IntentName: string;
  Params: TStrings): string;
var
  provinceCode: string;
begin
  provinceCode := '25823';
  if Params.Values['province_value'] = 'banten' then
    provinceCode := '51578';
  if Params.Values['province_value'] = 'aceh' then
    provinceCode := '1';
  with TKawalPemiluIntegration.Create do
  begin
    Result := ProvinceRealCountInfo(provinceCode);
    Free;
  end;
  if Result = '' then
    Result := SimpleBOT.GetResponse(IntentName + 'NoResponse')
  else
    Result := Result + '\n\n' + SimpleBOT.GetResponse(IntentName + 'Response');

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.kofaJadwalImsyakHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, kota, imsyak, maghrib: string;
  t: Int64;
begin
  Result := '';
  kota := Params.Values['kota_value'];
  if kota = '' then
    kota := 'jakarta selatan';
  if kota = 'jogja' then
    kota := 'yogyakarta';
  if kota = 'jogjakarta' then
    kota := 'yogyakarta';

  with TMasKofaJadwalShalatIntegration.Create do
  begin
    Username := '';
    Password := '';
    SessionID := Config[MASKOFA_SHALAT_SESSIONID];
    CityList.LoadFromFile(Config[MASKOFA_SHALAT_CITYLISTS]);
    Result := Find(kota);
    if Result <> '' then
    begin
      Result := '';
      s := FormatDateTime('d', Today);
      imsyak := jsonGetData(Data, 'data/' + s + '/Imsyak');
      maghrib := jsonGetData(Data, 'data/' + s + '/Maghrib');
      Result := Result + #10'*Imsyak: ' + jsonGetData(Data, 'data/' + s + '/Imsyak') + '*';
      Result := Result + #10'Shubuh: ' + jsonGetData(Data, 'data/' + s + '/Shubuh');
      Result := Result + #10'Dzuhur: ' + jsonGetData(Data, 'data/' + s + '/Dzuhur');
      Result := Result + #10'Ashr: ' + jsonGetData(Data, 'data/' + s + '/Ashr');
      Result := Result + #10'*Maghrib: ' + jsonGetData(Data, 'data/' + s + '/Maghrib') + '*';
      Result := Result + #10'Isya: ' + jsonGetData(Data, 'data/' + s + '/Isya');
    end;
    Free;
  end;

  if Result = '' then
    Result := SimpleBOT.GetResponse(IntentName + 'NoData')
  else
  begin
    if Params.Values['$2'] = 'imsyak' then
      Result := '*' + ucwords('Jadwal Imsyak ' + kota + ':*') + #10 + Result
    else
      Result := '*' + ucwords('Jadwal Puasa ' + kota + ':*') + #10 + Result;
    if Time > StrToTime('03:00') then
    begin
      if MinutesBetween(Time, StrToTime('03:00')) < 60 then
        Result := Result + #10#10 + SimpleBOT.GetResponse(IntentName + 'Sahur');
    end;
    if Time > StrToTime(imsyak) then
    begin
      if MinutesBetween(Time, StrToTime(imsyak)) < 5 then
        Result := Result + #10#10 + SimpleBOT.GetResponse(IntentName + 'Imsyak');
    end;
    if Time > StrToTime(maghrib) then
    begin
      if MinutesBetween(Time, StrToTime(maghrib)) < 10 then
      Result := Result + #10#10 + SimpleBOT.GetResponse(IntentName + 'Maghrib');
    end;
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.kofaJadwalSholatHandler(const IntentName: string;
  Params: TStrings): string;
var
  kota: string;
begin
  Result := '';
  kota := Params.Values['kota_value'];
  if kota = '' then
    kota := 'jakarta selatan';
  if kota = 'jogja' then
    kota := 'yogyakarta';
  if kota = 'jogjakarta' then
    kota := 'yogyakarta';

  with TMasKofaJadwalShalatIntegration.Create do
  begin
    Username := '';
    Password := '';
    SessionID := Config[MASKOFA_SHALAT_SESSIONID];
    CityList.LoadFromFile(Config[MASKOFA_SHALAT_CITYLISTS]);
    Result := Find(kota);
    Free;
  end;

  if Result = '' then
    Result := SimpleBOT.GetResponse(IntentName + 'NoData')
  else
    Result := ucwords('*Jadwal sholat ' + kota + ':*') + #10 + Result;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.alquranTerjemahanHandler(const IntentName: string;
  Params: TStrings): string;
var
  lst: TStrings;
begin
  Result := '';
  lst := Explode(Params.Values['surat_value'], ':');
  if lst.Count <> 2 then
  begin
    Result := 'cara penulisan salah, seharusnya:\n"terjemahan quran nosurat:noayat"';
    Exit;
  end;

  with TAlquranOnline.Create do
  begin
    //Result := FindTerjemahan(lst[0], lst[1]);
    Result := Find(lst[0], lst[1]);
    if AudioURL <> '' then
    begin
      FSendAudio := True;
      FFileURL := AudioURL;
      FCaption := Surat;
    end;
    Result := Terjemahan;
    Free;
  end;

  if Result = '' then
    Result := SimpleBOT.GetResponse(IntentName + 'NoData')
  else
    Result := SimpleBOT.GetResponse(IntentName + 'Response') + Result;

  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, '%surat_value%', FCaption + ' ' +
    Params.Values['surat_value'], [rfReplaceAll]);
  lst.Free;
end;

// jadwal/agenda
function TCarikWebModule.kloudlessCalendarEventListHandler(const IntentName: string;
  Params: TStrings): string;
var
  i: integer;
  s: string;
  startTime, endTime: TDateTime;
begin
  Result := '';

  // hari
  startTime := Today;
  endTime := IncDay(Today, 30);

  s := Params.Values['CalendarRange_value'];
  if (s = 'hari ini') or (s = 'sekarang') then
  begin
    startTime := Today;
    endTime := Tomorrow;
  end;
  if s = 'besok' then
  begin
    startTime := Tomorrow;
    endTime := IncDay(Tomorrow);
  end;
  if s = 'lusa' then
  begin
    startTime := IncDay(Tomorrow);
    endTime := IncDay(Tomorrow, 2);
  end;
  if (s = 'minggu ini') or (s = 'seminggu') then
  begin
    startTime := Today;
    endTime := IncDay(Today, 7);
  end;
  if (s = 'bulan ini') or (s = 'sebulan') then
  begin
    startTime := Today;
    endTime := IncDay(Today, 30);
  end;
  if (s = 'tahun ini') or (s = 'setahun') then
  begin
    startTime := Today;
    endTime := IncDay(Today, 360);
  end;

  //Result := FormatDateTime('yyyy/mm/dd HH:nn', startTime) + ' - ' + FormatDateTime('yyyy/mm/dd HH:nn', endTime);
  //exit;


  with TKlaudlessCalendarIntegration.Create do
  begin
    AppID := Config[KLAUDLESS_APPID];
    APIKey := Config[KLAUDLESS_APIKEY];
    Result := GetEventListAsJson(Config[KLAUDLESS_CALENDAR_ACCOUNTID],
      Config[KLAUDLESS_CALENDAR_ID], startTime, endTime);

    Result := EventList(generateCalendarID);
    for i := 0 to 10 do
    begin
      Result := StringReplace(Result, i2s(i) + '. ', _ICON_NUMBER_ARRAY[i] +
        ' ', [rfReplaceAll]);
    end;
    if Result <> '' then
      Result := SimpleBOT.GetResponse('CalendarData') + '\n\n' + Result;
    if Result = '' then
      Result := SimpleBOT.GetResponse('CalendarNoData');

    Free;
  end;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.kloudlessCalendarEventCreateHandler(
  const IntentName: string; Params: TStrings): string;
var
  jam, menit, jamSelesai, penambahanJam: integer;
  s, eventName: string;
  startTime, endTime: TDateTime;
  jamTime: TTime;
begin
  Result := '';

  // event name
  eventName := Params.Values['EventName_value'];
  if eventName = '' then
    eventName := 'Meeting';
  eventName := generateEventName(eventName);

  // hari
  startTime := Today;
  if Params.Values['CalendarRange_value'] = 'besok' then
    startTime := Tomorrow;
  if Params.Values['CalendarRange_value'] = 'lusa' then
    startTime := IncDay(Tomorrow);
  if Params.Values['CalendarRange_value'] = 'besok lusa' then
    startTime := IncDay(Tomorrow, 2);

  // tanggal
  s := Params.Values['Tanggal_value'];
  if s <> '' then
  begin
    startTime := StringHumanToDate(s);
  end;

  // jam
  menit := 0;
  jamSelesai := 1;
  penambahanJam := 0;
  if Params.Values['Nominal_value'] <> '' then
    Params.Values['Jam_value'] := Params.Values['Nominal_value'];
  s := Params.Values['Jam_value'];
  if s <> '' then
  begin
    jam := s2i(s);
    if jam = 0 then
    begin
      jamTime := StrToTime(s);
      jam := HourOf(jamTime);
      menit := MinuteOf(jamTime);
    end;
    if jam > 12 then
      jam := jam - 12;
    if Params.Values['Waktu_value'] = 'siang' then
    begin
      if (jam >= 1) and (jam < 4) then
        jam := jam + 12;
    end;
    if Params.Values['Waktu_value'] = 'sore' then
      jam := jam + 12;
    if (Params.Values['Waktu_value'] = 'malam') or
      (Params.Values['Waktu_value'] = 'malem') then
      jam := jam + 12;
    startTime := IncHour(startTime, jam);
  end
  else
  begin
    startTime := IncHour(startTime, 8);
    if Params.Values['Waktu_value'] = 'siang' then
      startTime := IncHour(startTime, 4);
    if Params.Values['Waktu_value'] = 'sore' then
      startTime := IncHour(startTime, 9);
    if (Params.Values['Waktu_value'] = 'malam') or
      (Params.Values['Waktu_value'] = 'malem') then
      startTime := IncHour(startTime, 11);
  end;// jam
  startTime := IncMinute(startTime, menit);


  endTime := IncHour(startTime, 1);
  if Params.Values['JamSelesai_value'] <> '' then
  begin
    endTime := StartOfTheDay(startTime);
    jamSelesai := s2i(Params.Values['JamSelesai_value']);
    if jamSelesai = 0 then
      jamSelesai := 1;
    if jamSelesai < 12 then
    begin
      if Params.Values['Waktu_value'] = 'sore' then
        jamSelesai := jamSelesai + 12;
      if Params.Values['Waktu_value'] = 'malam' then
        jamSelesai := jamSelesai + 12;
    end;
    endTime := IncHour(endTime, jamSelesai);
  end;

  // TEST
  //Result := 'x: ' + FormatDateTime('dd/mmm/yyyy HH:nn', startTime) + ' - ' + FormatDateTime('dd/mmm/yyyy HH:nn', endTime);
  //Exit;

  with TKlaudlessCalendarIntegration.Create do
  begin
    AppID := Config[KLAUDLESS_APPID];
    APIKey := Config[KLAUDLESS_APIKEY];
    Result := EventCreate(Config[KLAUDLESS_CALENDAR_ACCOUNTID],
      Config[KLAUDLESS_CALENDAR_ID], ucwords(eventName), startTime, endTime);

    Free;
  end;
  Result := SimpleBOT.GetResponse('CalendarEventCreated');
  Result := StringReplace(Result, '%eventname%', Params.Values['EventName_value'],
    [rfReplaceAll]);
  Result := StringReplace(Result, '%eventtime%',
    FormatDateTime('dd/mmm/yyyy HH:nn', startTime), [rfReplaceAll]);
end;

function TCarikWebModule.bmkgSimpleInfoHandler(const IntentName: string;
  Params: TStrings): string;
var
  s : string;
begin
  Result := '';
  with TBMKGIntegration.Create do
  begin
    Result := SimpleInfo;
    Result := StringReplace(Result, ', ', '\n', [rfReplaceAll]);

    //Result := '12-Jun-17 06:15:07 WIB Lok:8.36 LS,106.18 BT (179 ';
    s := Trim( StringCut('Lok:', 'BT', Result));
    if Pos( ' LU', s) > 0 then
    begin
      s := StringReplace( s, ' LU', '', [rfReplaceAll]);
    end
    else
    begin
      s := '-' + StringReplace( s, ' LS', '', [rfReplaceAll]);
    end;
    s := '\n\nhttps://www.google.com/maps/place/'+s+'/@'+s+',5z';

    Result := Result + s + SimpleBOT.GetResponse(IntentName + 'Disclaimer');
    Free;
  end;

  if Result = '' then
    Result := SimpleBOT.GetResponse('NotFound');

end;

function TCarikWebModule.openweatherInfoHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword: string;

  function translate(ASource: string): string;
  begin
    Result := SimpleBOT.SimpleAI.GetResponse('LanguageID', '', trim(ASource));
    if (Result = '') or (Result = '._') then
      Result := ASource;
  end;

begin
  Result := '';
  _keyword := Params.Values['keyword_value'];
  if _keyword = 'jogja' then
    _keyword := 'yogyakarta';
  if _keyword = 'jogjakarta' then
    _keyword := 'yogyakarta';
  if Pos(',', _keyword) < 1 then
    _keyword := _keyword + ',id';
  with TOpenWeatherMapIntegration.Create do
  begin
    Key := Config[OPENWEATHERMAP_KEY];
    Result := WeatherAsJson(_keyword);

    Result := 'Cuaca di ' + Data['name'] + ':';
    Result := Result + #10 + translate(Data['weather[0].main']) +
      ', ' + translate(Data['weather[0].description']);

    Result := Result + #10'Suhu min: ' + FormatFloat('#0.00',
      GetDataFloat('main.temp_min'));
    Result := Result + #10'Suhu max: ' + FormatFloat('#0.00',
      GetDataFloat('main.temp_max'));
    Result := Result + #10'Tekanan: ' + FormatFloat('#0.00',
      GetDataFloat('main.pressure'));

    Result := Result + #10'Kec. Angin: ' + FormatFloat('#0.00',
      GetDataFloat('wind.speed'));
    Result := Result + #10'' + FormatFloat('#0.000', GetDataFloat('wind.deg')) +
      ' derajat';

    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;
end;

function TCarikWebModule.apixuweatherInfoHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword, _suffix: string;
  _img: string;

  function translate(ASource: string): string;
  begin
    Result := SimpleBOT.SimpleAI.GetResponse('LanguageID', '', trim(ASource));
    if (Result = '') or (Result = '._') then
      Result := ASource;
  end;

begin
  Result := '';
  _keyword := Params.Values['keyword_value'];
  if _keyword = 'jogja' then
    _keyword := 'yogyakarta';
  if _keyword = 'jogjakarta' then
    _keyword := 'yogyakarta';
  if _keyword = '' then
  begin
    _keyword := 'jakarta';
    _suffix := '\n\nUntuk kota lain, bisa coba:\n *CUACA DI NAMAKOTA*';
  end;
  if Pos(',', _keyword) < 1 then
    _keyword := _keyword + ',indonesia';
  with TApixuIntegration.Create do
  begin
    Key := Config[APIXU_KEY];
    Result := WeatherAsJson(_keyword);
    if Result <> '' then
    begin
      _img := 'http:' + Data['current.condition.icon'];
      Result := 'Cuaca di ' + Data['location.name'] + ':';
      Result := Result + #10 + translate(Data['current.condition.text']);
      Result := Result + #10'Suhu: ' + FormatFloat('#0.0',
        GetDataFloat('current.temp_c')) + ' °C';

      Result := Result + #10'Kec. Angin: ' + FormatFloat('#0.00',
        GetDataFloat('current.wind_kph')) + ' kph';
      Result := Result + #10'Arah Angin: ' + FormatFloat('#0.00',
        GetDataFloat('current.wind_degree')) + '° (' + Data['current.wind_dir'] + ')';
      Result := Result + #10'Tekanan: ' + FormatFloat('#0.00',
        GetDataFloat('current.pressure_mb')) + ' mb';

      Result := Result + _suffix;
    end;
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);

    // force send photo
    {
    FSendPhoto := True;
    FFileURL := _img;
    FCaption := Result;
    }

    Free;
  end;
end;

function TCarikWebModule.beritaHariIniHandler(const IntentName: string;
  Params: TStrings): string;
var
  i: integer;
  s, urlRSS: string;
  json: TJSONData;
begin
  Result := '';

  s := Params.Values['Media_value'];
  urlRSS := Config['rss/' + s];
  if urlRSS = '' then
    urlRSS := Config['rss/antara'];

  with TRSSReaderLib.Create do
  begin
    LoadFromURL(urlRSS);

    s := AsJson;
    Free;
  end;

  if isEmpty(s) then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'NotFound');
    Exit;
  end;

  try
    json := GetJSON(s);
    i := json.Count;
    Result := SimpleBOT.GetResponse(IntentName + 'Response');
    for i := 0 to 4 do
    begin
      Result := Result + #10'- ' +
        trim(jsonGetData(json, 'items[' + i2s(i) + '].title')) + '.';
      Result := Result + #10 + jsonGetData(json, 'items[' + i2s(i) + '].link') + #10;
    end;
    json.Free;
  except
  end;

  if MessengerMode = mmLine then
  begin
    s := LineBerita('Berita ' + ucwords(Params.Values['Media_value']), s);
    if s <> '' then
    begin
      FSendRichContent := True;
      FRichContent := s;
      Result := SimpleBOT.GetResponse(IntentName + 'Waiting');
    end;
  end;

  if MessengerMode = mmFacebook then
  begin
    s := FacebookBerita('Berita ' + ucwords(Params.Values['Media_value']), s);
    if s <> '' then
    begin
      FSendRichContent := True;
      FRichContent := s;
      Result := SimpleBOT.GetResponse(IntentName + 'Waiting');
    end;
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.conversionHashHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '...';
  if Params.Values['$1'] = 'md5' then
  begin
    Result := 'md5("' + Params.Values['Source_value'] + '") = ```' +
      MD5Print(MD5String(Params.Values['Source_value'])) + '```';
  end;
  if Params.Values['$1'] = 'sha' then
  begin
    Result := 'sha1("' + Params.Values['Source_value'] + '") = ```' +
      SHA1Print(SHA1String(Params.Values['Source_value'])) + '```';
  end;
end;

function TCarikWebModule.jobPlanetInfoHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword: string;
begin
  Result := '';
  _keyword := Params.Values['keyword_value'];
  if _keyword = '' then
  begin
    Exit;
  end;

  with TJobPlanetIntegration.Create do
  begin
    Result := Info(_keyword);
    if CompanyCount > 1 then
    begin
      Result := SimpleBOT.GetResponse(IntentName + 'Response', '', 'prefix') + Result;
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '', 'suffix');
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '',
        'visit') + URL;
      ;
    end;
    Free;
  end;
  Result := Trim(StringReplace(Result, #10, '\n', [rfReplaceAll]));
  if Result = '' then
    Result := SimpleBOT.GetResponse('NotFound');
end;

function TCarikWebModule.jobPlanetReviewHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword: string;
begin
  Result := '';
  _keyword := Params.Values['keyword_value'];
  if _keyword = '' then
  begin
    Exit;
  end;

  with TJobPlanetIntegration.Create do
  begin
    Result := Review(_keyword);
    if CompanyCount > 1 then
    begin
      Result := SimpleBOT.GetResponse(IntentName + 'Response', '', 'prefix') + Result;
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '', 'suffix');
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '',
        'visit') + URL;
      ;
    end;
    Free;
  end;
  Result := Trim(StringReplace(Result, #10, '\n', [rfReplaceAll]));
  if Result = '' then
    Result := SimpleBOT.GetResponse('NotFound');
end;

function TCarikWebModule.jobPlanetSalaryHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword: string;
begin
  Result := '';
  _keyword := Params.Values['keyword_value'];
  if _keyword = '' then
  begin
    Exit;
  end;

  with TJobPlanetIntegration.Create do
  begin
    Result := Salaries(_keyword);
    if CompanyCount > 1 then
    begin
      Result := SimpleBOT.GetResponse(IntentName + 'Response', '', 'prefix') + Result;
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '', 'suffix');
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '',
        'visit') + URL;
      ;
    end;
    Free;
  end;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  if Result = '' then
    Result := SimpleBOT.GetResponse('NotFound');
end;

function TCarikWebModule.jobPlanetVacancyHandler(const IntentName: string;
  Params: TStrings): string;
var
  keyword: string;
begin
  keyword := Params.Values['keyword_value'];
  with TJobPlanetIntegration.Create do
  begin
    Result := 'Lowongan ' + keyword + ':\n';
    Result := Result + Vacancies(keyword);
    Free;
  end;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.jobPlanetInterviewHandler(const IntentName: string;
  Params: TStrings): string;
var
  keyword: string;
begin
  keyword := Params.Values['keyword_value'];
  if keyword = '' then
  begin
    Result := SimpleBOT.GetResponse('Incomplete');
    Exit;
  end;

  with TJobPlanetIntegration.Create do
  begin
    Result := Interview(keyword);
    if CompanyCount > 1 then
    begin
      Result := SimpleBOT.GetResponse(IntentName + 'Response', '', 'prefix') + Result;
      //Result := '*Interview di ' + keyword + ':*\n' + Result;
      Result := Result + SimpleBOT.GetResponse(IntentName + 'Response', '', 'suffix');
    end;

    Free;
  end;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, '%', ' persen', [rfReplaceAll]);

end;

function TCarikWebModule.smartHomeGeneralHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := SimpleBOT.GetResponse(IntentName + 'NoAccount');
  with TSmartHomeTestIntegration.Create do
  begin
    AccountID := UniqueID;
    //Active:= True;
    //AddDevice( 'Lampu Kamar');
    //AddDevice('Lampu Depan');
    if IsHaveDevice then
    begin
      Result := SimpleBOT.GetResponse(IntentName + 'Exist') + #10;
      Result := Result + GetDeviceAsText;
    end;
    Free;
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.smartHomeOnHandler(const IntentName: string;
  Params: TStrings): string;
var
  deviceName: string;
begin
  deviceName := Params.Values['PerangkatListrik_value'] + ' ' +
    Params.Values['apapun_value'];
  deviceName := ucwords(deviceName);

  Result := SimpleBOT.GetResponse('SmartHomeGeneralNoAccount');
  with TSmartHomeTestIntegration.Create do
  begin
    AccountID := UniqueID;
    AccountName := Carik.UserName;
    if IsHaveDevice then
    begin
      Result := SimpleBOT.GetResponse('SmartHomeGeneralNoDevice');
      if IsDeviceExist(deviceName) then
      begin
        SetDevice(deviceName, True);
        Result := SimpleBOT.GetResponse('SmartHomeGeneralDeviceStatus');
        Result := StringReplace(Result, '%device_name%', deviceName, [rfReplaceAll]);
        Result := StringReplace(Result, '%status%', 'nyala', [rfReplaceAll]);
        Result := StringReplace(Result, '%device_list%', GetDeviceAsText,
          [rfReplaceAll]);
      end;
    end;
    Free;
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.smartHomeOffHandler(const IntentName: string;
  Params: TStrings): string;
var
  deviceName: string;
begin
  deviceName := Params.Values['PerangkatListrik_value'] + ' ' +
    Params.Values['apapun_value'];
  deviceName := ucwords(deviceName);

  Result := SimpleBOT.GetResponse('SmartHomeGeneralNoAccount');
  with TSmartHomeTestIntegration.Create do
  begin
    AccountID := UniqueID;
    AccountName := Carik.UserName;
    if IsHaveDevice then
    begin
      Result := SimpleBOT.GetResponse('SmartHomeGeneralNoDevice');
      if IsDeviceExist(deviceName) then
      begin
        SetDevice(deviceName, False);
        Result := SimpleBOT.GetResponse('SmartHomeGeneralDeviceStatus');
        Result := StringReplace(Result, '%device_name%', deviceName, [rfReplaceAll]);
        Result := StringReplace(Result, '%status%', 'mati', [rfReplaceAll]);
        Result := StringReplace(Result, '%device_list%', GetDeviceAsText,
          [rfReplaceAll]);
      end;
    end;
    Free;
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.richContentHandler(const IntentName: string;
  Params: TStrings): string;
begin
  FSendRichContent := True;
  FRichContent := SimpleBOT.GetResponse('LineHomeMenu');
  LogUtil.add(FRichContent, 'LINE-RICH');
  Result := '';
end;

function TCarikWebModule.echoHandler(const IntentName: string; Params: TStrings
  ): string;
begin
  Result := Params.Values['$2'];
end;

function TCarikWebModule.PrepareTextToSpeech(AText: string): string;
begin
  Result := AText + ' ';
  Result := ReplaceAll(Result, ['aaa ', 'aa '], 'a ');
  Result := ReplaceAll(Result, ['iii ', 'ii '], 'i ');
  Result := ReplaceAll(Result, ['uuu ', 'uu '], 'u ');
  Result := ReplaceAll(Result, ['eee ', 'ee '], 'e ');
  Result := ReplaceAll(Result, ['ooo ', 'oo '], 'o ');
  Result := ReplaceAll(Result, ['aaa!', 'aa!'], 'a!');
  Result := ReplaceAll(Result, ['iii!', 'ii!'], 'i!');
  Result := ReplaceAll(Result, ['uuu!', 'uu!'], 'u!');
  Result := ReplaceAll(Result, ['eee!', 'ee!'], 'e!');
  Result := ReplaceAll(Result, ['ooo!', 'oo!'], 'o!');
  Result := ReplaceAll(Result, ['aaa?', 'aa?'], 'a?');
  Result := ReplaceAll(Result, ['iii?', 'ii?'], 'i?');
  Result := ReplaceAll(Result, ['uuu?', 'uu?'], 'u?');
  Result := ReplaceAll(Result, ['eee?', 'ee?'], 'e?');
  Result := ReplaceAll(Result, ['ooo?', 'oo?'], 'o?');
  Result := ReplaceAll(Result, ['aaa'], 'a');
  Result := ReplaceAll(Result, ['iii'], 'i');
  Result := ReplaceAll(Result, ['uuu'], 'u');
  Result := ReplaceAll(Result, ['eee'], 'e');
  Result := ReplaceAll(Result, ['ooo'], 'o');
  Result := ReplaceAll(Result, ['yyy'], 'i');
  Result := ReplaceAll(Result, ['*'], '');
  Result := StringReplace(Result, '. ', '., ', [rfReplaceAll]);
  Result := StringReplace(Result, '.'#10, '., ', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '\n', '._', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '.', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '.', [rfReplaceAll]);

  Result := StringReplace(Result, ' wr wb', 'warahmatulahi wabarokatuh', [rfReplaceAll]);

  if Length(Result) > TEXT2SPEECH_MAX_CHAR then
  begin
    Result := copy(Result, 0, TEXT2SPEECH_MAX_CHAR);
    Result := copy(Result, 0, RPos('_', Result) - 1);
  end;

  Result := Trim(Result);
end;

function TCarikWebModule.texttospeechHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Config[CARIK_TTS_URL] = '' then
    Exit;

  FFileURL := PrepareTextToSpeech(Params.Values['text_value']);

  FFileURL := Config[CARIK_TTS_URL] + FFileURL;
  FSendAudio := True;
end;

function TCarikWebModule.speakingModeOnHandler(const IntentName: string;
  Params: TStrings): string;
begin
  SpeakingMode := True;

  Result := SimpleBOT.GetResponse(IntentName + 'Response');
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.speakingModeOffHandler(const IntentName: string;
  Params: TStrings): string;
begin
  SpeakingMode := False;

  Result := SimpleBOT.GetResponse(IntentName + 'Response');
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.trainScheduleHandler(const IntentName: string;
  Params: TStrings): string;

  function generateView(AJsonString: string): string;
  var
    i: integer;
    s, s2: string;
    json: TJSONData;
  begin
    json := GetJSON(AJsonString);
    s := '';
    s2 := '';
    Result := '';
    try
      for i := 0 to json.GetPath('data').Count - 1 do
      begin
        s2 := jsonGetData(json, 'data[' + i2s(i) + ']/kereta/name') +
          ' ' + jsonGetData(json, 'data[' + i2s(i) + ']/kereta/class');
        if s2 <> s then
        begin
          s := s2;
          Result := Result + #10 + s + #10;
          Result := Result + jsonGetData(json, 'data[' + i2s(i) + ']/berangkat/jam') +
            ' - ' + jsonGetData(json, 'data[' + i2s(i) + ']/datang/jam') + #10;
        end;
        Result := Result + 'Rp. ' + jsonGetData(json, 'data[' + i2s(i) + ']/harga/rp') +
          ' (SubClass ' + jsonGetData(json, 'data[' + i2s(i) + ']/harga/subclass') + ')';
        if jsonGetData(json, 'data[' + i2s(i) + ']/tiket') = 'Habis' then
          Result := Result + ' HABIS';
        Result := Result + #10;
      end;
    except
    end;
  end;

var
  s, asal, tujuan: string;
  asalKey, tujuanKey: string;
  asalInfo, tujuanInfo: string;
  asalLst, tujuanLst: TStrings;
  d: TDate;
  indexDate: integer;
  trainSchedule: TIbacorTrainScheduleController;
begin
  Result := 'ga ketemu';
  d := Today;
  asal := Params.Values['KotaAsal_value'];
  tujuan := Params.Values['KotaTujuan_value'];
  s := Params.Values['Tanggal_value'];
  if s = 'besok' then
    d := Tomorrow;
  if s = 'lusa' then
    d := Date() + 2;
  if s = 'besok lusa' then
    d := Date() + 3;

  s := Params.Values['Tanggal_value'];
  if s <> '' then
  begin
    d := StringHumanToDate(s);
  end;

  indexDate := DaysBetween(Today, d);
  if indexDate > 90 then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'TooLong');
    Exit;
  end;

  asalLst := Explode(asal, ' ');
  tujuanLst := Explode(tujuan, ' ');

  trainSchedule := TIbacorTrainScheduleController.Create;
  trainSchedule.Token := Config[IBACOR_TOKEN];

  // get station list
  asalInfo := #10'Nama-nama stasiun di ' + ucwords(asalLst[0]) + ':'#10;
  s := trainSchedule.StationListAsString(asalLst[0]);
  asalInfo := asalInfo + s;
  tujuanInfo := #10'Nama-nama stasiun di ' + ucwords(tujuanLst[0]) + ':'#10;
  s := trainSchedule.StationListAsString(tujuanLst[0]);
  tujuanInfo := tujuanInfo + s;

  if (asalLst.Count < 2) or (tujuanLst.Count < 2) then
  begin
    Result := SimpleBOT.GetResponse('TiketKeretaInfo') + #10;

    if asalLst.Count = 1 then
    begin
      Result := Result + #10 + asalInfo;
    end;
    Result := Result + #10;
    if tujuanLst.Count = 1 then
    begin
      Result := Result + #10 + tujuanInfo;
    end;
    asalLst.Free;
    tujuanLst.Free;
    trainSchedule.Free;
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Exit;
  end;

  s := trainSchedule.DateKey(indexDate);
  if s = '' then
  begin
    Result := 'Data tanggal tidak tersedia';
    trainSchedule.Free;
    Exit;
  end;

  // station key
  asalKey := trainSchedule.StationKey(asalLst[0], asalLst[1]);
  if asalKey = '' then
  begin
    Result := 'Kota/Stasiun Asal tidak ditemukan.' + #10;
    if trainSchedule.IsValidCity(asalLst[0]) then
      Result := Result + asalInfo;
    trainSchedule.Free;
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Exit;
  end;

  // destination station key
  tujuanKey := trainSchedule.StationKey(tujuanLst[0], tujuanLst[1]);
  if tujuanKey = '' then
  begin
    Result := 'Kota/Stasiun Tujuan tidak ditemukan.' + #10;
    if trainSchedule.IsValidCity(tujuanLst[0]) then
      Result := Result + tujuanInfo;
    trainSchedule.Free;
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Exit;
  end;

  Result := trainSchedule.ScheduleStationToStation(s, asalKey, tujuanKey);
  if Result = '' then
  begin
    Result := 'Data tidak ditemukan';
    trainSchedule.Free;
    Exit;
  end;

  s := generateView(Result);
  Result := 'Jadwal Kereta dari ' + ucwords(asal) + ' ke ' + ucwords(tujuan) + #10;
  Result := Result + 'Tanggal: ' + FormatDateTime('dd-mm-yyy', d) + #10;
  Result := Result + s;

  trainSchedule.Free;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.bcaTestHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := 'bca';

  with TBCAIntegration.Create do
  begin
    SandBox := True;
    ClientID := '';
    ClientSecret := '';
    Result := GetToken;
    LogUtil.Add(Authorization, 'BCA');
    LogUtil.Add(ResultText, 'BCA');
    Free;
  end;
end;

function TCarikWebModule.yandexTranslateDetectHandler(const IntentName: string;
  Params: TStrings): string;
begin
  with TYandexTranslateIntegration.Create do
  begin
    Key := Config[YANDEX_KEY];
    Result := Translate(Flanguage, Text);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;
  if Result = '' then
    Result := SimpleBOT.GetResponse('NotFound');
end;

function TCarikWebModule.yandexSmartTranslateDetectHandler(const IntentName: string;
  Params: TStrings): string;
var
  fromLanguage, toLanguage, sourceText: string;
begin
  with TYandexTranslateIntegration.Create do
  begin
    Key := Config[YANDEX_KEY];
    fromLanguage := Params.Values['$1'];
    toLanguage := Params.Values['$2'];
    sourceText := Params.Values['$3'];
    if sourceText = '' then
    begin
      sourceText := Params.Values['kalimat_value'];
      fromLanguage := '';
      toLanguage := '';
    end;

    if (fromLanguage = '') and (toLanguage = '') then
    begin
      FLanguage := Detect(sourceText);
      if FLanguage <> 'id' then
        FLanguage := FLanguage + '-id';
      if FLanguage = 'id' then
      begin
        FLanguage := 'id-en';
      end;
    end;

    // force convert
    if fromLanguage = 'jv' then
      fromLanguage := 'id'; //>> try test 'anak laki-laki'


    if (fromLanguage <> '') and (toLanguage <> '') then
      FLanguage := fromLanguage + '-' + toLanguage;

    Result := Translate(Flanguage, sourceText);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;

  if Result <> '' then
  begin
    Result := StringReplace(SimpleBOT.GetResponse(IntentName + 'Response'),
      '$translated', Result, [rfReplaceAll]);
    Result := StringReplace(Result, '$original', sourceText, [rfReplaceAll]);
    MessageID := '';
  end;

end;

function TCarikWebModule.pajakKendaraanHandler(const IntentName: string;
  Params: TStrings): string;
begin
  with TPajakKendaraanIbacorIntegration.Create do
  begin
    Result := Find(Params.Values['nopol_value']);
    if Result <> '' then
      Result := StringReplace(SimpleBOT.GetResponse(IntentName + 'Info'),
        '%s', Result, [rfReplaceAll])
    else
      Result := SimpleBOT.GetResponse('NotFound');

    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;
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

function TCarikWebModule.mortgageCalculator(ALoanAmount: double;
  ALongInstallment: double; AInterest: double): double;
var
  a, b, c, total: double;
begin
  Result := 0;
  try
    a := AInterest / 100 / 12;
    c := exp((ALongInstallment * 12) * ln(1 + a));
    b := 1 / (1 - (1 / (c)));
    total := ALoanAmount * a * b;


    Result := total;
  except
  end;
end;

function TCarikWebModule.mortgageCalculatorHandler(const IntentName: string;
  Params: TStrings): string;
var
  _amount, _long, _interest, _cicilan: double;
begin
  _amount := s2f(StringHumanToNominal(Params.Values['LoanAmount_value']));
  _long := s2f(Params.Values['LongInstallment_value']);
  _interest := s2f(Params.Values['Interest_value']);

  if _long = 0 then
    _long := 10;
  if _interest = 0 then
    _interest := 10;

  _cicilan := mortgageCalculator(_amount, _long, _interest);

  DefaultFormatSettings.ThousandSeparator := '.';
  Result := SimpleBOT.GetResponse(IntentName + 'Response');
  Result := StringReplace(Result, '$LoanAmount$', FormatFloat('###,##0', _amount),
    [rfReplaceAll]);
  Result := StringReplace(Result, '$LongInstallment$', FormatFloat('###,##0', _long),
    [rfReplaceAll]);
  Result := StringReplace(Result, '$Interest$', f2s(_interest), [rfReplaceAll]);
  Result := StringReplace(Result, '$Cicilan$', FormatFloat('###,##0', _cicilan),
    [rfReplaceAll]);
end;

function TCarikWebModule.GenerateLineCarouselFromCulinaryData(
  ATitle, AJson: string): string;
var
  i: integer;
  jData: TJSONData;
  s: string;
begin
  Result := '';
  if AJson = '' then
    Exit;

  try
    jData := GetJSON(AJson);
    with TLineTemplateMessage.Create('carousel') do
    begin
      AltText := 'Tempat Kuliner ' + ATitle;
      Title := 'Informasi Tempat Kuliner ' + ATitle;
      Text := 'Informasi - Tempat Kuliner ' + ATitle;
      AltText := copy(ucwords(AltText), 0, 40);
      Title := copy(ucwords(Title), 0, 40);
      Text := copy(ucwords(Text), 0, 60);

      for i := 0 to jData.Count - 1 do
        //for i:=0 to 2 do
      begin
        with TLineTemplateMessage.Create('buttons') do
        begin
          AltText := jsonGetData(jData, '[' + i2s(i) + ']/name');
          Title := jsonGetData(jData, '[' + i2s(i) + ']/name');
          Text := '*Rating: ' + jsonGetData(jData, '[' + i2s(i) + ']/rating') +
            '*'#10 + jsonGetData(jData, '[' + i2s(i) + ']/address');
          AltText := copy(ucwords(AltText), 0, 40);
          Title := copy(ucwords(Title), 0, 40);
          Text := copy(ucwords(Text), 0, 60);
          ThumbnailImageURL := jsonGetData(jData, '[' + i2s(i) + ']/thumb');
          if ThumbnailImageURL = '' then
            ThumbnailImageURL := DEFAULT_RESTAURANT_IMAGE_URL;

          //AddActionPostBack( 'Detail', 'asdf?asfd=2&a=1');
          AddActionURI('Info Detil', jsonGetData(jData, '[' + i2s(i) + ']/url'));
          s := jsonGetData(jData, '[' + i2s(i) + ']/maps');
          if s <> '' then
            AddActionURI('Peta Lokasi', s)
          else
            AddActionURI('-', jsonGetData(jData, '[' + i2s(i) + ']/url'));
          //AddActionMessage( 'Selamat Pagi', 'pagi');

          s := GetTemplateAsJsonString;
          Free;
        end;
        AddColumnAsJson(s);

      end;

      Result := AsJSON;
      Free;
    end;

  except
    on E: Exception do
    begin
      LogUtil.Add(E.Message, 'LINE-kuliner');
    end;
  end;
end;

function TCarikWebModule.LineProperySearch(ATitle, AJson: string): string;
var
  i: integer;
  jData: TJSONData;
  s: string;
begin
  Result := '';
  if AJson = '' then
    Exit;

  try
    jData := GetJSON(AJson);
    with TLineTemplateMessage.Create('carousel') do
    begin
      AltText := ATitle;
      Title := ATitle;
      Text := ATitle;
      AltText := copy(ucwords(AltText), 0, 40);
      Title := copy(ucwords(Title), 0, 40);
      Text := copy(ucwords(Text), 0, 60);

      for i := 0 to 4 do
      begin
        with TLineTemplateMessage.Create('buttons') do
        begin
          AltText := jsonGetData(jData, '[' + i2s(i) + ']/title');
          Title := jsonGetData(jData, '[' + i2s(i) + ']/title');
          Text := jsonGetData(jData, '[' + i2s(i) + ']/description');
          Text := StripTags(Text);

          AltText := jsonGetData(jData, 'data.rows[' + i2s(i) + '].ads.tagline');
          Title := jsonGetData(jData, 'data.rows[' + i2s(i) + '].ads.pricing.price');
          Text := jsonGetData(jData, 'data.rows[' + i2s(i) + '].ads.tagline');
          Text := StripTags(Text);

          AltText := copy(ucwords(AltText), 0, 40);
          Title := copy(ucwords(Title), 0, 40);
          Text := copy(ucwords(Text), 0, 60);

          ThumbnailImageURL :=
            jsonGetData(jData, 'data.rows[' + i2s(i) + '].ads.image.default');

          s := jsonGetData(jData, 'data.rows[' + i2s(i) + '].ads.url');
          AddActionURI('Info Selengkapnya', s);
          //AddActionMessage( 'Selamat Pagi', 'pagi');

          s := GetTemplateAsJsonString;
          Free;
        end;
        AddColumnAsJson(s);

      end;

      Result := AsJSON;
      Free;
    end;

  except
    on E: Exception do
    begin
      LogUtil.Add(E.Message, 'LINE-property');
    end;
  end;
  jData.Free;
end;

function TCarikWebModule.LineBerita(ATitle, AJson: string): string;
var
  i, j: integer;
  jData: TJSONData;
  s: string;
begin
  Result := '';
  if AJson = '' then
    Exit;

  try
    jData := GetJSON(AJson);
    with TLineTemplateMessage.Create('carousel') do
    begin
      AltText := ATitle;
      Title := ATitle;
      Text := ATitle;
      AltText := copy(ucwords(AltText), 0, 40);
      Title := copy(ucwords(Title), 0, 40);
      Text := copy(ucwords(Text), 0, 60);

      jData := jData.GetPath('items');
      for i := 0 to 4 do
      begin
        with TLineTemplateMessage.Create('buttons') do
        begin
          AltText := jsonGetData(jData, '[' + i2s(i) + ']/title');
          Title := jsonGetData(jData, '[' + i2s(i) + ']/title');
          Text := jsonGetData(jData, '[' + i2s(i) + ']/description');
          Text := StripTags(Text);

          AltText := copy(ucwords(AltText), 0, 40);
          Title := copy(ucwords(Title), 0, 40);
          Text := copy(ucwords(Text), 0, 60);

          ThumbnailImageURL := jsonGetData(jData, '[' + i2s(i) + ']/image');
          if ThumbnailImageURL = '' then
          begin
            ThumbnailImageURL := DEFAULT_NEWS_IMAGE_URL;
            s := jsonGetData(jData, '[' + i2s(i) + ']/description');
            j := pos('img src="', s);
            if j > 0 then
            begin
              s := StringCut('img src="', '"', s);
              ThumbnailImageURL := s;
            end;
          end;
          ThumbnailImageURL :=
            StringReplace(ThumbnailImageURL, 'http://', 'https://', [rfReplaceAll]);
          //TODO: image http to https

          //AddActionPostBack( 'Detail', 'asdf?asfd=2&a=1');
          AddActionURI('Berita Selengkapnya', jsonGetData(jData, '[' +
            i2s(i) + ']/link'));
          //AddActionMessage( 'Selamat Pagi', 'pagi');

          s := GetTemplateAsJsonString;
          Free;
        end;
        AddColumnAsJson(s);

      end;

      Result := AsJSON;
      Free;
    end;

  except
    on E: Exception do
    begin
      LogUtil.Add(E.Message, 'LINE-news');
    end;
  end;
  jData.Free;
end;

function TCarikWebModule.FacebookBerita(ATitle, AJson: string): string;
var
  i: integer;
  s: string;
  jData: TJSONData;
  jElements: TJSONObject;
begin
  Result := '';
  if AJson = '' then
    Exit;

  jElements := TJSONObject.Create;
  try
    jData := GetJSON(AJson);
    jData := jData.GetPath('items');
    s := jData.AsJSON;

    with TFacebookTemplateElement.Create do
    begin
      Title:= 'judul';
      SubTitle:= 'desc';
      ImageURL:='img:///';
      ActionURL:= 'aaacccc';

      jElements.Add( AsJSON);
      jElements.Add( AsJSON);                    //xx
      //jElements.Arrays['elements'] := GetJSON( AsJSON);
      die( jElements.AsString);

      Free;
    end;

    for i := 0 to 4 do
    begin

    end;

  except
    on E: Exception do
    begin
      LogUtil.Add(E.Message, 'FB-news');
    end;
  end;

  jElements.Free;
end;

constructor TCarikWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.StorageType := stFile;
  SimpleBOT.StorageFileName := 'files/carik/carik-userdata.dat';
  Carik := TCarikController.Create;
  FLanguage := 'en-id';
  FSendAudio := False;
  FSendPhoto := False;
  FSendVenue := False;
  FSendRichContent := False;
  FFileURL := '';
  FCaption := '';
  FMessengerMode := mmNone;
end;

destructor TCarikWebModule.Destroy;
begin
  Carik.Free;
  SimpleBOT.Free;
  inherited Destroy;
end;

function TCarikWebModule.RemoveMarkDown(AText: string): string;
begin
  Result := StringReplace(AText, '```', '', [rfReplaceAll]);
  Result := StringReplace(Result, '_', '', [rfReplaceAll]);
  Result := Trim(Result);
end;

function TCarikWebModule.ObjectFocus: string;
var
  s: string;
begin
  Result := '';
  s := SimpleBOT.UserData['OBJECT_DATE'];
  if s = '' then
    Exit;
  if MinutesBetween(Now, StrToDateTime(s)) <= _OBJECT_DISCUSSION_MAXTIME then
    Result := SimpleBOT.UserData['OBJECT'];
end;

function TCarikWebModule.translateIndonesiaToSundaHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Params.Values['Text_value'] = '' then
    Exit;
  with TKamusSundaIntegration.Create do
  begin
    Result := TranslateIndonesianToSunda(Params.Values['Text_value']);
    if Result <> '' then
    begin
      Result := StringReplace(SimpleBOT.GetResponse(IntentName + 'Response'),
        '$translated', Result, [rfReplaceAll]);
      Result := StringReplace(Result, '$original', Params.Values['Text_value'],
        [rfReplaceAll]);
    end;
    Free;
  end;
end;

function TCarikWebModule.translateSundaToIndonesiaHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Params.Values['Text_value'] = '' then
    Exit;
  with TKamusSundaIntegration.Create do
  begin
    Result := TranslateSundaToIndonesian(Params.Values['Text_value']);
    if Result <> '' then
    begin
      Result := StringReplace(SimpleBOT.GetResponse(IntentName + 'Response'),
        '$translated', Result, [rfReplaceAll]);
      Result := StringReplace(Result, '$original', Params.Values['Text_value'],
        [rfReplaceAll]);
    end;
    Free;
  end;
end;

function TCarikWebModule.ProcessText(AMessage: string): string;
begin
  SimpleBOT.TrimMessage := True;

  SimpleBOT.OnError := @OnErrorHandler;  // Your Custom Message
  SimpleBOT.TrimMessage := False;
  Result := SimpleBOT.Exec(AMessage);
end;

procedure TCarikWebModule.BotInit;
begin
  SimpleBOT.Handler['define'] := @defineHandler;
  SimpleBOT.Handler['bot_start'] := @botStartHandler;
  SimpleBOT.Handler['resi_paket'] := @resiHandler;
  SimpleBOT.Handler['voucher_konvensional'] := @voucherConvensionalHandler;
  SimpleBOT.Handler['voucher'] := @voucherHandler;
  SimpleBOT.Handler['movie_play'] := @moviePlayHandler;
  SimpleBOT.Handler['movie_info'] := @movieInfoHandler;
  SimpleBOT.Handler['currency'] := @currencyHandler;
  SimpleBOT.Handler['tebak_gambar'] := @tebakGambarHandler;
  SimpleBOT.Handler['image_to_text'] := @ocrCognitiveHandler;
  SimpleBOT.Handler['image_translation'] := @imageTranslationHandler;
  SimpleBOT.Handler['image_to_figure'] := @imageSpecificCognitiveHandler;
  SimpleBOT.Handler['image_analyze'] := @imageAnalyzeCognitiveHandler;
  SimpleBOT.Handler['image_full_analyze'] := @imageFullAnalyzeHandler;
  SimpleBOT.Handler['lokasi'] := @lokasiHandler;
  SimpleBOT.Handler['lokasi_kuliner'] := @lokasiKulinerHandler;
  SimpleBOT.Handler['lokasi_kuliner_koordinat'] := @lokasiKulinerDenganKoordinatHandler;
  SimpleBOT.Handler['lokasi_koordinat'] := @lokasiDenganKoordinatHandler;
  SimpleBOT.Handler['bot_enable'] := @botEnableHandler;
  SimpleBOT.Handler['bot_disable'] := @botDisableHandler;
  SimpleBOT.Handler['smart_translate'] := @yandexSmartTranslateDetectHandler;
  SimpleBOT.Handler['translate_indonesia_sunda'] := @translateIndonesiaToSundaHandler;
  SimpleBOT.Handler['translate_sunda_indonesia'] := @translateSundaToIndonesiaHandler;

  SimpleBOT.Handler['pajak_kendaraan'] := @pajakKendaraanHandler;
  SimpleBOT.Handler['alquran_terjemahan'] := @alquranTerjemahanHandler;
  SimpleBOT.Handler['calendar_eventlist'] := @kloudlessCalendarEventListHandler;
  SimpleBOT.Handler['calendar_eventcreate'] := @kloudlessCalendarEventCreateHandler;

  SimpleBOT.Handler['mortgage_calculator'] := @mortgageCalculatorHandler;
  SimpleBOT.Handler['bmkg_simpleinfo'] := @bmkgSimpleInfoHandler;
  //SimpleBOT.Handler['openweather_info'] := @openweatherInfoHandler;
  SimpleBOT.Handler['openweather_info'] := @apixuweatherInfoHandler;
  SimpleBOT.Handler['berita_hariini'] := @beritaHariIniHandler;

  SimpleBOT.Handler['conversion_hash'] := @conversionHashHandler;

  // JobPlanet
  SimpleBOT.Handler['jobplanet_info'] := @jobPlanetInfoHandler;
  SimpleBOT.Handler['jobplanet_review'] := @jobPlanetReviewHandler;
  SimpleBOT.Handler['jobplanet_salary'] := @jobPlanetSalaryHandler;
  SimpleBOT.Handler['jobplanet_vacancy'] := @jobPlanetVacancyHandler;
  SimpleBOT.Handler['jobplanet_interview'] := @jobPlanetInterviewHandler;

  if FMessengerMode = mmTelegram then
  begin
    SimpleBOT.Handler['carik_start'] := @Carik.StartHandler;
    SimpleBOT.Handler['carik_stop'] := @Carik.StopHandler;
    SimpleBOT.Handler['carik_check'] := @Carik.CheckHandler;
    SimpleBOT.Handler['carik_topic'] := @Carik.TopicHandler;
    SimpleBOT.Handler['carik_send'] := @Carik.SendHandler;
    SimpleBOT.Handler['carik_admin_tambah'] := @carikAdminTambahHandler;
    SimpleBOT.Handler['carik_admin_hapus'] := @carikAdminHapusHandler;
    SimpleBOT.Handler['carik_group_info'] := @Carik.GroupInfoHandler;
    SimpleBOT.Handler['group_memberbaru'] := @carikMemberBaruHandler;
    SimpleBOT.Handler['group_memberbaru_abaikan'] := @carikMemberBaruAbaikanHandler;
    SimpleBOT.Handler['group_memberbaru_sapa'] := @carikMemberBaruSapaHandler;
    SimpleBOT.Handler['group_memberbaru_custommessage'] :=
      @carikNewMemberCustomMessageHandler;


    //todo: line: share location
  end;

  SimpleBOT.Handler['echo'] := @echoHandler;
  SimpleBOT.Handler['text_to_speech'] := @texttospeechHandler;
  SimpleBOT.Handler['speaking_mode_on'] := @speakingModeOnHandler;
  SimpleBOT.Handler['speaking_mode_off'] := @speakingModeOffHandler;

  // Tiket
  SimpleBOT.Handler['keretaapi_jadwal'] := @trainScheduleHandler;

  SimpleBOT.Handler['bca_test'] := @bcaTestHandler;
  SimpleBOT.Handler['quickcount'] := @quickCountHandler;
  SimpleBOT.Handler['kofa_jadwal_sholat'] := @kofaJadwalSholatHandler;
  SimpleBOT.Handler['kofa_jadwal_imsyak'] := @kofaJadwalImsyakHandler;

  // SmartHome - Test Only
  SimpleBOT.Handler['smarthome_general'] := @smartHomeGeneralHandler;
  SimpleBOT.Handler['smarthome_on'] := @smartHomeOnHandler;
  SimpleBOT.Handler['smarthome_off'] := @smartHomeOffHandler;

  // LINE
  SimpleBOT.Handler['rich_content'] := @richContentHandler;

end;

function TCarikWebModule.TrimFacebookMessage(const AMessage: string): string;
begin
  Result := RemoveMarkDown(AMessage);
end;

function TCarikWebModule.TrimLineMessage(const AMessage: string): string;
begin
  Result := RemoveMarkDown(AMessage);
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
