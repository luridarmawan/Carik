unit carik_webmodule;
{
  Remove non-alphanumeric characters?
  '/[^\p{L}\p{N} ]+/'
  preg_replace('/[^\p{L}\p{N} ]+/', '', $string);
}
{$mode objfpc}{$H+}

interface

uses
  fpjson, strutils, md5, sha1,
  common, json_lib, fastplaz_handler, carik_controller, simplebot_controller,
  logutil_lib, zomato_integration, yandextranslate_integration,
  movie_controller, cognitiveocr_integration,
  cognitivedomainspecific_integration, cognitiveanalyze_integration,
  clarifai_integration, alquranindonesia_integration,
  telegram_integration, googleplacesearch_integration,
  kamussunda_integration, bmkg_integration, openweathermap_integration,
  apixu_integration, bca_integration, witai_integration,
  line_integration,
  facebookmessenger_integration, portalpulsa_integration,
  googledistancematrix_integration, cognitivecustomvision_integration,
  kloudlesscalendar_integration, rss_lib, http_lib, IniFiles,
  rajaongkir_integration, googleanalytics_integration,
  kamuskemdikbud_integration, thesaurus_integration,
  fpexprpars, // formula
  {$if FPC_FULlVERSION >= 30200}
  fphttpclient, opensslsockets, fpopenssl, ssockets, sslsockets, sslbase,
  {$endif}
  process, dateutils, Classes, SysUtils, string_helpers, datetime_helpers;

{$include carik.inc}

type

  TOnMessageEvent = function (AMessage: String; var Handled: boolean): string of object;
  TOnSpamEvent = function (AMessage: String; AScore: integer; var Handled: boolean): string of object;
  TMessengerMode = (mmNone, mmTelegram, mmLine, mmFacebook, mmSkype, mmSlack, mmWhatsapp, mmInstagram, mmDiscord);

  { TCarikWebModule }

  TCarikWebModule = class(TMyCustomWebModule)
  private
    FActionCallback: string;
    FActiveContext: string;
    FBotID: string;
    FButtonCaption: string;
    FClientId: string;
    FCurrentInputType: string;
    FCustomActionAsText: string;
    FCustomActionFiles: TJSONArray;
    FCustomActionSuffix: string;
    FCustomReplyDataFromExternalNLP: TJSONUtil;
    FDelayReplay: boolean;
    FHideTextReply: boolean;
    FPackageName: string;
    FRequestAsJson: TJSONUtil;
    FCustomReplyName: string;
    FCustomReplyActionTypeFromExternalNLP: string;
    FCustomReplyTypeFromExternalNLP: string;
    FCustomReplyURLFromExternalNLP: string;
    FDashboardDeviceID: integer;
    FDeviceId: string;
    FExternalNLPIntentName: string;
    FExternalNLPIntentPattern: string;
    FExternalNLPWeight: integer;
    FExternalNLPStarted: boolean;
    FFormatNumber: string;
    FFormInputExpired: boolean;
    FGenericContent: boolean;
    FGPTTimeout: integer;
    FInputOptions: TJSONArray;
    FInputOptionTitle : string;
    FIsDebug: boolean;
    FMutedUntil: TDateTime;
    FOperation: string;
    FReplyDisable: boolean;
    FBOLD_CODE: string;
    FBotName: string;
    FCaption: string;
    FErrorCount: integer;
    FFileURL: string;
    FImageCaption: string;
    FImageID: string;
    FImagePath: string;
    FImageURL: string;
    FInvitedFullName: string;
    FInvitedUserName: string;
    FIsTranslate: boolean;
    FITALIC_CODE: string;
    FIterationParams: string;
    FIterationParamsPast : TStringList;
    FKickUser: boolean;
    FLanguage: string;
    FMessengerMode: TMessengerMode;
    FOnError: TOnMessageEvent;
    forceRespond: boolean;
    FRestrictUser: boolean;
    FRichContent: string;
    FSendAudio: boolean;
    FSendPhoto: boolean;
    FSendQuickReplayLocation: boolean;
    FSendRichContent: boolean;
    FCanSendTemplateCard: boolean;
    FSendVenue: boolean;
    FToken: string;
    FToken_I: Integer;
    FToken_O: Integer;
    FToken_T: Integer;
    FTriggeredText: string;
    FVenueAddress: string;
    FVenueLatitude: double;
    FVenueLongitude: double;
    FVenueName: string;
    FGroupData: TIniFile;

    // TELEGRAM
    function getActiveContext: string;
    function getAutoPrune: boolean;
    function getCustomActionSuffix: string;
    function getCustomReplyData: TJSONUtil;
    function getCustomReplyIsMainMenu: boolean;
    function getCustomReplyMenuLevel: string;
    function getCustomReplyMode: string;
    function getCustomReplyType: string;
    function getCustomReplyURL: string;
    function getIsCustomAction: boolean;
    function getIsMuted: boolean;
    function getIsObjectFocusExpired: boolean;
    function getisSpeakingMode: boolean;
    function getIsSuggest: boolean;
    function getIsTranslate: boolean;
    function getPrefixID: string;
    function getReplyType: string;
    function getRequestAsJson: TJSONUtil;
    function getUniqueID: string;
    function getWaitingInput: boolean;
    function isGroup: boolean;
    function isTelegramGroup: boolean;
    function getTelegramImageID: string;

    function generateCalendarID: string;
    function generateEventName(AEventName: string): string;

    // HANDLER
    function onewordHandler(const IntentName: string; Params: TStrings): string;
    function definisiHandler(const IntentName: string; Params: TStrings): string;
    function defineHandler(const IntentName: string; Params: TStrings): string;
    function iterationNextHandler(const IntentName: string; Params: TStrings): string;
    procedure setGroupData(const KeyName: string; AValue: string);
    function getGroupData(const KeyName: string): string;
    function userProfileHandler(const IntentName: string; Params: TStrings): string;
    function botStartHandler(const IntentName: string; Params: TStrings): string;
    function resiHandler(const IntentName: string; Params: TStrings): string;
    procedure setisSpeakingMode(AValue: boolean);
    function voucherConvensionalHandler(const IntentName: string;
      Params: TStrings): string;
    function voucherHandler(const IntentName: string; Params: TStrings): string;
    function movieInfoHandler(const IntentName: string; Params: TStrings): string;
    function moviePlayHandler(const IntentName: string; Params: TStrings): string;
    function distanceHandler(const IntentName: string; Params: TStrings): string;
    function distanceFromToHandler(const IntentName: string; Params: TStrings): string;
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
    function spamReportHandler(const IntentName: string; Params: TStrings): string;
    function carikAdminTambahHandler(const IntentName: string; Params: TStrings): string;
    function carikAdminHapusHandler(const IntentName: string; Params: TStrings): string;
    function carikNewMemberCustomMessageHandler(const IntentName: string;
      Params: TStrings): string;
    function carikMemberBaruHandler(const IntentName: string; Params: TStrings): string;
    function carikMemberBaruAbaikanHandler(const IntentName: string;
      Params: TStrings): string;
    function carikMemberBaruSapaHandler(const IntentName: string;
      Params: TStrings): string;

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
    function customMathHandler(const IntentName: string; Params: TStrings): string;

    function propertySearchHandler(const IntentName: string; Params: TStrings): string;
    function smartHomeGeneralHandler(const IntentName: string; Params: TStrings): string;
    function smartHomeOnHandler(const IntentName: string; Params: TStrings): string;
    function smartHomeOffHandler(const IntentName: string; Params: TStrings): string;

    function richContentHandler(const IntentName: string; Params: TStrings): string;

    function echoHandler(const IntentName: string; Params: TStrings): string;
    function texttospeechHandler(const IntentName: string; Params: TStrings): string;
    function speakingModeOnHandler(const IntentName: string; Params: TStrings): string;
    function speakingModeOffHandler(const IntentName: string; Params: TStrings): string;

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

    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoHeaders(Sender : TObject);
    procedure DoPassword(Sender: TObject; var RepeatRequest: Boolean);
    procedure ShowRedirect(ASender : TObject; Const ASrc : String; Var ADest : String);

    procedure HttpClientGetSocketHandler(Sender: TObject;
      const UseSSL: Boolean; out AHandler: TSocketHandler);
    function execPost(AURL: string; ACache: boolean = False): string;
    function execJson(AURL: string; ACache: boolean = False): string;
    procedure saveContext( const AParams: TStrings);
    procedure SaveUnknownChat(AText: string);
  public
    ProcessingTime: integer;
    ToggleSpammer: boolean;
    MessageID: string;
    TopicID: Integer;
    TopicName: string;
    OriginalText: string;
    ChannelId: string;
    Text: string;
    MessageType: string;
    isReplyMessage: boolean;
    replyFromMessageID, replyFromUserID, replyFromFullName, replyFromText: string;
    FileList: TJSONArray;
    SessionPrefix: string;
    Prefix: string;
    Suffix: string;
    LogChatPayload: TStringList;
    AutoDeleteMessage: integer;
    Carik: TCarikController;
    SimpleBOT: TSimpleBotModule;
    ElementArray: TJSONArray;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    function PrepareTextToSpeech(AText: string): string;
    function SpeechToText(AAudioFile: string; AConvert: boolean = True): string;

    function isTriggeredText(Message: string): boolean;
    function IsUserSuspended(AChannelID, AUserID: string): boolean;
    function IsBlackListed( AUserName: string; AUserId: string = ''): boolean;
    function IsSuspected( AUserId: string; AFullName: string): boolean;
    function IsGlobalUserBlackListed( AUserId: string): boolean;
    function IsGlobalGroupBlackListed( AGroupId: string): boolean;
    function isNewMember( AUserID: string; AGroupID: string; AInterval: integer = 10): boolean;
    function SpamScore( AUserID: string; AText: string; ForceCheck: boolean = False): integer;
    function IsSpammer( AUserID: string): boolean;
    function ReportSpam( AUserID: string; AUserName: string = ''; AReportBy: string = ''; AReportByName: string = ''): string;
    procedure LogChat(AChannelID: string; AGroupID: string; AGroupName: string;
      AUserID: string; AUserName: string; AFullName: string; AText: string; AReply: string;
      AIsGroup: boolean = True; AIsMentioned: boolean = True;
      AMessageID: integer = 0; AResultMessageID: integer = 0; AReplyFromMessageId: integer = 0);
    procedure LogJoin(AChannelID: string; AGroupID: string; AGroupName: string;
      AUserID: string; AUserName: string; AFullName: string; AInvitedBy: string; ARestrict: boolean = false;
      AUserLeft: boolean = false);
    procedure LogGroupAdd(AChannelID: string; AGroupID: string; AGroupName: string;
      AInvitedByID: string; AInvitedByUserName: string; AInvitedByName: string);
    procedure Analytics(AChannel, AIntent, AText, AUserID: string);
    function KnowledgeBase(AKeyword: string): string;
    function CarikSearch(AKeyword: string): string;
    function ExternalNLP(AText: string; AUseGPT: boolean = False): string;

    function IsCommand( AText:string): boolean;
    function isValidCommand(ACommandString: string): boolean;
    function ExecCommand(AText: string): string;

    function MaximumRetriesUnknownChat: integer;
    function GenerateResponseJson: string;

    // auto prune
    function SavePrune(AMessageID: string): boolean;
    function GetPrune: string;
    property GroupData[const KeyName: string]: string read getGroupData write setGroupData;
  published
    property IsDebug: boolean read FIsDebug write FIsDebug;
    property RequestAsJson: TJSONUtil read getRequestAsJson;
    property FormatNumber: string read FFormatNumber write FFormatNumber;
    property Operation: string read FOperation write FOperation;
    property BotID: string read FBotID;
    property BotName: string read FBotName write FBotName;
    property Token: string read FToken write FToken;
    property ClientId: string read FClientId write FClientId;
    property DeviceId: string read FDeviceId write FDeviceId;
    property IsDelayReplay: boolean read FDelayReplay write FDelayReplay;
    property ReplyDisable: boolean read FReplyDisable write FReplyDisable;
    property IterationParams: string read FIterationParams write FIterationParams;
    property PrefixId: string read getPrefixID;
    property UniqueID: string read getUniqueID;
    property MessengerMode: TMessengerMode read FMessengerMode write FMessengerMode;
    property Language: string read FLanguage write FLanguage;
    property SendAudio: boolean read FSendAudio write FSendAudio;
    property SendPhoto: boolean read FSendPhoto write FSendPhoto;
    property SendRichContent: boolean read FSendRichContent;
    property SendQuickReplayLocation: boolean read FSendQuickReplayLocation;
    property CanSendTemplateCard: boolean read FCanSendTemplateCard write FCanSendTemplateCard;
    property ButtonCaption: string read FButtonCaption write FButtonCaption;
    property RestrictUser: boolean read FRestrictUser write FRestrictUser;
    property KickUser: boolean read FKickUser write FKickUser;
    property FileURL: string read FFileURL;
    property Caption: string read FCaption;
    property RichContent: string read FRichContent;
    property GenericContent: boolean read FGenericContent write FGenericContent;
    property DashboardDeviceID: integer read FDashboardDeviceID write FDashboardDeviceID;
    property TriggeredText: string read FTriggeredText;
    property BOLD_CODE: string read FBOLD_CODE write FBOLD_CODE;
    property ITALIC_CODE: string read FITALIC_CODE write FITALIC_CODE;
    property ExternalNLPIntentName: string read FExternalNLPIntentName write FExternalNLPIntentName;
    property ExternalNLPIntentPattern: string read FExternalNLPIntentPattern;
    property ExternalNLPWeight: integer read FExternalNLPWeight;
    property ExternalNLPStarted: boolean read FExternalNLPStarted;
    property GPTTimeout: integer read FGPTTimeout write FGPTTimeout;
    property PackageName: string read FPackageName write FPackageName;

    property ActionCallback: string read FActionCallback;
    property IsMuted: boolean read getIsMuted;
    property MutedUntil: TDateTime read FMutedUntil write FMutedUntil;

    // OBJECT
    property ActiveContext: string read getActiveContext write FActiveContext;
    property isObjectFocusExpired: boolean read getIsObjectFocusExpired;
    function ObjectFocus: string; //deprecated
    function ContextFocus: string;


    function LanguageSetHandler(const IntentName: string; Params: TStrings): string;

    // Kamus
    function translateIndonesiaToSundaHandler(const IntentName: string;
      Params: TStrings): string;
    function translateSundaToIndonesiaHandler(const IntentName: string;
      Params: TStrings): string;
    function yandexTranslateDetectHandler(const IntentName: string;
      Params: TStrings): string;
    function yandexSmartTranslateDetectHandler(const IntentName: string;
      Params: TStrings): string;
    function translate(AText: string; AFrom: string = 'id'; ATo: string = 'en';
      ACache: boolean = False): string;

    function pajakKendaraanHandler(const IntentName: string; Params: TStrings): string;
    function ProcessText(AMessage: string): string;
    procedure BotInit;
    function OnNLPErrorHandler(const Message: string): string;

    function CleanupMessage(const AMessage: string): string;
    function TrimFacebookMessage(const AMessage: string): string;
    function TrimLineMessage(const AMessage: string): string;

    property ImageID: string read FImageID write FImageID;
    property ImageURL: string read FImageURL write FImageURL;
    property ImagePath: string read FImagePath write FImagePath;
    property ImageCaption: string read FImageCaption write FImageCaption;
    property AutoPrune: boolean read getAutoPrune;

    property InvitedUserName: string read FInvitedUserName write FInvitedUserName;
    property InvitedFullName: string read FInvitedFullName write FInvitedFullName;

    property SendVenue: boolean read FSendVenue;
    property VenueName: string read FVenueName;
    property VenueAddress: string read FVenueAddress;
    property VenueLatitude: double read FVenueLatitude;
    property VenueLongitude: double read FVenueLongitude;

    property SpeakingMode: boolean read getisSpeakingMode write setisSpeakingMode;
    property IsTranslate: boolean read getIsTranslate;
    property ErrorCount: integer read FErrorCount;
    property OnError : TOnMessageEvent Read FOnError Write FOnError;

    // Suggestion
    property IsSuggest: boolean read getIsSuggest;
    function FindSuggestion(AText: string; AReservedWord: string = ''): string;

    // CustomAction
    property IsCustomAction: boolean read getIsCustomAction;
    property ReplayType: string read getReplyType;
    property CustomActionAsText: string read FCustomActionAsText;
    property CustomActionSuffix: string read getCustomActionSuffix write FCustomActionSuffix;
    property CustomActionFiles: TJSONArray read FCustomActionFiles write FCustomActionFiles;
    property CustomReplyName: string read FCustomReplyName;
    property CustomReplyType: string read getCustomReplyType;
    property CustomReplyMenuLevel: string read getCustomReplyMenuLevel;
    property CustomReplyIsMainMenu: boolean read getCustomReplyIsMainMenu;
    property CustomReplyMode: string read getCustomReplyMode;
    property CustomReplyData: TJSONUtil read getCustomReplyData;
    property CustomReplyURL: string read getCustomReplyURL;
    property CustomReplyTypeFromExternalNLP: string read FCustomReplyTypeFromExternalNLP;
    property CustomReplyURLFromExternalNLP: string read FCustomReplyURLFromExternalNLP;
    property CustomReplyActionTypeFromExternalNLP: string read FCustomReplyActionTypeFromExternalNLP;
    property CustomReplyDataFromExternalNLP: TJSONUtil read FCustomReplyDataFromExternalNLP;
    procedure SaveActionToUserDataFromCard(AData: TJSONObject);
    procedure SaveActionToUserDataFromForm(AData: TJSONObject);
    procedure SaveActionToUserData(AActionType: string; AData: TJSONObject = nil);
    function GenerateTextFromCustomActionOption(AText: string): string;
    function RemoveDummyImageLink(AText: string): string;

    // Form Input Handler
    property WaitingInput: boolean read getWaitingInput;
    property CurrentInputType: string read FCurrentInputType;
    property FormInputExpired: boolean read FFormInputExpired;
    property InputOptions: TJSONArray read FInputOptions;
    property HideTextReply: boolean read FHideTextReply write FHideTextReply;
    function FormInputHandler(): boolean;
    function GetFormQuestion(AIndex: integer): string;
    function GetFormAnswerValue(AQuestionIndex: integer; AOptionIndex: integer = 0): string;
    function GetFormAnswerText(AQuestionIndex: integer; AOptionIndex: integer = 0): string;

    // SimpleNLP
    property Token_I: Integer read FToken_I write FToken_I;
    property Token_O: Integer read FToken_O write FToken_O;
    property Token_T: Integer read FToken_T write FToken_T;
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

  _CARIK_ONLY = 'Fitur lengkap bisa dilakukan melalui Carìk Bot ';

  _OBJECT_DISCUSSION_MAXTIME = 10; // in minutes
  //_ICON_WEKER = '⏰⏱';
  //_ICON_NUMBER = '0️⃣1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣8️⃣9️⃣🔟';
  _ICON_NUMBER_ARRAY: array [0..10] of string =
    ('0️⃣', '1️⃣', '2️⃣', '3️⃣', '4️⃣', '5️⃣',
    '6️⃣', '7️⃣', '8️⃣', '9️⃣', '🔟');

  _CARIK_SPEAKING_MODE = '_SPEAKING_MODE';

  DEFAULT_RESTAURANT_IMAGE_URL = 'https://fire.carik.id/images/restorant/default2.png';
  DEFAULT_NEWS_IMAGE_URL = 'https://fire.carik.id/images/news/news.png';
  TEXT2SPEECH_MAX_CHAR = 250;

  AI_CONFIG_TRIGGERWORD = 'ai/default/trigger_word';

  REGEX_EQUATION =
    '^[cos|sin|tan|tangen|sqr|sqrt|log|ln|sec|cosec|arctan|abs|exp|frac|int|round|trunc|shl|shr|ifs|iff|ifd|ifi|0-9*+ ().,-/:]+$';

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
    json.LoadFromJsonString(Request.Content, False);
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

function TCarikWebModule.getIsCustomAction: boolean;
begin
  if FCustomReplyTypeFromExternalNLP = 'action' then
    Result := True
  else
    Result := SimpleBOT.IsCustomAction;
end;

function TCarikWebModule.getIsMuted: boolean;
var
  s: string;
begin
  Result := False;
  if ClientId.IsEmpty then
    s := SimpleBOT.UserData['mute']
  else
    s := SimpleBOT.UserData[ClientId + '-mute'];
  if s.IsEmpty then
    Exit;
  FMutedUntil := FMutedUntil.FromString(s);
  if FMutedUntil > Now then Result := True;
  if not Result then
  begin
    if ClientId.IsEmpty then
      SimpleBOT.UserData['mute'] := ''
    else
      SimpleBOT.UserData[ClientId + '-mute'] := '';
  end;
end;

function TCarikWebModule.getCustomReplyData: TJSONUtil;
begin
  if Assigned(FCustomReplyDataFromExternalNLP) then
    Result := FCustomReplyDataFromExternalNLP
  else
    Result := SimpleBOT.SimpleAI.CustomReplyData;
end;

function TCarikWebModule.getCustomReplyIsMainMenu: boolean;
begin
  Result := SimpleBOT.SimpleAI.CustomReplyIsMainMenu;
end;

function TCarikWebModule.getCustomReplyMenuLevel: string;
begin
  Result := SimpleBOT.SimpleAI.CustomReplyMenuLevel;
end;

function TCarikWebModule.getAutoPrune: boolean;
begin
  Result := SimpleBOT.SimpleAI.AutoPrune;
end;

function TCarikWebModule.getCustomActionSuffix: string;
begin
  Result := SimpleBOT.SimpleAI.CustomReplySuffix;
  if Result.Trim.IsEmpty then
    Result := FCustomActionSuffix;
end;

function TCarikWebModule.getActiveContext: string;
begin
  if FActiveContext.IsEmpty then
    FActiveContext := SimpleBOT.SimpleAI.IntentName;
  Result := FActiveContext;
end;

function TCarikWebModule.getCustomReplyMode: string;
begin
  Result := SimpleBOT.CustomReplyMode;
end;

function TCarikWebModule.getCustomReplyType: string;
begin
  if FCustomReplyActionTypeFromExternalNLP.IsNotEmpty then
    Result := FCustomReplyActionTypeFromExternalNLP
  else
    Result := SimpleBOT.CustomReplyType;
end;

function TCarikWebModule.getCustomReplyURL: string;
begin
  Result := FCustomReplyURLFromExternalNLP;
  if FCustomReplyURLFromExternalNLP.IsEmpty then
    Result := SimpleBOT.CustomReplyURL;
end;

function TCarikWebModule.getisSpeakingMode: boolean;
var
  s: string;
begin
  s := Trim(_GET['botid']);
  if s <> '' then
  begin
    //ulil aktif suara, atau diam
  end;


  Result := True;
  s := UniqueID + _CARIK_SPEAKING_MODE;

  if SimpleBOT.UserData[s] <> '1' then
    Result := False;
end;

function TCarikWebModule.getIsSuggest: boolean;
var
  b: boolean;
  url: string;
begin
  try
    Result := False;
    Result := Config[SUGGESTION_ENABLE];
  except
  end;
end;

function TCarikWebModule.getIsTranslate: boolean;
var
  s: string;
begin
  Result := False;

  if SimpleBOT.LastSeen > 3600 then
  begin
    SimpleBOT.UserData['language'] := '';
    Exit;
  end;

  if ((SimpleBOT.UserData['language'] <> '') and
    (SimpleBOT.UserData['language'] <> 'id')) then
  begin
    Result := True;
    FIsTranslate := True;
    FLanguage := SimpleBOT.UserData['language'];
  end;

end;

function TCarikWebModule.getPrefixID: string;
begin
  Result := ChannelId;
  if Result.IsEmpty then
    Result := '0';
  if FMessengerMode = mmTelegram then
    Result := 'tl';
  if FMessengerMode = mmFacebook then
    Result := 'fb';
  if FMessengerMode = mmLine then
    Result := 'ln';
  if FMessengerMode = mmSkype then
    Result := 'sk';
  if FMessengerMode = mmSlack then
    Result := 'sl';
  if FMessengerMode = mmWhatsapp then
    Result := 'wa';
  if FMessengerMode = mmInstagram then
    Result := 'in';
  if FMessengerMode = mmDiscord then
    Result := 'di';

  // by pass
  if channelID = 'direct' then
    Result := DEFAULT_CHANNEL_ID;
  if channelID = 'android' then
    Result := ANDROID_CHANNEL_ID;
  if channelID = 'i' then
    Result := IOS_CHANNEL_ID;
  if channelID = 'ios' then
    Result := IOS_CHANNEL_ID;
  if channelID = 'whatsapp' then
    Result := WHATSAPP_CHANNEL_ID;
  if channelID = 'discord' then
    Result := DISCORD_CHANNEL_ID;
  if channelID = 'instagram' then
    Result := INSTAGRAM_CHANNEL_ID;
  if channelID = 'telegram_userbot' then
    Result := TELEGRAM_USERBOT_CHANNEL_ID;

  if channelID = 'sms' then
    Result := SMS_CHANNEL_ID;
  if channelID = 'twitter' then
    Result := TWITTER_CHANNEL_ID;
  if channelID = 'dualspace' then
    Result := DUALSPACE_CHANNEL_ID;
  if channelID = 'signal' then
    Result := SIGNAL_CHANNEL_ID;
  if channelID = 'bip' then
    Result := BIP_CHANNEL_ID;
  if channelID = 'zoom' then
    Result := ZOOM_CHANNEL_ID;

end;

function TCarikWebModule.getReplyType: string;
begin
  Result := SimpleBOT.ReplayType;
end;

function TCarikWebModule.getRequestAsJson: TJSONUtil;
begin
  if Assigned(FRequestAsJson) then
    Result := FRequestAsJson
  else
  begin
    FRequestAsJson := TJSONUtil.Create;
    FRequestAsJson.LoadFromJsonString(Request.Content, False);
    Result := FRequestAsJson;
  end;
end;

function TCarikWebModule.getUniqueID: string;
begin
  Result := '';
  if FMessengerMode = mmTelegram then
    Result := 'tl';
  if FMessengerMode = mmFacebook then
    Result := 'fb';
  if FMessengerMode = mmLine then
    Result := 'ln';
  if FMessengerMode = mmSkype then
    Result := 'sk';
  if FMessengerMode = mmSlack then
    Result := 'sl';
  if FMessengerMode = mmWhatsapp then
    Result := 'wa';
  if FMessengerMode = mmInstagram then
    Result := 'in';

  if isGroup then
    Result := Result + Carik.GroupChatID
  else
    Result := Result + Carik.UserID;

end;

function TCarikWebModule.getWaitingInput: boolean;
var
  s: string;
  dt: TDateTime;
begin
  try
    Result := s2b(SimpleBOT.UserData[WAITING_INPUT]);
  except
  end;
  if not Result then Exit;

  s := SimpleBOT.UserData[WAITING_INPUT_DATE];
  if not s.IsEmpty then
  begin
    dt := s.AsDateTime;
    if dt.MinutesDiff(Now) >= FORM_INPUT_TIMEOUT then
    begin
      FFormInputExpired := True;
      if dt.MinutesDiff(Now) < FORM_INPUT_TIMEOUT_MAX then
        Prefix := FORM_INPUT_EXPIRED;
      SimpleBOT.UserData[WAITING_INPUT] := '0';
      Result := False;
    end;
  end;
end;

function TCarikWebModule.isGroup: boolean;
begin
  Result := False;
  if FMessengerMode = mmTelegram then
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

function TCarikWebModule.onewordHandler(const IntentName: string;
  Params: TStrings): string;
begin
  if not SimpleBOT.isFormula then
    Exit;
  Result := SimpleBOT.Formula(Text);
end;

function TCarikWebModule.definisiHandler(const IntentName: string;
  Params: TStrings): string;
var
  keyName: string;
begin
  Result := '';

  keyName := Params.Values['Text'];
  if keyName = '' then
    Exit;

  Result := LoadCache('definisi-' + keyName);
  if Result <> '' then
    Exit;

  // use knowledge base first
  Result := KnowledgeBase(keyName);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  //TODO: safe to local thesaurus
  if Result <> '' then
    Exit;

  // use local dictionary
  with TThesaurusIntegration.Create do
  begin
    Result := Find(keyName);
    Free;
  end;
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, '--', '..', [rfReplaceAll]);
  if Result <> '' then
    Exit;

  with TKamusIntegration.Create do
  begin
    Result := Find(keyName);
    Free;
  end;

  if Result = '' then
  begin
    if ChannelId.IsEmpty or (ChannelId = 'direct') then
    begin
      Result := SimpleBOT.SimpleAI.GetResponse('NotFound');
      Result := Result + '\n' + _CARIK_ONLY;
      Exit;
    end;

    Result := CarikSearch(keyName);
    Result := Result.Replace(#13#10,'\n');
    Result := Result.Replace(#10#13,'\n');
    Result := Result.Replace(#13,'\n');
    Result := Result.Replace(#10,'\n');
    Result := trim(Result);
  end;

  if Result <> '' then
    SaveCache('definisi-' + keyName, Result);

  if Result = '' then
    Result := SimpleBOT.SimpleAI.GetResponse('NotFound');
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

function TCarikWebModule.iterationNextHandler(const IntentName: string;
  Params: TStrings): string;
var
  i: Integer;
  contextAbout, contextAction, s: String;
begin
  FIterationParams := Params.Values['keyword'];

  FIterationParamsPast := TStringList.Create;
  FIterationParamsPast.Text := SimpleBOT.UserData['CONTEXT_PARAM'];
  FIterationParamsPast.Text := FIterationParamsPast.Text.Replace('|', #10);

  contextAbout := SimpleBOT.UserData['CONTEXT'];
  contextAbout := contextAbout.Replace('_', ' ');
  contextAction := SimpleBOT.UserData['CONTEXT_ACTION'];

  for i:=FIterationParamsPast.Count-1 downto 0 do
  begin
    s := FIterationParamsPast.Names[i].Trim;
    if s.Contains('$') then Continue;
    if s.Contains('_value') then Continue;
    if s.Contains('pattern') then Continue;
    if s.IsEmpty then Continue;
  end;
  Result := SimpleBOT.IterationHandler( contextAction, FIterationParams);
end;

procedure TCarikWebModule.setGroupData(const KeyName: string; AValue: string);
var
  key: string;
begin
  if Carik.GroupChatID.IsEmpty then Exit;
  key := 'prune-' + KeyName;
  try
    FGroupData := TIniFile.Create(GROUP_DATA_FILENAME);
    FGroupData.WriteString( Carik.GroupChatID, key, AValue);
  except
    on E:Exception do
    begin
      LogUtil.Add(E.Message, 'GROUPDATA');
    end;
  end;
  FGroupData.Free;
end;

function TCarikWebModule.getGroupData(const KeyName: string): string;
var
  key: string;
begin
  Result := '';
  if Carik.GroupChatID.IsEmpty then Exit;
  key := 'prune-' + KeyName;
  try
    FGroupData := TIniFile.Create(GROUP_DATA_FILENAME);
    Result := FGroupData.ReadString( Carik.GroupChatID, key, '');
  except
    on E:Exception do
    begin
      LogUtil.Add(E.Message, 'GROUPDATA');
    end;
  end;
  FGroupData.Free;
end;

function TCarikWebModule.userProfileHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := Carik.FullName; //TODO: Get User Profile

  Result := Result + #13 + SimpleBOT.GetResponse(IntentName + 'Response');
  Result := Trim(Result);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
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
  Result := '';
  with TRajaOngkirIntegration.Create do
  begin
    AccountType := atBasic;
    if not (MessengerMode = mmNone) then
      BOLD_CODE:= '*';
    Key := Config[RAJAONGKIR_TOKEN];
    if Params.Values['vendor_value'] = '' then
      Result := Track('JNE', Params.Values['nomor_value'])
    else
      Result := Track(Params.Values['vendor_value'], Params.Values['nomor_value']);
    Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
    Free;
  end;
  if Result = '' then
    Result := Format(_CARIK_RESI_MSG_RESI_TIDAKDITEMUKAN,
      [Params.Values['nomor_value']]);

  {*deprecated
  with TResiIbacorController.Create do
  begin
    Token := Config[IBACOR_TOKEN];
    Vendor := Params.Values['vendor_value'];
    AirwayBill := Params.Values['nomor_value'];
    Result := Find();
    Free;
  end;
  }
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
    UserID := 'P21765';
    Key := 'dc40e6d1c4dcd48cd254cc5cc1248ea5';
    Secret := 'd10fdea4e4085145e63bf5521fe1a2314a30bd67fd047f76891799cf4401ab83';
    TransactionID := FormatDateTime('yyyymmddHHnnss', Now) + _nomor;

    if IsiPulsa(_nomor, '5') then
    begin

    end;
    LogUtil.Add(Message, 'PULSA');
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
    UserID := Config[PORTALPULSA_USERID];
    Key := Config[PORTALPULSA_KEY];
    Secret := Config[PORTALPULSA_SECRET];
    TransactionID := FormatDateTime('yyyymmddHHnnss', Now) + _nomor;

    if IsiPulsa(_nomor, '5') then
    begin

    end;
    LogUtil.Add(Message, 'PULSA');
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
    Key := Config['omdb/default/key'];
    Result := Find(Params.Values['judul_value']);
  end;
end;

function TCarikWebModule.moviePlayHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := 'https://www.youtube.com/results?search\_query=' +
    UrlEncode(Params.Values['title_value']);
end;

function TCarikWebModule.distanceHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := SimpleBOT.GetResponse(IntentName + 'NoData');

  with TGoogleDistanceIntegration.Create do
  begin
    Key := Config[GOOGLE_KEY];
    if not GetDistance(Params.Values['Origin_value'],
      Params.Values['Destination_value']) then
    begin
      Free;
      Exit;
    end;

    Result := SimpleBOT.GetResponse(IntentName + 'Found');
    Result := StringReplace(Result, '%Origin%', Origins, [rfReplaceAll]);
    Result := StringReplace(Result, '%Destination%', Destinations, [rfReplaceAll]);
    Result := StringReplace(Result, '%Distance%', DistanceAsText, [rfReplaceAll]);
    Result := StringReplace(Result, '%Duration%', DurationAsText, [rfReplaceAll]);
    Free;
  end;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
end;

function TCarikWebModule.distanceFromToHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Params.Values['Origin_value'] := Params.Values['$1'] + ',' + Params.Values['$2'];
  Params.Values['Destination_value'] := Params.Values['$3'];
  Result := distanceHandler(IntentName, Params);
end;

function TCarikWebModule.ocrCognitiveHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, _url: string;
begin
  Result := '';
  if Config[COGNITIVE_OCR_TOKEN] = '' then
    Exit;
  {
  if not isTelegramGroup then
  begin
    Result := _TELEGRAM_ERR_GROUP_ONLY;
    Exit;
  end;
  }
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
    Language := 'en';
    Result := Scan(_url);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Free;
  end;

  if Result <> '' then
  begin
    //Result := RemoveUnicode(Result);
    //Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    //Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
    Result := AnsiToUtf8(Result);
    Result := Result.Replace('*', '');

    s := SimpleBOT.GetResponse(IntentName + _RESPONSE);
    Result := Format(s, [Result]);
    LogUtil.Add(Result, 'OCR');
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
  {
  if not isTelegramGroup then
  begin
    Result := _TELEGRAM_ERR_GROUP_ONLY;
    Exit;
  end;
  }
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
    Details := 'celebrities';
    Result := Scan(_url);
    LogUtil.Add(Token, 'Token');
    LogUtil.Add(ResultText, 'ImageToFigure');
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

  {
  with TCognitiveCustomVision.Create do
  begin
    Key := Config[CUSTOMVISION_KEY];
    URL := Config[CUSTOMVISION_URL];
    s := Trim( Prediction(FImageURL));
    Free;
  end;
  if s <> '' then
    Result := '*Prediksi*:' + #10 + s;
  }

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
  if s <> '' then
    Result := Result + #10#10'*Tags:*'#10 + _tags + #10 + s;

  // tokoh dikenal
  with TCognitiveDomainSpecific.Create do
  begin
    Token := Config[COGNITIVE_OCR_TOKEN];
    Details := 'celebrities';
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
const
  LOKASI_RANGE = '(sini|disini|di sini|terdekat|dekat|near me|near by)';
  LOKASI_RANGE_2 = '(di|deket|dekat)';
var
  i: integer;
  s, _keyword, jamBuka, sType: string;
begin

  // Iteration - check
  if not FIterationParams.IsEmpty then
  begin
    s := SimpleBOT.SimpleAI.SimpleAILib.Intent.Entities.GetKey('Lokasi', FIterationParams);
    if s.IsEmpty then
    begin
      Params.Values['keyword_value'] := FIterationParams;
      Params.Values['Lokasi'] := FIterationParamsPast.Values['Lokasi_value'];
      Params.Values['Lokasi_value'] := FIterationParamsPast.Values['Lokasi_value'];
    end else
    begin
      Params.Values['keyword_value'] := FIterationParamsPast.Values['keyword_value'];
      Params.Values['Lokasi'] := s;
      Params.Values['Lokasi_value'] := s;
    end;
    Params.Values['action'] := FIterationParamsPast.Values['action'];
    Params.Values['intent_name'] := FIterationParamsPast.Values['intent_name'];
  end;
  // Iteration - check - end

  if Params.Values['Lokasi_value'] = '' then
  begin
    Params.Values['Lokasi'] := Params.Values['Kuliner'];
    Params.Values['Lokasi_value'] := Params.Values['Kuliner_value'];
  end;

  _keyword := Params.Values['keyword_value'];
  Result := Params.Values['Lokasi_value'] + ' ' + Params.Values['keyword_value'];

  if preg_match(LOKASI_RANGE, _keyword) or (Result = 'rumah sakit') or
    (_keyword = '') then
  begin
    _keyword := preg_replace(LOKASI_RANGE, '', _keyword);
    SimpleBOT.UserData['CONTEXT_DETAIL'] :=
      Params.Values['Lokasi_value'] + ' ' + _keyword;
    if SimpleBOT.UserData['CONTEXT_DETAIL'] = 'rs' then
      SimpleBOT.UserData['CONTEXT_DETAIL'] := 'rumah sakit';
    Result := SimpleBOT.GetResponse(IntentName + 'NoLocation');
    Result := StringReplace(Result, '%lokasi%', Params.Values['Lokasi_value'],
      [rfReplaceAll]);
    FSendQuickReplayLocation := True;
    FActionCallback := 'location.send|keyword=' + Params.Values['Lokasi_value'];
    Exit;
  end;

  _keyword := trim(Params.Values['Lokasi'] + ' ' + Params.Values['keyword_value']);
  saveContext(Params);

  // START SEARCH
  // Get Data Array
  s := '';
  with TGooglePlaceIntegration.Create do
  begin
    Key := Config[GOOGLE_KEY];
    ElementArray := SearchAsArray(_keyword, s2f(Params.Values['lat_value']),
      s2f(Params.Values['lon_value']));

    if ElementArray.Count = 0 then
      Result := SimpleBOT.GetResponse(IntentName + 'NotFound') //TODO: #Aup
    else
      FCanSendTemplateCard := True;

    FButtonCaption := 'Lihat Map';
    if MessengerMode = mmFacebook then
      MarkDown := False;
    if FMessengerMode = mmNone then
    begin
      BOLD_CODE := self.BOLD_CODE;
      MarkDown := true;
    end;
    s := DisplayAsText(ElementArray);  //TODO: wrong logic #Aup
    Free;
  end;

  if ElementArray.Count = 0 then
    s := SimpleBOT.GetResponse(IntentName + 'NotFound');

  if ((MessengerMode = mmFacebook)or(MessengerMode = mmLine)) then
  begin
    //Result := s;
    //Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
    //Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Result := '';
    if ElementArray.Count = 0 then
      Result := s;
    Exit;
  end;

  if MessengerMode = mmNone then
  begin

    sType := Params.Values['Lokasi_value'];
    if sType.IsEmpty then
      sType := Params.Values['keyword_value'];

    FActionCallback := 'display.list|from=data|type='+sType
      + '|keyword=' + Params.Values['keyword_value'];

  end;

  Result := s;
  Result := Trim(Result);
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
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
    SimpleBOT.UserData['CONTEXT_DETAIL'] := Params.Values['Lokasi_value'];
    Result := SimpleBOT.GetResponse(IntentName + 'NoLocation');
    Result := StringReplace(Result, '%lokasi%', Params.Values['Lokasi_value'],
      [rfReplaceAll]);
    FSendQuickReplayLocation := True;
    FActionCallback := 'location.send|keyword=' + Params.Values['Lokasi_value'];
    Exit;
  end;

  //_keyword := _keyword + ', Indonesia';
  with TZomatoIntegration.Create do
  begin
    URL := Config[ZOMATO_URL];
    Key := Config[ZOMATO_KEY];
    if FMessengerMode = mmFacebook then
      MarkDown := false;
    EntityType := '';
    EntityID := 94; //Indonesia
    s := SearchAsJson(_keyword, s2f(Params.Values['lat_value']),
      s2f(Params.Values['lon_value']), 5);
    //LogUtil.Add(ResultText, 'LINE-KULINER');
    //die( ResultText);
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
function TCarikWebModule.lokasiKulinerDenganKoordinatHandler(const IntentName: string;
  Params: TStrings): string;
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

function TCarikWebModule.spamReportHandler(const IntentName: string;
  Params: TStrings): string;
var
  s : string;
begin
  Result := '';
  if not Carik.IsPermitted then
    Exit;

  s := Params.Values['username'];
  s := s.Replace('@', '');
  Result := ReportSpam(s, '', Carik.UserID);
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

function TCarikWebModule.carikNewMemberCustomMessageHandler(const IntentName: string;
  Params: TStrings): string;
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
  if Carik.isCollectiveWelcomeGreeting then
  begin
    Exit;
  end;

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

function TCarikWebModule.alquranTerjemahanHandler(const IntentName: string;
  Params: TStrings): string;
var
  lst: TStrings;
  suratName, suratNo: string;
  ayat: string;
begin
  Result := '';

  suratName := Params.Values['NamaSurat_value'];
  if not suratName.IsEmpty then
  begin
    //clean up surat name
    suratName := suratName.Replace('al-', '');
    suratName := suratName.Replace('ar-', '');
    suratName := suratName.Replace('as-', '');
    suratName := suratName.Replace('al ', '');
    suratName := suratName.Replace('ar ', '');
    suratName := suratName.Replace('as ', '');
    suratName := suratName.Replace('''', '');
    suratName := suratName.Replace('-', '');
    suratName := suratName.Replace(' ', '');

    //get nomor surat
    lst := TStringList.Create;
    lst.LoadFromFile('files/quran-surat.txt');
    suratNo := lst.Values[suratName];
    lst.Free;

    if suratNo.IsEmpty then
    begin
      Result := SimpleBOT.GetResponse(IntentName + 'NoData');
      Exit;
    end;

    lst := Explode(suratNo, ';');
    suratNo := lst[0];
    lst.Free;

    ayat := Params.Values['Angka_value'];
    Params.Values['surat_value'] := suratNo + ':' + ayat;
  end;

  Params.Values['surat_value'] := Params.Values['surat_value'].Replace('.', ':');
  lst := Explode(Params.Values['surat_value'], ':');
  if lst.Count <> 2 then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'Penulisan');
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
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'Response') + Result;
    FActionCallback := 'audio.play|title=' + FCaption + '|url=' + FFileURL;
  end;

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
  endTime := IncDay(Today, 7);

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

  if MessengerMode = mmNone then
  begin
    Result := '';
    FActionCallback := 'calendar.list|start=' + FormatDateTime('yyyy/mm/dd HH:nn', startTime)
    + '|end=' + FormatDateTime('yyyy/mm/dd HH:nn', endTime);
    exit;
  end;

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

//--> senin jam 11 meeting sama anu
//--> info mudik

function TCarikWebModule.kloudlessCalendarEventCreateHandler(const IntentName: string;
  Params: TStrings): string;
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

  if FMessengerMode = mmNone then
  begin
    eventName := Params.Values['EventName_value'];
    if eventName = '' then
      eventName := 'Meeting';
    FActionCallback := 'calendar.add|name='+ucwords(eventName)
      +'|start=' + FormatDateTime('yyyy/mm/dd HH:nn', startTime)
      + '|end=' + FormatDateTime('yyyy/mm/dd HH:nn', endTime);
    Result := SimpleBOT.GetResponse('LayananBelumTersedia');
    Exit;
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
  s: string;
begin
  Result := '';
  with TBMKGIntegration.Create do
  begin
    Result := SimpleInfo;
    Result := StringReplace(Result, ', ', '\n', [rfReplaceAll]);

    //Result := '12-Jun-17 06:15:07 WIB Lok:8.36 LS,106.18 BT (179 ';
    s := Trim(StringCut('Lok:', 'BT', Result));
    if Pos(' LU', s) > 0 then
    begin
      s := StringReplace(s, ' LU', '', [rfReplaceAll]);
    end
    else
    begin
      s := '-' + StringReplace(s, ' LS', '', [rfReplaceAll]);
    end;
    s := '\n\nhttps://www.google.com/maps/place/' + s + '/@' + s + ',5z';

    Result := Result + s + SimpleBOT.GetResponse(IntentName + 'Disclaimer');
    Free;
  end;

  if Result = '' then
    Result := SimpleBOT.GetResponse('NotFound');

end;

function TCarikWebModule.openweatherInfoHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword, keywordTemp: string;

  function translateToID(ASource: string): string;
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
  keywordTemp := _keyword;
  if keywordTemp.IsEmpty then keywordTemp := 'NAMAKOTA';
  if Pos(',', _keyword) < 1 then
    _keyword := _keyword + ',id';
  with TOpenWeatherMapIntegration.Create do
  begin
    Key := Config[OPENWEATHERMAP_KEY];
    Result := WeatherAsJson(_keyword);

    Result := 'Cuaca di ' + Data['name'] + ':';
    Result := Result + #10 + translateToID(Data['weather[0].main']) +
      ', ' + translateToID(Data['weather[0].description']);

    Result := Result + #10'Kelembaban: ' + FormatFloat('#0',
      GetDataFloat('main.humidity'));
    Result := Result + #10'Suhu: ' + FormatFloat('#0.00',
      GetDataFloat('main.temp')) + '⁰C';
    Result := Result + #10'Terasa seperti: ' + FormatFloat('#0.00',
      GetDataFloat('main.feels_like')) + '⁰C';
    Result := Result + #10'Tekanan: ' + FormatFloat('#0.00',
      GetDataFloat('main.pressure'));

    Result := Result + #10'Kec. Angin: ' + FormatFloat('#0.00',
      GetDataFloat('wind.speed'));
    Result := Result + '; ' + FormatFloat('#0.0', GetDataFloat('wind.deg')) +
      ' derajat';

    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Result += '\nUntuk prakiraan cuaca seminggu ke depan, ketikkan:';
    Result += '\n `prakiraan cuaca di '+keywordTemp+'`';
    Result += '\n\nTetap prokes dan jaga diri yaa.';
    Free;
  end;
end;

function TCarikWebModule.apixuweatherInfoHandler(const IntentName: string;
  Params: TStrings): string;
var
  _keyword, _suffix: string;
  _img: string;
  json: TJSONUtil;

  function translateToID(ASource: string): string;
  begin
    Result := SimpleBOT.SimpleAI.GetResponse('LanguageID', '', trim(ASource));
    if (Result = '') or (Result = '._') then
      Result := ASource;
  end;

begin
  Result := '';
  _suffix := '';
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
    Key := Config[WEATHERSTACK_KEY];
    Result := WeatherAsJson(_keyword);
    if Result <> '' then
    begin
      json := TJSONUtil.Create;
      json.LoadFromJsonString(Result, False);
      if (not json['success']) then
      begin
        json.Free;
        result := openweatherInfoHandler(IntentName, Params);
        exit;
      end;

      _img := json.Item['current'].Item['weather_icons'].AsJSON;
      json.LoadFromJsonString(_img, False);
      _img := json.Data.Items[0].AsString;
      json.Free;
      //_img := 'http:' + Data['current.condition.weather_icons'];

      Result := 'Cuaca di ' + Data['location.name'] + ', ' + Data['location.region'];
      if FMessengerMode = mmTelegram then
        Result := Result + '[:]('+_img+')';
      Result := Result + #10 + translateToID(Data['current.condition.text']);
      Result := Result + #10'Suhu: ' + FormatFloat('#0.0',
        GetDataFloat('current.temperature')) + ' °C';
      Result := Result + #10'terasa seperti ' + FormatFloat('#0.0',
        GetDataFloat('current.feelslike')) + ' °C';

      Result := Result + #10'Kec. Angin: ' + FormatFloat('#0.00',
        GetDataFloat('current.wind_speed')) + ' kph';
      Result := Result + #10'Arah Angin: ' + FormatFloat('#0.00',
        GetDataFloat('current.wind_degree')) + '° (' + Data['current.wind_dir'] + ')';
      Result := Result + #10'Tekanan: ' + FormatFloat('#0.00',
        GetDataFloat('current.pressure')) + ' mb';
      Result := Result + #10'Kelembaban: ' + FormatFloat('#0.00',
        GetDataFloat('current.humidity')) + '';

      Result := Result + #10'Koordinat: '
        + FormatFloat('#0.0', GetDataFloat('location.lat'))
        + ','
        + FormatFloat('#0.0', GetDataFloat('location.lon'));
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

function TCarikWebModule.customMathHandler(const IntentName: string;
  Params: TStrings): string;
var
  mathParser: TFPExpressionParser;
  resultValue: double;
const
  AllowedOperator = ['+', '-', '/', '*', '^'];
begin
  Result := Params.Values['Formula_value'];
  Result := StringReplace(Result, ':', '/', [rfReplaceAll]);
  Result := StringReplace(Result, 'x', '*', [rfReplaceAll]);
  Result := StringReplace(Result, 'dibagi', '/', [rfReplaceAll]);
  Result := StringReplace(Result, 'bagi', '/', [rfReplaceAll]);
  Result := StringReplace(Result, 'dikali', '*', [rfReplaceAll]);
  Result := StringReplace(Result, 'kali', '*', [rfReplaceAll]);
  Result := StringReplace(Result, 'ditambah', '+', [rfReplaceAll]);
  Result := StringReplace(Result, 'tambah', '+', [rfReplaceAll]);
  Result := StringReplace(Result, 'dikurangi', '-', [rfReplaceAll]);
  Result := StringReplace(Result, 'dikurang', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '_', '-', [rfReplaceAll]);
  Result := StringReplace(Result, 'koma', '.', [rfReplaceAll]);
  Result := StringReplace(Result, 'rp.', '', [rfReplaceAll]);
  Result := StringReplace(Result, 'rp', '', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '', [rfReplaceAll]);
  Result := StringReplace(Result, 'nol', '0', [rfReplaceAll]);
  Result := Result.Replace('sama dengan', '');
  Result := Result.Replace('berapa', '');
  Result := Result.Replace('sama', '+');
  Result := Result.Trim;
  if Result.IsEmpty then
    Exit;
  if Result[1] = ',' then
    Result := Copy(Result, 2);
  if Result[1] = '.' then
    Result := Copy(Result, 2);
  Result := Result.Trim;
  Result := StringHumanToNominal(Result);
  Result := Result.Replace(' ' , '');
  if (Result[1] in AllowedOperator) then
  begin
    if SimpleBOT.UserData['math_result'].IsEmpty then
    begin
      Result := '..... :( ';
      Exit;
    end;
    Result := SimpleBOT.UserData['math_result'] + Result;
  end;
  Result := '(' + Result + ')';
  if not preg_match(REGEX_EQUATION, Result) then
  begin
    Result := ExternalNLP('berapa '+Params.Values['Formula_value']);
    if Result.IsEmpty then Result := '..... :( ';
    Exit;
  end;

  mathParser := TFPExpressionParser.Create(nil);
  try
    mathParser.BuiltIns := [bcMath, bcBoolean];
    mathParser.Expression := Result;
    resultValue := ArgToFloat(mathParser.Evaluate);
    SimpleBOT.UserData['math_result'] := f2s(resultValue);
    ThousandSeparator:='.';
    DecimalSeparator:=',';
    Result := FloatToStr(resultValue);
    Result := Format(FFormatNumber,[resultValue]);
    Result := Result.Replace(',000','');
  except
  end;
  mathParser.Free;
end;

function TCarikWebModule.propertySearchHandler(const IntentName: string;
  Params: TStrings): string;
var
  i, j: integer;
  s, search_type, search_category: string;
  http: THTTPLib;
  http_response: IHTTPResponse;
  jsonData: TJSONData;
begin
  Result := '';
  if Carik.IsGroup then
    Exit;

  search_type := string(Params.Values['PropertyType']);
  search_category := string(Params.Values['PropertyCategory']);
  if search_category = '' then
    search_category := 's';

  http := THTTPLib.Create;
  with http do
  begin
    URL := Config[RUMAH123_API_URL];
    AddHeader('Cache-Control', 'no-cache');
    //ContentType := 'application/x-www-form-urlencoded';
    AddHeader('X-Client-Type', Config[RUMAH123_API_CLIENT_TYPE]);
    FormData['type'] := search_type;
    FormData['category'] := search_category;
    FormData['area'] := string(Params.Values['area_value']);
    FormData['keyword'] := string(Params.Values['area_value']);
    http_response := Post;
  end;
  http.Free;

  if http_response.ResultCode <> 200 then
  begin
    Result := SimpleBOT.GetResponse(IntentName + 'NotFound');
    Exit;
  end;

  try
    jsondata := GetJSON(http_response.ResultText);
    j := jsonData.GetPath('data.total_rows').AsInteger;
    if j > RUMAH123_LISTING_MAX then
      j := RUMAH123_LISTING_MAX;

    if MessengerMode <> mmLine then
    begin
      for i := 0 to j - 1 do
      begin
        s := jsonData.GetPath('data.rows[' + i2s(i) + '].ads.tagline').AsString + #10;
        s := s + jsonData.GetPath('data.rows[' + i2s(i) +
          '].ads.pricing.price').AsString + #10;
        s := s + jsonData.GetPath('data.rows[' + i2s(i) + '].ads.url').AsString + #10;

        Result := Result + s + #10;
      end;
    end;

    if MessengerMode = mmLine then
    begin
      s := LineProperySearch('Pencarian Property', http_response.ResultText);
      if s <> '' then
      begin
        FSendRichContent := True;
        FRichContent := s;
        Result := SimpleBOT.GetResponse(IntentName + 'Waiting');
      end;

    end;

  except
  end;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
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

function TCarikWebModule.echoHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := Params.Values['$2'];
end;

function TCarikWebModule.PrepareTextToSpeech(AText: string): string;
begin
  Result := AText + ' ';
  Result := StripNonAscii(Result);
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
  //Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '\n', '. ', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '.', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '.', [rfReplaceAll]);

  Result := StringReplace(Result, ' wr wb', 'warahmatulahi wabarokatuh', [rfReplaceAll]);

  if Length(Result) > TEXT2SPEECH_MAX_CHAR then
  begin
    Result := copy(Result, 0, TEXT2SPEECH_MAX_CHAR);
    Result := copy(Result, 0, RPos('_', Result) - 1);
  end;

  if Result = '_-' then
    Result := '';

  Result := Trim(Result);
end;

function TCarikWebModule.SpeechToText(AAudioFile: string; AConvert: boolean): string;
var
  s, mp3FileName: string;
begin
  Result := '';
  if not FileExists(AAudioFile) then
    Exit;

  mp3FileName := AAudioFile;
  if AConvert then
  begin
    mp3FileName := ChangeFileExt(AAudioFile, '.mp3');
    if FileExists(mp3FileName) then
      DeleteFile(mp3FileName);

    if not Exec(Config[FFMPEG_PATH], ['-i', AAudioFile, '-ac', '1', mp3FileName],
      s, swoNone) then
    begin
      Exit;
    end;
    DeleteFile(AAudioFile);
  end;

  with TWitAiIntegration.Create do
  begin
    Token := Config[WITAI_TOKEN];
    Result := SpeechToText(mp3FileName);
    if Result = '' then
      LogUtil.Add('err: ' + ResultText, 'S2T')
    else
      LogUtil.Add('text: ' + Result, 'S2T');
    Free;
  end;

end;

function TCarikWebModule.isTriggeredText(Message: string): boolean;
var
  i, j: integer;
  s, stime, strigger_name, strigger, LMessage: string;
  jData: TJSONData;
  d: TDateTime;
  tmpFormatSettings: TFormatSettings;
begin
  Result := False;
  LMessage := ' ' + Message + ' ';
  jData := nil;

  tmpFormatSettings := FormatSettings;
  tmpFormatSettings.DateSeparator := '-';
  tmpFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  tmpFormatSettings.LongDateFormat := 'yyyy-MM-dd HH:nn:ss';

  { TODO:
  if last visit < 1 menit,
    maka cek trigger2 khusus
  selain itu, check trigger global
  }

  try
    s := Config.GetObject(AI_CONFIG_TRIGGERWORD).AsJSON;
    jData := GetJSON(s);
    for i := 0 to jData.Count - 1 do
    begin
      strigger_name := Config.GetObject(AI_CONFIG_TRIGGERWORD).Names[i];
      for j := 0 to jData.Items[i].Count - 1 do
      begin
        strigger := ' ' + jData.Items[i].Items[j].AsString + ' ';
        if preg_match(strigger, LMessage) then
        begin
          s := SafeText(strigger_name);
          s := ReplaceAll(s, ['(', ')'], '');
          s := mysql_real_escape_string(s);
          s := TRIGGER_SESSION_PREFIX + Carik.GroupName + Carik.GroupChatID + '-' + s + '_time';

          stime := Carik.Data.ReadString(TRIGGER_SESSION_PREFIX, s,
            FormatDateTime('yyyy-mm-dd HH:nn:ss', IncMinute(Now, TRIGGER_INTERVAL + 2)));
          try
            d := StrToDateTime(stime, tmpFormatSettings);
            //j := MinutesBetween(Now, d);
            if MinutesBetween(Now, d) <= TRIGGER_INTERVAL then // in minutes
            begin
              Break;
            end;
          except
          end;

          Carik.Data.WriteString(TRIGGER_SESSION_PREFIX, s, FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
          FTriggeredText := Trim(strigger);
          Result := True;
          break;
        end;
      end;

    end;
  except
  end;


  if Assigned(jData) then
    jData.Free;
  exit;

  try
    s := Config[AI_CONFIG_TRIGGERWORD];
    jData := GetJSON(s);
    for i := 0 to jData.Count - 1 do
    begin
      strigger := trim(jData.Items[i].AsString);
      if preg_match(strigger, LMessage) then
      begin
        s := SafeText(strigger);
        s := ReplaceAll(s, ['(', ')'], '');
        s := mysql_real_escape_string(s);
        s := TRIGGER_SESSION_PREFIX + Carik.GroupName + Carik.GroupChatID + '-' + s + '_time';

        stime := Carik.Data.ReadString(TRIGGER_SESSION_PREFIX, s,
          FormatDateTime('yyyy-mm-dd HH:nn:ss', IncMinute(Now, TRIGGER_INTERVAL + 2)));
        try
          d := StrToDateTime(stime, tmpFormatSettings);
          j := MinutesBetween(Now, d);
          if MinutesBetween(Now, d) <= TRIGGER_INTERVAL then // in minutes
          begin
            Break;
          end;
        except
        end;

        Carik.Data.WriteString(TRIGGER_SESSION_PREFIX, s, FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
        FTriggeredText := Trim(strigger);
        Result := True;
        break;
      end;
    end;

  except
  end;

end;

function TCarikWebModule.IsUserSuspended(AChannelID, AUserID: string): boolean;
var
  s, check_url, client_id: string;
  http_response: IHTTPResponse;
  user_data: TJSONData;
begin
  Result := False;
  if AUserID = '' then
    Exit;
  try
    check_url := '';
    check_url := Config[USERSTATUS_URL];
    client_id := Config[USERSTATUS_CLIENTID];
  except
  end;
  if client_id = '' then
    Exit;
  if check_url = '' then
    Exit;

  try
    with THTTPLib.Create do
    begin
      URL := check_url;
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('X-Client-Type', 'beta'); //TODO: client type
      FormData['channel_id'] := AChannelID;
      FormData['client_id'] := client_id;
      FormData['user_id'] := AUserID;
      http_response := Post;
      if http_response.ResultCode = 200 then
      begin
        user_data := GetJSON(http_response.ResultText);
        s := jsonGetData(user_data, 'user.status');
        if s = 'disabled' then
          Result := True;
      end;

      Free;
    end;
  except
  end;

end;

function TCarikWebModule.IsBlackListed(AUserName: string; AUserId: string
  ): boolean;
var
  i: integer;
  log_url: string;
  spamList: TIniFile;
  http_response: IHTTPResponse;
  json: TJSONUtil;
begin
  Result := False;

  // get info from blacklist repository
  try
    log_url := Config[BLACKLISTCHECK_URL] + '?_=1&channel=telegram&id=' + AUserId;
    with THTTPLib.Create do
    begin
      URL := log_url;
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('X-Client-Type', 'beta'); //TODO: client type
      http_response := Get;
      if http_response.ResultCode = 200 then
      begin
        json := TJSONUtil.Create;
        json.LoadFromJsonString(http_response.ResultText, False);
        i := json['code'];
        if i = 0 then
        begin
          Free;
          Result := True;
          Exit;
        end;
      end;
      Free;
    end;
  except
  end;

  if not FileExists(BLACKLIST_FILENAME) then
    Exit;

  spamList := TIniFile.Create(BLACKLIST_FILENAME);
  if spamList.ValueExists('blacklist', 'tl-' + AUserName) then
  begin
    spamList.Free;
    Result := True;
    Exit;
  end;

  // Check from combot
  if not AUserId.IsEmpty then
  begin
    if IsSpammer(AUserId) then
    begin
      ReportSpam('+'+AUserId);
      Result := True;
    end;
  end;

  spamList.Free;
end;

function TCarikWebModule.IsSuspected(AUserId: string; AFullName: string
  ): boolean;
begin
  Result := False;
  AFullName := AFullName.Replace(' ', '');
  if AFullName.Length < 3 then
    Result := True;
  if not isVowelExists(AFullName) then
    Result := True;
end;

function TCarikWebModule.IsGlobalUserBlackListed(AUserId: string): boolean;
var
  i: integer;
  s, id: string;
  lst: TStringList;
begin
  Result := False;
  try
    if Config.GetObject('blacklist').Find('users').Count = 0 then
      Exit;

    id := '';
    for i:=0 to Config.GetObject('blacklist').Find('users').Count-1 do
    begin
      id := Config.GetObject('blacklist').Find('users').Items[i].Items[0].AsString;
      if id = AUserId then
      begin
        Result := True;
        Break;
      end;
    end;
  except
  end;

  if not FileExists(BLACKLIST_GLOBAL_FILENAME) then
    Exit;

  lst := TStringList.Create;
  lst.LoadFromFile(BLACKLIST_GLOBAL_FILENAME);
  for i := 0 to lst.Count -1 do
  begin
    id := lst[i];
    id := RemoveCharactersBefore(id, ',');
    if FMessengerMode = mmWhatsapp then
    begin
      if (AUserId = WHATSAPP_CHANNEL_ID + '-' + id) then
      begin
        Result := True;
        break;
      end;
    end;
  end;

  lst.Free;
end;

function TCarikWebModule.IsGlobalGroupBlackListed(AGroupId: string): boolean;
var
  i: integer;
  id: string;
begin
  Result := False;
  try
    if Config.GetObject('blacklist/groups').Count = 0 then
      Exit;

    id := '';
    for i:=0 to Config.GetObject('blacklist/groups').Count-1 do
    begin
      id := Config.GetObject('blacklist/').Find('groups').Items[i].Items[0].AsString;
      if id = AGroupId then
      begin
        Result := True;
        Break;
      end;
    end;
  except
  end;

end;

function TCarikWebModule.isNewMember(AUserID: string; AGroupID: string;
  AInterval: integer): boolean;
var
  s, checkUrl: string;
  json: TJSONUtil;
  FS: TFormatSettings;
begin
  //AUserID := '842005875';
  //AGroupID := '-1001081482233';

  Result := False;
  checkUrl := Config[GROUPMEMBER_URL];
  if checkUrl.IsEmpty then
    Exit;
  checkUrl := checkUrl + '?_=1&channel=telegram&id=' + AUserID
    + '&gid=' + AGroupID;
  s := file_get_contents(checkUrl);
  json := TJSONUtil.Create;
  json.LoadFromJsonString(s, False);
  if (json['code'] <> 0) or (json['count']=0) then
  begin
    json.Free;
    Exit;
  end;
  s := json['last_join_date'];
  s := s.Replace('-','/');

  FS := DefaultFormatSettings;
  FS.DateSeparator := '/';
  FS.ShortDateFormat := 'yyyy/mm/dd';
  FS.ShortTimeFormat := 'hh:mm:ss';

  if MinutesBetween( Now, StrToDateTime(s, FS)) < AInterval then
    Result := True;
  json.Free;
end;

function TCarikWebModule.SpamScore(AUserID: string; AText: string;
  ForceCheck: boolean): integer;
var
  i, j, additionScore: integer;
  s, triggerName, triggerWord: string;
  jData: TJSONData;
begin
  Result := 0;
  AText := AText.ToLower;
  if isLookLikeURL(AText) or ForceCheck then
  begin
    Result := Result + 10;
    //if StringsExists('t.me/', AText) then
    //  Result := Result + 71;
    if StringsExists('t.me/joinchat', AText) then
      Result := Result + 71;
    if StringsExists('telegram.me/joinchat', AText) then
      Result := Result + 71;

    //exception
    if preg_match('stackoverflow.com', AText) then
      Result := Result - 80;
    if preg_match('.ini ', AText) then
      Result := Result - 80;

    if isNewMember(AUserID, Carik.GroupChatID, NEW_MEMBER_INTERVAL_POST_PERMITTED) then // last join N minutes ago
    begin
      Result := Result + 70;
    end;
  end;

  //TODO: add spam score detection
  if AText.IsExists('$') then Result := Result + 10;
  s := Config.GetObject(SPAM_WORD).AsJSON;
  jData := GetJSON(s);
  for i := 0 to jData.Count - 1 do
  begin
    additionScore := 30;
    triggerName := Config.GetObject(SPAM_WORD).Names[i];
    for j := 0 to jData.Items[i].Count - 1 do
    begin
      triggerWord := jData.Items[i].Items[j].AsString;
      if preg_match(triggerWord, AText) then
      begin
        Result := Result + additionScore;
      end;
    end;
  end;
  jData.Free;

  if Result < 80 then
  begin
    //TODO: check from spam-score api;
  end;

  if Result >= SPAM_SCORE_THRESHOLD then
  begin
    LogUtil.Add(AUserID + ': ' + AText, 'SPAM('+i2s(Result)+')');
  end;

end;

function TCarikWebModule.IsSpammer(AUserID: string): boolean;
var
  i: integer;
  s, url: String;
  json: TJSONUtil;
  isDataExist: boolean;
begin
  Result := False;
  if AUserID.IsEmpty then
    Exit;
  if (FMessengerMode <> mmTelegram) then
    Exit;

  url := 'https://api.cas.chat/check?user_id=' + AUserID;
  s := file_get_contents(url, False);
  if s.IsEmpty then
    Exit;

  json := TJSONUtil.Create;
  json.LoadFromJsonString(s, False);
  try
    isDataExist := False;
    isDataExist := json['ok'];
  except
  end;
  Result := isDataExist;
  {
  if isDataExist then
  begin
    i := json['result/offenses'];
    if i > SPAM_CAS_OFFENSE then
      Result := True;
  end;
  }

  json.Free;
end;

function TCarikWebModule.ReportSpam(AUserID: string; AUserName: string;
  AReportBy: string; AReportByName: string): string;
var
  log_url: string;
  spamList: TIniFile;
  http_response: IHTTPResponse;
  json: TJSONUtil;
begin
  Result := 'Ada kegagalan nih.';
  if AUserID.IsEmpty then Exit;
  try
    log_url := Config[BLACKLISTADD_URL] + '?_=1&channel=telegram';
    with THTTPLib.Create do
    begin
      URL := log_url;
      LogUtil.Add( 'url: ' + URL, 'SPAMREPORT');
      LogUtil.Add( 'par: ' + AUserID + '/' + AUserName + '/' + AReportBy + '/', 'SPAMREPORT');
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('X-Client-Type', 'beta'); //TODO: client type
      FormData['UserID'] := AUserID.Replace('+','');
      FormData['UserName'] := AUserName;
      FormData['ReportBy'] := AReportBy;
      FormData['ReportByName'] := AReportByName;
      http_response := Post;
      LogUtil.Add( AUserID.Replace('+','') + '/' + AUserName + ': ' + http_response.ResultText, 'SPAMREPORT');

      json:= TJSONUtil.Create;
      json.LoadFromJsonString(http_response.ResultText, False);
      Result:= json['text'];
      json.Free;

      Result := AUserID + ' reported as spammer';
      if Result.IsEmpty then
        Result := 'kegagalan dalam melaporkan spammer';

      Free;
    end;
  except
  end;

  if not FileExists(BLACKLIST_FILENAME) then
    Exit;

  try
    spamList := TIniFile.Create(BLACKLIST_FILENAME);
    spamList.WriteString('blacklist', 'tl-'+AUserID, FormatDateTime('yyyy-mm-dd HH:nn:ss',Now));
    spamList.Free;
  except
  end;

end;


procedure TCarikWebModule.Analytics(AChannel, AIntent, AText, AUserID: string);
var
  _trackingID: string;
begin
  _trackingID := Config[GOOGLEANALYTICS_TRACKING_ID];
  if _trackingID.IsEmpty then
    Exit;
  if AIntent.Equals('GroupInvitationNotMe') then Exit;
  if AIntent.Equals('Greeting') then Exit;
  if AIntent.Equals('Menyapa') then Exit;

  with TGoogleAnalyticsIntegration.Create do
  begin
    TrackingID := Config[GOOGLEANALYTICS_TRACKING_ID];
    ClientID := AUserID;
    HitType := 'event';
    Payloads['an'] := FBotName;
    Payloads['ai'] := FBotName;
    Payloads['av'] := AChannel + '-v0';
    Payloads['ec'] := AChannel;
    Payloads['ea'] := AIntent;
    Payloads['dp'] := '/' + AIntent;
    Payloads['el'] := UrlEncode(AText);
    Payloads['lbl'] := 'lbl1';
    Payloads['uid'] := AUserID;
    if not Send then
      LogUtil.Add(URL, 'Analytics-Failed');
    Free;
  end;

  with TGoogleAnalyticsIntegration.Create do
  begin
    TrackingID := Config[GOOGLEANALYTICS_TRACKING_ID];
    ClientID := AUserID;
    HitType := 'pageview';
    Payloads['dt'] := AIntent;
    Payloads['dp'] := UrlEncode(AIntent + '/' + AText);
    Send;
    Free;
  end;

end;

function TCarikWebModule.KnowledgeBase(AKeyword: string): string;
var
  kb_url: string;
  kb_json: TJSONData;
  http_response: IHTTPResponse;
begin
  Result := '';
  try
    kb_url := '';
    kb_url := Config[KNOWLEDGEBASE_URL];
  except
  end;

  if kb_url = '' then
    Exit;

  try
    with THTTPLib.Create do
    begin
      URL := kb_url;
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('X-Client-Type', 'beta'); //TODO: client type
      FormData['keyword'] := AKeyword;
      http_response := Post;
      if http_response.ResultCode = 200 then
      begin
        kb_json := GetJSON(http_response.ResultText);
        Result := jsonGetData(kb_json, 'text');
      end;

      Free;
    end;
  except
  end;

end;

function TCarikWebModule.CarikSearch(AKeyword: string): string;
var
  searchToken: string;
  search_url: string;
  search_json: TJSONData;
  http_response: IHTTPResponse;
begin
  Result := '';
  try
    search_url := '';
    search_url := Config[CARIKSEARCH_URL];
    searchToken := Config[CARIKSEARCH_TOKEN];
  except
  end;

  if search_url = '' then
    Exit;

  try
    with THTTPLib.Create do
    begin
      URL := search_url;
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('X-Client-Type', 'beta'); //TODO: client type
      FormData['keyword'] := AKeyword;
      FormData['ChannelId'] := ChannelId;
      FormData['token'] := searchToken;
      http_response := Post;
      if http_response.ResultCode = 200 then
      begin
        search_json := GetJSON(http_response.ResultText);
        Result := trim(jsonGetData(search_json, 'text'));
      end;
      Free;
    end;
  except
  end;
end;

function TCarikWebModule.ExternalNLP(AText: string; AUseGPT: boolean): string;
var
  i, processing_time: integer;
  nlp_enable, use_gpt: boolean;
  s, nlp_url, nlpAction: string;
  nlp_json: TJSONData;
  requestData: TJSONUtil;
  http_response: IHTTPResponse;
begin
  Result := '';
  FExternalNLPWeight := 0;
  processing_time := 0;
  use_gpt := False;
  try
    nlp_url := '';
    nlp_url := Config[EXTERNAL_NLP_URL];
    nlp_enable := Config[EXTERNAL_NLP_ENABLE];
    use_gpt := Config[EXTERNAL_NLP_USEGPT];
  except
  end;
  if nlp_url = '' then
    Exit;
  if AUseGPT then use_gpt := True;

  if not nlp_enable then
    Exit;

  //nlp_url := nlp_url + '?client_id='+ClientId;
  //if use_gpt then nlp_url += '&gpt=1';
  //nlp_url += '&text=' + UrlEncode(AText);

  requestData := TJSONUtil.Create;
  try
    with THTTPLib.Create do
    begin
      ConnectTimeout := EXTERNAL_NLP_TIMEOUT;
      if ((use_gpt = True) and (FGPTTimeout > 0)) then
        ConnectTimeout := FGPTTimeout;
      URL := nlp_url;
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('X-Client-Type', 'beta'); //TODO: client type
      AddHeader('_source', 'carik');
      AddHeader('User-Agent', 'carik/nlp');

      ContentType := 'application/json';
      requestData['data/user_id'] := PrefixId + '-' + Carik.UserID;
      requestData['data/group_id'] := Carik.GroupChatID;
      requestData['data/group_name'] := Carik.GroupName;
      requestData['data/channel_id'] := ChannelId;
      requestData['data/client_id'] := ClientId;
      requestData['data/FullName'] := Carik.FullName;
      requestData['data/full_name'] := Carik.FullName;
      if FDelayReplay then requestData['data/delay_reply'] := 1;
      if requestData['data/original_text'] = '' then requestData['data/original_text'] := OriginalText;
      for i:=0 to SimpleBOT.SimpleAI.Parameters.Count-1 do
      begin
        requestData['data/'+SimpleBOT.SimpleAI.Parameters.Names[i]] :=
            SimpleBOT.SimpleAI.Parameters.ValueFromIndex[i];
      end;
      requestData['data/message'] := Text;
      requestData['data/text'] := Text;

      RequestBody := TStringStream.Create(requestData.AsJSON);
      http_response := Post;
      if AppData.debug then
        LogUtil.Add(http_response.ResultText, 'LLM');

      // gunakan block di bawah
      if http_response.ResultCode = 200 then
      begin
        SimpleBOT.SimpleAI.AdditionalParameters.Values['external'] := 'true';
        FExternalNLPStarted := True;
        nlp_json := GetJSON(http_response.ResultText, False);
        s := jsonGetData(nlp_json, 'package');
        if s.IsNotEmpty then
          SimpleBOT.SimpleAI.AdditionalParameters.Values['package'] := s;

        // get token usage
        FToken_I := s2i(jsonGetData(nlp_json, 'token_usage/i'));
        FToken_O := s2i(jsonGetData(nlp_json, 'token_usage/o'));
        FToken_T := s2i(jsonGetData(nlp_json, 'token_usage/t'));

        FExternalNLPWeight := s2i(jsonGetData(nlp_json, 'weight'));
        ProcessingTime := s2i(jsonGetData(nlp_json, 'processing_time'));
        SimpleBOT.SimpleAI.AdditionalParameters.Values['external_processing_time'] := i2s(ProcessingTime);
        Result := jsonGetData(nlp_json, 'text');
        Result := Result.Replace(#10, '\n');
        FExternalNLPIntentName := jsonGetData(nlp_json, 'response/intents/name');
        FExternalNLPIntentPattern := jsonGetData(nlp_json, 'response/intents/pattern');
        nlpAction := jsonGetData(nlp_json, 'type');
        FCustomReplyActionTypeFromExternalNLP := '';
        FCustomReplyTypeFromExternalNLP := nlpAction;
        if (nlpAction.IsNotEmpty) then
        begin
          FCustomReplyActionTypeFromExternalNLP := jsonGetData(nlp_json, 'action/type');
          FCustomReplyURLFromExternalNLP := jsonGetData(nlp_json, 'action/url');
          FCustomReplyName := jsonGetData(nlp_json, 'action/name');
          FCustomActionSuffix:= jsonGetData(nlp_json, 'action/suffix');
          // compatibility
          if FCustomActionSuffix.IsEmpty then FCustomActionSuffix := jsonGetData(nlp_json, 'suffix');;
          //ulil -----
          SaveActionToUserData(FCustomReplyActionTypeFromExternalNLP, TJSONObject(nlp_json.GetPath('action.data')));
          if FCustomActionAsText.IsNotEmpty then
          begin
            //Result += '\n'+FCustomActionAsText.Replace(#10, '\n');
          end;
        end;
      end;

      //compatibility with form input handler
      if http_response.ResultCode = 200 then
      begin
        FCustomReplyDataFromExternalNLP := TJSONUtil.Create;
        FCustomReplyDataFromExternalNLP.LoadFromJsonString(http_response.ResultText, False);
        //Suffix := FCustomReplyDataFromExternalNLP['text'];

        FCustomReplyTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['type'];
        FCustomReplyActionTypeFromExternalNLP := 'text';
        if FCustomReplyTypeFromExternalNLP.IsNotEmpty then //action
        begin
          FCustomReplyActionTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['action/type'];
          FCustomReplyURLFromExternalNLP := FCustomReplyDataFromExternalNLP['action/url'];
          FCustomReplyName := FCustomReplyDataFromExternalNLP['action/name'];
          FCustomActionSuffix := FCustomReplyDataFromExternalNLP['action/suffix'];
          // compatibility
          if FCustomActionSuffix.IsEmpty then FCustomActionSuffix := jsonGetData(nlp_json, 'suffix');;
          SaveActionToUserData(FCustomReplyActionTypeFromExternalNLP, TJSONObject(FCustomReplyDataFromExternalNLP.Data.GetPath('action.data')));
          FCustomReplyDataFromExternalNLP.LoadFromJsonString(FCustomReplyDataFromExternalNLP.Data.GetPath('action.data').AsJSON, False);
          if FCustomActionAsText.IsNotEmpty then
          begin
            {
            Suffix += '\n'+ACTION_CAPTION+'\n'+FCustomActionAsText.Replace(#10, '\n');
            s := FCustomReplyDataFromExternalNLP.Data.Items[0].Items[0].GetPath('text').AsString;
            s := ACTION_SUFFIX.Replace('%s', s);
            Suffix += '\n' + s;
            }
          end;

          // files
          FCustomActionFiles := Nil;
          try
            FCustomActionFiles := TJSONArray(FCustomReplyDataFromExternalNLP.Data.GetPath('files'));
          except
          end;

        end;

      end;

      //TODO: ulil chek if 'files' exist

      Free;
    end;
  except
    on E:Exception do
    begin
      LogUtil.Add(E.Message, 'ENLP');
    end;
  end;

  requestData.Free;
end;

function TCarikWebModule.texttospeechHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if Config[CARIK_TTS_URL] = '' then
    Exit;

  FFileURL := PrepareTextToSpeech(Params.Values['sentence_value']).UrlEncode;
  FFileURL := Config[CARIK_TTS_URL] + FFileURL + '?token=' + Config[CARIK_TTS_TOKEN];

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
  s, fromLanguage, toLanguage, sourceText: string;
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

    s := Params.Values['language'];
    if not s.IsEmpty then
    begin
      toLanguage := Params.Values['language'];
      sourceText := Params.Values['kalimat'];
      fromLanguage := Detect(sourceText);
      FLanguage := fromLanguage + '-' + toLanguage;
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
  end else
    Result := 'Maaf, sedang gangguan layanan translasi bahasa.\nCoba lagi nanti yaa.';

end;

function TCarikWebModule.translate(AText: string; AFrom: string;
  ATo: string; ACache: boolean): string;
var
  s: string;
begin
  Result := AText;
  with TYandexTranslateIntegration.Create do
  begin
    Cache := ACache;
    Key := Config[YANDEX_KEY];
    s := Translate(AFrom + '-' + ATo, AText);
    s := StringReplace(s, #10, '\n', [rfReplaceAll]);
    if s <> '' then
      Result := s;
    Free;
  end;
end;

function TCarikWebModule.pajakKendaraanHandler(const IntentName: string;
  Params: TStrings): string;
var
  http_response: IHTTPResponse;
begin
  Result := SimpleBOT.GetResponse('NotFound');

  with THTTPLib.Create do
  begin
    URL := Config[PAJAKMOTOR_API] + Params.Values['nopol_value'];
    if URL = '' then
    begin
      Free;
      Exit;
    end;
    http_response := Get;

    if http_response.ResultCode = 200 then
    begin
      if Trim(http_response.ResultText) <> '' then
        Result := Trim(http_response.ResultText);
    end;

    Free;
  end;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);


  {
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
  }
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

function TCarikWebModule.IsCommand(AText: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  if AText = '' then
    Exit;
  lst := Explode(AText, ':');
  if lst.Count = 1 then
  begin
    lst.Free;
    Exit;
  end;

  if isValidCommand(lst[0]) then
    Result := True;
  lst.Free;
end;

function TCarikWebModule.isValidCommand(ACommandString: string): boolean;
var
  s: string;
begin
  Result := False;
  for s in CommandList do
  begin
    if ACommandString = s then
      Exit(True);
  end;
end;

function TCarikWebModule.ExecCommand(AText: string): string;
var
  url: string;
  lst: TStrings;
begin
  Result := '';
  lst := Explode(AText, ':');
  case lst[0] of
    CMD_POST_RICH:
    begin
      url := Trim(Copy(AText, Pos(':', AText) + 1));
      Result := execPost(url);
    end;
    CMD_JSON_RICH:
    begin
      url := Trim(Copy(AText, Pos(':', AText) + 1));
      Result := execJson(url);
    end;
  end;
  lst.Free;
end;

function TCarikWebModule.MaximumRetriesUnknownChat: integer;
var
  s: string;
begin
  Result := s2i(Config[UNKNOWNCHAT_RETRIES]);
  s := Config[UNKNOWNCHAT_CALLBACK];
  if s.IsEmpty then
    Result := 0;
end;

function TCarikWebModule.GenerateResponseJson: string;
var
  jsonOutput: TJSONUtil;
  customReplyDataAsArray, inputData: TJSONArray;
begin
  jsonOutput := TJSONUtil.Create;
  jsonOutput.LoadFromJsonString(SimpleBOT.SimpleAI.ResponseJson, False);
  if FExternalNLPIntentName.IsNotEmpty then
  begin
    jsonOutput['response/intents/name'] := 'ex:'+FExternalNLPIntentName;
    jsonOutput['response/intents/external'] := True;;
  end;
  if FExternalNLPIntentPattern.IsNotEmpty then
    jsonOutput['response/intents/pattern'] := ''+FExternalNLPIntentPattern;
  if Prefix.IsNotEmpty then
    jsonOutput['response/prefix'] := Prefix;
  if Suffix.IsNotEmpty then
    jsonOutput['response/suffix'] := Suffix;
  if Assigned(FCustomReplyDataFromExternalNLP) then
  begin
    jsonOutput['action/action/type'] := CustomReplyType;
    customReplyDataAsArray := TJSONArray(FCustomReplyDataFromExternalNLP.AsJSON);
    customReplyDataAsArray := TJSONArray(GetJSON(FCustomReplyDataFromExternalNLP.AsJSON, False));
    jsonOutput.ValueArray['action/action/data'] := customReplyDataAsArray;

    //compatibility
    jsonOutput['action/type'] := CustomReplyType;
    jsonOutput['action/suffix'] := CustomActionSuffix;
    jsonOutput.ValueArray['action/data'] := customReplyDataAsArray;
  end;
  if CustomActionSuffix.IsNotEmpty then
    jsonOutput['action/suffix'] := CustomActionSuffix;
  if CustomReplyIsMainMenu then
    jsonOutput['action/main'] := True;
  if CustomReplyMenuLevel.IsNotEmpty then
  begin
    jsonOutput['action/level'] := CustomReplyMenuLevel;
    jsonOutput['level'] := CustomReplyMenuLevel;
  end;

  // Input Type
  if (FCurrentInputType.IsNotEmpty and (FCurrentInputType <> 'string')) then
  begin
    jsonOutput['action/input/type'] := FCurrentInputType;
    jsonOutput['action/input/title'] := FInputOptionTitle;
    if Assigned(FInputOptions) then
    begin
      inputData := TJSONArray.Create;
      inputData.Add(FInputOptions);
      jsonOutput.ValueArray['action/input/data'] := inputData;
    end;
  end;

  // show token usage
  jsonOutput['response/token_usage/i'] := Token_I;
  jsonOutput['response/token_usage/o'] := Token_O;
  jsonOutput['response/token_usage/t'] := Token_T;

  jsonOutput['processing_time'] := ProcessingTime;
  if SimpleBOT.SimpleAI.ElapsedTime > 0 then
  begin
    jsonOutput['processing_time'] := SimpleBOT.SimpleAI.ElapsedTime.ToString.ToInteger;
  end;

  if FIsDebug then
    Result := jsonOutput.AsJSONFormated
  else
    Result := jsonOutput.AsJSON;
end;

function TCarikWebModule.SavePrune(AMessageID: string): boolean;
var
  key : string;
begin
  Result := false;
  if not Carik.isGroup then Exit;
  key := SimpleBot.SimpleAI.IntentName + '/' + Text.Replace(' ', '-');
  GroupData[key] := AMessageID;
  Result := True;
end;

function TCarikWebModule.GetPrune: string;
var
  key : string;
begin
  Result := '';
  if not Carik.isGroup then Exit;
  key := SimpleBot.SimpleAI.IntentName + '/' + Text.Replace(' ', '-');
  Result := GroupData[key];
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
begin
  Result := '';
  if AJson = '' then
    Exit;

  //TODO: Facebook News

end;

procedure TCarikWebModule.DoProgress(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  If (ContentLength=0) then
    Writeln('Reading headers : ',CurrentPos,' Bytes.')
  else If (ContentLength=-1) then
    Writeln('Reading data (no length available) : ',CurrentPos,' Bytes.')
  else
    Writeln('Reading data : ',CurrentPos,' Bytes of ',ContentLength);
end;

procedure TCarikWebModule.DoHeaders(Sender: TObject);
var
  i: integer;
begin
  Writeln('Response headers received:');
  With (Sender as TFPHTTPClient) do
    For I:=0 to ResponseHeaders.Count-1 do
      Writeln(ResponseHeaders[i]);
end;

// fcl-web/examples/httpclient/httpget.pas
procedure TCarikWebModule.DoPassword(Sender: TObject; var RepeatRequest: Boolean
  );
begin

end;

procedure TCarikWebModule.ShowRedirect(ASender: TObject; const ASrc: String;
  var ADest: String);
begin
  Writeln('Following redirect from ',ASrc,'  ==> ',ADest);
end;

procedure TCarikWebModule.HttpClientGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  AHandler := nil;
  If UseSSL then begin
    AHandler := TSSLSocketHandler.Create;
    TSSLSocketHandler(AHandler).SSLType := stAny;
  end else begin
  end;
end;

function TCarikWebModule.execPost(AURL: string; ACache: boolean): string;
var
  i: integer;
  s: string;
  lst: TStrings;
  httpResponse: IHTTPResponse;
begin
  Result := '';
  if FOperation.IsEmpty then FOperation := RequestAsJson['op'];

  with THTTPLib.Create(AURL) do
  begin
    try
      // set header from nlp config
      AddHeader('_source', 'carik');
      s := SimpleBOT.GetResponse(SimpleBOT.SimpleAI.IntentName, '', 'header');
      s := StringReplace(s, ':', '=', [rfReplaceAll]);
      lst := Explode(s, '|');
      for i := 0 to lst.Count - 1 do
      begin
        if lst.Names[i] <> '' then
          AddHeader(lst.Names[i], lst.ValueFromIndex[i]);
      end;
      lst.Free;

      // additional parameter
      for i := 0 to SimpleBOT.SimpleAI.SimpleAILib.Parameters.Count - 1 do
      begin
        FormData[SimpleBOT.SimpleAI.SimpleAILib.Parameters.Names[i]] :=
          UrlEncode(SimpleBOT.SimpleAI.SimpleAILib.Parameters.ValueFromIndex[i]);
      end;
      httpResponse := Post();
      Result := httpResponse.ResultText;
      if not IsJsonValid(Result) then
      begin
        Result := Result.Replace(#13, '\n');
        Result := Result.Replace(#10, '\n');
      end;
    except
      on e: Exception do
      begin
        //
      end;
    end;

    Free;
  end;
end;

function TCarikWebModule.execJson(AURL: string; ACache: boolean): string;
var
  pathName: string;
  lst: TStrings;
  json: TJSONUtil;
  jsonData: TJSONData;
begin
  Result := AURL;
  pathName := 'text';
  lst := Explode(AURL, '|');
  if lst.Count > 1 then
  begin
    pathName := lst[0];
    AURL := lst[1];
    Result := pathName;
  end;
  lst.Free;

  Result := execPost( AURL);
  if Result = '' then
    Exit;

  //jsonData := GetJSON(Result, True);
  //Result := jsonData.GetPath(pathName).AsUnicodeString;
  //exit;
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Result, False);
    Result := json[pathName];
  except
    Result := '';
  end;

  Result := Result.Replace(#13, '\n');
  Result := Result.Replace(#10, '\n');
end;

procedure TCarikWebModule.saveContext(const AParams: TStrings);
var
  lst : TStringList;
begin
  lst := TStringList.Create;
  lst.Text := AParams.Text;
  lst.Values['pattern'] := '';
  SimpleBOT.UserData['CONTEXT_ACTION'] := lst.Values['action'];
  //SimpleBOT.UserData['CONTEXT'] := lst.Values['intent_name'];
  lst.Text := lst.Text.Replace(#13,'|');
  lst.Text := lst.Text.Replace(#10,'|');
  SimpleBOT.UserData['CONTEXT_PARAM'] := lst.Text;
  SimpleBOT.UserData['CONTEXT_DATE'] := DateTimeToStr(Now);
  lst.Free;
end;

procedure TCarikWebModule.LogChat(AChannelID: string; AGroupID: string;
  AGroupName: string; AUserID: string; AUserName: string; AFullName: string;
  AText: string; AReply: string; AIsGroup: boolean; AIsMentioned: boolean;
  AMessageID: integer; AResultMessageID: integer; AReplyFromMessageId: integer);
var
  urlExplode: TStrings;
  log_url: string;
  is_group, is_mentioned: string;
  httpResponse: IHTTPResponse;
  requestJson, responseJson: TJSONUtil;
begin
  if AText = '' then
    Exit;
  try
    log_url := '';
    log_url := Config[CHATLOG_URL];
  except
  end;
  if log_url = '' then
    Exit;

  urlExplode := log_url.Explode('?');
  log_url := urlExplode[0] + '?clientId=' + FClientId;

  if AGroupID = AUserID then
    AGroupID := '';

  is_group := '0';
  is_mentioned := '1';
  if AIsGroup then
    is_group := '1';
  if AIsMentioned then
    is_mentioned := '0';

  requestJson := TJSONUtil.Create;
  responseJson := TJSONUtil.Create;
  with THTTPLib.Create(log_url) do
  begin
    AllowRedirect := True;
    //AddHeader('_source', 'carik');
    //AddHeader('User-Agent', 'carik/nlp');
    //AddHeader('Cache-Control', 'no-cache');
    //AddHeader('X-Client-Type', 'beta'); //TODO: client type
    if not FClientId.IsEmpty then
    begin
      requestJson['data/clientId'] := FClientId;
      requestJson['data/clientID'] := FClientId;//compatibility
    end;
    requestJson['data/channelID'] := AChannelID;
    requestJson['data/groupID'] := AGroupID;
    requestJson['data/group_id'] := AGroupID;
    requestJson['data/groupName'] := AGroupName;
    requestJson['data/group_name'] := AGroupName;
    requestJson['data/userID'] := AUserID;
    requestJson['data/userName'] := (AUserName);
    requestJson['data/fullName'] := AFullName;
    requestJson['data/text'] := (AText);
    requestJson['data/reply'] := (AReply);
    requestJson['data/isGroup'] := is_group;
    requestJson['data/isMentioned'] := is_mentioned;
    requestJson['data/messageID'] := AMessageID.ToString;
    requestJson['data/resultMessageID'] := AResultMessageID.ToString;
    requestJson['data/replyFromMessageID'] := AReplyFromMessageId.ToString;

    //requestJson['data/context'] := SimpleBOT.SimpleAI.IntentName;
    requestJson['data/context'] := ActiveContext;

    requestJson['data/intentName'] := SimpleBOT.SimpleAI.IntentName;
    requestJson['data/intents/name'] := SimpleBOT.SimpleAI.IntentName;
    requestJson['data/dashboard_device_id'] := DashboardDeviceID;
    if MessageType.IsNotEmpty then
      requestJson['data/message_type'] := MessageType;
    if IsDelayReplay then requestJson['data/delay_reply'] := 1;

    requestJson.Clear; //TODO: delete the requestJson definition above
    if not FClientId.IsEmpty then
    begin
      requestJson['client_id'] := FClientId;
    end;
    requestJson['message/message_id'] := AMessageID.ToString;
    requestJson['message/text'] := (AText);
    requestJson['message/reply'] := (AReply);
    requestJson['message/from/id'] := AUserID;
    requestJson['message/from/name'] := AFullName;
    requestJson['message/from/username'] := AUserName;
    requestJson['message/chat/channel'] := AChannelID;
    requestJson['message/chat/id'] := AMessageID.ToString;
    requestJson['message/chat/is_mentioned'] := is_mentioned;
    requestJson['message/chat/is_group'] := is_group;
    requestJson['message/intents/name'] := SimpleBOT.SimpleAI.IntentName;
    if AReplyFromMessageId > 0 then requestJson['message/chat/is_reply'] := 0;
    if AIsGroup then
    begin
      requestJson['message/chat/group_name'] := AGroupName;
      requestJson['message/chat/group_id'] := AGroupID;
    end;

    if TopicID > 0 then
    begin
      requestJson['message/chat/topic_name'] := TopicName;
      requestJson['message/chat/topic_id'] := TopicID;
    end;

    if MessageType.IsEqualTo('image') then
    begin
      requestJson.ValueArray['data/files'] := FileList;
    end;

    responseJson.LoadFromJsonString(LogChatPayload.Text, False);
    requestJson.Data.Add('response', responseJson.Data);
    requestJson.Data.Add('nlp_response', responseJson.Data);

    ContentType := 'application/json';
    RequestBody := TStringStream.Create(requestJson.AsJSON);
    httpResponse := Post;
    if _GET['_DEBUG'] <> '1' then
    begin
      //LogUtil.Add(httpResponse.ResultText, 'logchat');
    end;
    Free;
  end;
  try
    responseJson.Free;
    //requestJson.Free;
  except
    on e : Exception do
    begin
    end;
  end;
end;

procedure TCarikWebModule.LogJoin(AChannelID: string; AGroupID: string;
  AGroupName: string; AUserID: string; AUserName: string; AFullName: string;
  AInvitedBy: string; ARestrict: boolean; AUserLeft: boolean);
var
  log_url: string;
  http_response: IHTTPResponse;
  requestJson: TJSONUtil;
begin
  if AChannelID <> TELEGRAM_CHANNEL_ID then // Telegram only
    Exit;
  try
    log_url := '';
    log_url := Config[JOINLOG_URL]  + '?channel=telegram';
  except
    exit;
  end;
  if log_url = '' then
    Exit;

  with THTTPLib.Create do
  begin
    URL := log_url;
    AddHeader('Cache-Control', 'no-cache');
    AddHeader('X-Client-Type', 'beta'); //TODO: client type
    {
    FormData['ChannelID'] := AChannelID;
    FormData['GroupID'] := AGroupID;
    FormData['GroupName'] := AGroupName;
    FormData['UserID'] := AUserID;
    FormData['UserName'] := AUserName;
    FormData['FullName'] := AFullName;
    FormData['MessageID'] := MessageID;
    FormData['InvitedBy'] := AInvitedBy;
    if ARestrict then
      FormData['Restrict'] := 'yes';
    if AUserLeft then
      FormData['LeftFromGroup'] := 'yes';
    }
    requestJson := TJSONUtil.Create;
    requestJson['data/ChannelID'] := AChannelID;
    requestJson['data/GroupID'] := AGroupID;
    requestJson['data/GroupName'] := AGroupName;
    requestJson['data/UserID'] := AUserID;
    requestJson['data/UserName'] := AUserName;
    requestJson['data/FullName'] := AFullName;
    requestJson['data/MessageID'] := MessageID;
    requestJson['data/InvitedBy'] := AInvitedBy;
    if ARestrict then
      requestJson['data/Restrict'] := 'yes';
    if AUserLeft then
      requestJson['data/LeftFromGroup'] := 'yes';

    ContentType := 'application/json';
    RequestBody := TStringStream.Create(requestJson.AsJSON);
    try
      http_response := Post;
      LogUtil.Add( 'response-join: ' + http_response.ResultText, 'JOIN');
    except
    end;
    requestJson.Free;
    //die //ulil
    Free;
  end;
end;

procedure TCarikWebModule.LogGroupAdd(AChannelID: string; AGroupID: string;
  AGroupName: string; AInvitedByID: string; AInvitedByUserName: string;
  AInvitedByName: string);
var
  log_url: string;
begin
  if AChannelID <> TELEGRAM_CHANNEL_ID then // Telegram only
    Exit;
  try
    log_url := '';
    log_url := Config[GROUPADDLOG_URL] + '?channel=telegram';
  except
    exit;
  end;
  if log_url = '' then
    Exit;

  with THTTPLib.Create do
  begin
    URL := log_url;
    AddHeader('Cache-Control', 'no-cache');
    AddHeader('X-Client-Type', 'beta'); //TODO: client type
    FormData['ChannelID'] := AChannelID;
    FormData['GroupID'] := AGroupID;
    FormData['GroupName'] := AGroupName;
    FormData['InvitedByID'] := AInvitedByID;
    FormData['InvitedByUserName'] := AInvitedByUserName;
    FormData['InvitedByName'] := AInvitedByName;
    Post;
    Free;
  end;
end;

procedure TCarikWebModule.SaveUnknownChat(AText: string);
var
  log_url: string;
  httpResponse: IHTTPResponse;
  requestJson: TJSONUtil;
begin
  if AText = '' then
    Exit;
  try
    log_url := '';
    log_url := Config[UNKNOWN_STATS_URL];
  except
  end;
  if log_url = '' then
    Exit;

  requestJson := TJSONUtil.Create;
  with THTTPLib.Create do
  begin
    URL := log_url;
    AddHeader('Cache-Control', 'no-cache');
    AddHeader('X-Client-Type', 'beta'); //TODO: client type
    //FormData['text'] := AText;
    //FormData['client_id'] := ClientId;
    requestJson['data/text'] := AText;
    requestJson['data/client_id'] := ClientId;
    ContentType := 'application/json';
    RequestBody := TStringStream.Create(requestJson.AsJSON);
    httpResponse := Post;
    Free;
  end;
  requestJson.Free;

end;

constructor TCarikWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
var
  s: string;
begin
  inherited CreateNew(AOwner, CreateMode);
  FIsDebug := False;
  if (_GET['_DEBUG'] = '1') then FIsDebug:= True;
  FGenericContent := True;
  FDashboardDeviceID := 0;
  FOperation := _GET['op'];
  if FOperation.IsEmpty then FOperation := _POST['op'];
  FBotID := _GET['botid'];
  FBotName := _GET['name'];
  FToken := _GET['token'];
  FClientId := _GET['clientId'];
  FDeviceId:= _GET['DeviceID'];
  if FBotID.IsEmpty then   FBotID := _GET['bot_id'];
  if FClientId.IsEmpty then FClientId := _GET['client_id'];;
  if FDeviceId.IsEmpty then FDeviceId:= _GET['device_id'];;

  if FDeviceId.IsEmpty then
    FDeviceId := Header['DeviceID'];
  if not FBotID.IsEmpty then
  begin
    s := 'config/config-'+FBotID+'.json';
    if FileExists(s) then
      Config.Filename := s;
  end;
  if FClientId.IsEmpty then
    FClientId := Config[CONFIG_CLIENT_ID];
  if FClientId.IsEmpty then FClientId := _POST['clientId'];
  if FClientId.IsNotEmpty then
    LogUtil.Prefix := FClientId + '-';

  if FBotName.IsEmpty then
    FBotName := Config[CONFIG_BOTNAME];
  try
    s := Config[CONFIG_REPLY_DISABLE];
    FReplyDisable := False;
    FReplyDisable := s2b(s);
  except
  end;
  s := _GET['reply'];
  if s.IsNotEmpty then
  begin
    FReplyDisable := not s2b(s);
  end;
  s := _GET['generic'].Trim;
  if s.IsNotEmpty then
    FGenericContent := s2b(s);

  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.FirstSessionResponse := False;
  if not FBotID.IsEmpty then SimpleBOT.AdditionalParameters.Values['bot_id'] := FBotID;

  //TODO: if FOperation.IsEmpty then
    SimpleBOT.LoadConfig;
  SimpleBOT.BotName := FBotName;
  SimpleBOT.StorageType := stFile;
  SimpleBOT.StorageFileName := 'files/carik/carik-userdata.dat';
  s := Config[_NLP_CONFIG_USERDATA_STORAGE];
  if s = 'redis' then
  begin
    SimpleBOT.StorageType := stRedis;
  end;
  Carik := TCarikController.Create;
  FLanguage := 'en-id';
  FSendAudio := False;
  FSendPhoto := False;
  FSendVenue := False;
  FKickUser  := False;
  FRestrictUser := False;
  FSendRichContent := False;
  FSendQuickReplayLocation := False;
  FCanSendTemplateCard := False;
  FCustomActionAsText := '';
  FCustomActionSuffix := '';
  FIsTranslate := False;
  FLanguage := '';
  FFileURL := '';
  FCaption := '';
  FButtonCaption := 'More';
  FTriggeredText := '';
  FErrorCount := 0;
  FMessengerMode := mmNone;
  ChannelId := '';
  SessionPrefix := '';
  FFormatNumber := '%5.3N';

  FBOLD_CODE := '*';
  FITALIC_CODE := '_';
  FActionCallback := '';
  FIterationParams := '';

  ToggleSpammer := False;
  Prefix := '';
  Suffix := '';
  AutoDeleteMessage := 0; // disable
  ElementArray := nil;

  FFormInputExpired := False;
  FCurrentInputType := '';
  FCustomReplyName := '';
  FCustomReplyTypeFromExternalNLP := '';
  FCustomReplyURLFromExternalNLP := '';
  FCustomReplyActionTypeFromExternalNLP := '';
  FExternalNLPStarted := False;
  FGPTTimeout := 0;
  FPackageName := '';
  LogChatPayload := TStringList.Create;
  isReplyMessage := False;
  ProcessingTime := 0;
  TopicID := 0;
  TopicName := '';
  FHideTextReply := False;
  FDelayReplay := False;
  FToken_I := 0;
  FToken_O := 0;
  FToken_T := 0;
end;

destructor TCarikWebModule.Destroy;
begin
  LogChatPayload.Free;
  if Assigned(FCustomActionFiles) then FCustomActionFiles.Free;
  if Assigned(FInputOptions) then FInputOptions.Free;
  if Assigned(ElementArray) then
    ElementArray.Free;
  if Assigned(FIterationParamsPast) then
    FIterationParamsPast.Free;
  if Assigned(FCustomReplyDataFromExternalNLP) then
    FCustomReplyDataFromExternalNLP.Free;
  Carik.Free;
  SimpleBOT.Free;
  inherited Destroy;
end;

function TCarikWebModule.ObjectFocus: string; // deprecated
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

function TCarikWebModule.ContextFocus: string;
var
  s: string;
begin
  Result := '';
  s := SimpleBOT.UserData['CONTEXT_DATE'];
  if s = '' then
    Exit;
  if MinutesBetween(Now, StrToDateTime(s)) <= _OBJECT_DISCUSSION_MAXTIME then
    Result := SimpleBOT.UserData['CONTEXT'];
end;

function TCarikWebModule.LanguageSetHandler(const IntentName: string;
  Params: TStrings): string;
begin
  SimpleBOT.UserData['language'] := Params.Values['Language'];
  if ((SimpleBOT.UserData['language'] = '') or
    (SimpleBOT.UserData['language'] = 'id')) then
  begin
    FIsTranslate := False;
    Result := SimpleBOT.GetResponse('GreetingSemangat');
  end
  else
  begin
    FIsTranslate := True;
    FLanguage := Params.Values['Language'];
    SimpleBOT.UserData['language'] := FLanguage;
    SimpleBOT.UserData['language_datetime'] := FormatDateTime('yyyy-mm-dd HH:nn:ss', Now);
    Result := SimpleBOT.GetResponse(IntentName + 'Response');
    Result := StringReplace(Result, '%Language_value%',
      Params.Values['Language_value'], [rfReplaceAll]);
  end;
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
var
  s, externalResult: string;
  json: TJSONUtil;
begin
  SimpleBOT.ConnectTimeout := EXTERNAL_ACCESS_TIMEOUT;
  SimpleBOT.TimeOutMessage := ERR_TIMEOUT_MESSAGE;
  try
    SimpleBOT.IsStemming := Config[STEMMING_ENABLED];
    SimpleBOT.StandardWordCheck := Config[STANDARDWORD_CHECKING];
  except
  end;

  if not Carik.UserID.IsEmpty then
  begin
    SimpleBOT.SimpleAI.AdditionalParameters.Values['UserID'] := PrefixId + '-' + Carik.UserID;
    SimpleBOT.SimpleAI.AdditionalParameters.Values['user_id'] := PrefixId + '-' + Carik.UserID;
  end;
  SimpleBOT.SimpleAI.AdditionalParameters.Values['OriginalText'] := OriginalText.Trim;
  SimpleBOT.SimpleAI.AdditionalParameters.Values['original_text'] := OriginalText.Trim;
  SimpleBOT.SimpleAI.AdditionalParameters.Values['message'] := AMessage;
  SimpleBOT.SimpleAI.AdditionalParameters.Values['text'] := AMessage;
  if not ChannelId.IsEmpty then
  begin
    SimpleBOT.SimpleAI.AdditionalParameters.Values['ChannelId'] := ChannelId;
    SimpleBOT.SimpleAI.AdditionalParameters.Values['channel_id'] := ChannelId;
  end;
  if ClientId.IsNotEmpty then
  begin
    SimpleBOT.SimpleAI.AdditionalParameters.Values['ClientId'] := ClientId;;
    SimpleBOT.SimpleAI.AdditionalParameters.Values['client_id'] := ClientId;
    if DeviceId.IsNotEmpty then SimpleBOT.SimpleAI.AdditionalParameters.Values['dashboard_device_id'] := DeviceId;
    if IsDelayReplay then SimpleBOT.SimpleAI.AdditionalParameters.Values['delay_reply'] := '1';
  end;
  if Carik.IsGroup then
    SimpleBOT.SimpleAI.AdditionalParameters.Values['GroupID_'] := Carik.GroupChatID;
  if Carik.IsInvitation then
  begin
    SimpleBOT.SimpleAI.AdditionalParameters.Values['UserID'] := PrefixId + '-' + Carik.InvitedUserId;
    SimpleBOT.SimpleAI.AdditionalParameters.Values['user_id'] := PrefixId + '-' + Carik.InvitedUserId;
    SimpleBOT.SimpleAI.AdditionalParameters.Values['UserID_'] := Carik.InvitedUserId;
  end
  else
  begin
    SimpleBOT.SimpleAI.AdditionalParameters.Values['UserID_'] := Carik.UserID;
  end;
  if not FGenericContent then
    SimpleBOT.SimpleAI.AdditionalParameters.Values['generic'] := 'false';
  if DeviceId.IsNotEmpty then
    SimpleBOT.SimpleAI.AdditionalHeaders.Values['DeviceID'] := DeviceId;
  //SimpleBOT.DefaultSearchPattern := '\w+';
  Result := SimpleBOT.Exec(AMessage);
  if SimpleBOT.Weight > 0 then
  begin
    externalResult := ExternalNLP(AMessage);
    if externalResult.IsNotEmpty then
      if FExternalNLPWeight < SimpleBOT.Weight then
      begin
        if SimpleBOT.IsMerge then
        begin
          externalResult += '\n\n' + SimpleBOT.ResponseText.Text;
        end;
        Result := externalResult;
        SimpleBOT.ResponseText.Text := externalResult;
        SimpleBOT.SimpleAI.Parameters.Values['external_nlp'] := '1';
      end;
  end;
  if (SimpleBOT.ResponseText.Text = '') and not (FSendAudio or
    FSendPhoto or FSendRichContent or FSendVenue) then
  begin
    if (not FSendVenue) and (not FSendRichContent) then
    begin
      //ulil - no responses
      //SimpleBOT.ResponseText.Text := SimpleBOT.GetResponse('DataTidakAdaResponse');
      //Result := SimpleBOT.SimpleAI.ResponseJson;
    end;
  end;

  if not SimpleBOT.SimpleAI.ImageURL.IsEmpty then
  begin
    FSendPhoto := True;
    FFileURL := SimpleBOT.SimpleAI.ImageURL;
    FImageCaption := SimpleBOT.SimpleAI.ImageCaption;
    FImageCaption := FImageCaption.Replace('\n', #10);
    FCaption := FImageCaption;
  end;

  if getIsTranslate then
  begin
    s := SimpleBOT.SimpleAI.ResponseText[0];
    SimpleBOT.SimpleAI.ResponseText[0] := translate(s, 'id', FLanguage, True);
    LogUtil.Add(s + '|' + SimpleBOT.SimpleAI.ResponseText[0], 'RESP');

    //TODO: lali
    json := TJSONUtil.Create;
    json.LoadFromJsonString(SimpleBOT.SimpleAI.ResponseJson, False);
    Result := json.AsJSONFormated;
    json.Free;
  end;

  if SimpleBOT.SimpleAI.IntentName.IsNotEmpty then
    SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT] := '0';
end;

procedure TCarikWebModule.BotInit;
begin
  SimpleBOT.OnError := @OnNLPErrorHandler;  // Your Custom Message
  SimpleBOT.TrimMessage := False;

  SimpleBOT.Handler['oneword'] := @onewordHandler;
  SimpleBOT.Handler['define'] := @defineHandler;
  SimpleBOT.Handler['definisi'] := @definisiHandler;
  SimpleBOT.Handler['iteration_next'] := @iterationNextHandler;
  SimpleBOT.Handler['user_profile'] := @userProfileHandler;

  SimpleBOT.Handler['bot_start'] := @botStartHandler;
  SimpleBOT.Handler['resi_paket'] := @resiHandler;
  SimpleBOT.Handler['voucher_konvensional'] := @voucherConvensionalHandler;
  SimpleBOT.Handler['voucher'] := @voucherHandler;
  SimpleBOT.Handler['movie_play'] := @moviePlayHandler;
  SimpleBOT.Handler['movie_info'] := @movieInfoHandler;
  SimpleBOT.Handler['jarak'] := @distanceHandler;
  SimpleBOT.Handler['distance_fromto'] := @distanceFromToHandler;
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
  SimpleBOT.Handler['language_set'] := @LanguageSetHandler;

  SimpleBOT.Handler['pajak_kendaraan'] := @pajakKendaraanHandler;
  SimpleBOT.Handler['alquran_terjemahan'] := @alquranTerjemahanHandler;
  SimpleBOT.Handler['calendar_eventlist'] := @kloudlessCalendarEventListHandler;
  SimpleBOT.Handler['calendar_eventcreate'] := @kloudlessCalendarEventCreateHandler;

  SimpleBOT.Handler['mortgage_calculator'] := @mortgageCalculatorHandler;
  SimpleBOT.Handler['bmkg_simpleinfo'] := @bmkgSimpleInfoHandler;
  SimpleBOT.Handler['openweather_info'] := @openweatherInfoHandler;
  //SimpleBOT.Handler['openweather_info'] := @apixuweatherInfoHandler;
  SimpleBOT.Handler['berita_hariini'] := @beritaHariIniHandler;

  SimpleBOT.Handler['conversion_hash'] := @conversionHashHandler;
  SimpleBOT.Handler['custom_math'] := @customMathHandler;

  if FMessengerMode = mmTelegram then
  begin
    //SimpleBOT.Handler['carik_start'] := @Carik.StartHandler;
    //SimpleBOT.Handler['carik_stop'] := @Carik.StopHandler;
    SimpleBOT.Handler['carik_check'] := @Carik.CheckHandler;
    //SimpleBOT.Handler['carik_topic'] := @Carik.TopicHandler;
    SimpleBOT.Handler['carik_send'] := @Carik.SendHandler;
    SimpleBOT.Handler['carik_admin_tambah'] := @carikAdminTambahHandler;
    SimpleBOT.Handler['carik_admin_hapus'] := @carikAdminHapusHandler;
    SimpleBOT.Handler['carik_group_info'] := @Carik.GroupInfoHandler;
    SimpleBOT.Handler['group_memberbaru'] := @carikMemberBaruHandler;
    SimpleBOT.Handler['group_memberbaru_abaikan'] := @carikMemberBaruAbaikanHandler;
    SimpleBOT.Handler['group_memberbaru_sapa'] := @carikMemberBaruSapaHandler;
    SimpleBOT.Handler['group_memberbaru_custommessage'] :=
      @carikNewMemberCustomMessageHandler;

    SimpleBOT.Handler['spam_report'] := @spamReportHandler;

    //todo: line: share location
  end;

  SimpleBOT.Handler['echo'] := @echoHandler;
  SimpleBOT.Handler['text_to_speech'] := @texttospeechHandler;
  SimpleBOT.Handler['speaking_mode_on'] := @speakingModeOnHandler;
  SimpleBOT.Handler['speaking_mode_off'] := @speakingModeOffHandler;

  SimpleBOT.Handler['bca_test'] := @bcaTestHandler;

  // Property Search
  SimpleBOT.Handler['property_search'] := @propertySearchHandler;

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
  Result := preg_replace('\*(.*?)\*', '$1', Result);
end;

function TCarikWebModule.TrimLineMessage(const AMessage: string): string;
begin
  Result := RemoveMarkDown(AMessage);
  Result := preg_replace('\*(.*?)\*', '$1', Result);
  Result := Result.Replace('\n', #10);
end;

function TCarikWebModule.FindSuggestion(AText: string; AReservedWord: string
  ): string;
var
  s, url: string;
  json: TJSONUtil;

begin
  Result := '';
  s := '';
  url := Config[SUGGESTION_URL];
  if url.IsEmpty then
    Exit;
  url := url + UrlEncode(AText) + '&reserved=' + AReservedWord;
  //s := file_get_contents(url, False);

  With TFPHTTPClient.Create(Nil) do
  begin
    try
      AllowRedirect := True;
      //OnRedirect := @ShowRedirect;
      //OnDataReceived := @DoProgress;
      //OnHeaders := @DoHeaders;
      //OnGetSocketHandler := @HttpClientGetSocketHandler;
      s := Get(url);
    except
      on E: Exception do
      begin
      end;
    end;
    Free;
  end;

  json := TJSONUtil.Create;
  json.LoadFromJsonString(s, False);
  Result := json['result/text'];

  json.Free;
end;

procedure TCarikWebModule.SaveActionToUserDataFromCard(AData: TJSONObject);
var
  i, j, indexAction: integer;
  s, s2, actionData, buttonTitle, captionDisplay, actionURL, quoteMark : string;
  buttonAsArray: TJSONArray;
  lst: TStrings;
begin
  quoteMark := '*';
  if MessengerMode = mmInstagram then
    quoteMark := '';
  FCustomActionAsText := '';
  //if not (CustomReplyType = 'card') then Exit;

  SimpleBOT.UserData[MESSAGE_TYPE] := 'action';
  SimpleBOT.UserData[MESSAGE_ACTION_DATE] := Now.AsString;
  SimpleBOT.UserData[MESSAGE_ACTION_TYPE] := 'card';

  buttonAsArray := TJSONArray(AData);
  indexAction := 0;
  buttonTitle := SimpleBOT.SimpleAI.CustomReply['action/button_title'];
  if buttonTitle.IsEmpty then
    buttonTitle := 'Detail';
  for i := 0 to buttonAsArray.Count-1 do
  begin
    actionData := jsonGetData(buttonAsArray.Items[i], 'title') + '|';
    actionURL := jsonGetData(buttonAsArray.Items[i], 'url');
    captionDisplay := '[Detail]('+actionURL+')';
    s := jsonGetData(buttonAsArray.Items[i], 'callback_data');
    if s.IsEmpty then
    begin
      s := 'url=' + UrlEncode(actionURL) + '&text=Kunjungi ' + actionURL;
    end
    else
    begin
      captionDisplay := 'Ketik angka "'+i2s(i+1)+'"';
    end;
    actionData := actionData + s;
    SimpleBOT.UserData[MESSAGE_ACTION_DATA_+indexAction.ToString] := actionData;

    indexAction := indexAction + 1;
    FCustomActionAsText := FCustomActionAsText
      + #10 + quoteMark + jsonGetData(buttonAsArray.Items[i], 'title') + quoteMark;
    s := jsonGetData(buttonAsArray.Items[i], 'sub_title');
    if not s.IsEmpty then
      FCustomActionAsText := FCustomActionAsText + #10 + s;
    FCustomActionAsText := FCustomActionAsText
      + #10 + captionDisplay
      + #10;
  end;

  SimpleBOT.UserData[MESSAGE_ACTION_COUNT] := indexAction.ToString;
  FCustomActionAsText := FCustomActionAsText.Trim;
end;

procedure TCarikWebModule.SaveActionToUserDataFromForm(AData: TJSONObject);

var
  i, questionCount: integer;
  fileName: string;
  formAsArray: TJSONArray;
  tmpSuffix, formUrl: string;
begin
  FCustomActionAsText := '';
  //if not (CustomReplyType = 'form') then Exit;

  SimpleBOT.UserData[MESSAGE_TYPE] := 'action';
  SimpleBOT.UserData[MESSAGE_ACTION_DATE] := Now.AsString;
  SimpleBOT.UserData[MESSAGE_ACTION_TYPE] := 'form';
  SimpleBOT.UserData[MESSAGE_ACTION_URL] := CustomReplyURL;
  if FCustomReplyName.IsNotEmpty then
    SimpleBOT.UserData[MESSAGE_ACTION_NAME] := FCustomReplyName;
  formAsArray := TJSONArray(AData);

  fileName := getPrefixID() + '-' + Carik.UserID + '-' + SimpleBOT.SimpleAI.IntentName + '.json';
  SimpleBOT.UserData[FORM_INPUT_FILENAME] := fileName;
  SimpleBOT.UserData[FORM_SESSION] := SHA1Print(SHA1String(Carik.UserID+Now.AsString));
  SimpleBOT.UserData[FORM_TOPIC] := '1';
  SimpleBOT.UserData[FORM_QUESTION] := '1';
  fileName := AppData.tempDir + FORM_PATH + fileName;
  if not DirectoryExists(AppData.tempDir + FORM_PATH) then
    ForceDirectories(AppData.tempDir + FORM_PATH);
  string(formAsArray.FormatJSON()).SaveToFile(fileName);

  questionCount := 0;
  for i:=0 to formAsArray.Count-1 do
  begin
    questionCount += formAsArray.Items[i].Count;
  end;
  SimpleBOT.UserData[FORM_QUESTION_TOTAL] := questionCount.ToString;

  tmpSuffix := '';
  if questionCount > 1 then
  begin
    tmpSuffix := '\nTerdapat ';
    if formAsArray.Count > 1 then
      tmpSuffix += formAsArray.Count.ToString + ' kategori, dan ';
    tmpSuffix += 'total ' + questionCount.ToString + ' pertanyaan.';
  end;
  tmpSuffix += FORM_INPUT_HASHTAG_CANCEL.Replace('%botname%', SimpleBOT.BotName);
  if FCustomActionSuffix.IsNotEmpty then tmpSuffix := FCustomActionSuffix;
  if SimpleBOT.SimpleAI.ReplySuffix.IsNotEmpty then tmpSuffix := SimpleBOT.SimpleAI.ReplySuffix;

  tmpSuffix += '\n\n' + GetFormQuestion(1);

  Suffix := tmpSuffix;
  if FMessengerMode = mmLine then Suffix := Suffix.Replace('\n', #10);

  SimpleBOT.UserData[WAITING_INPUT] := '1';
  SimpleBOT.UserData[WAITING_INPUT_DATE] := Now.AsString;
end;

procedure TCarikWebModule.SaveActionToUserData(AActionType: string;
  AData: TJSONObject);
var
  i, j, indexAction: integer;
  s, actionData, firstMenuTitle, previousAction, currentAction, title: string;
  buttonAsArray: TJSONArray;
begin
  FCustomActionAsText := '';

  if AActionType='card' then
  begin
    if ((FMessengerMode = mmFacebook)or(FMessengerMode = mmFacebook)) then
      Exit;
    SaveActionToUserDataFromCard(AData);
    Exit;
  end;

  if AActionType='form' then
  begin
    SaveActionToUserDataFromForm(AData);
    Exit;
  end;

  if AActionType='list' then
  begin
    SimpleBOT.UserData[MESSAGE_ACTION_COUNT] := '0';
  end;

  if not ((AActionType='button')
    or (AActionType='quickreply')
    or (AActionType='menu')
    or (AActionType='list')
    ) then
    Exit;

  previousAction := SimpleBOT.UserData[CURRENT_ACTION];
  if (previousAction <> Text) then
  begin
    SimpleBOT.UserData[PREVIOUS_ACTION] := previousAction;
    SimpleBOT.UserData[CURRENT_ACTION] := Text;
  end;

  SimpleBOT.UserData[MESSAGE_TYPE] := 'action';
  SimpleBOT.UserData[MESSAGE_ACTION_DATE] := Now.AsString;
  SimpleBOT.UserData[MESSAGE_ACTION_TYPE] := AActionType;
  SimpleBOT.UserData[MESSAGE_ACTION_LEVEL] := CustomReplyMenuLevel;
  if CustomReplyIsMainMenu then
    SimpleBOT.UserData[MESSAGE_ACTION_IS_MAIN] := '1'
  else
    SimpleBOT.UserData[MESSAGE_ACTION_IS_MAIN] := '0';
  if FCustomReplyName.IsNotEmpty then
    SimpleBOT.UserData[MESSAGE_ACTION_NAME] := FCustomReplyName;

  buttonAsArray := TJSONArray(AData);
  indexAction := 0;
  firstMenuTitle := '';
  for i := 0 to buttonAsArray.Count-1 do
  begin
    for j := 0 to buttonAsArray.Items[i].Count-1 do
    begin
      try
        s := buttonAsArray.Items[i].Items[j].GetPath('callback_data').AsString;
        actionData := buttonAsArray.Items[i].Items[j].GetPath('text').AsString
          + '|' + s;
      except
        actionData := '';
        try
          s := buttonAsArray.Items[i].Items[j].GetPath('url').AsString;
          actionData := buttonAsArray.Items[i].Items[j].GetPath('text').AsString
            + '|url=' + s;
        except
        end;
      end;
      if actionData.IsEmpty then
      begin
        s := jsonGetData( buttonAsArray.Items[i].Items[j], 'request_location');
        if s = 'True' then
        begin
          actionData := jsonGetData( buttonAsArray.Items[i].Items[j], 'text')
            + '|location=true';
        end;
      end;
      SimpleBOT.UserData[MESSAGE_ACTION_DATA_+indexAction.ToString] := actionData;

      indexAction := indexAction + 1;
      title := buttonAsArray.Items[i].Items[j].GetPath('text').AsString;
      FCustomActionAsText := FCustomActionAsText + #10 + '*' + indexAction.ToString + '*'
       + '. ' + title;
      if firstMenuTitle.IsEmpty then
        firstMenuTitle := buttonAsArray.Items[i].Items[j].GetPath('text').AsString;
    end;

  end;

  SimpleBOT.UserData[MESSAGE_ACTION_COUNT] := indexAction.ToString;
  FCustomActionAsText := FCustomActionAsText.Trim;
  firstMenuTitle:= firstMenuTitle.RemoveEmoji().RemoveUnicode().RemoveMarkDown();
  if FGenericContent then
  begin
    FCustomActionSuffix := ACTION_SUFFIX.Replace('%s', firstMenuTitle.Replace(#13,'').Replace(#10,''));
  end
  else
  begin
    FCustomActionAsText := '';
  end;
end;

function TCarikWebModule.FormInputHandler: boolean;
  procedure resetForm;
  begin
    SimpleBOT.UserData[WAITING_INPUT] := '0';
    SimpleBOT.UserData[FORM_QUESTION] := '0';
    SimpleBOT.UserData[FORM_QUESTION_TOTAL] := '0';
    SimpleBOT.UserData[MESSAGE_ACTION_NAME] := '';
  end;
  function submitFormCancel: string;
  var
    postData: TJSONUtil;
    httpResponse: IHTTPResponse;
  begin
    Result := '';
    postData := TJSONUtil.Create;
    postData['post_date'] := Now.AsString;
    postData['user_id'] := PrefixId + '-' + Carik.UserID;
    postData['full_name'] := Carik.FullName;
    postData['client_id'] := ClientId;
    postData['session'] := SimpleBOT.UserData[FORM_SESSION];
    postData['data/submit'] := CANCEL;
    with THTTPLib.Create(SimpleBOT.UserData[MESSAGE_ACTION_URL]) do
    begin
      try
        AddHeader('_source', 'carik');
        AddHeader('Cache-Control', 'no-cache');
        ContentType := 'application/json';
        RequestBody := TStringStream.Create(postData.AsJSON);
        httpResponse := Post();
        if httpResponse.ResultCode = 200 then
        begin
          FCustomReplyDataFromExternalNLP := TJSONUtil.Create;
          FCustomReplyDataFromExternalNLP.LoadFromJsonString(httpResponse.ResultText, False);
          Suffix := FCustomReplyDataFromExternalNLP['text'];
          Result := Suffix;

          //TODO: build custom action
          FCustomReplyTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['type'];
          FCustomReplyActionTypeFromExternalNLP := 'text';
          if FCustomReplyTypeFromExternalNLP.IsNotEmpty then
          begin
            FCustomReplyActionTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['action/type'];
            FCustomReplyURLFromExternalNLP := FCustomReplyDataFromExternalNLP['action/url'];
            FCustomReplyName := FCustomReplyDataFromExternalNLP['action/name'];
            SaveActionToUserData(FCustomReplyActionTypeFromExternalNLP, TJSONObject(FCustomReplyDataFromExternalNLP.Data.GetPath('action.data')));
            FCustomReplyDataFromExternalNLP.LoadFromJsonString(FCustomReplyDataFromExternalNLP.Data.GetPath('action.data').AsJSON, False);
            if FCustomActionAsText.IsNotEmpty then
            begin
              {
              Suffix += '\n'+ACTION_CAPTION+'\n'+FCustomActionAsText.Replace(#10, '\n');
              s := FCustomReplyDataFromExternalNLP.Data.Items[0].Items[0].GetPath('text').AsString;
              s := ACTION_SUFFIX.Replace('%s', s);
              Suffix += '\n' + s;
              }
            end;
          end;

        end;
      except
      end;
      Free;
    end;

    postData.Free;
  end;

  function submitForm:boolean;
  var
    formSession: string;
  begin
    formSession := SimpleBOT.UserData[FORM_SESSION];


  end;

var
  i, j, questionIndex, questionTotal: integer;
  valueMax, valueMin, lengthMax, lengthMin: integer;
  s, inputType, inputName, url, answerText, validationURL, cancelationKeyword: string;
  lst: TStrings;
  postData: TJSONUtil;
  httpResponse: IHTTPResponse;
begin
  Result := False;
  answerText := '';

  // check reset form
  if ((Text = FORM_INPUT_RESET_CODE)or
     (Text = 'batal')or
     (Text = 'cancel')or
     (Text = '"'+FORM_INPUT_RESET_CODE+'"')) then
  begin
    s := SimpleBOT.UserData[MESSAGE_ACTION_NAME];
    resetForm();
    Text := '';
    Suffix := submitFormCancel();
    if Suffix.IsEmpty then
      Suffix := Format(FORM_INPUT_CANCEL, [s]);
    Result := True;
    Exit;
  end;

  if not WaitingInput then Exit;

  questionIndex := s2i(SimpleBOT.UserData[FORM_QUESTION]);
  questionTotal := s2i(SimpleBOT.UserData[FORM_QUESTION_TOTAL]);

  // reload last question
  if ((Text = FORM_INPUT_RELOAD_CODE)or
    (Text = '"'+FORM_INPUT_RELOAD_CODE+'"')) then
  begin
    SimpleBOT.UserData[FORM_QUESTION] := questionIndex.ToString;
    SimpleBOT.UserData[WAITING_INPUT_DATE] := Now.AsString;
    Suffix := GetFormQuestion(questionIndex);
    Result := True;
    Exit;
  end;

  inputType := SimpleBOT.UserData[FORM_INPUT_TYPE];
  inputName := SimpleBOT.UserData[FORM_INPUT_NAME];
  valueMax := s2i(SimpleBOT.UserData[FORM_INPUT_VALUE_MAX]);
  valueMin := s2i(SimpleBOT.UserData[FORM_INPUT_VALUE_MIN]);
  lengthMax := s2i(SimpleBOT.UserData[FORM_INPUT_LENGTH_MAX]);
  lengthMin := s2i(SimpleBOT.UserData[FORM_INPUT_LENGTH_MIN]);
  validationURL := SimpleBOT.UserData[FORM_INPUT_VALIDATION_URL];
  cancelationKeyword := SimpleBOT.UserData[FORM_INPUT_CANCELATION_KEYWORD];

  // reset form with specific keyword
  if Text = cancelationKeyword then
  begin
    resetForm();
    Text := cancelationKeyword;
    Result := False;
    Exit;
  end;

  if inputType = 'string' then
  begin
    //validation
    if ((lengthMin > 0) and (Text.Length<lengthMin)) then
    begin
      Suffix := Format(FORM_ERR_LENGTH_MIN, [lengthMin]) + FORM_INPUT_HASHTAG_CANCEL3;
      if cancelationKeyword.IsNotEmpty then Suffix += Format(FORM_ERR_CANCELATION_KEYWORD, [cancelationKeyword]);
      Result := True;
      Exit;
    end;
    if ((lengthMax > 0) and (Text.Length>lengthMax)) then
    begin
      Suffix := Format(FORM_ERR_LENGTH_MAX, [lengthMax]) + FORM_INPUT_HASHTAG_CANCEL3;
      if cancelationKeyword.IsNotEmpty then Suffix += Format(FORM_ERR_CANCELATION_KEYWORD, [cancelationKeyword]);
      Result := True;
      Exit;
    end;
  end;

  // check date
  if inputType = 'date' then
  begin
    if not Text.isDate('/') then
    begin
      Suffix := FORM_ERR_FORMAT_DATE + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
      Result := True;
      Exit;
    end;
  end;
  // check email
  if inputType = 'email' then
  begin
    if not Text.IsEmail then
    begin
      Suffix := FORM_ERR_FORMAT_EMAIL + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
      Result := True;
      Exit;
    end;
  end;
  // check numeric
  if (inputType = 'numeric') or (inputType = 'number') then
  begin
    Text := Text.Replace('.','').ToLower;
    Text := Text.Replace('rp', '');
    Text := RemoveEmoji(Text);
    Text := RemoveUnicode(Text);
    Text := StripNonAscii(Text).Trim;
    Text := StringHumanToNominal(Text);
    if not Text.IsNumeric then
    begin
      Suffix := FORM_ERR_FORMAT_NUMERIC + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
      Result := True;
      Exit;
    end;

    // validation
    if ((valueMin > 0) and (Text.AsInteger<valueMin)) then
    begin
      Suffix:= Format(FORM_ERR_VALUE_MIN, [valueMin]) + FORM_INPUT_HASHTAG_CANCEL3;
      if cancelationKeyword.IsNotEmpty then Suffix += Format(FORM_ERR_CANCELATION_KEYWORD, [cancelationKeyword]);
      Result := True;
      Exit;
    end;
    if ((valueMax > 0) and (Text.AsInteger>valueMax)) then
    begin
      Suffix:= Format(FORM_ERR_VALUE_MAX, [valueMax]) + FORM_INPUT_HASHTAG_CANCEL3;
      if cancelationKeyword.IsNotEmpty then Suffix += Format(FORM_ERR_CANCELATION_KEYWORD, [cancelationKeyword]);
      Result := True;
      Exit;
    end;

  end;
  // check boolean
  if inputType = 'boolean' then
  begin
    Text := Text.ToUpper;
    if (Text in YES_COMMAND) then
      Text := 'Y'
    else if (Text in NO_COMMAND) then
      Text := 'N'
    else
    begin
      Suffix := FORM_ERR_FORMAT_BOOLEAN + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
      Result := True;
      Exit;
    end;
  end;
  // check option
  if ((inputType = 'option') or (inputType = 'list')) then
  begin
    Text := RemoveEmoji(Text);
    Text := RemoveUnicode(Text);
    Text := StripNonAscii(Text);
    Text := Text.Replace('??', '');
    Text := Text.Trim;
    Text := StringHumanToNominal(Text);
    if not Text.IsNumeric then
    begin
      Suffix := FORM_ERR_FORMAT_OPTION + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
      Result := True;
      Exit;
    end;
    i := s2i(SimpleBOT.UserData[FORM_INPUT_OPTION_COUNT]);
    if ((Text.ToInteger=0)or(Text.ToInteger > i)) then
    begin
      Suffix := FORM_ERR_FORMAT_OPTION_INVALID + ' ' + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
      Result := True;
      Exit;
    end;
    s := GetFormAnswerValue(questionIndex, Text.AsInteger);
    answerText := GetFormAnswerText(questionIndex, Text.AsInteger);
    if s.IsNotEmpty then
    begin
      Text := s;
    end;
  end;

  // validation
  if validationURL.IsNotEmpty then
  begin
    ;
  end;

  // save data
  if inputName.IsEmpty then inputName := 'a'+questionIndex.ToString;
  if answerText.IsNotEmpty then
    SimpleBOT.UserData[FORM_DATA+'t'+questionIndex.ToString] := answerText;
  SimpleBOT.UserData[FORM_DATA+questionIndex.ToString] := inputName+'|'+Text;

  // cek kirim result
  if questionIndex >= questionTotal then
  begin
    // generate json data
    s := SimpleBOT.UserData[FORM_SESSION];
    postData := TJSONUtil.Create;
    postData['post_date'] := Now.AsString;
    postData['user_id'] := PrefixId + '-' + Carik.UserID;
    postData['full_name'] := Carik.FullName;
    postData['client_id'] := ClientId;
    postData['session'] := s;
    for j:=0 to SimpleBOT.SimpleAI.Parameters.Count-1 do
    begin
      s := SimpleBOT.SimpleAI.Parameters.Names[j];
      postData[s] := SimpleBOT.SimpleAI.Parameters.ValueFromIndex[j];
    end;
    for j:=0 to SimpleBOT.AdditionalParameters.Count-1 do
    begin
      s := SimpleBOT.AdditionalParameters.Names[j];
      postData[s] := SimpleBOT.AdditionalParameters.ValueFromIndex[j];
    end;
    postData['data/user_id'] := PrefixId + '-' + Carik.UserID;
    postData['data/channel_id'] := ChannelId;
    postData['data/client_id'] := ClientId;
    postData['data/FullName'] := Carik.FullName;
    postData['data/full_name'] := Carik.FullName;
    for i:=1 to questionTotal do
    begin
      s := SimpleBOT.UserData[FORM_DATA+i.ToString];
      answerText := SimpleBOT.UserData[FORM_DATA+'t'+i.ToString];
      lst := Explode(s, '|');
      if answerText.IsNotEmpty then
        postData['data/'+lst[0]+'_t'] := answerText;
      //postData['data/'+lst[0]+'t'] := answerText; //TODO: remove for  compatibility
      postData['data/'+lst[0]] := lst[1];
      lst.Free;
    end;
    postData['data/submit'] := OK;
    if (_GET['_FORMDEBUG'] = '1') then
    begin
      url := SimpleBOT.UserData[MESSAGE_ACTION_URL];
      die(postData.AsJSONFormated+#13+url); //ulil formpost
    end;
    //die(postData.AsJSONFormated); //ulil formpost

    // Submit
    url := SimpleBOT.UserData[MESSAGE_ACTION_URL];
    LogUtil.Add(postData.AsJSON, 'FORM-DATA');
    LogUtil.Add(url, 'FORM-URL');
    with THTTPLib.Create(url) do
    begin
      try
        AddHeader('_source', 'carik');
        AddHeader('Cache-Control', 'no-cache');
        ContentType := 'application/json';
        ConnectTimeout := FORM_SUBMIT_TIMEOUT;
        RequestBody := TStringStream.Create(postData.AsJSON);
        httpResponse := Post();
        if (_GET['_FORMDEBUG'] = '1') then
        begin
          //die(httpResponse.ResultText);
        end;
        LogUtil.Add(httpResponse.ResultText, 'FORM-RESULT');
        if httpResponse.ResultCode <> 200 then
        begin
          Suffix := FORM_ERR_SUBMIT_FAILED + '(#'+httpResponse.ResultCode.ToString+')';
        end
        else
        begin
          FCustomReplyDataFromExternalNLP := TJSONUtil.Create;
          FCustomReplyDataFromExternalNLP.LoadFromJsonString(httpResponse.ResultText, False);
          Suffix := FCustomReplyDataFromExternalNLP['text'];

          //TODO: build custom action
          FCustomReplyTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['type'];
          FCustomReplyActionTypeFromExternalNLP := 'text';
          if FCustomReplyTypeFromExternalNLP.IsNotEmpty then
          begin
            FCustomReplyActionTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['action/type'];
            FCustomReplyURLFromExternalNLP := FCustomReplyDataFromExternalNLP['action/url'];
            FCustomReplyName := FCustomReplyDataFromExternalNLP['action/name'];
            try
              SaveActionToUserData(FCustomReplyActionTypeFromExternalNLP, TJSONObject(FCustomReplyDataFromExternalNLP.Data.GetPath('action.data')));
            except
            end;
            try
              FCustomReplyDataFromExternalNLP.LoadFromJsonString(FCustomReplyDataFromExternalNLP.Data.GetPath('action.data').AsJSON, False);
            except
            end;
            if FCustomActionAsText.IsNotEmpty then
            begin
              {
              Suffix += '\n'+ACTION_CAPTION+'\n'+FCustomActionAsText.Replace(#10, '\n');
              s := FCustomReplyDataFromExternalNLP.Data.Items[0].Items[0].GetPath('text').AsString;
              s := ACTION_SUFFIX.Replace('%s', s);
              Suffix += '\n' + s;
              }
            end;

            // files
            FCustomActionFiles := Nil;
            try
              LogUtil.Add('ada file', 'FORM');
              FCustomActionFiles := TJSONArray(FCustomReplyDataFromExternalNLP.Data.GetPath('files'));
            except
            end;

          end;

        end;
      except
        on e: Exception do
        begin
          Suffix := FORM_ERR_SUBMIT_EXCEPTION;
          LogUtil.Add(e.Message + '//' + httpResponse.ResultText, 'FORM-ERROR');
          if (_GET['_DEBUG'] = '1') then
            Suffix += '\n' + e.Message;
        end;
      end;

      Free;
    end;//THTTPLib

    resetForm();
    Result := True;
    Exit;
  end;

  // get new question
  questionIndex += 1;
  SimpleBOT.UserData[FORM_LAST_MESSAGE_ID] := MessageID;
  SimpleBOT.UserData[FORM_QUESTION] := questionIndex.ToString;
  SimpleBOT.UserData[WAITING_INPUT_DATE] := Now.AsString;
  Suffix := GetFormQuestion(questionIndex);
  Result := True;
end;

function TCarikWebModule.GetFormQuestion(AIndex: integer): string;
var
  i, topic, subCount, currentCount, index: integer;
  s, fileName, inputTitle, inputName, inputType, optionList, validationUrl,
    cancelationKeyword: string;
  valueMax, valueMin, lengthMax, lengthMin: integer;
  lst: TStringList;
  formAsArray: TJSONArray;
  formAsJson: TJSONData;
  oItem: TJSONObject;
begin
  Result := '';
  inputTitle := '';
  fileName := SimpleBOT.UserData[FORM_INPUT_FILENAME];
  fileName := AppData.tempDir + FORM_PATH + fileName;
  if not FileExists(fileName) then Exit;
  lst := TStringList.Create;
  lst.LoadFromFile(fileName);

  formAsArray := GetJSON(lst.Text,False) as TJSONArray;

  topic := 0;
  subCount := 0;
  index := 0;
  for i:=0 to formAsArray.Count-1 do
  begin
    currentCount := formAsArray.Items[i].Count;
    subCount += currentCount;
    if AIndex <= subCount then
    begin
      index := AIndex - (subCount-currentCount) - 1;
      break;
    end;
    topic := topic + 1;
  end;
  try
    inputTitle := formAsArray.Items[topic].Items[index].GetPath('title').AsString;
    FInputOptionTitle := inputTitle;
  except
    on E:Exception do
    begin
      die('err: ' + topic.ToString + '/' + index.ToString + #10 + formAsArray.AsJSON);
    end;
  end;
  inputName := formAsArray.Items[topic].Items[index].GetPath('name').AsString;
  inputType := formAsArray.Items[topic].Items[index].GetPath('type').AsString;

  lengthMax := jsonGetData(formAsArray.Items[topic].Items[index], 'length_max').AsInteger;
  lengthMin := jsonGetData(formAsArray.Items[topic].Items[index], 'length_min').AsInteger;
  valueMax := jsonGetData(formAsArray.Items[topic].Items[index], 'value_max').AsInteger;
  valueMin := jsonGetData(formAsArray.Items[topic].Items[index], 'value_min').AsInteger;
  validationUrl := jsonGetData(formAsArray.Items[topic].Items[index], 'validation_url');
  cancelationKeyword := jsonGetData(formAsArray.Items[topic].Items[index], 'cancelation_keyword');

  FCurrentInputType := inputType;

  if inputTitle.StrPos('#') = 1 then begin
    s := '('+AIndex.ToString+'/'+SimpleBOT.UserData[FORM_QUESTION_TOTAL]+') ';
    inputTitle := inputTitle.Replace('#', s);
  end;

  SimpleBOT.UserData[FORM_INPUT_TITLE] := inputTitle.Replace(#13,'\n').Replace(#10,'\n');
  SimpleBOT.UserData[FORM_INPUT_NAME] := inputName;
  SimpleBOT.UserData[FORM_INPUT_TYPE] := inputType;
  SimpleBOT.UserData[FORM_INPUT_LENGTH_MAX] := lengthMax.ToString;
  SimpleBOT.UserData[FORM_INPUT_LENGTH_MIN] := lengthMin.ToString;
  SimpleBOT.UserData[FORM_INPUT_VALUE_MAX] := valueMax.ToString;
  SimpleBOT.UserData[FORM_INPUT_VALUE_MIN] := valueMin.ToString;
  SimpleBOT.UserData[FORM_INPUT_VALIDATION_URL] := validationUrl;
  SimpleBOT.UserData[FORM_INPUT_CANCELATION_KEYWORD] := cancelationKeyword;

  if inputType = 'date' then
  begin
    inputTitle += FORM_INPUT_DATE_FORMAT;
  end;
  if inputType = 'option' then
  begin
    optionList := '';
    FInputOptions := TJSONArray.Create;
    for i:=1 to formAsArray.Items[topic].Items[index].GetPath('options').Count do
    begin
      s := formAsArray.Items[topic].Items[index].GetPath('options').Items[i-1].AsString;
      optionList += '\n'+i.ToString+'. ' + s;

      oItem := TJSONObject.Create;
      oItem.Add('text', s);
      oItem.Add('callback_data', 'text='+i2s(i));
      FInputOptions.Add( oItem);
    end;
    inputTitle += optionList;
    inputTitle += FORM_INPUT_OPTION_FORMAT;
    SimpleBOT.UserData[FORM_INPUT_OPTION_COUNT] := formAsArray.Items[topic].Items[index].GetPath('options').Count.ToString;
  end;
  if inputType = 'list' then
  begin
    optionList := '';
    FInputOptions := TJSONArray.Create;
    for i:=1 to formAsArray.Items[topic].Items[index].GetPath('options').Count do
    begin
      s := formAsArray.Items[topic].Items[index].GetPath('options').Items[i-1].AsString;
      optionList += '\n'+i.ToString+'. ' + s;

      oItem := TJSONObject.Create;
      oItem.Add('text', s);
      oItem.Add('callback_data', 'text='+i2s(i));
      FInputOptions.Add( oItem);
    end;
    inputTitle += optionList;
    inputTitle += FORM_INPUT_OPTION_FORMAT;
    SimpleBOT.UserData[FORM_INPUT_OPTION_COUNT] := formAsArray.Items[topic].Items[index].GetPath('options').Count.ToString;
  end;
  formAsArray.Free;
  Result := inputTitle + ':';
end;

function TCarikWebModule.GetFormAnswerValue(AQuestionIndex: integer;
  AOptionIndex: integer): string;
var
  i, index, topic, subCount, currentCount: integer;
  fileName: string;
  lst: TStringList;
  formAsArray: TJSONArray;
begin
  Result := '';
  fileName := SimpleBOT.UserData[FORM_INPUT_FILENAME];
  fileName := AppData.tempDir + FORM_PATH + fileName;
  if not FileExists(fileName) then Exit;
  lst := TStringList.Create;
  lst.LoadFromFile(fileName);

  formAsArray := GetJSON(lst.Text,False) as TJSONArray;

  index := 0;
  topic := 0;
  subCount := 0;
  for i:=0 to formAsArray.Count-1 do
  begin
    currentCount := formAsArray.Items[i].Count;
    subCount += currentCount;
    if AQuestionIndex <= subCount then
    begin
      index := AQuestionIndex - (subCount-currentCount) - 1;
      break;
    end;
    topic := topic + 1;
  end;
  try
    Result := formAsArray.Items[topic].Items[index].GetPath('values').Items[AOptionIndex-1].AsString;
  except
  end;

  formAsArray.Free;
  lst.Free;
end;

function TCarikWebModule.GetFormAnswerText(AQuestionIndex: integer;
  AOptionIndex: integer): string;
var
  i, index, topic, subCount, currentCount: integer;
  fileName: string;
  lst: TStringList;
  formAsArray: TJSONArray;
begin
  Result := '';
  fileName := SimpleBOT.UserData[FORM_INPUT_FILENAME];
  fileName := AppData.tempDir + FORM_PATH + fileName;
  if not FileExists(fileName) then Exit;
  lst := TStringList.Create;
  lst.LoadFromFile(fileName);

  formAsArray := GetJSON(lst.Text,False) as TJSONArray;

  index := 0;
  topic := 0;
  subCount := 0;
  for i:=0 to formAsArray.Count-1 do
  begin
    currentCount := formAsArray.Items[i].Count;
    subCount += currentCount;
    if AQuestionIndex <= subCount then
    begin
      index := AQuestionIndex - (subCount-currentCount) - 1;
      break;
    end;
    topic := topic + 1;
  end;
  try
    Result := formAsArray.Items[topic].Items[index].GetPath('options').Items[AOptionIndex-1].AsString;
  except
  end;

  formAsArray.Free;
  lst.Free;
end;

function TCarikWebModule.GenerateTextFromCustomActionOption(AText: string
  ): string;
var
  i: integer;
  s: string;
  actionText, levels: TStrings;
  dt: TDateTime;
begin
  Result := AText;
  if not AText.IsNumeric then Exit;
  if AText.IsExists('.') then Exit;

  // check get previous menu
  if AText = '0' then
  begin
    s := trim(SimpleBOT.UserData[PREVIOUS_ACTION]);
    //if s.IsEmpty then
    begin
      s := trim(SimpleBOT.UserData[MESSAGE_ACTION_LEVEL]);
      if s.IsNotEmpty then
      begin
        levels := Explode(s, '.');
        i := levels.Count;
        if i = 1 then s := 'menu';
        if i = 2 then s := trim(SimpleBOT.UserData[PREVIOUS_ACTION]);
        if i > 2 then
        begin
          levels.Delete(i-1);
          s := Implode(TStringList(levels), '.');
        end;
      end;
    end;

    if s.IsNotEmpty then
      Result := s;
    Exit;
  end;

  s := SimpleBOT.UserData[MESSAGE_ACTION_DATE];
  if not s.IsEmpty then
  begin
    dt := s.AsDateTime;
    if dt.MinutesDiff(Now) >= CALLBACK_QUERY_TIMEOUT then
    begin
      if dt.MinutesDiff(Now) < CALLBACK_QUERY_TIMEOUT_PREFIX then
      begin
        Prefix := FORM_ERR_TIMEOUT;
        FHideTextReply := True;
      end;
      Exit;
    end;
  end;

  if SimpleBOT.UserData[MESSAGE_ACTION_COUNT].IsEmpty then
    Exit;
  i := SimpleBOT.UserData[MESSAGE_ACTION_COUNT].AsInteger;
  if AText.AsInteger > i then
  begin
    Result := ACTION_ERR_NLP_TOOBIG;
    Exit;
  end;

  s := MESSAGE_ACTION_DATA_ + i2s(AText.AsInteger-1);
  s := SimpleBOT.UserData[s];
  actionText := Explode(s,'|'); //#0: title #1:content formatted
  s := actionText[1];
  actionText.Free;
  actionText := Explode(s,'&');
  s := actionText.Values['text'];
  if s.IsEmpty then
  begin
    s := actionText.Values['url'];
    if s.IsNotEmpty then
      s := 'icho Kunjungi: ' + s;
  end;
  Result := s;
  actionText.Free;

  SimpleBOT.SimpleAI.AdditionalParameters.Values['converted_text'] := s;
end;

function TCarikWebModule.RemoveDummyImageLink(AText: string): string;
begin
  // check markdown #1: /\[[^\]]*\]\([^)]*\)*/
  // check markdown #2: (?:__|[*#])|\[(.*?)\]\(.*?\)
  Result := preg_replace('\[.\]\(.*?\)', '', AText);
end;

function TCarikWebModule.OnNLPErrorHandler(const Message: string): string;
var
  nec, retries: integer;
  s, defaultReplay, muteDuration, callbackIntent: string;
  isHandled: boolean;
begin
  Result := '';
  FErrorCount := FErrorCount + 1;
  if (FErrorCount > NLP_ERROR_COUNT_MAX) then
  begin
    Exit;
  end;
  nec := SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT].AsInteger;

  // check if any custom handler
  If Assigned(FOnError) then
  begin
    isHandled := False;
    Result := FOnError(OriginalText, isHandled);
    if isHandled then
    begin
      Exit;
    end;
  end;

  if ((ChannelId='') or (ChannelId='direct')) then Exit;
  s := Trim(Message);
  //s := StringReplace(SimpleBOT.GetResponse('InginTahu', ''), '%word%', s, [rfReplaceAll]);
  s := StringReplace(SimpleBOT.GetResponse('none', ''), '%word%', s, [rfReplaceAll]);
  defaultReplay := s;
  Result := s;

  if SimpleBOT.isFormula then
  begin
    Result := SimpleBOT.Formula(Message);
    if not SimpleBOT.IsMathSuccess then
    begin
      Result := ExternalNLP(Message);
    end;
    SaveUnknownChat(Message);
    Analytics('global', 'unknown', Message, Carik.UserID);
    Exit;
  end;

  if ((IsTranslate) and (FErrorCount < NLP_ERROR_COUNT_MAX)) then
  begin
    s := translate(Message, FLanguage, 'id', True);
    if s <> Message then
    begin
      SimpleBOT.OriginalMessage := Message;
      SimpleBOT.SimpleAI.ResponseText.Clear;
      s := ProcessText(s);
      Result := SimpleBOT.SimpleAI.ResponseText.Text;
      Exit;
    end;

  end
  else
  begin
    if IsSuggest then
    begin
      Result := defaultReplay;
      s := FindSuggestion(Message);
      if not s.IsEmpty then
      begin
        SimpleBOT.OnError := nil;
        SimpleBOT.OriginalMessage := Message;
        SimpleBOT.SimpleAI.ResponseText.Clear;
        s := ProcessText(s);
        Result := SimpleBOT.SimpleAI.ResponseText.Text;
      end;
    end;//IsSuggest
  end;

  s := ExternalNLP(Message);
  if s.IsEmpty then
  begin
    nec += 1;
    retries := MaximumRetriesUnknownChat;
    if ((retries > 0)and(nec>=retries)) then
    begin
      muteDuration := Config[UNKNOWNCHAT_MUTE_DURATION];
      if (muteDuration.ToInteger>0) then
      begin
        muteDuration := Now.IncMinute(muteDuration.ToInteger).AsString;
        SimpleBOT.UserData[ClientId + '-mute'] := muteDuration;
      end;
      callbackIntent := Config[UNKNOWNCHAT_CALLBACK];
      Result := SimpleBOT.GetResponse(callbackIntent, '');
    end;
    SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT] := nec.ToString;
    SaveUnknownChat(Message);
    Analytics('global', 'unknown', Message, Carik.UserID);
  end else
  begin
    try
      SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT] := '0';
    except
    end;
    Result := s;
  end;

  LogUtil.Add(Carik.UserID + ':' + Carik.FullName + ':' + Message + '/' + Carik.GroupChatID
    + '-' + Carik.GroupName, 'NLPERROR');
end;

function TCarikWebModule.CleanupMessage(const AMessage: string): string;
begin
  Result := ' ' + AMessage + ' ';
  Result := StringReplace(Result, ' bot ', '', [rfReplaceAll, rfIgnoreCase]);
  Result := Trim(Result);
end;


end.
