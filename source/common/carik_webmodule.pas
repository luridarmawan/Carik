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
  logutil_lib,
  movie_controller,
  alquranindonesia_integration,
  telegram_integration, googleplacesearch_integration,
  kamussunda_integration, openweathermap_integration,
  witai_integration,
  line_integration,
  facebookmessenger_integration,
  googledistancematrix_integration,
  rss_lib, http_lib, IniFiles,
  rajaongkir_integration,
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
    // respons berasal dari ExternalNLP/LLM: jangan jalankan command di dalamnya
    FUntrustedResponse: boolean;
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
    FITALIC_CODE: string;
    FIterationParams: string;
    FIterationParamsPast : TStringList;
    FKickUser: boolean;
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
    FIsCustomActionFileExist:boolean;

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
    function getPrefixID: string;
    function getReplyType: string;
    function getRequestAsJson: TJSONUtil;
    function getUniqueID: string;
    function getWaitingInput: boolean;
    function isGroup: boolean;
    function isTelegramGroup: boolean;


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
    function movieInfoHandler(const IntentName: string; Params: TStrings): string;
    function moviePlayHandler(const IntentName: string; Params: TStrings): string;
    function distanceHandler(const IntentName: string; Params: TStrings): string;
    function distanceFromToHandler(const IntentName: string; Params: TStrings): string;
    function lokasiHandler(const IntentName: string; Params: TStrings): string;
    function lokasiDenganKoordinatHandler(const IntentName: string;
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


    function openweatherInfoHandler(const IntentName: string; Params: TStrings): string;
    function beritaHariIniHandler(const IntentName: string; Params: TStrings): string;

    function conversionHashHandler(const IntentName: string; Params: TStrings): string;
    function customMathHandler(const IntentName: string; Params: TStrings): string;

    function propertySearchHandler(const IntentName: string; Params: TStrings): string;

    function richContentHandler(const IntentName: string; Params: TStrings): string;

    function echoHandler(const IntentName: string; Params: TStrings): string;
    function texttospeechHandler(const IntentName: string; Params: TStrings): string;
    function speakingModeOnHandler(const IntentName: string; Params: TStrings): string;
    function speakingModeOffHandler(const IntentName: string; Params: TStrings): string;


    function botEnableHandler(const IntentName: string; Params: TStrings): string;
    function botDisableHandler(const IntentName: string; Params: TStrings): string;

    function mortgageCalculator(ALoanAmount: double; ALongInstallment: double = 10;
      AInterest: double = 8): double;
    function mortgageCalculatorHandler(const IntentName: string;
      Params: TStrings): string;

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
    property UntrustedResponse: boolean read FUntrustedResponse;
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



    // Kamus
    function translateIndonesiaToSundaHandler(const IntentName: string;
      Params: TStrings): string;
    function translateSundaToIndonesiaHandler(const IntentName: string;
      Params: TStrings): string;

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
    property IsCustomActionFileExist: boolean read FIsCustomActionFileExist write FIsCustomActionFileExist;
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

implementation

uses security_lib;

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
    // alternation nama fungsi utuh (dulu character class [cos|sin|...] sehingga
    // string apa pun dari huruf-huruf itu dianggap persamaan)
    '^((cos|sin|tan|tangen|sqr|sqrt|log|ln|sec|cosec|arctan|abs|exp|frac|int|round|trunc|shl|shr|ifs|iff|ifd|ifi)|[0-9*+ ().,\-/:^])+$';
  // 'x' sebagai tanda kali hanya di antara angka/kurung (bukan di dalam 'exp', 'max')
  REGEX_TIMES = '([0-9)])\s*x\s*([0-9(])';

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
  Result := False; // belum diimplementasikan

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
  Result := False;
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


function TCarikWebModule.onewordHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
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
  Result := '';

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

  FreeAndNil(FIterationParamsPast); // bisa dipanggil lebih dari sekali per request
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
  FGroupData := nil; // cegah double free bila TIniFile.Create raise
  try
    FGroupData := TIniFile.Create(GROUP_DATA_FILENAME);
    FGroupData.WriteString( Carik.GroupChatID, key, AValue);
  except
    on E:Exception do
    begin
      LogUtil.Add(E.Message, 'GROUPDATA');
    end;
  end;
  FreeAndNil(FGroupData);
end;

function TCarikWebModule.getGroupData(const KeyName: string): string;
var
  key: string;
begin
  Result := '';
  if Carik.GroupChatID.IsEmpty then Exit;
  key := 'prune-' + KeyName;
  FGroupData := nil; // cegah double free bila TIniFile.Create raise
  try
    FGroupData := TIniFile.Create(GROUP_DATA_FILENAME);
    Result := FGroupData.ReadString( Carik.GroupChatID, key, '');
  except
    on E:Exception do
    begin
      LogUtil.Add(E.Message, 'GROUPDATA');
    end;
  end;
  FreeAndNil(FGroupData);
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
  Result := '';
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
  Result := '';
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
  Result := '';
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
  s: string;
  fs: TFormatSettings;
const
  AllowedOperator = ['+', '-', '/', '*', '^'];
begin
  Result := Params.Values['Formula_value'];
  Result := StringReplace(Result, ':', '/', [rfReplaceAll]);
  // ulangi agar "2x3x4" (berbagi angka) terganti semua
  repeat
    s := Result;
    Result := preg_replace(REGEX_TIMES, '$1*$2', Result);
  until Result = s;
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
  if Result.IsNotEmpty and (Result[1] = '.') then
    Result := Copy(Result, 2);
  Result := Result.Trim;
  Result := StringHumanToNominal(Result);
  Result := Result.Replace(' ' , '');
  if Result.IsEmpty then
  begin
    Result := '..... :( ';
    Exit;
  end;
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
    if Result.IsEmpty then Result := '..... :( '
    else FUntrustedResponse := True;
    Exit;
  end;

  mathParser := TFPExpressionParser.Create(nil);
  try
    mathParser.BuiltIns := [bcMath, bcBoolean];
    mathParser.Expression := Result;
    resultValue := ArgToFloat(mathParser.Evaluate);
    SimpleBOT.UserData['math_result'] := f2s(resultValue);
    // format Indonesia hanya untuk teks keluaran; jangan ubah setting global
    // (dulu ThousandSeparator/DecimalSeparator global -> f2s menulis "1,5"
    // ke math_result sehingga perhitungan lanjutan gagal)
    fs := DefaultFormatSettings;
    fs.ThousandSeparator := '.';
    fs.DecimalSeparator := ',';
    Result := Format(FFormatNumber, [resultValue], fs);
    Result := Result.Replace(',000','');
  except
    Result := ExternalNLP('berapa '+Params.Values['Formula_value']);
    if Result.IsEmpty then Result := 'duuhh... saya bingung dehh.. '
    else FUntrustedResponse := True;
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
    Result := copy(Result, 1, TEXT2SPEECH_MAX_CHAR);
    // potong di batas kata. Dulu mencari '_' (spasi tidak lagi diganti '_')
    // sehingga RPos = 0 dan teks > 250 karakter menjadi kosong.
    if RPos(' ', Result) > 0 then
      Result := copy(Result, 1, RPos(' ', Result) - 1);
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
  triggers: TJSONObject;
  words: TJSONData;
  d: TDateTime;
  tmpFormatSettings: TFormatSettings;
  inCooldown: boolean;
begin
  Result := False;
  LMessage := ' ' + Message + ' ';

  tmpFormatSettings := FormatSettings;
  tmpFormatSettings.DateSeparator := '-';
  tmpFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  tmpFormatSettings.LongDateFormat := 'yyyy-MM-dd HH:nn:ss';

  { TODO:
  if last visit < 1 menit,
    maka cek trigger2 khusus
  selain itu, check trigger global
  }

  // objek config dipinjam (jangan di-Free); nil bila trigger_word tidak ada
  triggers := Config.GetObject(AI_CONFIG_TRIGGERWORD);
  if not Assigned(triggers) then
    Exit;

  try
    for i := 0 to triggers.Count - 1 do
    begin
      strigger_name := triggers.Names[i];
      words := triggers.Items[i];
      for j := 0 to words.Count - 1 do
      begin
        strigger := ' ' + words.Items[j].AsString + ' ';
        if not preg_match(strigger, LMessage) then
          Continue;

        s := SafeText(strigger_name);
        s := ReplaceAll(s, ['(', ')'], '');
        s := mysql_real_escape_string(s);
        s := TRIGGER_SESSION_PREFIX + Carik.GroupName + Carik.GroupChatID + '-' + s + '_time';

        stime := Carik.Data.ReadString(TRIGGER_SESSION_PREFIX, s,
          FormatDateTime('yyyy-mm-dd HH:nn:ss', IncMinute(Now, TRIGGER_INTERVAL + 2)));
        inCooldown := False;
        try
          d := StrToDateTime(stime, tmpFormatSettings);
          inCooldown := MinutesBetween(Now, d) <= TRIGGER_INTERVAL; // in minutes
        except
        end;
        // trigger ini masih cooldown: lewati, cek trigger berikutnya
        if inCooldown then
          Break;

        Carik.Data.WriteString(TRIGGER_SESSION_PREFIX, s, FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
        FTriggeredText := Trim(strigger);
        Result := True;
        Break;
      end;
      // dulu loop luar tetap berjalan sehingga trigger lain menimpa FTriggeredText
      if Result then
        Break;
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
  s, log_url: string;
  spamList: TIniFile;
  http_response: IHTTPResponse;
  json: TJSONUtil;
  http: THTTPLib;
begin
  Result := False;

  // get info from blacklist repository
  http := nil;
  json := nil;
  try
    try
      log_url := Config[BLACKLISTCHECK_URL] + '?_=1&channel=telegram&id=' + UrlEncode(AUserId);
      http := THTTPLib.Create;
      http.URL := log_url;
      http.AddHeader('Cache-Control', 'no-cache');
      http.AddHeader('X-Client-Type', 'beta'); //TODO: client type
      http_response := http.Get;
      if http_response.ResultCode = 200 then
      begin
        json := TJSONUtil.Create;
        json.LoadFromJsonString(http_response.ResultText, False);
        // code 0 = terdaftar. Key kosong/tidak valid bukan berarti terdaftar.
        s := json['code'];
        if TryStrToInt(Trim(s), i) and (i = 0) then
          Result := True;
      end;
    except
    end;
  finally
    json.Free;
    http.Free;
  end;
  if Result then
    Exit;

  // blacklist lokal (opsional): cocokkan user id (ditulis ReportSpam) dan username.
  // Dulu: file tidak ada -> Exit, sehingga pengecekan Combot/CAS di bawah terlewat.
  if FileExists(BLACKLIST_FILENAME) then
  begin
    spamList := TIniFile.Create(BLACKLIST_FILENAME);
    try
      if (AUserId.IsNotEmpty and spamList.ValueExists('blacklist', 'tl-' + AUserId))
        or (AUserName.IsNotEmpty and spamList.ValueExists('blacklist', 'tl-' + AUserName)) then
        Result := True;
    finally
      spamList.Free;
    end;
    if Result then
      Exit;
  end;

  // Check from combot
  if not AUserId.IsEmpty then
  begin
    if IsSpammer(AUserId) then
    begin
      // tercatat di blacklist lokal sehingga pesan berikutnya tidak lapor ulang
      ReportSpam(AUserId);
      Result := True;
    end;
  end;
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
  spamWords: TJSONObject;
  lstURL: TStringList;
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
    // pola regex: titik harus di-escape. Dulu '.ini ' cocok dengan kata "ini "
    // di kalimat apa pun sehingga skor spam berkurang 80 (spam lolos).
    if preg_match('stackoverflow\.com', AText) then
      Result := Result - 80;
    if preg_match('\w\.ini\b', AText) then // nama file seperti php.ini
      Result := Result - 80;

    if isNewMember(AUserID, Carik.GroupChatID, NEW_MEMBER_INTERVAL_POST_PERMITTED) then // last join N minutes ago
    begin
      Result := Result + 70;
    end;
  end;

  //TODO: add spam score detection
  if AText.IsExists('$') then Result := Result + 10;
  // GetObject mengembalikan nil bila SPAM_WORD tidak ada di config (objek dipinjam, jangan di-Free)
  spamWords := Config.GetObject(SPAM_WORD);
  if Assigned(spamWords) then
  begin
    for i := 0 to spamWords.Count - 1 do
    begin
      additionScore := 30;
      triggerName := spamWords.Names[i];
      jData := spamWords.Items[i];
      for j := 0 to jData.Count - 1 do
      begin
        triggerWord := jData.Items[j].AsString;
        if preg_match(triggerWord, AText) then
        begin
          LogUtil.Add(triggerName + '/' + triggerWord, 'SPAM-CHECK');
          Result := Result + additionScore;
        end;
      end;
    end;
  end;

  if (Result < SPAM_SCORE_THRESHOLD) and FileExists(BLACKLIST_URL_FILENAME) then
  begin
    // check blacklisted URL
    lstURL := TStringList.Create;
    try
      lstURL.LoadFromFile(BLACKLIST_URL_FILENAME);
      for i := 0 to lstURL.Count -1 do
      begin
        s := LowerCase(lstURL[i]).Trim;
        if s.IsEmpty then Continue;
        if s.IsExists('#') then Continue;
        if Pos( s, AText) > 0 then
        begin
          Result := Result + SPAM_SCORE_THRESHOLD;
        end;
      end;
    finally
      lstURL.Free;
    end;

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
      try
        json.LoadFromJsonString(http_response.ResultText, False);
        Result:= json['text'];
      except
        Result := '';
      end;
      json.Free;

      // pakai pesan dari server bila ada (dulu selalu ditimpa -> selalu "sukses")
      if Result.IsEmpty then
      begin
        if http_response.ResultCode = 200 then
          Result := AUserID.Replace('+','') + ' reported as spammer'
        else
          Result := 'kegagalan dalam melaporkan spammer';
      end;

      Free;
    end;
  except
  end;

  // catat di blacklist lokal dengan key yang sama seperti pengecekan di
  // IsBlackListed ('tl-<id>', tanpa '+'); file dibuat bila foldernya ada
  if not DirectoryExists(ExtractFilePath(ExpandFileName(BLACKLIST_FILENAME))) then
    Exit;

  try
    spamList := TIniFile.Create(BLACKLIST_FILENAME);
    try
      spamList.WriteString('blacklist', 'tl-' + AUserID.Replace('+',''),
        FormatDateTime('yyyy-mm-dd HH:nn:ss',Now));
    finally
      spamList.Free;
    end;
  except
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
  nlp_json, actionNode, filesNode: TJSONData;
  requestData: TJSONUtil;
  http_response: IHTTPResponse;
  http: THTTPLib;
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
  nlp_json := nil;
  http := THTTPLib.Create;
  try
    with http do
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
          actionNode := nlp_json.FindPath('action.data');
          if Assigned(actionNode) then
            SaveActionToUserData(FCustomReplyActionTypeFromExternalNLP, TJSONObject(actionNode));
          if FCustomActionAsText.IsNotEmpty then
          begin
            //Result += '\n'+FCustomActionAsText.Replace(#10, '\n');
          end;
        end;
      end;

      //compatibility with form input handler
      if http_response.ResultCode = 200 then
      begin
        // ExternalNLP bisa dipanggil beberapa kali per request
        FreeAndNil(FCustomReplyDataFromExternalNLP);
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
          actionNode := FCustomReplyDataFromExternalNLP.Data.FindPath('action.data');
          if Assigned(actionNode) then
          begin
            SaveActionToUserData(FCustomReplyActionTypeFromExternalNLP, TJSONObject(actionNode));
            FCustomReplyDataFromExternalNLP.LoadFromJsonString(actionNode.AsJSON, False);
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

          // files: simpan salinan; FCustomActionFiles dibebaskan di destructor
          FreeAndNil(FCustomActionFiles);
          filesNode := nil;
          if Assigned(FCustomReplyDataFromExternalNLP.Data) then
            filesNode := FCustomReplyDataFromExternalNLP.Data.FindPath('files');
          if Assigned(filesNode) and (filesNode is TJSONArray) then
            FCustomActionFiles := TJSONArray(filesNode.Clone);

        end;

      end;

      //TODO: ulil chek if 'files' exist
    end;
  except
    on E:Exception do
    begin
      LogUtil.Add(E.Message, 'ENLP');
    end;
  end;

  http.Free;
  nlp_json.Free;
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


function TCarikWebModule.botEnableHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  //todo: check di whatsapp ulil
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
  fs: TFormatSettings;
begin
  _amount := s2f(StringHumanToNominal(Params.Values['LoanAmount_value']));
  _long := s2f(Params.Values['LongInstallment_value']);
  _interest := s2f(Params.Values['Interest_value']);

  if _long = 0 then
    _long := 10;
  if _interest = 0 then
    _interest := 10;

  _cicilan := mortgageCalculator(_amount, _long, _interest);

  // format ribuan Indonesia lokal saja (dulu mengubah DefaultFormatSettings global)
  fs := DefaultFormatSettings;
  fs.ThousandSeparator := '.';
  fs.DecimalSeparator := ',';
  Result := SimpleBOT.GetResponse(IntentName + 'Response');
  Result := StringReplace(Result, '$LoanAmount$', FormatFloat('###,##0', _amount, fs),
    [rfReplaceAll]);
  Result := StringReplace(Result, '$LongInstallment$', FormatFloat('###,##0', _long, fs),
    [rfReplaceAll]);
  Result := StringReplace(Result, '$Interest$', f2s(_interest), [rfReplaceAll]);
  Result := StringReplace(Result, '$Cicilan$', FormatFloat('###,##0', _cicilan, fs),
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
  // Pesan user sendiri memuat command (mis. dipantulkan oleh echo): selalu tolak.
  if ContainsText(OriginalText, CMD_POST_RICH + ':')
    or ContainsText(OriginalText, CMD_JSON_RICH + ':') then
  begin
    LogUtil.Add('command ditolak (dari pesan user): ' + Copy(AText, 1, 200), 'SECURITY');
    Exit;
  end;
  // Respons dari ExternalNLP/LLM: hanya bila security/allow_external_commands = true
  // dan security/allowed_hosts diisi (host tujuan dicek lagi di execPost).
  if FUntrustedResponse and not IsExternalCommandAllowed then
  begin
    LogUtil.Add('command ditolak (dari ExternalNLP): ' + Copy(AText, 1, 200), 'SECURITY');
    Exit;
  end;

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
  try
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
    customReplyDataAsArray := nil;
    try
      customReplyDataAsArray := TJSONArray(GetJSON(FCustomReplyDataFromExternalNLP.AsJSON, False));
    except
    end;
    // ValueArray mengambil kepemilikan node: tiap path harus dapat salinan sendiri
    if Assigned(customReplyDataAsArray) then
      jsonOutput.ValueArray['action/action/data'] := TJSONArray(customReplyDataAsArray.Clone);

    //compatibility
    jsonOutput['action/type'] := CustomReplyType;
    jsonOutput['action/suffix'] := CustomActionSuffix;
    if Assigned(customReplyDataAsArray) then
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
      // FInputOptions tetap milik module (dibebaskan di destructor)
      inputData := TJSONArray.Create;
      inputData.Add(FInputOptions.Clone);
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

  if FIsCustomActionFileExist and Assigned(FCustomActionFiles) then
  begin
    // FCustomActionFiles tetap milik module (dibebaskan di destructor)
    jsonOutput.ValueArray['action/files'] := TJSONArray(FCustomActionFiles.Clone);
  end;

  if FIsDebug then
    Result := jsonOutput.AsJSONFormated
  else
    Result := jsonOutput.AsJSON;
  finally
    jsonOutput.Free;
  end;
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

  // anti SSRF: tolak host lokal/privat & host di luar security/allowed_hosts
  if not IsSafeOutboundURL(AURL) then
  begin
    LogUtil.Add('URL ditolak: ' + Copy(AURL, 1, 200), 'SECURITY');
    Exit;
  end;

  with THTTPLib.Create(AURL) do
  begin
    try
      AllowRedirect := False; // redirect bisa mengarah ke host internal
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
  http: THTTPLib;
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
  try
    log_url := urlExplode[0] + '?clientId=' + FClientId;
  finally
    urlExplode.Free;
  end;

  if AGroupID = AUserID then
    AGroupID := '';

  is_group := '0';
  is_mentioned := '0';
  if AIsGroup then
    is_group := '1';
  if AIsMentioned then
    is_mentioned := '1'; // dulu terbalik: AIsMentioned=True dikirim '0'

  requestJson := TJSONUtil.Create;
  responseJson := TJSONUtil.Create;
  http := THTTPLib.Create(log_url);
  try
  with http do
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
    requestJson['message/message_id'] := MessageID;
    requestJson['message/text'] := (AText);
    requestJson['message/reply'] := (AReply);
    requestJson['message/from/id'] := AUserID;
    requestJson['message/from/name'] := AFullName;
    requestJson['message/from/username'] := AUserName;
    requestJson['message/chat/channel'] := AChannelID;
    // id chat (grup bila ada, selain itu user), bukan id pesan
    if AGroupID <> '' then
      requestJson['message/chat/id'] := AGroupID
    else
      requestJson['message/chat/id'] := AUserID;
    requestJson['message/chat/is_mentioned'] := is_mentioned;
    requestJson['message/chat/is_group'] := is_group;
    requestJson['message/intents/name'] := SimpleBOT.SimpleAI.IntentName;
    requestJson['message/dashboard_device_id'] := DashboardDeviceID;

    if AReplyFromMessageId > 0 then requestJson['message/chat/is_reply'] := 1;
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

    // FileList milik jsonData request; kirim salinannya agar tidak double free
    if MessageType.IsEqualTo('image') and Assigned(FileList) then
    begin
      requestJson.ValueArray['data/files'] := TJSONArray(FileList.Clone);
    end;

    try
      responseJson.LoadFromJsonString(LogChatPayload.Text, False);
    except
    end;
    // setiap parent harus memiliki node sendiri
    if Assigned(responseJson.Data) then
    begin
      requestJson.Data.Add('response', responseJson.Data.Clone);
      requestJson.Data.Add('nlp_response', responseJson.Data.Clone);
    end;

    ContentType := 'application/json';
    RequestBody := TStringStream.Create(requestJson.AsJSON);
    try
      httpResponse := Post;
    except
    end;
    //LogUtil.Add(responseJson.AsJSON, 'logchat');
    //LogUtil.Add(log_url, 'logchat');
    //LogUtil.Add(httpResponse.ResultText, 'logchat');
  end;
  finally
    http.Free;
    responseJson.Free;
    requestJson.Free;
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
      LogUtil.Add( 'submit: ' + requestJson.AsJSON, 'JOIN', False, AppData.logDir + 'logjoin.log');
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
  // _DEBUG hanya berlaku bila systems/debug = true
  if (_GET['_DEBUG'] = '1') and IsDebugAllowed then FIsDebug:= True;
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
  // botid dipakai sebagai nama file config: hanya [A-Za-z0-9_-]
  if FBotID.IsNotEmpty and IsSafeIdentifier(FBotID) then
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
  FCustomActionFiles := nil;
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
  FIsCustomActionFileExist := False;
  FExternalNLPStarted := False;
  FUntrustedResponse := False;
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
  FreeAndNil(LogChatPayload);
  FreeAndNil(FCustomActionFiles);
  FreeAndNil(FInputOptions);
  FreeAndNil(ElementArray);
  FreeAndNil(FIterationParamsPast);
  FreeAndNil(FCustomReplyDataFromExternalNLP);
  FreeAndNil(Carik);
  FreeAndNil(SimpleBOT);
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
    if DeviceId.IsNotEmpty then SimpleBOT.SimpleAI.AdditionalParameters.Values['dashboard_device_id'] := DashboardDeviceID.ToString;
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
        FUntrustedResponse := True;
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
  SimpleBOT.Handler['movie_play'] := @moviePlayHandler;
  SimpleBOT.Handler['movie_info'] := @movieInfoHandler;
  SimpleBOT.Handler['jarak'] := @distanceHandler;
  SimpleBOT.Handler['distance_fromto'] := @distanceFromToHandler;
  SimpleBOT.Handler['lokasi'] := @lokasiHandler;
  SimpleBOT.Handler['lokasi_koordinat'] := @lokasiDenganKoordinatHandler;
  SimpleBOT.Handler['bot_enable'] := @botEnableHandler;
  SimpleBOT.Handler['bot_disable'] := @botDisableHandler;
  SimpleBOT.Handler['translate_indonesia_sunda'] := @translateIndonesiaToSundaHandler;
  SimpleBOT.Handler['translate_sunda_indonesia'] := @translateSundaToIndonesiaHandler;

  SimpleBOT.Handler['alquran_terjemahan'] := @alquranTerjemahanHandler;

  SimpleBOT.Handler['mortgage_calculator'] := @mortgageCalculatorHandler;
  SimpleBOT.Handler['openweather_info'] := @openweatherInfoHandler;
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


  // Property Search
  SimpleBOT.Handler['property_search'] := @propertySearchHandler;


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
    // Facebook & LINE menampilkan kartu secara native (dulu: mmFacebook ditulis dua kali)
    if ((FMessengerMode = mmFacebook)or(FMessengerMode = mmLine)) then
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
      title := title.SafeText;
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
    // URL form berasal dari respons NLP: cegah data user terkirim ke host internal/asing
    if not IsSafeOutboundURL(SimpleBOT.UserData[MESSAGE_ACTION_URL]) then
    begin
      LogUtil.Add('URL form ditolak: ' + Copy(SimpleBOT.UserData[MESSAGE_ACTION_URL], 1, 200), 'SECURITY');
      postData.Free;
      Exit;
    end;
    with THTTPLib.Create(SimpleBOT.UserData[MESSAGE_ACTION_URL]) do
    begin
      try
        AllowRedirect := False;
        AddHeader('_source', 'carik');
        AddHeader('Cache-Control', 'no-cache');
        ContentType := 'application/json';
        RequestBody := TStringStream.Create(postData.AsJSON);
        httpResponse := Post();
        if httpResponse.ResultCode = 200 then
        begin
          FreeAndNil(FCustomReplyDataFromExternalNLP);
          FCustomReplyDataFromExternalNLP := TJSONUtil.Create;
          FCustomReplyDataFromExternalNLP.LoadFromJsonString(httpResponse.ResultText, False);
          Suffix := FCustomReplyDataFromExternalNLP['text'];
          Result := Suffix;

          //TODO: build custom action
          FCustomReplyTypeFromExternalNLP := FCustomReplyDataFromExternalNLP['type'];
          FCustomReplyActionTypeFromExternalNLP := 'text';
          if FCustomReplyTypeFromExternalNLP.IsNotEmpty then
          begin
            LogUtil.Add('external data not empty', 'FORM');
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
  // kode reset (dari tombol form) selalu berlaku; kata "batal"/"cancel" hanya
  // saat sedang mengisi form, agar tidak memicu POST pembatalan di luar form
  if ((Text = FORM_INPUT_RESET_CODE) or
     (Text = '"'+FORM_INPUT_RESET_CODE+'"') or
     (((Text = 'batal') or (Text = 'cancel')) and WaitingInput)) then
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
  // check url
  if inputType = 'url' then
  begin
    if not Text.IsURL then
    begin
      Suffix := FORM_ERR_FORMAT_URL + FORM_INPUT_HASHTAG_CANCEL2.Replace('%botname%', SimpleBOT.BotName);
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
    if ((Text.AsInteger=0)or(Text.AsInteger > i)) then
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
      if lst.Count >= 2 then
      begin
        if answerText.IsNotEmpty then
          postData['data/'+lst[0]+'_t'] := answerText;
        //postData['data/'+lst[0]+'t'] := answerText; //TODO: remove for  compatibility
        postData['data/'+lst[0]] := lst[1];
      end;
      lst.Free;
    end;
    postData['data/submit'] := OK;
    if (_GET['_FORMDEBUG'] = '1') and IsDebugAllowed then
    begin
      url := SimpleBOT.UserData[MESSAGE_ACTION_URL];
      die(postData.AsJSONFormated+#13+url); //ulil formpost
    end;
    //die(postData.AsJSONFormated); //ulil formpost

    // Submit
    url := SimpleBOT.UserData[MESSAGE_ACTION_URL];
    LogUtil.Add(postData.AsJSON, 'FORM-DATA');
    LogUtil.Add(url, 'FORM-URL');
    // URL form berasal dari respons NLP: cegah data user terkirim ke host internal/asing
    if not IsSafeOutboundURL(url) then
    begin
      LogUtil.Add('URL form ditolak: ' + Copy(url, 1, 200), 'SECURITY');
      Suffix := FORM_ERR_SUBMIT_FAILED;
      postData.Free;
      resetForm();
      Result := True;
      Exit;
    end;
    with THTTPLib.Create(url) do
    begin
      try
        AllowRedirect := False;
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
          FreeAndNil(FCustomReplyDataFromExternalNLP);
          FCustomReplyDataFromExternalNLP := TJSONUtil.Create;
          FCustomReplyDataFromExternalNLP.LoadFromJsonString(httpResponse.ResultText, False);
          Suffix := FCustomReplyDataFromExternalNLP['text'];

          // check files - taruh di sini karena konflik dengan code di bawah *1
          try
            if not (FCustomReplyDataFromExternalNLP.Data.FindPath('action.files') = nil) then
            begin
              FIsCustomActionFileExist := True;
              FreeAndNil(FCustomActionFiles);
              FCustomActionFiles := TJSONArray(GetJSON(FCustomReplyDataFromExternalNLP.Data.GetPath('action.files').AsJSON));
            end;
          except
            on E:Exception do
            begin
              //LogUtil.Add('Error: ' + E.Message, 'FORM');
            end;
          end;

          //TODO: build custom action
          //TODO: ref *1
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

          end;

        end;
      except
        on e: Exception do
        begin
          Suffix := FORM_ERR_SUBMIT_EXCEPTION;
          // httpResponse nil bila Post() sendiri yang raise
          if Assigned(httpResponse) then
            LogUtil.Add(e.Message + '//' + httpResponse.ResultText, 'FORM-ERROR')
          else
            LogUtil.Add(e.Message, 'FORM-ERROR');
          if FIsDebug then
            Suffix += '\n' + e.Message;
        end;
      end;

      Free;
    end;//THTTPLib
    postData.Free;

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
  formAsArray := nil;
  lst := TStringList.Create;
  try
    lst.LoadFromFile(fileName);
    formAsArray := GetJSON(lst.Text,False) as TJSONArray;
  except
    on E:Exception do
      LogUtil.Add('invalid form file ' + fileName + ': ' + E.Message, 'FORM');
  end;
  lst.Free;
  if not Assigned(formAsArray) then Exit;

  try
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
      // jangan die() dengan isi form ke user; die() juga tidak menghentikan prosedur
      LogUtil.Add('err: ' + topic.ToString + '/' + index.ToString + ' ' + E.Message, 'FORM');
      Exit;
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
    FreeAndNil(FInputOptions);
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
    FreeAndNil(FInputOptions);
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
  Result := inputTitle + ':';
  finally
    formAsArray.Free;
  end;
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
  // "00" dsb. -> index -1; perlakukan sebagai teks biasa
  if AText.AsInteger < 1 then
    Exit;

  s := MESSAGE_ACTION_DATA_ + i2s(AText.AsInteger-1);
  s := SimpleBOT.UserData[s];
  actionText := Explode(s,'|'); //#0: title #1:content formatted
  if actionText.Count < 2 then
  begin
    // data aksi kosong/rusak
    actionText.Free;
    Exit;
  end;
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
      if Result.IsNotEmpty then FUntrustedResponse := True;
    end;
    SaveUnknownChat(Message);
    Exit;
  end;

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
      // saran berhasil: jangan ditimpa hasil ExternalNLP di bawah
      if Result.Trim <> '' then
      begin
        SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT] := '0';
        Exit;
      end;
      Result := defaultReplay;
    end;
  end;//IsSuggest

  s := ExternalNLP(Message);
  if s.IsEmpty then
  begin
    nec += 1;
    retries := MaximumRetriesUnknownChat;
    if ((retries > 0)and(nec>=retries)) then
    begin
      muteDuration := Config[UNKNOWNCHAT_MUTE_DURATION];
      // s2i: config kosong/non-numerik -> 0, bukan EConvertError
      if (s2i(muteDuration)>0) then
      begin
        muteDuration := Now.IncMinute(s2i(muteDuration)).AsString;
        SimpleBOT.UserData[ClientId + '-mute'] := muteDuration;
      end;
      callbackIntent := Config[UNKNOWNCHAT_CALLBACK];
      Result := SimpleBOT.GetResponse(callbackIntent, '');
    end;
    SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT] := nec.ToString;
    SaveUnknownChat(Message);
  end else
  begin
    try
      SimpleBOT.UserData[LABEL_NLP_ERROR_COUNT] := '0';
    except
    end;
    Result := s;
    FUntrustedResponse := True;
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
