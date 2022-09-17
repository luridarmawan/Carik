unit telegram_handler;
{
  [x]
  curl "http://local-bot.carik.test/ai/telegram.bin" -d ""
}

{$mode objfpc}{$H+}

interface

uses
  json_lib, fpjson,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets, fpopenssl,
  {$endif}
  carik_controller,
  carik_webmodule, logutil_lib, telegram_integration, witai_integration,
  process, Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib,
  array_helpers, string_helpers, datetime_helpers;

{$include ../common/carik.inc}
_DEVELOPMENT_ = False;

type

  { TTelegramHandler }

  TTelegramHandler = class(TCarikWebModule)
  private
    FOnMessage: TOnMessageEvent;
    FOnSpam: TOnSpamEvent;
    forceRespond: boolean;
    replyFrom, replyFromUsername, replyFromName: string;

    function isWhiteListed(AUserName: string): boolean;
    function isMentioned(AMessage: string): boolean;
    function isReply(ASuffix: string = 'Bot'): boolean;
    function isReplyFromBot(ASuffix: string = 'Bot'): boolean;
    function isCallbackQueryExpired(ADateAsString: string): boolean;
    function isSapaMemberBaru(AGroupID: string): boolean;

    // Handler
    function groupUserKickRequestHandler(const IntentName: string; Params: TStrings): string;
    function groupUserBanRequestHandler(const IntentName: string; Params: TStrings): string;
    function kulgramTopicHandler(const IntentName: string; Params: TStrings): string;
    function kulgramStartHandler(const IntentName: string; Params: TStrings): string;
    function kulgramStopHandler(const IntentName: string; Params: TStrings): string;

    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function sendInlineKeyboard(ACaption: string; ADataAsJson: TJSONUtil): boolean;
    function sendKeyboard(ACaption: string; ADataAsJson: TJSONUtil): boolean;
  public
    //UserID, ChatID, ChatType,
    TELEGRAM: TTelegramIntegration;

    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;

    Property OnMessage : TOnMessageEvent Read FOnMessage Write FOnMessage;
    Property OnSpam : TOnSpamEvent Read FOnSpam Write FOnSpam;
  end;

implementation

uses common;

constructor TTelegramHandler.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;

  TELEGRAM := TTelegramIntegration.Create;
  TELEGRAM.Token := Config[TELEGRAM_TOKEN];
  if Token.IsNotEmpty then
    TELEGRAM.Token := Token;
  replyFrom := '';
  replyFromName := '';
  replyFromUsername := '';
end;

destructor TTelegramHandler.Destroy;
begin
  TELEGRAM.Free;
  inherited Destroy;
end;

function TTelegramHandler.isWhiteListed(AUserName: string): boolean;
var
  s: string;
  lst: TStrings;
begin
  Result := False;
  s := Config[TELEGRAM_WHITELIST];
  lst := Explode(s,',');
  if lst.Count > 0 then
  begin
    if lst.IndexOf(AUserName) >= 0 then
      Result := True;
  end;
  lst.Free;
end;

function TTelegramHandler.isMentioned(AMessage: string): boolean;
const
  NotAllowed: array[1..24] of string =
    ('.', ';', '/', '?', ':', ',', '=', '&', '#', '+',
    '<', '>', '"', '%', '{', '}', '|', '\', '^', '~', '[', ']', '`', ''''
    );
var
  s: String;
begin
  Result := False;
  s := AMessage.ToLower + ' ';
  s := ReplaceAll(s, NotAllowed, ' ');
  if pos('@' + BotName.ToLower + 'bot ', s) > 0 then
    Result := True;
  // force dectect as Bot  (____Bot)
  //if pos(' bot ', ' ' + LowerCase(Message) + ' ') > 0 then
  //  Result := True;
end;

function TTelegramHandler.isReply(ASuffix: string): boolean;
var
  s: string;
  json: TJSONUtil;
begin
  Result := False;
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Request.Content);
    s := json['message/reply_to_message/from/username'];
    //replyFrom := json['message/reply_to_message/from/id'];
    //try
    //  replyFromUsername := json['message/reply_to_message/from/username'];
    //except
    //end;
    replyFrom := TELEGRAM.ReplyFromID;
    replyFromUsername := TELEGRAM.ReplyFromUserName;

    replyFromName := string(trim(json['message/reply_to_message/from/first_name'] + ' '
      + json['message/reply_to_message/from/last_name'])).Trim;
    if not ASuffix.IsEmpty then
    begin
      if pos(ASuffix, s) > 0 then // if not from Bot
        Result := True;
    end
    else
      Result := True;
  except
  end;

  if Result then
  begin
    LogUtil.Add('u:'+replyFromName +'/'+ replyFromUsername, 'REPLY');
    if not replyFromUsername.IsEqualTo('CarikBot') then //TODO: make it configurable
      Result := False;;
    s := Request.Content.Replace(#13,'').Replace(#10,'');
    LogUtil.Add(s, 'REPLY');

    if Result then
      LogUtil.Add('--> action', 'REPLY');
  end;


end;

function TTelegramHandler.isReplyFromBot(ASuffix: string): boolean;
var
  currentUser: string;
  json: TJSONUtil;
begin
  Result := False;
  json := TJSONUtil.Create;
  json.LoadFromJsonString(Request.Content);
  try
    //replyFrom := json['message/reply_to_message/from/id'];
    //replyFromUsername := json['message/reply_to_message/from/username'];
    replyFrom := TELEGRAM.ReplyFromID;
    replyFromUsername := TELEGRAM.ReplyFromUserName;

    if replyFromUsername.IsExists(ASuffix) then
    begin
      Result := True;
      Exit;
    end;

    replyFromName := string(trim(json['message/reply_to_message/from/first_name'] + ' '
          + json['message/reply_to_message/from/last_name'])).Trim;
  except
  end;

  json.Free;
end;

// format: yyyymmddhhnnss
// ex: 20200419230151
function TTelegramHandler.isCallbackQueryExpired(ADateAsString: string
  ): boolean;
var
  dt: TDateTime;
begin
  Result := False;
  if ADateAsString.IsEmpty then
    Exit;
  if ADateAsString.Length <> 14 then
    Exit;

  ADateAsString := ADateAsString.Substring(0,4) + '-'
    + ADateAsString.Substring(4,2) + '-' + ADateAsString.Substring(6,2) + ' '
    + ADateAsString.Substring(8,2) + ':' + ADateAsString.Substring(10,2) + ':'
    + ADateAsString.Substring(12,2);

  try
    dt := ADateAsString.AsDateTime;
    if dt.MinutesDiff(Now) > CALLBACK_QUERY_TIMEOUT then
      Result := True;
  except
  end;
end;

function TTelegramHandler.isSapaMemberBaru(AGroupID: string): boolean;
var
  url, s: string;
  o: TJSONUtil;
begin
  Result := True;
  if AGroupID.IsEmpty then Exit;
  url := Config[GROUPINFO_URL];
  if url.IsEmpty then Exit;

  url += '&channel=telegram&id='+TELEGRAM.ChatID;
  s := file_get_contents(url);
  LogUtil.Add(AGroupID + ':' + s, 'SAPA');
  if s.IsEmpty then Exit;

  o := TJSONUtil.Create;
  o.LoadFromJsonString(s);

  try
    Result := o['data/options/welcome_member_collective'];
    Result := not Result;
  except
  end;

  o.Free;
end;

function TTelegramHandler.groupUserKickRequestHandler(const IntentName: string;
  Params: TStrings): string;
var
  userId, groupId: string;
begin
  if TELEGRAM.ReplyFromUserName = (SimpleBOT.BotName + 'Bot') then
  begin
    Result := SimpleBOT.GetResponse('GroupUserKickWhiteListed') + '...';
    exit;
  end;

  userId := Params.Values['UserId'];
  groupId := Params.Values['GroupId'];
  LogUtil.Add(groupId + '|' + userId, 'KICK');

  if TELEGRAM.IsAdmin() then
  begin

    // check apakah Bot punya permission
    if not TELEGRAM.IsAdminFromUsername(SimpleBOT.BotName + 'Bot', False) then
    begin
      Result := SimpleBOT.GetResponse('GroupUserKickNotAllowed') + '\n'
        + TELEGRAM.AdminListAsString;
      Exit;
    end;

    if TELEGRAM.IsAdmin(userId, False) then
    begin
      Result := 'Admin nihh..\n' + SimpleBOT.GetResponse('GroupUserKickWhiteListed') + '...';
      Exit;
    end;

    TELEGRAM.KickUser(groupId, userId, 'Requet by ' + TELEGRAM.FullName);
    if TELEGRAM.ResultText = '{"ok":true,"result":true}' then
    begin
      Result := 'User telah dikeluarkan dari group.';
      TELEGRAM.DeleteMessage(groupId, TELEGRAM.ReplyFromMessageID);
    end
    else
      Result := 'Saya tidak bisa menghapus user ini, saya colek admin yaa.\n' + TELEGRAM.AdminListAsString;
    ReportSpam(userId, replyFromName, TELEGRAM.UserID);
  end
  else
  begin
    if TELEGRAM.IsAdmin(userId, False) then
    begin
      Result := 'Ini Admin.\n' + SimpleBOT.GetResponse('GroupUserKickWhiteListed') + '...';
      Exit;
    end;
    Result := 'Saya teruskan ke temen² admin. \nColek ' + TELEGRAM.AdminListAsString;
    ReportSpam(userId, replyFromName, TELEGRAM.UserID);
  end;
end;

function TTelegramHandler.groupUserBanRequestHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := groupUserKickRequestHandler(IntentName, Params);
end;

function TTelegramHandler.kulgramTopicHandler(const IntentName: string;
  Params: TStrings): string;
var
  topic: string;
begin
  Result := carik_controller._NOTULEN_MSG_NOTPERMITTED;
  if not TELEGRAM.IsAdmin() then
    Exit;

  topic := Params.Values['topic_value'].Replace('"','');
  Carik.GroupData.WriteString(Carik.GroupName, 'topic', topic);
  Carik.GroupData.WriteString(Carik.GroupName, 'id', Carik.GroupChatID);

  if Carik.IsRecording then
  begin
    // TODO: direct save to file
  end;

  Result := 'Baik, topik saat ini *"' + topic.UcWords + '"*';
end;

function TTelegramHandler.kulgramStartHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := _NOTULEN_MSG_NOTPERMITTED;
  if not TELEGRAM.IsAdmin() then
    Exit;

  LogUtil.Add('starting ... ', 'carik');
  Carik.GroupData.WriteString(Carik.GroupName, 'name', Carik.GroupName);
  if Carik.Start then
  begin
    LogUtil.Add('--- ' + Carik.GroupName + ' recorded', 'notulen');
    Result := _NOTULEN_MSG_START + format(_NOTULEN_MSG_RECORDNUMBER, [Carik.RecordNumber]);
  end
  else
  begin
    Result := _NOTULEN_MSG_CANNOT_START;
  end;

end;

function TTelegramHandler.kulgramStopHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := _NOTULEN_MSG_NOTPERMITTED;
  if not TELEGRAM.IsAdmin() then
    Exit;
  Carik.Stop;
  Result := _NOTULEN_MSG_STOP;
end;

// Init First
procedure TTelegramHandler.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  if not _DEVELOPMENT_ then
    Response.ContentType := 'application/json';
end;

function TTelegramHandler.sendInlineKeyboard(ACaption: string; ADataAsJson: TJSONUtil
  ): boolean;
var
  captionText: string;
begin
  Result := False;
  if ADataAsJson.Data.Count = 0 then
    Exit;

  captionText := ACaption.Replace('\n', #10);
  if TELEGRAM.SendInlineKeyboard( TELEGRAM.ChatID, captionText, ADataAsJson) then
  begin
    Result := True;
  end;
end;

function TTelegramHandler.sendKeyboard(ACaption: string; ADataAsJson: TJSONUtil
  ): boolean;
var
  captionText: string;
begin
  Result := False;
  if ADataAsJson.Data.Count = 0 then
    Exit;

  captionText := ACaption.Replace('\n', #10);
  if TELEGRAM.SendKeyboard( TELEGRAM.ChatID, captionText, ADataAsJson) then
  begin
    Result := True;
  end;
end;

// GET Method Handler
procedure TTelegramHandler.Get;
begin
  OutputJson(0, 'Invalid Method');
end;

// POST Method Handler
procedure TTelegramHandler.Post;
var
  updateID, lastUpdateID: longint;
  i, j, spamScoreTotal: integer;
  s, imgTag, audioCaption, voiceFileName, mp3FileName, tmpStr, fileType, fileCaption, url: string;
  disableMarkdown, isHandled, localReplyDisable, canSendMessage, isEditMessage: boolean;
  forceSendMessage, forceReply: boolean;
  replyText: TStringList;
  json: TJSONUtil;
  filesAsArray: TJSONArray;
begin
  if _GET['_debug'] <> '1' then
    if not _DEVELOPMENT_ then CloseConnection('{}'); //ulil
  isHandled := False;
  MessengerMode := mmTelegram;
  ChannelId := 'telegram';
  updateID := 0;
  spamScoreTotal := 0;
  forceRespond := False;
  disableMarkdown := False;
  canSendMessage := True;
  isEditMessage := False;
  forceSendMessage := False;
  forceReply := false;
  if AppData.debug then
  begin
    s := Request.Content.Replace(#13,'').Replace(#10,'');
    LogUtil.Add(s, 'TELE');
  end;

  TELEGRAM.RequestContent := Request.Content;
  OriginalText := TELEGRAM.Text;
  //if TELEGRAM.ChatID = '-1001240569966' then
  //  LogUtil.Add(Request.Content.Replace(#13,'').Replace(#10,''), 'JAVASCRIPT');

  SessionController.SessionPrefix := 'telegram';
  SessionController.SessionSuffix := TELEGRAM.UserID;
  SessionController.ForceUniqueID := TELEGRAM.UserID;
  SessionController.StartSession;

  updateID := TELEGRAM.UpdateID;
  MessageID := TELEGRAM.MessageID;

  Carik.UserPrefix := 'tl';
  Carik.UserID := TELEGRAM.UserID;
  Carik.UserName := TELEGRAM.UserName;
  Carik.FullName := TELEGRAM.FullName;
  Carik.GroupChatID := TELEGRAM.ChatID;
  Carik.GroupName := TELEGRAM.GroupName;
  Carik.IsGroup := TELEGRAM.IsGroup;
  if TELEGRAM.FullName.IsEmpty then
  begin
    TELEGRAM.GetChatInfo(TELEGRAM.UserID);
    Carik.FullName := trim( TELEGRAM.ChatInfo['result/first_name'] + ' ' + TELEGRAM.ChatInfo['result/last_name']);
  end;
  SimpleBOT.SessionUserID := UniqueID;
  SimpleBOT.FirstSessionResponse := s2b(Config[TELEGRAM_BOT_FIRST_SESSION_RESPONSE]);


  if IsGlobalUserBlackListed(Carik.UserPrefix + '-' + Carik.UserID) then
  begin
    LogUtil.Add('user blacklist: ' + Carik.UserID + '|' + Carik.FullName,'TELE');
    Response.Content := '{"status":"group blaclisted"}';
    Exit;

  end;
  if TELEGRAM.IsGroup then
  begin
    if IsGlobalGroupBlackListed(Carik.UserPrefix + '-' + Carik.GroupChatID) then
    begin
      LogUtil.Add('group blacklist: ' + Carik.GroupChatID + '|' + Carik.GroupName,'TELE');
      Response.Content := '{"status":"group blaclisted"}';
      Exit;
    end;
  end;

  Text := TELEGRAM.Text;

  // check callback query
  if TELEGRAM.IsCallbackQuery then
  begin
    s := TELEGRAM.CallbackData.Values['_'];
    if isCallbackQueryExpired(s) then
    begin
      Text += #10#10'_*session expired_';
      if not TELEGRAM.IsTextMentionExists then
        TELEGRAM.EditMessage(TELEGRAM.ChatID, TELEGRAM.MessageID, Text);
      Response.Content := '{"status":"callback expired"}';
      Exit;
    end;
    if not TELEGRAM.CallbackData.Values['text'].IsEmpty then
    begin
      Text := TELEGRAM.CallbackData.Values['text'];
      if TELEGRAM.IsGroup then
        Text := '@' + BotName + 'Bot ' + Text;
    end;
    if TELEGRAM.CallbackData.Values['mode'] = 'replace' then
      isEditMessage := True;
    if TELEGRAM.CallbackData.Values['mode'] = 'edit' then
      isEditMessage := True;
    //if not TELEGRAM.IsGroup then
    //  isEditMessage := True;

    if TELEGRAM.IsGroup then
    begin
      Prefix := '';
      if TELEGRAM.FullName.IsNotEmpty then
      begin
        Prefix += '[@'+TELEGRAM.FullName.Trim+']';
      end else
      begin
        Prefix += '[@'+TELEGRAM.UserName.Trim+']';
      end;
      Prefix += '(tg://user?id='+TELEGRAM.UserID+')';
      Prefix += '\n';
    end;;

    //TODO: exec command
  end;

  {$include before.pas}

  localReplyDisable := s2b(Config[TELEGRAM_BOT_REPLY_DISABLE]);
  if (localReplyDisable or replyDisable) then
  begin
    LogChat(TELEGRAM_CHANNEL_ID, TELEGRAM.ChatID, TELEGRAM.GroupName, TELEGRAM.UserID, Carik.UserName, Carik.FullName, OriginalText, '', Carik.IsGroup, False, TELEGRAM.MessageID.ToInteger, 0, s2i(TELEGRAM.ReplyFromMessageID));
    die('{"state":"silent"}');
    exit;
  end;

  //todo: cara membuat
  // isImage
  if TELEGRAM.isImage(False) then
  begin
    if ((TELEGRAM.ImageCaption = '') and TELEGRAM.IsGroup) then
    begin
      Response.ContentType := 'application/json';
      Response.Content := '{"status":"no caption image"}';
      if Carik.IsRecording then
      begin
        Carik.GroupChatID := TELEGRAM.ChatID;
        Carik.GroupName := TELEGRAM.GroupName;
        Carik.RecordTelegramMessage(Request.Content);
        Response.Content := '{"status":"record image"}';
      end;
      Exit;
    end;
    Text := TELEGRAM.ImageCaption;
    if Text = '' then
      Text := CMD_FULL_IMAGE_ANALYZE;
    if Pos('translate', Text) > 0 then
    begin
      if TELEGRAM.IsGroup then
        Text := '@carikbot ' + CMD_IMAGE_TRANSLATION
      else
        Text := CMD_IMAGE_TRANSLATION;
    end;

    TELEGRAM.isImage(True);
    ImageID := TELEGRAM.ImageID;
    ImagePath := TELEGRAM.ImagePath;
    ImageURL := TELEGRAM.ImageURL;
  end;//-- isImage

  // Is Location
  if TELEGRAM.IsLocation then
  begin
    SimpleBOT.UserData['LOC_LAT'] := FloatToStr(TELEGRAM.LocationLatitude);
    SimpleBOT.UserData['LOC_LON'] := FloatToStr(TELEGRAM.LocationLongitude);
    SimpleBOT.UserData['LOC_NAME'] := TELEGRAM.LocationName;
    SimpleBOT.UserData['LOC_DATE'] := DateTimeToStr(Now);

    // still on topic, find location
    if ContextFocus <> '' then
    begin
      Text := ContextFocus + ' ' + SimpleBOT.UserData['LOC_LAT'] +
        ' ' + SimpleBOT.UserData['LOC_LON'] + ' ' + SimpleBOT.UserData['CONTEXT_DETAIL'];
      LogUtil.Add(Text, 'LOKASI');
    end;
  end;// Is Location

  //TODO: if emoticons
  Carik.UserPrefix := 'tl';
  Carik.UserID := TELEGRAM.UserID;
  Carik.UserName := TELEGRAM.UserName;
  Carik.FullName := TELEGRAM.FullName;
  Carik.GroupChatID := TELEGRAM.ChatID;
  Carik.GroupName := TELEGRAM.GroupName;
  Carik.IsGroup := TELEGRAM.IsGroup;
  if Carik.IsRecording then
  begin
    Carik.RecordTelegramMessage(Request.Content); //todo: disable it, use db
  end;

  // check last telegram session
  lastUpdateID := s2i(SimpleBOT.UserData[BotID+'_UPDATE_ID']);
  if updateID < lastUpdateID then
  begin
    Response.Content := '{"status":"expired"}';
    Exit;
  end;
  SimpleBOT.UserData[BotID+'_UPDATE_ID'] := updateID.ToString;

  // only for telegram group
  if TELEGRAM.IsGroup then
  begin
    if Text = '' then
      Text := TELEGRAM.ImageCaption;

    // check document
    if TELEGRAM.IsDocument then
    begin
      // dangerous file
      if ExtractFileExt(TELEGRAM.FileName.ToLower) = '.exe' then
      begin
        forceSendMessage := True;
        forceReply := True;
        Text := 'lapor file berbahaya';
        Suffix := TELEGRAM.GroupAdminList(TELEGRAM.ChatID);
      end;
    end;

    if not isReply then
    begin
      if not isMentioned(Text) then
      begin
        if TELEGRAM.IsUserLeft then
        begin
          LogJoin(TELEGRAM_CHANNEL_ID, TELEGRAM.ChatID, '', TELEGRAM.LeftUserID, '', '', TELEGRAM.InvitedBy, False, True);
          LogUtil.Add('left:'+TELEGRAM.ChatID+'/'+TELEGRAM.UserID, 'USER');
          Response.Content := '{"status":"userleft"}';
          Exit;
        end;
        if TELEGRAM.IsInvitation then
        begin
          s := Request.Content.Replace(#13,'').Replace(#10,'');
          LogUtil.Add(s, 'JOIN');

          Carik.IsInvitation := True;
          if IsSuspected(TELEGRAM.InvitedUserId, TELEGRAM.InvitedFullName) then
          begin
            s := TELEGRAM.GroupAdminList(TELEGRAM.ChatID);
            s := ReplaceAll(s,['.','-','_',',','|'],'').Replace('  ',' ').Trim;
            s := s.Replace('[]','[user]');
            Suffix := 'hhmm... Carik mencium sesuatu di sini.\n';
            Suffix := Suffix + s + '\n';

            Suffix := '\n[Halo '+TELEGRAM.InvitedFullName+'](tg://user?id='+ TELEGRAM.InvitedUserId + ') silakan ubah nama anda yang lebih familiar dibaca agar tidak dianggap sebagai spammer.\n';

          end;

          if not isSapaMemberBaru(TELEGRAM.ChatID) then
          begin
//            LogUtil.Add('abaikan member ' + Carik.GroupName, 'MEMBER');
            if not Suffix.IsEmpty then
            begin
              Text := '/invitationspammer ' + TELEGRAM.InvitedUserName + ' | ' +
                TELEGRAM.InvitedFullName;
            end
            else
            begin
              LogJoin(TELEGRAM_CHANNEL_ID, TELEGRAM.ChatID, TELEGRAM.GroupName, TELEGRAM.InvitedUserId, TELEGRAM.InvitedUserName, TELEGRAM.InvitedFullName, TELEGRAM.InvitedBy, RestrictUser);
              Response.Content := '{"status":"invitation"}';
              Exit;
            end;
          end;
//          LogUtil.Add('sapa member ' + Carik.GroupName, 'MEMBER');

          SimpleBOT.AdditionalParameters.Values['GroupName'] := TELEGRAM.GroupName;
          Carik.FullName := TELEGRAM.InvitedFullName.Replace('.','');
          Carik.FullName := Carik.FullName.Replace('/','');
          Carik.FullName := Carik.FullName.Replace('_','');
          Carik.FullName := ReplaceAll(Carik.FullName,['.','-','_',',','|'],'').Replace('  ',' ').Trim;
          Carik.InvitedUserId := TELEGRAM.InvitedUserId;
          Carik.InvitedUserName := TELEGRAM.InvitedUserName;
          Carik.InvitedFullName := TELEGRAM.InvitedFullName;
          Carik.InvitedFullName := ReplaceAll(Carik.InvitedFullName,['.','-','_',',','|'],'').Replace('  ',' ').Trim;

          Text := '/invitation ' + TELEGRAM.InvitedUserName + ' | ' +
            Carik.FullName;
          if TELEGRAM.InvitedUserName[1] = '+' then
          begin
            if not Carik.FullName.IsEmpty then
              Text := '/invitation ' + Carik.FullName + ' | ' +
                Carik.FullName;
          end;

          if isBlackListed( TELEGRAM.InvitedUserName, TELEGRAM.InvitedUserId) then
          begin
            Text := '/invitationspammer ' + TELEGRAM.InvitedUserName + ' | ' +
              TELEGRAM.InvitedFullName;
            RestrictUser := True;
            if not Suffix.IsEmpty then
              Suffix := TELEGRAM.GroupAdminList(TELEGRAM.ChatID);
            //Suffix := '\n@' + Suffix.Replace(',', ', @');
          end;

          InvitedUserName := TELEGRAM.InvitedUserName;
          InvitedFullName := TELEGRAM.InvitedFullName;
          if (Pos('_', InvitedUserName) > 0) then disableMarkdown := True;
          if (Pos('_', InvitedFullName) > 0) then disableMarkdown := True;

          //if InvitedUserName[1] = '+' then
          //  Suffix := '\n['+TELEGRAM.InvitedFullName+'](tg://user?id='+ TELEGRAM.InvitedUserId + ')';

          if TELEGRAM.InvitedUserName = BotName + 'Bot' then
          begin
            Carik.Invited;
            LogGroupAdd(TELEGRAM_CHANNEL_ID, TELEGRAM.ChatID, TELEGRAM.GroupName, TELEGRAM.UserID, TELEGRAM.UserName, TELEGRAM.FullName);
          end else begin
            LogJoin(TELEGRAM_CHANNEL_ID, TELEGRAM.ChatID, TELEGRAM.GroupName, TELEGRAM.InvitedUserId, TELEGRAM.InvitedUserName, TELEGRAM.InvitedFullName, TELEGRAM.InvitedBy, RestrictUser);
            LogUtil.Add(Request.Content.Replace(#13,'').Replace(#10,''), 'JOIN');
          end;
          exit; delete
        end
        else
        begin
          if not forceRespond then
          begin
            if isTriggeredText(Text) then
            begin
              //
            end
            else
            if ((Pos('spam', Text.ToLower) = 1)
              or(Pos('@admin', Text.ToLower) = 1)
              or (Text.ToLower.IsExists('/scam'))
              or (Text.ToLower.IsExists('/admin'))
              or (Text.ToLower.IsExists('/report'))
              or (Text.ToLower.IsExists('/spam'))
            )
            and (not isReplyFromBot('CarikBot')) then //TODO: make it configurable
            begin
              if not isWhiteListed(replyFromUsername) then
              begin
                if (Pos('spam', Text.ToLower) = 1) then
                begin
                  s := ReportSpam(replyFrom, replyFromName, TELEGRAM.UserID);
                  if s.IsNotEmpty then
                    LogUtil.Add(s, 'SPAM');
                end;

                Text := 'spamreport ' + replyFrom;
                Suffix := TELEGRAM.GroupAdminList(TELEGRAM.ChatID);
                //Suffix := '\n@' + Suffix.Replace(',', ', @');
                LogUtil.Add(TELEGRAM.ChatID + ':' + TELEGRAM.ReplyFromUserID + ': ' + TELEGRAM.ReplyFromUserName, 'SPAM');
                AutoDeleteMessage := 24*60;
                ToggleSpammer := True;
              end else
              begin
                Text := 'spamreportwhitelisted ' + replyFromName;
                LogUtil.Add(TELEGRAM.UserID + '/' + replyFrom + ': ' + replyFromName, 'SPAM-whitelist');
              end;
            end
            else
            begin
              // general chat in group

              //todo: get group info, GroupInfo['something']
              //if (TELEGRAM.IsPicture or isURL(Text)) and Assigned(FOnSpam) then //simple force checking
              if Assigned(FOnSpam) then //simple force checking
              begin
                //if Carik.isSpamChecking then
                s := '';
                if s.IsNotEmpty then //TODO: fix function isSpamChecking
                begin
                  spamScoreTotal := spamScoreTotal + SpamScore(Carik.UserID, Text, TELEGRAM.IsPicture);
                  //spamScoreTotal := spamScoreTotal + SpamScore(Carik.UserID, Text, True);//force check
                  //spamScoreTotal := 0;
                  if spamScoreTotal >= SPAM_SCORE_THRESHOLD then
                  begin
                    isHandled := False;
                    s := FOnSpam(Text, spamScoreTotal, isHandled);
                    if isHandled then
                    begin
                      TELEGRAM.SendMessage(TELEGRAM.ChatID, s, MessageID);
                      LogChat(TELEGRAM_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName, Carik.UserID, Carik.UserName, Carik.FullName, '', s, True, False, s2i(TELEGRAM.MessageID), s2i(TELEGRAM.ResultMessageID), s2i(TELEGRAM.ReplyFromMessageID));
                      LogUtil.Add(TELEGRAM.ResultText, 'SPAM-SENT'); //todo: remove
                    end;
                    //die('spamming ...: ' + Carik.UserID + '/' + Carik.FullName);
                  end;
                end;

              end;

              s := '';
              if not ImageID.IsEmpty then
                s := '[img='+ImagePath+']';

              tmpStr := Text.ToLower;
              if tmpStr.IsPregMatch('^(kick|tendang)') then
              begin
                Text := '_groupkickrequest ' + TELEGRAM.ChatID + ' '  + TELEGRAM.ReplyFromID;
                //if TELEGRAM.ReplyFromUserName = (SimpleBOT.BotName + 'Bot') then
                //  Text := '_gropkickwhitelisted';
                forceSendMessage := True;
                AutoDeleteMessage := 24*60;
              end;
              if tmpStr.IsPregMatch('^(ban|banned)$') then
              begin
                Text := '_groupbanrequest ' + TELEGRAM.ChatID + ' '  + TELEGRAM.ReplyFromID;
                if TELEGRAM.ReplyFromUserName = (SimpleBOT.BotName + 'Bot') then
                  Text := '_gropkickwhitelisted';
                forceSendMessage := True;
                AutoDeleteMessage := 24*60;
              end;

              // Default LogChat
              if not forceSendMessage then
              begin
                s := s + OriginalText;
                LogChat(TELEGRAM_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName, Carik.UserID, Carik.UserName, Carik.FullName, s, '', True, False, MessageID.ToInteger, 0, s2i(TELEGRAM.ReplyFromMessageID));
                Response.Content := '{"status":"nomention"}';
                Exit;
              end;

            end;
          end;//forceRespond
          LogUtil.Add('noz', 'CHECK');

        end;
      end;

    end;

    if TELEGRAM.IsReply then
    begin
    end;//if TELEGRAM.IsReply

  end;//-- if TELEGRAM.IsGroup

  //ulil check collective

  // remove mention from text
  Text := Text.ToLower;
  Text := Text.Replace('@' + LowerCase(BotName + '_Bot'),'');
  Text := Text.Replace('@' + LowerCase(BotName + 'Bot'),'');
  Text := Trim(Text);
  if Text = '' then
  begin
    Response.Content := '{"status":"empty"}';
    Exit;
  end;

  //remove LogUtil.Add('A: ' + TimeUsage.ToString + ' | ' + Text, 'LD');
  SimpleBOT.TrimMessage := True;
  // TODO: REMOVE - force
  SimpleBOT.FirstSessionResponse := False;
  SimpleBOT.SecondSessionResponse := False;

  SimpleBOT.UserData['Name'] := TELEGRAM.UserName;
  SimpleBOT.UserData['FullName'] := TELEGRAM.FullName;

  //TODO: add to other platform
  SimpleBOT.AdditionalParameters.Values['UserID'] := 'tl-' + TELEGRAM.UserID; //TODO: tambahkan ke messenger lain
  SimpleBOT.AdditionalParameters.Values['ChatID'] := 'tl-' + TELEGRAM.ChatID; //TODO: tambahkan ke messenger lain
  SimpleBOT.AdditionalParameters.Values['FullName'] := TELEGRAM.FullName; //TODO: tambahkan ke messenger lain
  SimpleBOT.AdditionalParameters.Values['full_name'] := TELEGRAM.FullName;
  SimpleBOT.AdditionalParameters.Values['MessageID'] := MessageID;
  if TELEGRAM.IsInvitation then
  begin
    SimpleBOT.AdditionalParameters.Values['UserID'] := TELEGRAM.InvitedUserId;
    SimpleBOT.AdditionalParameters.Values['FullName'] := TELEGRAM.InvitedFullName;
    SimpleBOT.AdditionalParameters.Values['full_name'] := TELEGRAM.InvitedFullName;
  end;
  if TELEGRAM.IsGroup then
  begin
    SimpleBOT.AdditionalParameters.Values['GroupID'] := 'tl-' + TELEGRAM.ChatID;
    SimpleBOT.AdditionalParameters.Values['GroupName'] := TELEGRAM.GroupName;
  end;

  // check if any custom handler
  //remove LogUtil.Add('B: ' + TimeUsage.ToString + ' | ' + Text, 'LD');
  if Assigned(FOnMessage) then
  begin
    isHandled := False;
    replyText := TStringList.Create;
    replyText.Text := FOnMessage(OriginalText, isHandled);
    for j:=0 to SimpleBOT.AdditionalParameters.Count-1 do
    begin
      s := SimpleBOT.AdditionalParameters.Names[j];
      SimpleBOT.SimpleAI.Parameters.Values[s] := SimpleBOT.AdditionalParameters.ValueFromIndex[j]; //manual add parameter
    end;
  end;

  if FormInputHandler() then
  begin
    isHandled := True;
  end;

  if not isHandled then
    Text := GenerateTextFromCustomActionOption(Text);
  if IsMuted then
  begin
    LogChat(TELEGRAM_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName, Carik.UserID, Carik.UserName, Carik.FullName, OriginalText, '',
      Carik.IsGroup, False, MessageID.ToInteger, 0, s2i(TELEGRAM.ReplyFromMessageID));
    OutputJson(11, 'muted: ' + SimpleBOT.UserData['mute']);
  end;

  Text := Text.Replace(',','');
  if preg_match('(\/start )(.+)$', Text) then
  begin
    Text := Text.Replace('_',' ').Replace('/start ', '').Trim;
    TELEGRAM.SendMessage(TELEGRAM.ChatID, 'Anda mengirimkan perintah "*'+Text+'*" dari tautan luar.\nMohon ditunggu');
  end;


  if not isHandled then
  begin
    BotInit;
    SimpleBOT.Handler['group_user_kick_request'] := @groupUserKickRequestHandler;
    SimpleBOT.Handler['group_user_ban_request'] := @groupUserBanRequestHandler;
    SimpleBOT.Handler['carik_topic'] := @kulgramTopicHandler;
    SimpleBOT.Handler['carik_start'] := @kulgramStartHandler;
    SimpleBOT.Handler['carik_stop'] := @kulgramStopHandler;

    if TriggeredText = '' then
      Response.Content := ProcessText(Text)
    else
      Response.Content := ProcessText(TriggeredText);
  end else begin
    SimpleBOT.SimpleAI.ResponseText.Text := replyText.Text;
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
    replyText.Free
  end;

  if IsCommand(SimpleBOT.SimpleAI.ResponseText.Text) then
  begin
    SimpleBOT.SimpleAI.ResponseText.Text :=
      ExecCommand(SimpleBOT.SimpleAI.ResponseText.Text);
    if SimpleBOT.SimpleAI.ResponseText.Text = '' then
      SimpleBOT.SimpleAI.ResponseText.Text :=
        SimpleBOT.GetResponse('DataTidakAdaResponse');
    //TODO: generate user data and object
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;

  imgTag := '';
  if not ImageID.IsEmpty then
    imgTag := '[img='+ImagePath+']';

  if not TELEGRAM.IsGroup then
  begin
    if IsUserSuspended( TELEGRAM_CHANNEL_ID, Carik.UserID) then
    begin
      if AppData.debug then
         LogUtil.Add( Carik.UserID + ' suspended', 'USERCHECK');
      Exit;
    end;
  end;

  // custom action: button, quickreply
  if IsCustomAction then
  begin
    SaveActionToUserData(CustomReplyType, CustomReplyData.Data);
  end;

  if not Prefix.IsEmpty then
  begin
    SimpleBOT.SimpleAI.ResponseText[0] := Prefix + SimpleBOT.SimpleAI.ResponseText[0];
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;
  if not Suffix.IsEmpty then
  begin
    if SimpleBOT.SimpleAI.ResponseText.Count = 0 then
      SimpleBOT.SimpleAI.ResponseText.Add(Suffix)
    else
    begin
      j := SimpleBOT.SimpleAI.ResponseText.Count-1;
      SimpleBOT.SimpleAI.ResponseText[j] := SimpleBOT.SimpleAI.ResponseText[j] + Suffix;
    end;
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;

  if not SimpleBOT.IsMarkup then
  begin
    TELEGRAM.ParseMode := '';
    SimpleBOT.SimpleAI.ResponseText[0] := RemoveMarkDown(SimpleBOT.SimpleAI.ResponseText[0]);
  end;
  if disableMarkdown then
    TELEGRAM.ParseMode := '';

  if _DEVELOPMENT_ then Exit;//ulil

  //TODO: rekam pembicaraan sendiri

  if TELEGRAM.IsGroup then
  begin
    if Carik.IsDisabled then
    begin
      if not forceRespond then
      begin
        Response.ContentType := 'application/json';
        Response.Content := '{"status":"silent"}';
        Exit;
      end;
    end;
  end;

  //Exec Command
  {
  if Carik.IsCommand(SimpleBOT.SimpleAI.ResponseText.Text) then
  begin
    SimpleBOT.SimpleAI.ResponseText.Text :=
      Carik.ExecCommand(SimpleBOT.SimpleAI.ResponseText.Text);
    if SimpleBOT.SimpleAI.ResponseText.Text = '' then
      SimpleBOT.SimpleAI.ResponseText.Text :=
        SimpleBOT.GetResponse('DataTidakAdaResponse');
    //TODO: generate user data and object
    Response.Content := SimpleBOT.SimpleAI.ResponseJson;
  end;
  }

  if SimpleBOT.SimpleAI.Action = '' then // no mention reply, if no 'action'
    MessageID := '';
  if SimpleBOT.SimpleAI.Action = 'telegram_menu' then
    MessageID := '';
  if forceReply then
    MessageID := TELEGRAM.MessageID;

  //remove LogUtil.Add('D: ' + TimeUsage.ToString + ' | ' + Text, 'LD');
  if SimpleBOT.SimpleAI.ResponseText.Count > 0 then
  begin

    // if speaking mode
    if SpeakingMode then
    begin
      if Config[CARIK_TTS_URL] <> '' then
      begin
        audioCaption := SimpleBOT.SimpleAI.ResponseText[0];
        audioCaption := copy(audioCaption, 0, pos('\n', audioCaption) - 1);

        s := PrepareTextToSpeech(SimpleBOT.SimpleAI.ResponseText.Text);
        if s <> '' then
          TELEGRAM.SendAudio(TELEGRAM.ChatID, Config[CARIK_TTS_URL] + s,
            audioCaption, MessageID);
      end;
    end;

    // Send Message
    if canSendMessage then
    begin
      if isEditMessage then
      begin
        LogUtil.Add(Request.Content, 'debug-0');
        LogUtil.Add(TELEGRAM.CallbackInlineKeyboard.AsJSON, 'debug-1');
        json := TJSONUtil.Create;
        s := UCStoString(Request.Content);
        LogUtil.Add(s, 'debug-2');
        json.LoadFromJsonString(s, False);
        LogUtil.Add(json.AsJSON, 'debug-3');
        json.free;

        TELEGRAM.EditMessage(TELEGRAM.ChatID, TELEGRAM.MessageID,
          SimpleBOT.SimpleAI.ResponseText[0].Replace('\n',#10),
          TELEGRAM.CallbackInlineKeyboard);
      end else
      begin
        if IsCustomAction then
        begin
          if CustomReplyType = 'button' then
          begin
            if sendInlineKeyboard(SimpleBOT.SimpleAI.ResponseText.Text, CustomReplyData) then
            begin
              //
            end;
          end;
          if CustomReplyType = 'quickreply' then
          begin
            if TELEGRAM.IsGroup then
            begin
              if sendInlineKeyboard(SimpleBOT.SimpleAI.ResponseText.Text, CustomReplyData) then
              begin
                //
              end;
            end else
            begin
              if sendKeyboard(SimpleBOT.SimpleAI.ResponseText.Text, CustomReplyData) then
              begin
                //
              end;
            end;
          end;
          if CustomReplyType = 'card' then
          begin
            SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
              + '\n\n' + CustomActionAsText.Replace(#10,'\n');
            TELEGRAM.SendMessage(TELEGRAM.ChatID, SimpleBOT.SimpleAI.ResponseText[0], MessageID);
          end;
          if ((CustomReplyType = 'menu') or (CustomReplyType = 'list')) then
          begin
            if not CustomActionAsText.IsEmpty then
            begin
              SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
                + '\n' + ACTION_CAPTION + '\n' + CustomActionAsText.Replace(#10,'\n');
              if CustomActionSuffix.IsNotEmpty then
                SimpleBOT.SimpleAI.ResponseText.Text := SimpleBOT.SimpleAI.ResponseText.Text.Trim
                  + '\n' + CustomActionSuffix.Replace(#10,'\n');
              Response.Content := SimpleBOT.SimpleAI.ResponseJson;
            end;
            TELEGRAM.SendMessage(TELEGRAM.ChatID, SimpleBOT.SimpleAI.ResponseText[0], MessageID);
          end;
          if CustomReplyType = 'form' then
          begin
            if not TELEGRAM.SendMessage(TELEGRAM.ChatID, SimpleBOT.SimpleAI.ResponseText[0], MessageID) then
            begin
              LogUtil.Add('FORM: '+SimpleBOT.SimpleAI.ResponseText[0], 'DEBUG');
            end;
          end;
          if CustomReplyType = 'none' then
          begin
            if not TELEGRAM.SendMessage(TELEGRAM.ChatID, SimpleBOT.SimpleAI.ResponseText[0], MessageID) then
            begin
              LogUtil.Add('none: '+SimpleBOT.SimpleAI.ResponseText[0], 'DEBUG');
            end;
          end;


          // check autoprune
          if AutoPrune then
          begin
            s := GetPrune;
            if not s.IsEmpty then
            begin
              TELEGRAM.DeleteMessage(TELEGRAM.ChatID, s);
            end;
            SavePrune(TELEGRAM.ResultMessageID);
          end;// /AutoPrune

        end
        else
        begin

          // Global/Default Sender
          If TELEGRAM.IsInvitation then
            MessageID := '';
          //s := AnsiToUtf8(SimpleBOT.SimpleAI.ResponseText[0]);
          s := SimpleBOT.SimpleAI.ResponseText[0];
          TELEGRAM.SendMessage(TELEGRAM.ChatID, s, MessageID);
          LogUtil.Add(TELEGRAM.ChatID + '/' + TELEGRAM.UserID + '('+TELEGRAM.GroupName+')::' + OriginalText + ' |-> ' + s, 'SENTLOG1');
          LogUtil.Add(Request.Content.Replace(#13,'').Replace(#10,''), 'SENTLOG2');
        end;// /IsCustomAction
      end;
      if AppData.debug then
      begin
        LogUtil.Add(Carik.UserName + ': ' + TELEGRAM.ResultText, 'RESULT');
      end;
    end;

    if TELEGRAM.ResultCode <> 200 then
    begin
      s := TELEGRAM.ChatID + '['+TELEGRAM.GroupName+']: ';
      LogUtil.Add(s + TELEGRAM.ResultText, '--TELE#1');
      LogUtil.Add(s + SimpleBOT.SimpleAI.ResponseText[0], '--TELE#2');
    end;
    if SimpleBOT.SimpleAI.ResponseText.Count > 1 then
    begin
      for j := 1 to SimpleBOT.SimpleAI.ResponseText.Count - 1 do
      begin
        s := SimpleBOT.SimpleAI.ResponseText[j];
        if s <> '' then
        begin
          if not TELEGRAM.SendMessage(TELEGRAM.ChatID, s, '') then
          begin
            LogUtil.Add('regular: '+SimpleBOT.SimpleAI.ResponseText[0], 'DEBUG');
          end;
          LogUtil.Add(TELEGRAM.ChatID + '/' + TELEGRAM.UserID + '('+TELEGRAM.GroupName+'):' + OriginalText + '|' + s, 'SENTLOG3');
        end;
        // TODO: rekam percakapan si BOT

      end;
    end;

  end;// if SimpleBOT.SimpleAI.ResponseText.Count > 0
  //remove LogUtil.Add('E: ' + TimeUsage.ToString + ' | ' + Text, 'LD');

  if SendVenue then
  begin
    TELEGRAM.SendVenue(TELEGRAM.ChatID, VenueName, VenueAddress,
      VenueLatitude, VenueLongitude, '');
  end;

  if SendAudio then
  begin
    TELEGRAM.SendAudio(TELEGRAM.ChatID, FileURL, Caption, MessageID);
  end;

  if SendPhoto then
  begin
    TELEGRAM.SendPhotoFromURL( TELEGRAM.ChatID, FileURL, ImageCaption, MessageID);
    LogUtil.Add(TELEGRAM.ResultText, 'IMG');
    if TELEGRAM.ResultCode <> 200 then
    begin
      TELEGRAM.SendMessage( TELEGRAM.ChatID, 'Maaf, gambar tidak berhasil dikirimkan', MessageID);
    end;
  end;

  // send files
  try
    filesAsArray := SimpleBOT.SimpleAI.CustomReply.ValueArray['action/files'];
    if filesAsArray.Count > 0 then
    begin
      for i := 0 to filesAsArray.Count-1 do
      begin
        fileType := SimpleBOT.SimpleAI.CustomReply['action/files['+i.ToString+']/type'];
        if fileType = 'audio' then
        begin
          url := SimpleBOT.SimpleAI.CustomReply['action/files['+i.ToString+']/url'];
          fileCaption := SimpleBOT.SimpleAI.CustomReply['action/files['+i.ToString+']/caption'];
          TELEGRAM.SendAudio(TELEGRAM.ChatID, url, fileCaption, MessageID);
        end;
      end;
    end;
  except
  end;

  {
  if KickUser then
  begin
    TELEGRAM.KickUser(TELEGRAM.ChatID, TELEGRAM.UserID, 'spammer');
  end;
  if RestrictUser then
  begin
    TELEGRAM.RestrictUser(TELEGRAM.ChatID, TELEGRAM.UserID, 0);
    LogUtil.Add('RESTRICT', TELEGRAM.ResultText);
  end;
  }

  s := imgTag+OriginalText;
  if TELEGRAM.IsCallbackQuery then
  begin
    s := 'callback|'+SimpleBOT.SimpleAI.IntentName;
  end;
  LogChat(TELEGRAM_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName,
    Carik.UserID, Carik.UserName, Carik.FullName, s, SimpleBOT.SimpleAI.ResponseText.Text,
    Carik.IsGroup, True, TELEGRAM.MessageID.ToInteger, s2i(TELEGRAM.ResultMessageID),
    s2i(TELEGRAM.ReplyFromMessageID));

  if Carik.IsGroup then
  begin
    //LogChat(TELEGRAM_CHANNEL_ID, Carik.GroupChatID, Carik.GroupName,
    //  '-1', TELEGRAM.ResultMessageID, SimpleBOT.SimpleAI.ResponseText.Text, '',
    //  Carik.IsGroup, False);
  end;

  s := Text;
  if TELEGRAM.IsCallbackQuery then
  begin
    s := 'callback|'+SimpleBOT.SimpleAI.IntentName;
  end;
  Analytics('telegram', SimpleBOT.SimpleAI.IntentName, s, 'tl-' + Carik.UserID);
  Response.ContentType := 'application/json';

  if AutoDeleteMessage > 0 then
  begin
    //auto delete message
  end;
end;

initialization
  //SessionController.SessionID := 'ztelegram';


end.
