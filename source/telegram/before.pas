
  //TODO: DELETE old file

  // isVoice
  if TELEGRAM.IsVoice then
  begin
    Text := '';
    TELEGRAM.SendMessage(TELEGRAM.ChatID, SimpleBOT.GetResponse('VoiceWaiting'),
      TELEGRAM.MessageID);

    s := TELEGRAM.GetFilePath(TELEGRAM.VoiceID);
    if s = '' then
    begin
      Response.ContentType := 'application/json';
      Response.Content := '{"status":"voice file not found"}';
      Exit;
    end;
    voiceFileName := VOICE_TMP_PATH + ExtractFileName(s);
    if TELEGRAM.DownloadFile(s, voiceFileName) then
    begin
      if FileExists(voiceFileName) then
      begin

        // convert ogg to mp3
        mp3FileName := StringReplace(voiceFileName, '.oga', '.mp3', [rfReplaceAll]);
        if FileExists(mp3FileName) then
          DeleteFile(mp3FileName);
        // ffmpeg -i file_2313.oga -ac 1 output.mp3
        if Exec(Config[FFMPEG_PATH], ['-i', voiceFileName, '-ac', '1', mp3FileName],
          s, swoNone) then
        begin
          // speech to text
          LogUtil.Add(mp3FileName, 'S2T');
          with TWitAiIntegration.Create do
          begin
            Token := Config[WITAI_TOKEN];
            //ContentType := TELEGRAM.VoiceType; use default
            Text := SpeechToText(mp3FileName);
            LogUtil.Add('text: ' + Text, 'S2T');
            Free;
          end;
        end; //-- convert ogg to mp3

        if Text <> '' then
        begin
          s := SimpleBOT.GetResponse('VoiceResult');
          s := format(s, [Text]);
          TELEGRAM.EditMessage(TELEGRAM.ChatID, TELEGRAM.ResultMessageID, s);
          MessageID := TELEGRAM.ResultMessageID;
          forceRespond := True;
        end
        else
        begin
          s := SimpleBOT.GetResponse('VoiceResultNone');
          s := StringReplace(s, '%username%', TELEGRAM.UserName, [rfReplaceAll]);
          TELEGRAM.EditMessage(TELEGRAM.ChatID, TELEGRAM.ResultMessageID, s);
          MessageID := TELEGRAM.ResultMessageID;
          Exit;
        end;
      end;

    end;
  end;//-- isVoice

