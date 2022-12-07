const
  CANCEL = 'CANCEL';
  DEFAULT_CHANNEL_ID = '1';
  TELEGRAM_CHANNEL_ID = '2';
  FACEBOOK_CHANNEL_ID = '3';
  LINE_CHANNEL_ID = '4';
  WHATSAPP_CHANNEL_ID = '5';
  SKYPE_CHANNEL_ID = '6';
  SLACK_CHANNEL_ID = '7';
  ANDROID_CHANNEL_ID = '8';
  IOS_CHANNEL_ID = '9';
  INSTAGRAM_CHANNEL_ID = '10';
  TELEGRAM_USERBOT_CHANNEL_ID = '11';

  WHATSAPP_BUSINESS_CHANNEL_ID = '12';
  SMS_CHANNEL_ID = '13';
  TWITTER_CHANNEL_ID = '14';
  DUALSPACE_CHANNEL_ID = '15';
  SIGNAL_CHANNEL_ID = '16';
  BIP_CHANNEL_ID = '17';
  ZOOM_CHANNEL_ID = '17';
  DISCORD_CHANNEL_ID = '18';

  YES_COMMAND : array [0..4] of string = ('YES', 'YA', 'OK', 'Y', 'SUDAH');
  NO_COMMAND : array [0..5] of string = ('NO', 'N', 'TIDAK', 'TDK', 'T', 'BELUM');

  BOTNAME_DEFAULT = 'Carik';
  CONFIG_BOTNAME = 'ai/default/name';
  CONFIG_CLIENT_ID = 'ai/default/client_id';
  CONFIG_REPLY_DISABLE = 'ai/default/reply_disable';
  CONFIG_FIRST_SESSION_RESPONSE = 'ai/default/first_session_response';

  KNOWLEDGEBASE_URL = 'services/knowledgebase_url';
  CHATLOG_URL = 'services/chatlog_url';
  JOINLOG_URL = 'services/joinlog_url';
  GROUPADDLOG_URL = 'services/groupaddlog_url';
  GROUPMEMBER_URL = 'services/groupmember_url';
  GROUPINFO_URL = 'services/groupinfo_url';
  BLACKLISTCHECK_URL = 'services/blacklistcheck_url';
  BLACKLISTADD_URL = 'services/blacklistadd_url';

  USERSTATUS_URL = 'ai/default/user_status/url';
  USERSTATUS_CLIENTID = 'ai/default/user_status/client_id';
  UNKNOWN_STATS_URL = 'ai/default/unknown_stats_url';
  EXTERNAL_NLP_ENABLE = 'ai/default/external_nlp/enable';
  EXTERNAL_NLP_URL = 'ai/default/external_nlp/url';
  EXTERNAL_NLP_USEGPT = 'ai/default/external_nlp/use_gpt';
  SUGGESTION_ENABLE = 'suggestion/enable';
  SUGGESTION_URL = 'suggestion/url';

  UNKNOWNCHAT_RETRIES = 'ai/default/unknown_chat/maximum_retries';
  UNKNOWNCHAT_CALLBACK = 'ai/default/unknown_chat/callback_intent';
  UNKNOWNCHAT_MUTE_DURATION = 'ai/default/unknown_chat/mute_duration';

  CARIKSEARCH_URL = 'carik_search/url';
  CARIKSEARCH_TOKEN = 'carik_search/token';

  CLARIFAI_TOKEN = 'clarifai/default/token';
  CLARIFAI_CLIENTID = 'clarifai/default/client_id';
  CLARIFAI_CLIENTSECRET = 'clarifai/default/client_secret';

  CUSTOMVISION_KEY = 'customvision/default/key';
  CUSTOMVISION_URL = 'customvision/default/url';

  TELEGRAM_TOKEN = 'telegram/default/token';
  TELEGRAM_WHITELIST = 'telegram/default/whitelist';
  TELEGRAM_BOT_REPLY_DISABLE = 'telegram/default/reply_disable';
  TELEGRAM_BOT_FIRST_SESSION_RESPONSE = 'telegram/default/first_session_response';
  FACEBOOK_TOKEN = 'facebook/default/token';
  FACEBOOK_BOT_REPLY_DISABLE = 'facebook/default/reply_disable';
  FACEBOOK_BOT_FIRST_SESSION_RESPONSE = 'facebook/default/first_session_response';
  LINE_TOKEN = 'line/default/token';
  LINE_BOT_REPLY_DISABLE = 'line/default/reply_disable';
  LINE_BOT_FIRST_SESSION_RESPONSE = 'line/default/first_session_response';

  WITAI_TOKEN = 'witai/default/token';
  GOOGLE_KEY = 'google/default/key';
  GOOGLEANALYTICS_TRACKING_ID = 'googleanalytics/default/tracking_id';
  ZOMATO_URL = 'zomato/default/url';
  ZOMATO_KEY = 'zomato/default/key';
  YANDEX_KEY = 'yandex/default/key';
  COGNITIVE_OCR_TOKEN = 'cognitive/ocr/key';
  OPENWEATHERMAP_KEY = 'openweathermap/default/key';
  WEATHERSTACK_KEY = 'weatherstack/default/key';
  IBACOR_TOKEN = 'ibacor/default/token';
  RAJAONGKIR_TOKEN = 'rajaongkir/default/key';

  PORTALPULSA_USERID = 'PortalPulsa/default/userid';
  PORTALPULSA_KEY = 'PortalPulsa/default/key';
  PORTALPULSA_SECRET = 'PortalPulsa/default/secret';



  KLAUDLESS_APPID = 'klaudless/default/appid';
  KLAUDLESS_APIKEY = 'klaudless/default/apikey';
  KLAUDLESS_CALENDAR_ACCOUNTID = 'klaudless/default/calendar_accountid';
  KLAUDLESS_CALENDAR_ID = 'klaudless/default/calendar_id';

  MASKOFA_SHALAT_SESSIONID = 'maskofa/jadwalshalat/sessionid';
  MASKOFA_SHALAT_CITYLISTS = 'maskofa/jadwalshalat/citylists';

  MSBOTFRAMEWORK_CLIENT_ID = 'msbotframework/default/client_id';
  MSBOTFRAMEWORK_CLIENT_SECRET = 'msbotframework/default/client_secret';

  FFMPEG_PATH = 'systems/ffmpeg_path';
  VOICE_TMP_PATH = 'ztemp/voice/';

  CMD_FULL_IMAGE_ANALYZE = 'full image analyze';
  CMD_IMAGE_TRANSLATION = 'image translation';

  CARIK_TTS_URL = 'carik/tts_url';

  RUMAH123_API_URL = 'rumah123/default/api_url';
  RUMAH123_API_CLIENT_TYPE = 'rumah123/default/api_client_type';
  RUMAH123_LISTING_MAX = 3;

  PAJAKMOTOR_API = 'pajakmotor/api_url';

  STEMMING_ENABLED = 'ai/default/stemming';
  STANDARDWORD_CHECKING = 'ai/default/standard_word_check';

  TRIGGER_INTERVAL = 60;
  TRIGGER_SESSION_PREFIX = 'TRIGGER_';

  // Quick Replay / Button
  QUICK_REPLY_IMAGE_DEFAULT = 'https://listimg.pinclipart.com/picdir/s/337-3378299_vector-drawing-transparent-background-green-button-icon-clipart.png';

  // internal command carik
  CMD_POST_RICH = 'post-rich';
  CMD_JSON_RICH = 'json-rich';
  CommandList: array  [1..2] of string =
    (CMD_POST_RICH, CMD_JSON_RICH);

  NEW_MEMBER_INTERVAL_POST_PERMITTED = 20;
  SPAM_SCORE_THRESHOLD = 80;
  SPAM_WORD = 'ai/default/spam_word';
  SPAM_CAS_OFFENSE = 1; // api.cas.chat

  GROUP_DATA_FILENAME = 'files/carik/carik-groupdata.dat';
  CALLBACK_QUERY_TIMEOUT = 5; // 5 minutes
  CALLBACK_QUERY_TIMEOUT_PREFIX = 30; // 5 minutes
  MESSAGE_TYPE = 'message_type';
  MESSAGE_ACTION_TYPE = 'action_type';
  MESSAGE_ACTION_DATE = 'action_date';
  MESSAGE_ACTION_NAME = 'action_name';
  MESSAGE_ACTION_URL = 'action_url';
  MESSAGE_ACTION_DATA_ = 'action_data_';
  MESSAGE_ACTION_COUNT = 'action_count';
  MESSAGE_ACTION_SUFFIX = 'action_suffix';
  ACTION_ERR_NLP_TOOBIG = 'action_err_too_big';
  // Pilihan menu:
  //ACTION_CAPTION = '\nᴩɪʟɪʜᴀɴ:';
  ACTION_CAPTION = '\nKetik *Nomor* sesuai dengan pilihan berikut:';
  ACTION_SUFFIX = '\nContoh, untuk memilih menu *%s*, ketik: *1*';
  NLP_ERROR_COUNT_MAX = 2;
  NLP_DEFAULT_WEIGHT = 100;
  LABEL_NLP_ERROR_COUNT = 'nec';

  EXTERNAL_ACCESS_TIMEOUT = 3000;
  EXTERNAL_NLP_TIMEOUT = 30000;
  ERR_TIMEOUT_MESSAGE = 'Maaf, saat ini layanan belum bisa diakses. Coba lagi nanti yaa.';
  ERR_INVALID_PARAMETER = 'Invalid parameter.';
  ERR_INVALID_METHOD = 'Invalid method.';

  OPERATION_USERSET = 'userset';
  SPAM_CHECK_GREETING = '"Hi Human, silakan klik tombol ini agar anda tidak dianggap sebagai spammer."';

  // Form Input Handler
  PREVIOUS_ACTION = 'previous_action';
  CURRENT_ACTION = 'current_action';
  WAITING_INPUT = 'waiting_input';
  WAITING_INPUT_DATE = 'waiting_input_date';
  FORM_INPUT_TIMEOUT = 5; // 5 minutes
  FORM_SESSION = 'form_session';
  FORM_INPUT_TIMEOUT_MAX = 60; // 60 minutes
  FORM_INPUT_RESET_CODE = '#';
  FORM_INPUT_RELOAD_CODE = '!';
  FORM_INPUT_EXPIRED = '\nSebelumnya kamu sempat melakukan pengisian form, tapi sayang batas waktu pengisian sudah berakhir.\n\n';
  FORM_INPUT_HASHTAG_CANCEL = '\n*Kirim # untuk membatalkan.*'
      + '\n*Ulangi* jawaban jika Carik lama memberikan respon, atau ketik "*'+FORM_INPUT_RELOAD_CODE+'*" (tanda seru).';
  FORM_INPUT_HASHTAG_CANCEL2 = '\nAtau ketik # untuk membatalkan.'
      + '\n*Ulangi* jawaban jika Carik lama memberikan respon, atau ketik "*'+FORM_INPUT_RELOAD_CODE+'*" (tanda seru).';
  FORM_INPUT_CANCEL = 'Baik, pengisian form %s telah dibatalkan.\nTerima kasih.';
  FORM_INPUT_DATE_FORMAT = '\nTulis dalam format tgl/bln/tahun\nMisal: 17/08/1945';
  FORM_INPUT_OPTION_FORMAT = '\n\nTulis dalam angka sesuai pilihan di atas';
  FORM_INPUT_FILENAME = 'form_filename';
  FORM_INPUT_TITLE = 'input_title';
  FORM_INPUT_NAME = 'input_name';
  FORM_INPUT_TYPE = 'input_type';
  FORM_INPUT_OPTION_COUNT = 'input_option_count';
  //FORM_INDEX = 'form_index';
  FORM_TOPIC = 'form_topic';
  FORM_LAST_MESSAGE_ID = 'form_last_message_id';
  FORM_QUESTION = 'form_question';
  FORM_QUESTION_TOTAL = 'form_question_total';
  FORM_PATH = DirectorySeparator + 'form' + DirectorySeparator;
  FORM_DATA = 'fdata_';
  FORM_ERR_FORMAT_DATE = 'Format jawaban tidak sesuai, pastikan Anda memasukkan tanggal yang benar, format DD/MM/YYYY.\nContoh: 17/08/1945';
  FORM_ERR_FORMAT_EMAIL = 'Jawaban tidak sesuai, pastikan email yang anda masukkan sudah benar.';
  FORM_ERR_FORMAT_NUMERIC = 'Jawaban tidak sesuai, pastikan yang anda masukkan berformat angka/bilangan';
  FORM_ERR_FORMAT_BOOLEAN = 'Jawaban tidak sesuai, pastikan anda menjawab "Y" atau "T".';
  FORM_ERR_FORMAT_OPTION = 'Jawaban tidak sesuai, pastikan yang anda masukkan berformat angka sesuai pilihan di atas.';
  FORM_ERR_FORMAT_OPTION_INVALID = 'Jawaban tidak sesuai, pastikan angka sesuai dengan pilihan di atas.';
  FORM_ERR_SUBMIT_FAILED = 'Mohon maaf, data Anda gagal dikirim. Silakan coba lagi nanti.';
  FORM_ERR_SUBMIT_EXCEPTION = 'Maaf, terjadi kegagalan dalam pengiriman data. Silakan coba lagi nanti.';
