

# AI 123

## What is it?

AI 123 merupakan bot sederhana yang digunakan unt melakukan stemming terhadap input user sehingga menghasilkan result search sesuai suggestion.
 
Memiliki fitur menjawab otomatis, dan belajar suatu definisi kata sederhana.
Kecerdasan Bot ini tergantung dari data entity dan intent yang Anda miliki, serta logic handler yang Anda buat.

Contoh penggunaan bot sederhana dengan SimpleBOT ini bisa anda coba dari situs [ai.fastplaz.com](http://ai.fastplaz.com) atau bisa melalu aplikasi chat **Telegram**, silahkan hubungi contact *'Fastplaz Bot'*.

Aplikasi AI 123 ini sudah kompatibel dengan Telegram API, sehingga anda bisa menggunakannya sebagai Telegram Bot.

## Why use it?

**Ringan Tanpa Beban**

AI123 merupakan turunan dari SimpleBOT, dan SimpleBOT sendiri dibuat dengan sederhana, simple dan ringan. SimpleBOT adalah _binary application_ sehingga diharapkan akan lebih cepat dan ringan. 

Kompatibel dengan shared hosting umumnya.

Untuk penggunaan custom, cukup dibutuhkan instalasi Apache Web Server regular.


## How to use it


### Requirements

- [FastPlaz_runtime](http://www.fastplaz.com/)
- [SimpleBOT package](https://github.com/luridarmawan/SimpleAI/)

### Instalasi

**install requirement**

```bash
$ mkdir -p AI123/source/vendors
$ cd AI123/source
$ git clone git@git.realestate.com.au:rumah123/ai123.git

# change to branch development

# install vendors
#   change to branch development if needed

$ cd vendors
$ git clone https://github.com/fastplaz/fastplaz.git
$ git clone https://github.com/luridarmawan/SimpleAI.git


```

**Compile dari IDE**

Jika menggunakan Lazarus, buka file "ai123.lpi" dan *compile* file tersebut.

Akan terbentuk file binary di 'public_html/ai123.bin'

**Compile dari Command-Line**

```bash
cd ai123
./clean.sh
./build.sh
.
.
ai123.lpr(13,124)
Assembling (pipe) lib/ai123.s
Compiling resource lib/ai123.or
Linking ../../public_html/ai123.bin
.
.
source$ _

```

**Custom Build**

untuk konfigurasi custom, misal untuk perubahan path tempat library berada, bisa dilakukan dengan melakukan modifikasi di file **extra.cfg**.


### SimpleBOT USAGE

```delphi
  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.OnError := @OnErrorHandler;  // Your Custom Message
  SimpleBOT.Handler['isipulsa'] := @customHandler; // Custom Handler
  SimpleBOT.Handler['property_search'] := @propertySearchHandler; // another custom Handler
  text_response := SimpleBOT.Exec(Text);
  SimpleBOT.Free;

```

Fungsi 'OnErrorHandler' bisa digunakan untuk melakukan trapping terhadap kata/kalimat yang belum diakomodir oleh data SimpleAI

```delphi
function TMainModule.OnErrorHandler(const Message: string): string;
begin
  .
  .
  .
  // save to log file
  LogUtil.Add(Message, 'AI');
  
  // or save to database
  .
  .
  Result := 'Your custom messages';
end;
```


### Input

method: POST

data disematkan di dalam body post, dengan format berikut

```
{"message":{"message_id":0,"text":"Your Message","chat":{"id":0}}}
```

format ini mengikuti pola message dari Telegram.


### Format JSON Output

```
{
	"code": 0,
	"request": {
		"text": ""
	},
	"response": {
		"intents": {
			"action": "",
			"name": "",
			"parameters": {}
		},
		"text": []
	}
}
```



### Pengujian

Pengujian dari command-line bisa dilakukan dengan syntax berikut:

```
curl "http://local-bot.rumah123.com/folder/" -X POST -d '{"message":{"message_id":0,"chat":{"id":0},"text":"Hi"}}'
```

atau bisa dengan menggunakan aplikasi RESTClient lainnya.


![Format](img/format_01.png "Format")


### Web Folder Structure

![Folder Structure](img/folder-structure.png "Folder Structure")


### Video Tutorial

![Video Tutorial](img/video-tutorial.gif "Video Tutorial")


