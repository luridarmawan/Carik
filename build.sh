echo
echo ----- Compiling Telegram Bot
cd source/carik
mkdir lib
fpc carik.lpr @extra.cfg

echo
echo ---- Compiling LINE Bot
cd ../line
mkdir lib
fpc line.lpr @extra.cfg

