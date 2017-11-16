echo
echo ----- Compiling GENERIC Bot
cd source/carik
mkdir lib
fpc carik.lpr @extra.cfg

echo
echo ---- Compiling LINE Bot
cd ../line
mkdir lib
fpc line.lpr @extra.cfg

echo
echo ---- Compiling TELEGRAM Bot
cd ../telegram
mkdir lib
fpc telegram.lpr @extra.cfg

echo
echo ---- Compiling FACEBOOK Bot
cd ../facebook
mkdir lib
fpc facebook.lpr @extra.cfg

