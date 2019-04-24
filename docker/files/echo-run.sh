#!/bin/bash

# pull requirement files
#cd /projects/vendors/fastplaz/
#git pull origin development

# pull update
echo Pull update from echo repository
cd /projects/echo/
git pull

# compile and deploy
echo Compiling ....
cd /projects/echo/source/echo && ./compile.sh

echo
echo Update and Compiling done
echo
echo Running Apache ..
echo
echo Try from your browser
echo http://localhost:8080/?val1=value1
echo
echo Try with any parameters using GET or POST method
echo
echo 
echo press Ctrl+C to exit
/usr/sbin/apache2ctl -D FOREGROUND
