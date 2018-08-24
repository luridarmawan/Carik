#!/bin/bash
echo Carik - Run Container
echo
echo Sample Project: Carik Bot Framework
echo   dir: /projects/carik
echo 
echo How to compile:
echo   $ cd /projects/carik/source/echo/
echo   $ ./clean.sh
echo   $ ./compile.sh
echo
echo Output File:
echo   /projects/echo/public_htm/carik.bin
echo

docker run -it -p 8080:80 carik bash 
#docker run -it -v ~/shared-folder:/shared-folder -p 8080:80 carik bash
