#!/bin/bash
echo Carik - Run Container
echo
echo

docker run -it -p 8080:80 carik
#docker run -it -v ~/shared-folder:/shared-folder -p 8080:80 fastplaz/$OS$RELEASE
