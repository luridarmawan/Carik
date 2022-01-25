#!/bin/bash
mkdir -p lib
fpc carik.lpr @extra.cfg
chmod 755 ../../public_html/carik/carik.bin
