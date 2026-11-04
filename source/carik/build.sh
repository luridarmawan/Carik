#!/bin/bash
mkdir -p lib/$(fpc -iTP)-$(fpc -iTO)
START=$(date +%s)
fpc carik.lpr @extra.cfg

chmod 755 ../../public_html/carik/carik.bin

echo ">> selesai dalam $(( $(date +%s) - START )) detik"

