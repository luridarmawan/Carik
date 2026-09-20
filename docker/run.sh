#!/bin/bash
echo "Carik - Run Container"
echo "Menjalankan Carik Bot di container (port 8080)..."
echo "Buka http://localhost:8080/ di browser Anda."
echo

docker run -it --rm -p 8080:80 --name carik-bot carik
