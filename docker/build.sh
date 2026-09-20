#!/bin/bash
echo "Carik - Build Docker Image"

# Build image dari root repositori atau dari folder docker
if [ -f "Dockerfile" ]; then
    docker build -t carik .
elif [ -f "files/Dockerfile" ]; then
    docker build -f files/Dockerfile -t carik ..
elif [ -f "../Dockerfile" ]; then
    docker build -f ../Dockerfile -t carik ..
else
    echo "Dockerfile tidak ditemukan."
    exit 1
fi

echo "Build selesai. Image 'carik' siap dijalankan."
