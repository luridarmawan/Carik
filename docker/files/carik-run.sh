#!/bin/bash
set -e

echo "=== Carik Bot Container Starting ==="

cd /projects/carik

# Compile bot binary jika belum ada atau saat dibutuhkan
if [ ! -f "public_html/carik/carik.bin" ]; then
    echo "Binary carik.bin belum ditemukan, memulai kompilasi..."
    ./clean.sh 2>/dev/null || true
    ./build.sh
    echo "Kompilasi selesai."
fi

# Pastikan permission direktori temporary aman untuk runtime web server
chmod -R 777 /projects/carik/public_html/carik/ztemp 2>/dev/null || true

echo "Menjalankan Apache web server..."
echo "Akses Carik di: http://localhost:8080/"
exec /usr/sbin/apache2ctl -D FOREGROUND
