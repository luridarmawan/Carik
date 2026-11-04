#!/bin/bash
#
# Build Carik di WSL dengan cepat.
#
# Source tetap diedit di drive Windows (/mnt/d/...), tapi compile dilakukan
# di filesystem native WSL ($BUILD_DIR) karena akses /mnt/* via 9P sangat lambat.
# Hasil carik.bin disalin kembali ke public_html/carik/.
#
# Pemakaian:
#   ./build-wsl.sh          build incremental
#   ./build-wsl.sh clean    hapus cache build (unit .ppu/.o), lalu build ulang
#
# Lokasi build bisa diganti lewat env BUILD_DIR (default: ~/carik-build).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
CARIK_ROOT="$(readlink -f "$SCRIPT_DIR/../..")"          # .../Carik/git/Carik
VENDORS="$(readlink -f "$CARIK_ROOT/../vendors")"         # .../Carik/git/vendors

SRC_FASTPLAZ="$(readlink -f "$VENDORS/fastplaz")/FastPlaz"
SRC_SIMPLEAI="$(readlink -f "$VENDORS/SimpleAI")/source/package/src"

BUILD_DIR="${BUILD_DIR:-$HOME/carik-build}"
B_CARIK="$BUILD_DIR/Carik"
B_VENDORS="$BUILD_DIR/vendors"
OUTPUT_BIN="$CARIK_ROOT/public_html/carik/carik.bin"

for d in "$SRC_FASTPLAZ" "$SRC_SIMPLEAI" "$CARIK_ROOT/source/common"; do
  [ -d "$d" ] || { echo "ERROR: folder tidak ditemukan: $d" >&2; exit 1; }
done

if [ "${1:-}" = "clean" ]; then
  echo ">> clean: hapus $BUILD_DIR"
  rm -rf "$BUILD_DIR"
fi

# sync SRC/ -> DST/ ; file hasil compile & folder lib/ tidak ikut disalin
# dan (untuk rsync) tidak dihapus di tujuan.
sync_dir() {
  local src="$1" dst="$2"
  mkdir -p "$dst"
  if command -v rsync >/dev/null 2>&1; then
    rsync -a --delete \
      --exclude '.git/' --exclude 'lib/' \
      --exclude '*.ppu' --exclude '*.o' --exclude '*.a' \
      --exclude '*.compiled' --exclude '*.bin' --exclude '*.exe' \
      "$src/" "$dst/"
  else
    # fallback tanpa rsync: hanya salin file yang lebih baru (tidak menghapus file lama)
    cp -a -u "$src/." "$dst/"
  fi
}

START=$(date +%s)
echo ">> sync source ke $BUILD_DIR"
sync_dir "$CARIK_ROOT/source/carik"  "$B_CARIK/source/carik"
sync_dir "$CARIK_ROOT/source/common" "$B_CARIK/source/common"
sync_dir "$SRC_FASTPLAZ"             "$B_VENDORS/fastplaz/FastPlaz"
sync_dir "$SRC_SIMPLEAI"             "$B_VENDORS/SimpleAI/source/package/src"
mkdir -p "$B_CARIK/public_html/carik"

echo ">> compile"
cd "$B_CARIK/source/carik"
mkdir -p "lib/$(fpc -iTP)-$(fpc -iTO)"
fpc carik.lpr @extra.cfg

echo ">> salin carik.bin -> $OUTPUT_BIN"
mkdir -p "$(dirname "$OUTPUT_BIN")"
cp -f "$B_CARIK/public_html/carik/carik.bin" "$OUTPUT_BIN"
chmod 755 "$OUTPUT_BIN"

echo ">> selesai dalam $(( $(date +%s) - START )) detik"
