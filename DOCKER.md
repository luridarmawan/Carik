# Panduan Menjalankan Carik dengan Docker & Docker Compose

Dokumen ini menjelaskan cara membangun dan menjalankan **Carik Bot Engine** menggunakan Docker container berbasis image `fastplaz/ubuntu`. Dengan pendekatan container, proses kompilasi Free Pascal / FastPlaz dan penyediaan web server Apache terisolasi dengan rapi tanpa perlu instalasi Free Pascal atau Lazarus di mesin host.

---

## Prasyarat

- [Docker](https://docs.docker.com/get-docker/) (versi 20.10 ke atas)
- [Docker Compose](https://docs.docker.com/compose/) (opsional, disarankan untuk orkestrasi praktis)

---

## 1. Menjalankan Menggunakan Docker Compose (Direkomendasikan)

Cara termudah dan paling praktis untuk menjalankan Carik secara lokal maupun di server:

```bash
# Build image dan jalankan container di background
docker compose up -d --build

# Melihat log container
docker compose logs -f

# Menghentikan container
docker compose down
```

Setelah berjalan, Carik dapat diakses melalui browser atau HTTP client di:
`http://localhost:8080/`

Port default adalah `8080`. Jika ingin menggunakan port lain, set environment variable `PORT`:
```bash
PORT=9000 docker compose up -d
```

### Volume Persistence pada Compose
Docker Compose telah memetakan folder-folder penting ke host:
- `./public_html/carik/config`: Konfigurasi bot (`config.json`)
- `./public_html/carik/data`: Data bot (`.dat`)
- `./public_html/carik/files`: Data NLP, kata baku, kata non-baku, dsb.
- `./public_html/carik/ztemp`: Direktori cache, log, dan temporary files

Perubahan pada file konfigurasi atau kamus di host akan langsung terbaca oleh container tanpa perlu rebuild image.

---

## 2. Menjalankan Menggunakan Docker CLI / Helper Scripts

Jika lebih menyukai pengelolaan langsung dengan script shell yang disediakan:

### a. Build Image
Dari root repositori:
```bash
./docker/build.sh
# atau langsung dengan docker command:
docker build -t carik .
```

### b. Menjalankan Container
```bash
./docker/run.sh
# atau:
docker run -it --rm -p 8080:80 --name carik-bot carik
```

### c. Masuk ke Lingkungan Interaktif (Bash Shell)
Untuk kebutuhan debugging, pengecekan dependensi, atau eksperimen kompilasi manual:
```bash
./docker/bash.sh
# atau:
docker run -it --rm -p 8080:80 --name carik-dev carik bash
```

Di dalam container, source code Carik berada di `/projects/carik` dan vendor library berada di `/projects/vendors/`.

---

## 3. Struktur Direktori & Alur di Container

- `/projects/carik` : Root source code proyek Carik.
- `/projects/vendors/SimpleAI` : Library Natural Language Processing turunan SimpleBOT.
- `/projects/vendors/fastplaz` : Web framework FastPlaz bawaan image base.
- `/var/www/html` : Symlink ke `/projects/carik/public_html/carik`.
- Dokumen root dilayani oleh Apache 2 dengan modul CGI / Rewrite aktif untuk mengeksekusi binary FastPlaz (`carik.bin`, `telegram.bin`, `line.bin`, `facebook.bin`).
