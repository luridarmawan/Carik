FROM fastplaz/ubuntu AS base

# Arguments & Metadata
ARG DEBIAN_FRONTEND=noninteractive
LABEL maintainer="Carik Community"
LABEL description="Container image for Carik Bot Framework built on FastPlaz"

# Environment
ENV PROJECT_DIR="/projects/carik"
ENV WEB_DIR="/projects/carik/public_html/carik"

# Expose HTTP port
EXPOSE 80

# Pastikan repositori dependensi SimpleAI tersedia di /projects/vendors/
RUN mkdir -p /projects/vendors && \
    if [ ! -d "/projects/vendors/SimpleAI" ]; then \
        git clone -b development https://github.com/luridarmawan/SimpleAI.git /projects/vendors/SimpleAI; \
    fi

# Salin source code proyek Carik ke container
WORKDIR /projects/carik
COPY . /projects/carik

# Setup direktori ztemp dan konfigurasi Apache web root
RUN chmod -R 777 /projects/carik/public_html/carik/ztemp && \
    rm -rf /var/www/html && \
    ln -s /projects/carik/public_html/carik /var/www/html

# Salin startup script
COPY docker/files/carik-run.sh /app/run.sh
RUN chmod 755 /app/run.sh

CMD ["/app/run.sh"]
