FROM fastplaz/ubuntu AS base

# Configuration
ARG GIT_REPO="https://github.com/luridarmawan/Carik.git"
ARG WEB_DIR="/projects/Carik/public_html"

# Make port 80 available to the world outside this container
EXPOSE 80
EXPOSE 443

# DEPENDENCIES
RUN cd /projects/vendors/ && git clone -b development https://github.com/luridarmawan/SimpleAI.git

# remove old template and pull source from echo repository
RUN git clone -b development $GIT_REPO
#RUN chmod -R 777 $WEB_DIR/ztemp/

# setup apache web dir
RUN mv /var/www/html /var/www/html-old
RUN ln -s $WEB_DIR /var/www/html

# Add file autu run
ADD files/echo-run.sh /app/run.sh
RUN chmod 755 /app/run.sh

