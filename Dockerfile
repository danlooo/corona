FROM rocker/shiny-verse:4.0.4
WORKDIR /app

COPY crontab /etc/cron.d/crontab
RUN \
  apt-get update && \
  apt-get -y install cron && \
  chmod 0644 /etc/cron.d/crontab && \
  /usr/bin/crontab /etc/cron.d/crontab

COPY . ./
RUN Rscript install.R
RUN mv shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 80
CMD sh /app/start.sh
